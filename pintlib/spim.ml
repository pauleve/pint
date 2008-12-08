
open Goal;;

type proc = string
type chan = Delay of string | Channel of string
type action = Call of chan | Take of chan
;;

let proc_of_ml m l = m^string_of_int l
;;

let string_of_chan = function
	  Delay s -> "delay@"^s
	| Channel s -> s
;;
let string_of_action = function
	  Call (Delay d) -> string_of_chan (Delay d)
	| Call c -> "?"^string_of_chan c
	| Take c -> "!"^string_of_chan c
;;

let append_smap k vl smap =
	let vl' = try SMap.find k smap with Not_found -> []
	in
	SMap.add k (vl::vl') smap
;;

let decisions_of_rules mrules =
	(* group by metaproc/level *)
	let folder (m,l,a) conds mproc =
		let l' = match a with Goal.Inc -> l+1 | Goal.Dec -> l-1
		in
		let p = proc_of_ml m l
		and p' = proc_of_ml m l'
		in
		let choices = List.map (fun cond -> (cond,p')) conds
		in
		let choices' = try SMap.find p mproc with Not_found -> []
		in
		SMap.add p (choices'@choices) mproc
	in
	RuleMap.fold folder mrules SMap.empty
;;
let ps_of_cond cond =
	let folder m dom ps =
		let levels = Domain.elements dom
		in assert (List.length levels = 1);
		(proc_of_ml m (List.hd levels))::ps
	in
	SMap.fold folder cond []
;;

let channels_of_rules mrules =
	let mdecs = decisions_of_rules mrules
	in
	let channels p chans (cond,p') =
		match ps_of_cond cond with
			  [] -> Delay (p^p')::chans
			| ps -> Channel ((String.concat "" ps)^p^p')::chans
	in
	let folder p choices chans =
		List.fold_left (channels p) chans choices
	in
	SMap.fold folder mdecs []
;;

let body_of_rules mrules init =

	let mdecs = decisions_of_rules mrules
	in
	(* build actions from decisions *)
	let string_of_controller ps =
		let term q = if q.[0] = '_' then String.sub q 1 (String.length q - 1)
			else q
		in
		"c_"^String.concat "" (List.sort (fun p p' -> compare (term p) (term p')) ps)
	in
	let build p (mcall,mtake,dchans,ichans,obs) (cond,p') =
		match ps_of_cond cond with
			  [] -> (* delay *)
				let chan = Delay (p^p')
				in
				let mcall = append_smap p ([Call chan],p') mcall
				and dchans = chan::dchans
				in
				mcall,mtake,dchans,ichans,obs

			| [q] -> (* simple reaction *)
				let chan = Channel (q^p^p')
				in
				let mcall = append_smap p ([Call chan],p') mcall
				and mtake = append_smap q ([Take chan],q) mtake
				and dchans = chan::dchans
				in
				mcall,mtake,dchans,ichans,obs

			| ps -> (* multiple reaction *)
				let q = string_of_controller ps
				in
				(* plug controller to p *)
				let chan = Channel ((String.concat "" ps)^p^p')
				in
				let mcall = append_smap p ([Call chan],p') mcall
				and mtake = append_smap q ([Take chan],q) mtake
				and dchans = chan::dchans
				and obs = ps::obs
				in
				mcall,mtake,dchans,ichans,obs
	in
	let folder p choices (mcall,mtake,dchans,ichans,obs) =
		List.fold_left (build p) (mcall,mtake,dchans,ichans,obs) choices
	in
	let mcall,mtake,dchans,ichans,obs = 
		SMap.fold folder mdecs (SMap.empty,SMap.empty,[],[],[])
	in

	(* make observers *)
	let take_before_moving p' lchan rchan p choices =
		let apply (actions, q) =
			(if p = p' && q <> p' then
				actions@[Take lchan]
			else if p <> p' && q = p' then
				actions@[Take rchan]
			else actions),q
		in
		List.map apply choices
	in
	let folder (mcall,mtake,dchans,ichans) ps =
		let q = string_of_controller ps
		in
		let register_proc (mcall,mtake,dchans,ichans) p =
			let lchan = Channel (q^"_"^p)
			and rchan = Channel (q^p)
			in
			let mcall = SMap.mapi (take_before_moving p lchan rchan) mcall
			and ichans = lchan::rchan::ichans
			in
			mcall,mtake,dchans,ichans
		in
		let mcall,mtake,dchans,ichans = 
			List.fold_left register_proc (mcall,mtake,dchans,ichans) ps
		in
		let bps = List.map (fun p -> [[p];["_"^p]]) ps
		in
		let all_states = List.map List.flatten (Util.cross_list bps)
		in
		let register_state (mcall,mtake,dchans,ichans) ps =
			let p = string_of_controller ps
			in
			let folder (mcall,mtake,dchans,ichans) w =
				let nw = if w.[0] = '_' then String.sub w 1 (String.length w - 1)
						else ("_"^w)
				in
				let p' = string_of_controller (Util.list_replace w nw ps)
				in
				let mcall = append_smap p ([Call (Channel (q^nw))],p') mcall
				in
				mcall,mtake,dchans,ichans
			in
			List.fold_left folder (mcall,mtake,dchans,ichans) ps
		in
		List.fold_left register_state (mcall,mtake,dchans,ichans) all_states
	in
	let mcall,mtake,dchans,ichans =
		List.fold_left folder (mcall,mtake,dchans,ichans) obs
	in

	(* merge mcall and mtake *)
	let folder p choices mspim =
		let choices' = try SMap.find p mspim with Not_found -> []
		in
		SMap.add p (choices'@choices) mspim
	in
	let mspim = SMap.fold folder mtake mcall
	in

	(* string conversions *)
	let string_of_def_ichan = function
		Channel c -> "new "^c^":chan"
		| _ -> failwith "delays cannot be internals"
	in
	let string_of_choice (actions,p) =
		let string_of_actions actions = 
			String.concat ";" (List.map string_of_action actions)
		in
		(* try to split call and take *)
		let folder (call,take) action =
			match action with
				  Call _ -> assert (take = []);
					action::call, take
				| Take _ -> call, action::take
		in
		let calls,takes = List.fold_left folder ([],[]) actions
		in
		match calls with
			  [] -> string_of_actions takes^";"^p^"()"
			| _ -> 
				let p = if List.length takes > 0 then
							"("^(string_of_actions takes)^"|"^p^"())"
						else (p^"()")
				in
				string_of_actions calls^";"^p
	in
	let folder p choices sl =
		let s = p^"() = "^
			match choices with
				  [] -> failwith "TODO: proc without actions"
				| [c] -> string_of_choice c
				| _ -> "do "^String.concat " or " 
					(List.map string_of_choice choices)
		in
		s::sl
	in
	let sichans = String.concat "\n" (List.map string_of_def_ichan ichans)
	and sdefs = SMap.fold folder mspim []
	in
	(* by default set all rates to 1. *)
	let dchans = List.map (fun chan -> chan,1.) dchans
	in

	(* initial run *)
	let folder m l species =
		proc_of_ml m l::species
	in
	let species = SMap.fold folder init []
	in
	let obs_state species ps =
		let ps' = List.map (fun p -> (if List.mem p species then "" else "_")^p) ps
		in
		string_of_controller ps'
	in
	let species = species @ (List.map (obs_state species) obs)
	in
	let srun = "run ("^(String.concat " | " (List.map (fun p -> p^"()") species))^")"
	in


	dchans,
	sichans^"\n\n"^
	"\tlet\n"^(String.concat "\n\tand\n" sdefs)^"\n\n"^
	srun^"\n"
;;

let update_channels_rate rate channels =
	let replace (c,r) =
		if List.mem c channels then (c,rate) else (c,r)
	in
	List.map replace
;;

let string_of_rate rate = 
	let s = string_of_float rate
	in
	s ^ if s.[String.length s - 1] = '.' then "0" else ""
;;

let string_of_dchans dchans =
	let string_of_dchan (chan, rate) =
		match chan with
			  Delay s -> "val "^s^" = "^string_of_rate rate
			| Channel s -> "new "^s^"@"^(string_of_rate rate)^":chan"
	in
	String.concat "\n" (List.map string_of_dchan dchans)
;;

let species_of_mdom mdom = 
	let folder m dom species =
		species @ List.map (fun l -> m^(string_of_int l)^"()")
					(Domain.elements dom)
	in
	SMap.fold folder mdom []
;;

let full_source dchans body toplot headers =
	String.concat "\n" headers^"\n"^
	"directive plot "^(String.concat ";" toplot)^"\n\n"^
	(string_of_dchans dchans)^"\n\n"^body
;;


