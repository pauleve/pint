
open Ph_types;;

type piproc_arg_action = ArgReset | ArgUpdate of (string * string) list;;

(* spim_of_ph with stochasticity_absorption *)
let spim_of_ph2 (ps,hits) init_state properties =
	let chanl_of_process p = "l_"^p
	and p_level (p,l) = p^"("^string_of_int l^")"
	and pi_name_level (p,l) = p^string_of_int l
	in
	let register_metaproc piprocs (p,l) =
		piprocs @ List.map (fun l -> (p,l),[]) (Util.range 0 l)
	in
	let piprocs = List.fold_left register_metaproc [] ps
	in
	let add_pichoice piprocs piproc pi =
		let pis = List.assoc piproc piprocs
		in
		(piproc,pi::pis)::List.remove_assoc piproc piprocs
	in
	let register_hit p2 ((p1,r),l) (piprocs,channels,counter) =
		let cid = "hit"^string_of_int counter
		and p2' = (fst p2,l)
		and notify_level = "!"^chanl_of_process (fst p2)^"("^string_of_int l^")"
		in
		let i_cid = "i_"^cid
		and next = notify_level^";%%", [p2',ArgReset]
		in
		let stochasticity_absorption pl action (nexts,nextd) = match r with 
			    RateInf -> action^nexts, nextd
			  | Rate _ -> action^"if "^i_cid^" = 1 then ("^nexts^") else (%%)", nextd@[pl,ArgUpdate [i_cid,"-1"]]
		in
		let piprocs = 
			if p1 = p2 then (
				(* delay *)
				let action = if r <> RateInf then "delay@"^cid^";" else ""
				in
				let pi = stochasticity_absorption p2 action next
				in
				add_pichoice piprocs p2 pi
			) else (
				let piprocs = add_pichoice piprocs p1 ("!"^cid^";%%", [p1,ArgUpdate []])
				in
				let pi = stochasticity_absorption p2 ("?"^cid^";") next
				in
				add_pichoice piprocs p2 pi
			)
		in
		let c = (cid, r, p2 <> p1)
		in
		(piprocs,c::channels,counter+1)
	in
	let piprocs,channels,counter = Hashtbl.fold register_hit hits (piprocs,[],0)
	in
	let extract_args (piproc, pi) =
		let extract_args (_, d) = List.flatten 
			(List.map (fun (pl,arg_action) -> match arg_action with ArgUpdate args -> List.map fst args | _ -> []) d)
		in
		piproc, List.flatten (List.map extract_args pi)
	in
	let piprocs_args = List.map extract_args piprocs
	in

	let string_of_picall (pl, arg_action) =
		let args = List.assoc pl piprocs_args
		in
		(pi_name_level pl)^"("^(String.concat "," (match arg_action with
		  ArgUpdate au -> List.map (fun arg ->
		  	arg^try List.assoc arg au with Not_found -> "") args
		| ArgReset -> List.map (fun arg -> "sa") args
		))^")"
	in

	let string_of_pi piproc (pis, pid) =
		Util.string_apply "%%" pis (List.map string_of_picall pid)
	in
		
	let string_of_channel (cid, rate, ischan) = match ischan with
		  true -> "new "^cid^(match rate with 
		  				  Rate f -> "@("^Util.string_of_float0 f^"*float_of_int sa)"
						| RateInf -> "")^":chan"
		| false -> (match rate with
						  Rate f -> "val "^cid^"="^Util.string_of_float0 f^"*float_of_int sa"
						| RateInf -> "")
	and string_of_piproc (piproc, choices) =
		let args = List.assoc piproc piprocs_args
		in

		(pi_name_level piproc)^"("^(String.concat "," (List.map
			(fun arg -> arg^":int") args))^") = "
		^ match choices with
		  [] -> "!dead"
		| [pi] -> string_of_pi piproc pi
		| pis -> "do "^String.concat " or " (List.map (string_of_pi piproc) pis)
	in

	let def_level_channels = String.concat "\n"
		(List.map (fun (p,l) -> "new "^chanl_of_process p^":chan(int)") ps)
	and pl_to_plot = String.concat ";" (List.flatten
		(List.map (fun (p,l) -> List.map (fun l -> p_level (p,l)) (Util.range 0 l)) ps))
	and pl = "let "^String.concat "\nand "
		(List.map (fun (p,l) -> p^"(level:int) = ?"^chanl_of_process p^"(level); "^p^"(level)") ps)
	in
	
	let defs = "new dead:chan\n" ^ 
		(String.concat "\n" (List.map string_of_channel channels))
	and body = "let "^
		String.concat "\nand " (List.map string_of_piproc piprocs)

	and directives = String.concat "\n" [
		"directive sample "^Util.string_of_float0 (float_of_string (List.assoc "sample" properties));
		"directive plot "^pl_to_plot;
		"\nval sa = "^(List.assoc "stochasticity_absorption" properties)^" (* stochasticity absorption *)";
		"\n(* level watchers *)";
		def_level_channels; pl
	]

	and run = "run ("^(String.concat " | " 
					(List.map (fun ((n,_),l) -> string_of_picall ((n,l),ArgReset)
						^ "|" ^ p_level (n,l))
							(List.combine ps init_state))) ^ ")\n"
	in
	directives ^ "\n\n" ^ defs ^ "\n\n" ^ body ^ "\n\n" ^ run

let spim_of_ph (ps,hits) init_state properties =
	let name_of_process (p,l) = p^string_of_int l^"()"
	in
	let register_metaproc piprocs (p,l) =
		piprocs @ List.map (fun l -> (name_of_process (p,l),[])) (Util.range 0 l)
	in
	let piprocs = List.fold_left register_metaproc [] ps
	in
	let add_pichoice piprocs piproc pi =
		let pis = List.assoc piproc piprocs
		in
		(piproc,pi::pis)::List.remove_assoc piproc piprocs
	in
	let register_hit p2 ((p1,r),l) (piprocs,channels,counter) =
		let cid = "hit"^string_of_int counter
		and p2' = (fst p2,l)
		in
		let np1 = name_of_process p1
		and np2 = name_of_process p2
		and np2' = name_of_process p2'
		in
		let piprocs = 
			if p1 = p2 then (
				(* delay *)
				let pi = if r = RateInf then np2' else ("delay@"^cid^";"^np2')
				in
				add_pichoice piprocs np2 pi
			) else (
				let pi1 = "!"^cid^";"^np1
				and pi2 = "?"^cid^";"^np2'
				in
				let piprocs = add_pichoice piprocs np1 pi1
				in
				add_pichoice piprocs np2 pi2
			)
		in
		let c = (cid, r, p2 <> p1)
		in
		(piprocs,c::channels,counter+1)
	in
	let piprocs,channels,counter = Hashtbl.fold register_hit hits (piprocs,[],0)
	in
	let string_of_channel (cid, rate, ischan) = match ischan with
		  true -> "new "^cid^(match rate with 
		  				Rate f -> "@"^Util.string_of_float0 f
						| RateInf -> "")^":chan"
		| false -> (match rate with
						Rate f -> "val "^cid^"="^Util.string_of_float0 f
						| RateInf -> "")
	and string_of_piproc (piproc, choices) =
		piproc ^ " = " ^ match choices with
				  [] -> "!dead"
				| [pi] -> pi
				| pis -> "do "^String.concat " or " pis
	in
	
	let defs = "new dead:chan\n" ^ 
		(String.concat "\n" (List.map string_of_channel channels))
	and body = "let "^
		String.concat "\nand " (List.map string_of_piproc piprocs)

	and directives = String.concat "\n" [
		"directive sample "^Util.string_of_float0 (float_of_string (List.assoc "sample" properties));
		"directive plot "^String.concat ";" (fst (List.split piprocs))]

	and run = "run ("^(String.concat " | " 
					(List.map (fun ((n,_),l) -> name_of_process (n,l))
						(List.combine ps init_state))) ^ ")\n"
	in
	directives ^ "\n\n" ^ defs ^ "\n\n" ^ body ^ "\n\n" ^ run
;;

