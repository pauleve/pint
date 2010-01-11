
open Ph_types;;

type opts = {
	alpha: float; (* 1-confidence *)
	round_fi: float * float -> int * int (* firing interval rounding *)
}

let string_of_float0 = Util.string_of_float0;;

type piproc_arg_action = ArgReset | ArgUpdate of (string * string) list;;

(* spim_of_ph with stochasticity_absorption *)
let spim_of_ph (ps,hits) init_state =
	let chanl_of_process a = "l_"^a
	and p_level (a,i) = a^"("^string_of_int i^")"
	and p_name (a,i) = a^string_of_int i
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
	let register_hit p2 ((p1,rsa),l) (piprocs,channels,counter) =
		let hitid = "hit"^string_of_int counter
		and p2' = (fst p2,l)
		and notify_level = "!"^chanl_of_process (fst p2)^"("^string_of_int l^")"
		in
		let hitcounter = "c_"^hitid
		and nexts, nextd = notify_level^";%%", [p2',ArgReset]
		in
		let stochasticity_absorption p action = match rsa with
			  None | Some(_,1) -> (action^nexts, nextd)
			| Some(_,sa) -> (
				action^"if "^hitcounter^" = "^string_of_int sa^" then ("^
					nexts^") else (%%)", nextd@[p,ArgUpdate [hitcounter,"+1"]])
		in
		let piprocs = 
			if p1 = p2 then (
				(* delay *)
				let action = "delay@"^hitid^";"
				in
				let pi = stochasticity_absorption p1 action
				in
				add_pichoice piprocs p2 pi
			) else (
				(* a_i *)
				let pi = ("!"^hitid^";%%", [p1,ArgUpdate []])
				in
				let piprocs = add_pichoice piprocs p1 pi
				in
				(* b_j *)
				let pi = stochasticity_absorption p2 ("?"^hitid^";")
				in
				add_pichoice piprocs p2 pi
			)
		in
		let c = (hitid, rsa, p2 <> p1)
		in
		(piprocs,c::channels,counter+1)
	in
	let piprocs,channels,counter = Hashtbl.fold register_hit hits (piprocs,[],0)
	in
	let extract_args (p, pis) =
		let extract_args (_, d) = List.flatten 
			(List.map (fun (p,arg_action) -> match arg_action with 
				  ArgUpdate args -> List.map fst args
				| _ -> []) d)
		in
		p, List.flatten (List.map extract_args pis)
	in
	let p_args = List.map extract_args piprocs
	in

	let string_of_picall (p, arg_action) =
		let args = List.assoc p p_args
		in
		(p_name p)^"("^(String.concat "," (match arg_action with
		  ArgUpdate au -> List.map (fun arg ->
		  	arg^try List.assoc arg au with Not_found -> "") args
		| ArgReset -> List.map (fun arg -> "1") args
		))^")"
	in

	let string_of_pi piproc (pis, pid) =
		Util.string_apply "%%" pis (List.map string_of_picall pid)
	in
		
	let string_of_channel (hitid, rsa, ischan) =
		match rsa with
			  None -> assert ischan; ("new "^hitid^":chan")
			| Some (r,sa) ->
				let r = string_of_float0 (r *. float_of_int sa)
				in
				if ischan then ("new "^hitid^"@"^r^":chan") else ("val "^hitid^"="^r)
	and string_of_piproc (p, choices) =
		let args = List.assoc p p_args
		in
		(p_name p)^"("^(String.concat "," 
				(List.map (fun arg -> arg^":int") args))^") = "
		^ match choices with
		  [] -> "!dead"
		| [pi] -> string_of_pi p pi
		| pis -> "do "^String.concat " or " (List.map (string_of_pi p) pis)
	in

	(* plot list *)
	let plot_list = String.concat ";" (List.flatten
		(List.map (fun (a,l_a) -> 
			List.map (fun i -> p_level (a,i)) (Util.range 0 l_a)) ps))
	

	(* level changes notifiers *)
	and def_notifiers = String.concat "\n"
		(List.map (fun (a,l_a) -> "new "^chanl_of_process a^":chan(int)") ps)

	and notifiers = "let "^String.concat "\nand "
		(List.map (fun (a,l_a) -> 
			a^"(level:int) = ?"^chanl_of_process a^"(level); "^a^"(level)") ps)
	in

	(* channels definitions *)
	let defs = "new dead:chan\n" ^ 
		(String.concat "\n" (List.map string_of_channel channels))
	
	(* processes definitions *)
	and body = "let "^
		String.concat "\nand " (List.map string_of_piproc piprocs)^
		"\n\nlet w() = delay@0.5; w()" (* wake-up process *)

	(* initial state *)
	and run = "run ("^(String.concat " | " (List.map (fun p -> 
						string_of_picall (p,ArgReset)^ "|" ^ p_level p)
						(list_of_state init_state))) ^
					"| w())\n"
	in
	String.concat "\n" [
		"directive sample "^Util.string_of_float0 Ph_types.directive.sample;
		"directive plot w();"^plot_list;
		"";
		"(* level changes notifiers *)";
		def_notifiers;
		notifiers;
		"";
		defs;
		"";
		body;
		"";
		run
	]
;;


let prism_mdp_of_ph (ps,hits) init_state =
	let modname p = "proc_"^p
	and statemod p = p
	in

	let module_of_proc (a,l_a) =
		let decl = (statemod a)^": [0.."^(string_of_int (max 1 l_a))^"] init "^
					(string_of_int (state_value init_state a))
					^"; // state"
		in
		(a, ([decl],[],[]))
	in
	let modules = List.map module_of_proc ps
	in

	let module_update modules (id,(decls,actions,counters)) =
		let _decls,_actions,_counters = List.assoc id modules
		in
		(id, (_decls@decls,_actions@actions,_counters@counters))
		::List.remove_assoc id modules
	in
	let modules_update = List.fold_left module_update
	in
	let string_of_module (a, (decls, actions,counters)) =
		let reset_counters = "("^(String.concat "'=1) & (" counters)^"'=1)"
		in
		let apply = Str.global_replace (Str.regexp_string "%%") reset_counters
		in
		"module "^(modname a)^"\n"^
		"\t"^(String.concat "\n\t" decls)^"\n\n"^
		"\t"^apply (String.concat "\n\t" actions)^"\n\n"^
		"endmodule"
	in

	let prism_is_state a i = 
		statemod a^"="^string_of_int i
	and prism_set_state a i' =
		"("^statemod a^"'="^string_of_int i'^")"
	in

	let register_hit (b,j) (((a,i),rsa),k) (modules, hitid) =
		let modules =
			if (a,i) = (b,j) then (
				let mod_a = (
						[],
						["[] "^prism_is_state a i^" -> "^prism_set_state a k^";"],
						[]
					)
				in
				modules_update modules [a,mod_a]
			) else (
				let sync = "[h_"^string_of_int hitid^"] "
				in
				let action_a = sync^prism_is_state a i^" -> "
					^prism_set_state a i^";"
				and mod_b = (
						[],
						[sync^prism_is_state b j^" -> "^prism_set_state b k^";"],
						[]
					)
				in
				modules_update modules [a,([],[action_a],[]);b,mod_b]
			)
		in modules, hitid + 1
	in
	let modules, _ = Hashtbl.fold register_hit hits (modules,0)
	in


	let header = "mdp"
	in
	header ^ "\n\n" ^ (String.concat "\n\n" (List.map string_of_module modules))
			^ "\n\n"
;;

let prism_of_ph (ps,hits) init_state =
	let modname p = "proc_"^p
	and statemod p = p
	and hitcounter hitid = "c_"^string_of_int hitid
	in

	let module_of_proc (a,l_a) =
		let decl = (statemod a)^": [0.."^(string_of_int (max 1 l_a))^"] init "^
					(string_of_int (state_value init_state a))
					^"; // state"
		in
		(a, ([decl],[],[]))
	in
	let modules = List.map module_of_proc ps
	in

	let module_update modules (id,(decls,actions,counters)) =
		let _decls,_actions,_counters = List.assoc id modules
		in
		(id, (_decls@decls,_actions@actions,_counters@counters))
		::List.remove_assoc id modules
	in
	let modules_update = List.fold_left module_update
	in
	let string_of_module (a, (decls, actions,counters)) =
		let reset_counters = "("^(String.concat "'=1) & (" counters)^"'=1)"
		in
		let apply = Str.global_replace (Str.regexp_string "%%") reset_counters
		in
		"module "^(modname a)^"\n"^
		"\t"^(String.concat "\n\t" decls)^"\n\n"^
		"\t"^apply (String.concat "\n\t" actions)^"\n\n"^
		"endmodule"
	in

	let prism_is_state a i = 
		statemod a^"="^string_of_int i
	and prism_set_state a i' =
		"("^statemod a^"'="^string_of_int i'^")"
	in

	let register_hit (b,j) (((a,i),rsa),k) (modules, hitid) =
		let modules =
			let str_r, sa = match rsa with
				None -> "", 1
				| Some(r,sa) ->
					string_of_float0 (r *. float_of_int sa)^": ", sa
			in
			if (a,i) = (b,j) then (
				let mod_a = 
					if sa = 1 then (
						[],
						["[] "^prism_is_state a i^" -> "^str_r^prism_set_state a k^";"],
						[]
					) else (
						let hc = hitcounter hitid
						in
						[hc ^": [1.."^string_of_int sa^"] init 1;"],
						["[] "^prism_is_state a i^" & "^hc^"<"^string_of_int sa^
								" -> "^str_r^"("^hc^"'="^hc^"+1);"
						;"[] "^prism_is_state a i^" & "^hc^"="^string_of_int sa^
								" -> "^str_r^prism_set_state a k^" & %%;"],
						[hc]
					)
				in
				modules_update modules [a,mod_a]
			) else (
				let sync = "[h_"^string_of_int hitid^"] "
				in
				let action_a = sync^prism_is_state a i^" -> "^
					str_r^prism_set_state a i^";"
				and mod_b =
					if sa = 1 then (
						[],
						[sync^prism_is_state b j^" -> "^prism_set_state b k^";"],
						[]
					) else (
						let hc = hitcounter hitid
						in
						[hc ^": [1.."^string_of_int sa^"] init 1;"],
						[sync^prism_is_state b j^" & "^hc^"<"^string_of_int sa^
								" -> ("^hc^"'="^hc^"+1);"
						;sync^prism_is_state b j^" & "^hc^"="^string_of_int sa^
								" -> "^
								prism_set_state b k^" & %%;"],
						[hc]
					)
				in
				modules_update modules [a,([],[action_a],[]);b,mod_b]
			)
		in modules, hitid + 1
	in
	let modules, _ = Hashtbl.fold register_hit hits (modules,0)
	in


	let header = "ctmc"
	in
	header ^ "\n\n" ^ (String.concat "\n\n" (List.map string_of_module modules))
			^ "\n\n"
;;

(*
let prism2_of_ph (ps,hits) init_state =
	let modname p = "proc_"^p
	and statemod p = p
	and hitcounter hitid = "c_"^string_of_int hitid
	in

	let module_of_proc (a,l_a) =
		let decl = (statemod a)^": [0.."^(string_of_int l_a)^"] init "^
					(string_of_int (List.assoc a init_state))
					^"; // state"
		in
		(a, ([decl],[],[]))
	in
	let modules = List.map module_of_proc ps
	in

	let module_update modules (id,(decls,actions,counters)) =
		let _decls,_actions,_counters = List.assoc id modules
		in
		(id, (_decls@decls,_actions@actions,_counters@counters))
		::List.remove_assoc id modules
	in
	let modules_update = List.fold_left module_update
	in
	let string_of_module (a, (decls, actions,counters)) =
		let reset_counters = "("^(String.concat "'=1) & (" counters)^"'=1)"
		in
		let apply = Str.global_replace (Str.regexp_string "%%") reset_counters
		in
		"module "^(modname a)^"\n"^
		"\t"^(String.concat "\n\t" decls)^"\n\n"^
		"\t"^apply (String.concat "\n\t" actions)^"\n\n"^
		"endmodule"
	in

	let prism_is_state a i = 
		statemod a^"="^string_of_int i
	and prism_set_state a i' =
		"("^statemod a^"'="^string_of_int i'^")"
	in

	let register_hit (b,j) (((a,i),(r,sa)),k) (modules, hitid) =
		let modules =
			let sa = sa_value sa
			in
			let r = string_of_float0 (r *. float_of_int sa)
			in
			if (a,i) = (b,j) then (
				let mod_a = 
					if sa = 1 then (
						[],
						["[] "^prism_is_state a i^" -> "^r^": "^prism_set_state a k^";"],
						[]
					) else (
						let hc = hitcounter hitid
						in
						[hc ^": [1.."^string_of_int sa^"] init 1;"],
						["[] "^prism_is_state a i^" & "^hc^"<"^string_of_int sa^
								" -> "^r^": ("^hc^"'="^hc^"+1);"
						;"[] "^prism_is_state a i^" & "^hc^"="^string_of_int sa^
								" -> "^r^": "^prism_set_state a k^" & %%;"],
						[hc]
					)
				in
				modules_update modules [a,mod_a]
			) else (
				let sync = "[h_"^string_of_int hitid^"] "
				in
				let action_a = sync^prism_is_state a i^" -> "^
					r^": "^prism_set_state a i^";"
				and mod_b =
					if sa = 1 then (
						[],
						[sync^prism_is_state b j^" -> "^prism_set_state b k^";"],
						[]
					) else (
						let hc = hitcounter hitid
						in
						[hc ^": [1.."^string_of_int sa^"] init 1;"],
						[sync^prism_is_state b j^" & "^hc^"<"^string_of_int sa^
								" -> ("^hc^"'="^hc^"+1);"
						;sync^prism_is_state b j^" & "^hc^"="^string_of_int sa^
								" -> "^
								prism_set_state b k^" & %%;"],
						[hc]
					)
				in
				modules_update modules [a,([],[action_a],[]);b,mod_b]
			)
		in modules, hitid + 1
	in
	let modules, _ = Hashtbl.fold register_hit hits (modules,0)
	in


	let header = "ctmc"
	in
	header ^ "\n\n" ^ (String.concat "\n\n" (List.map string_of_module modules))
			^ "\n\n"
;;
*)

let dump_of_ph (ps,hits) init_state =
(*	(String.concat "\n" (List.map (fun (pname, pvalue) -> 
	"directive "^pname^" "^pvalue) properties))
	^"\n\n"^ *)
	(String.concat "\n" (List.map (fun (a,i) ->
	"process "^a^" "^string_of_int i) ps))
	^"\n\n"^
	let string_of_rsa = function
		  None -> " @ Inf"
		| Some(r,sa) -> " @ "^string_of_float r^" ~ "^string_of_int sa
	in
	let string_of_hits (b,j) (((a,i),rsa),k) str =
		str^
		a^" "^string_of_int i^" -> "^b^" "^string_of_int j^" "^string_of_int k
		^string_of_rsa rsa
		^"\n"
	in
	Hashtbl.fold string_of_hits hits ""
	^"\n\n"
;;

let romeo_pid (ps,_) (a,i) =
	let sorts = List.map fst ps
	in
	let sort_id a = Util.index_of a sorts
	in
	let base_id = let folder acc (a,i) =
			(i+1+List.hd acc)::acc
		in List.rev (List.fold_left folder [1] ps)
	in
	string_of_int (List.nth base_id (sort_id a) + i)
;;

let romeo_of_ph opts (ps,hits) init_state =
	let sorts = List.map fst ps
	in
	let sort_id a = Util.index_of a sorts
	in
	let proc_id = romeo_pid (ps,hits)
	in
	let string_of_proc (a,i) =
		"<place id=\""^proc_id (a,i)^"\" label=\""^a^" "^string_of_int i^"\" "^
			" initialMarking=\""^(if state_value init_state a = i then "1" else "0")^"\">\n"^
			"\t<graphics><position x=\""^string_of_int (i*100+100)^"\" y=\""^string_of_int (100*sort_id a+100)^"\"/></graphics>\n"^
			"\t<scheduling gamma=\"0\" omega=\"0\"/>\n"^
		"</place>"

	and string_of_hit (b,j) (((a,i),rsa),k) ((hid, pids), str) =
		let dmin, dmax = match rsa with
			  None -> "0", "0"
			| Some(r,sa) -> 
				let fi = Param.firing_interval opts.alpha r sa
				in
				let dmin, dmax = opts.round_fi fi
				in
				string_of_int dmin, string_of_int dmax
		in
		let pid = 1 + try List.assoc (b,j) pids with Not_found -> 0
		and hlabel = a^" "^string_of_int i^"->"^b^" "^string_of_int j^" "^string_of_int k
		in
		(hid+1,	((b,j),pid)::List.remove_assoc (a,i) pids),
		str ^
		"<transition id=\""^string_of_int hid^"\" label=\""^hlabel^"\"  "^
			"eft=\""^dmin^"\" lft=\""^dmax^"\" "^
			"eft_param=\"a"^string_of_int hid^"\" lft_param=\"b"^string_of_int hid^"\" >\n"^
			"\t<graphics><position x=\""^string_of_int (j*100+50+100)^"\" "^
				"y=\""^string_of_int (100*sort_id b+pid*15-40+100)^"\"/>"^
				"<deltaLabel deltax=\"5\" deltay=\"5\"/>"^
			"</graphics>\n"^
		"</transition>\n"^
		"<arc place=\""^proc_id (b,j)^"\" transition=\""^string_of_int hid^"\" type=\"PlaceTransition\" weight=\"1\"/>\n"^
		(if a <> b then "<arc place=\""^proc_id (a,i)^"\" transition=\""^string_of_int hid^"\" type=\"read\" weight=\"1\"/>\n" else "")^
		"<arc place=\""^proc_id (b,k)^"\" transition=\""^string_of_int hid^"\" type=\"TransitionPlace\" weight=\"1\"/>"^
		"\n\n"
	in

	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<TPN>\n"
	(* places definitions *)
	^ (String.concat "\n\n" (List.flatten (List.map (fun (a,i) -> List.map (fun j -> string_of_proc (a,j)) (Util.range 0 i)) ps)))
	^"\n\n"
	(* hits *)
	^ snd (Hashtbl.fold string_of_hit hits ((1,[]), ""))
	^ "\n</TPN>\n"
;;

let tina_of_ph (ps,hits) init_state =
	let proc_id ai = string_of_process ai
	and tr_id = ref (-1)
	in
	let string_of_proc (a,i) =
		"pl "^proc_id (a,i)^" ("^(if state_value init_state a = i then "1" else "0")^")\n"
	and string_of_hit bj ((ai,rsa),k) =
		tr_id := !tr_id + 1;
		"tr t"^string_of_int (!tr_id)^" [0,w[ "^proc_id ai^"?1 "^proc_id bj
			^" -> "^proc_id (fst bj,k)^"\n"
	in
	let fold_hits key value buf =
		buf ^ string_of_hit key value
	in
	(* transitions definitions *)
	Hashtbl.fold fold_hits hits "";
	^ 
	(* places definitions *)
	(String.concat "" (List.map (fun ai -> string_of_proc ai) (list_of_state init_state)))
;;


