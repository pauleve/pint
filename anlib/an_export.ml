
open PintTypes
open AutomataNetwork

type opts = {
	contextual_ptnet: bool;
}

let dump_of_an an ctx =
	let fold_defs a def_states buf =
		let def_states = List.map fst def_states
		in
		(a, "\""^a^"\" ["^(String.concat ", "
			(List.map string_of_sig_state def_states))^"]\n")::buf
	and fold_tr (a,i,j) cond buf =
		let cond = SMap.bindings cond
		in
		((a,i,j,cond), "\""^a^"\" "^(string_of_astate an a i)^" -> "
			^(string_of_astate an a j) ^
		(if [] = cond then "" else
			(" when "^String.concat " and "
				(List.map (string_of_localstate an) cond)))
		^ "\n")::buf
	and lss = LSSet.elements (lsset_of_ctx ctx)
	in
	let defs = Hashtbl.fold fold_defs an.automata []
	and trs = Hashtbl.fold fold_tr an.conditions []
	in
	let defs = List.sort compare defs
	and trs = List.sort compare trs
	in
	let defs = List.map snd defs
	and trs = List.map snd trs
	in
	(String.concat "" defs)
	^ "\n"
	^ (String.concat "" trs)
	^ "\n"
	^ match lss with [] -> "" | _ -> ("initial_context " ^ (String.concat ", "
	(List.map (string_of_localstate an) lss))^"\n")

let ph_of_an an ctx =
	let ph_of_ls = Ph_types.pintstring_of_proc
	in
	let fold_defs a def_states buf =
		let l = List.length def_states - 1
		in
		(a, "process "^a^" "^string_of_int l^"\n")::buf
	and fold_tr (a,i,j) cond buf =
		let cond = SMap.bindings cond
		in
		let bounce = " -> "^ph_of_ls (a,i)^" "^string_of_int j
		in
		let str = match cond with [] -> ph_of_ls (a,i) ^ bounce
			| [bk] -> ph_of_ls bk ^ bounce
			| _ ->
				(* TODO: improve *)
				("COOPERATIVITY(["
					^ (String.concat ";" (List.map fst cond))
					^ "]" ^ bounce ^ ", [["
					^ (String.concat ";" (List.map string_of_int
						(List.map snd cond)))
					^ "]])")
		in
		((a,i,j,cond), str^"\n")::buf
	and lss = LSSet.elements (lsset_of_ctx ctx)
	in
	let defs = Hashtbl.fold fold_defs an.automata []
	and trs = Hashtbl.fold fold_tr an.conditions []
	and lss = List.filter (fun (_,i) -> i > 0) lss
	in
	let defs = List.sort compare defs
	and trs = List.sort compare trs
	in
	let defs = List.map snd defs
	and trs = List.map snd trs
	in
	(String.concat "" defs)
	^ "\n"
	^ (String.concat "" trs)
	^ "\n"
	^ "initial_context " ^ (String.concat ", " (List.map ph_of_ls lss))
	^ "\n"

let pep_of_an ?(goal=None) ?(mapfile="") opts an ctx =
	let idx_of_place places ls =
		try
			(LSMap.find ls places, places)
		with Not_found -> (
			let idx = 1 + LSMap.cardinal places
			in
			let places = LSMap.add ls idx places
			in
			(idx, places)
		)
	in
	let idx_of_places places =
		let fold_ls (idxs, places) ls =
			let idx, places = idx_of_place places ls
			in
			(idx::idxs, places)
		in
		List.fold_left fold_ls ([], places)
	in
	let register_transition ?(trname=None) (a,i,j) conds (id, places, transitions, tp, pt, ra) =
		let conds = SMap.bindings conds
		in
		let sid = string_of_int id
		and str = match trname with None ->
					(a ^ " " ^ (string_of_astate ~protect:false an a i)
					^ " -> " ^ (string_of_astate ~protect:false an a j)
					^ (match conds with [] -> "" | _ -> " when "^
						String.concat " and " 
							(List.map (string_of_localstate ~protect:false an)
							conds)))
				| Some name -> name
		in
		let transitions = (id, sid^"\""^str^"\"", (a,j))::transitions
		and idxs, places = idx_of_places places conds
		in
		let sidxs = List.map string_of_int idxs
		and tp, pt, places =
			if a <> "" then
				let id_ai, places = idx_of_place places (a,i)
				in
				let id_aj, places = idx_of_place places (a,j)
				in
				let sai = string_of_int id_ai
				and saj = string_of_int id_aj
				in
				let tp = (sid^"<"^saj)::tp
				and pt = (sai^">"^sid)::pt
				in
				tp, pt, places
			else (tp, pt, places)
		in
		let (tp, pt, ra) =
			if opts.contextual_ptnet then
				let ra = ra @ List.map (fun sai -> sid^"<"^sai) sidxs
				in
				tp, pt, ra
			else
				let tp = if a <> "" then tp @ List.map (fun sai -> sid^"<"^sai) sidxs else tp
				and pt = pt @ List.map (fun sai -> sai^">"^sid) sidxs
				in
				tp, pt, ra
		in
		(id+1, places, transitions, tp, pt, ra)
	in
	let pep = (1, LSMap.empty, [], [], [], [])
	in
	let pep = match goal with None -> pep
		| Some (conds, trname) ->
			register_transition ~trname:(Some trname) ("", 0, 1) conds pep
	in
	let (_, places, transitions, tp, pt, ra) =
			Hashtbl.fold register_transition an.conditions pep
	in
	let register_localstate ai id places =
		(id, string_of_int id^"\""^string_of_localstate ~protect:false an ai^"\""
				^(if ctx_has_localstate ai ctx then "M1" else "M0"), ai)::places
	in
	let places = LSMap.fold register_localstate places []
	in
	let places = List.sort compare places
	and transitions = List.sort compare transitions
	in
	let mid (_,a,_) = a
	in
	let mapplaces = List.map (fun (i,_,ai) -> (i,ai)) places
	and maptransitions = List.map (fun (i,_,ai) -> (i,ai)) transitions
	and places = List.map mid (List.sort compare places)
	and transitions = List.map mid (List.sort compare transitions)
	in
	(if mapfile <> "" then
		let string_of_map (id, (a,i)) = 
					string_of_int id^" "^a^" "^string_of_int i
		in
		let mapdata = 
			  string_of_int (List.length mapplaces)
			^ "\n" ^ (String.concat "\n" (List.map string_of_map mapplaces))
			^ "\n" ^ string_of_int (List.length maptransitions)
			^ "\n" ^ (String.concat "\n" (List.map string_of_map maptransitions))
		in
		Util.dump_to_file mapfile mapdata
	);
	"PEP\nPTNet\nFORMAT_N\n"
	^ "PL\n" ^ (String.concat "\n" places) ^ "\n"
	^ "TR\n" ^ (String.concat "\n" transitions) ^ "\n"
	^ "TP\n" ^ (String.concat "\n" tp) ^ "\n"
	^ "PT\n" ^ (String.concat "\n" pt) ^ "\n"
	^ (if opts.contextual_ptnet then "RA\n" ^ (String.concat "\n" ra) ^ "\n"
		else "")

let romeo_of_an ?(map=None) ?(mapfile="") an ctx =
	let repr_i = string_of_astate ~protect:false an
	in
	let repr_ls (a,i) = a^"_"^repr_i a i
	in
	let idx_of_place places ls =
		try
			(LSMap.find ls places, places)
		with Not_found -> (
			let idx = 1 + LSMap.cardinal places
			in
			let places = LSMap.add ls idx places
			in
			(idx, places)
		)
	in
	let idx_of_places places =
		let fold_ls (idxs, places) ls =
			let idx, places = idx_of_place places ls
			in
			(idx::idxs, places)
		in
		List.fold_left fold_ls ([], places)
	in
	let register_transition (a,i,j) conds (id, places, transitions) =
		let conds = SMap.bindings conds
		in
		let sid = string_of_int id
		and repr_tr = a^"_"^(repr_i a i)^"_"^(repr_i a j)
					^(match conds with [] -> "" | _ -> "_"^
						String.concat "__" (List.map repr_ls conds))
		in
		let idxs, places = idx_of_places places conds
		in
		let id_ai, places = idx_of_place places (a,i)
		in
		let id_aj, places = idx_of_place places (a,j)
		in
		let sai = string_of_int id_ai
		and saj = string_of_int id_aj
		and sidxs = List.map string_of_int idxs
		in
		let repr =
		"<transition id=\""^sid^"\" label=\""^repr_tr^"\"  "
			^"eft=\"0\" lft=\"0\" "
			^"eft_param=\"a"^sid^"\" lft_param=\"b"^sid^"\" >\n"
			^"\t<graphics><position x=\""^string_of_int (j*100+50+100)^"\" y=\"200\"/>"
				^ "<deltaLabel deltax=\"5\" deltay=\"5\"/>"
			^"</graphics>\n"
		^"</transition>\n"
		^"<arc place=\""^sai^"\" transition=\""^sid^"\" type=\"PlaceTransition\" weight=\"1\"/>\n"
		^"<arc place=\""^saj^"\" transition=\""^sid^"\" type=\"TransitionPlace\" weight=\"1\"/>\n"
		^ String.concat "\n" (List.map (fun sbk ->
			"<arc place=\""^sbk^"\" transition=\""^sid^"\" type=\"read\" weight=\"1\"/>")
				sidxs)
		in
		(id+1, places, repr::transitions)
	in
	let (ntid, mapplaces, transitions) =
			Hashtbl.fold register_transition an.conditions
				(1, LSMap.empty, [])
	in
	let register_ctx a is =
		ISet.fold (fun i mapplaces -> snd (idx_of_place mapplaces (a,i))) is
	in
	let mapplaces = SMap.fold register_ctx ctx mapplaces
	in
	let register_localstate (a,i) id places =
		let initial = ISet.equal (SMap.find a ctx) (ISet.singleton i)
		in
		let repr =
		"<place id=\""^string_of_int id^"\" label=\""^repr_ls (a,i)^"\" "
			^" initialMarking=\""^(if initial then "1" else "0")^"\">\n"
			^"\t<graphics><position x=\""^(string_of_int (id*100+100))^"\" y=\"100\"/></graphics>\n"
			^"\t<scheduling gamma=\"0\" omega=\"0\"/>\n"
		^"</place>"
		in
		repr::places
	in
	let places = LSMap.fold register_localstate mapplaces []
	in
	let register_tbd pid a i (ntid, transitions) =
		let sid = string_of_int ntid
		and sai = string_of_int (LSMap.find (a,i) mapplaces)
		in
		let tr =
		"<transition id=\""^sid^"\" label=\"DET_"^repr_ls (a,i)^"\"  "
			^"eft=\"0\" lft=\"0\" "
			^"eft_param=\"a"^sid^"\" lft_param=\"b"^sid^"\" >\n"
			^"\t<graphics><position x=\""^string_of_int (0*100+75+80)^"\" y=\"200\"/>"
				^ "<deltaLabel deltax=\"5\" deltay=\"5\"/>"
			^"</graphics>\n"
		^"</transition>\n"
		^"<arc place=\""^pid^"\" transition=\""^sid^"\" type=\"PlaceTransition\" weight=\"1\"/>\n"
		^"<arc place=\""^sai^"\" transition=\""^sid^"\" type=\"TransitionPlace\" weight=\"1\"/>"
		in
		(ntid+1, tr::transitions)
	in
	let register_tbd a is (npid, ntid, places, transitions) = if ISet.cardinal is > 1 then
		let pid = string_of_int npid
		in
		let place =
		"<place id=\""^pid^"\" label=\""^a^"_TBD\" initialMarking=\"1\">\n"
			^"\t<graphics><position x=\""^(string_of_int (npid*100+100))^"\" y=\"100\"/></graphics>\n"
			^"\t<scheduling gamma=\"0\" omega=\"0\"/>\n"
		^"</place>"
	in
		let ntid, transitions = ISet.fold (register_tbd pid a) is (ntid, transitions)
		in
		(npid+1, ntid, place::places, transitions)
		else (npid, ntid, places, transitions)
	in
	let _, _, places, transitions = SMap.fold register_tbd ctx
		(1+List.length places,  ntid, places, transitions)
	in
	(match map with Some map -> LSMap.iter (fun ai id ->
			Hashtbl.add map ai (repr_ls ai, id)) mapplaces | None -> ());
	(if mapfile <> "" then
		let string_of_map ((a,i), id) =
					string_of_int id^" "^repr_ls (a,i)
		in
		let mapdata = string_of_int (LSMap.cardinal mapplaces)
			^"\n" ^ (String.concat "\n"
				(List.map string_of_map (LSMap.bindings mapplaces)))
			^"\n"
		in
		Util.dump_to_file mapfile mapdata
	);
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<TPN>\n"
	^ (String.concat "\n" places) ^ "\n"
	^ (String.concat "\n" transitions) ^ "\n</TPN>\n"


let prism_of_an an ctx =
	let a_counter = ref 0
	and a2i = ref SMap.empty
	in
	let index a =
		try SMap.find a !a2i
		with Not_found -> (
			a_counter := !a_counter + 1;
			a2i := SMap.add a !a_counter !a2i;
			!a_counter
		)
	in
	let modname a =
		let i = index a
		in
		"A"^string_of_int i
	and varname a =
		let i = index a
		in
		"s"^string_of_int i
	in
	let prism_state (a,i) =
		varname a ^ "=" ^ string_of_int i
	and prism_state' (a,i) =
		varname a ^ "'=" ^ string_of_int i
	in
	let prism_of_transition a i j conds =
		let conds = SMap.bindings conds
		in
		"\t[] " ^ (String.concat " & " (List.map prism_state ((a,i)::conds)))
			^ " -> (" ^ prism_state' (a,j)^");\n"
	in
	let prism_of_transitions a i =
		let transitions = Hashtbl.find an.transitions (a,i)
		in
		let fold_jump j str_trs =
			let conds_list = Hashtbl.find_all an.conditions (a,i,j)
			in
			let str_conds = List.map (prism_of_transition a i j) conds_list
			in
			str_trs ^ String.concat "" str_conds
		in
		ISet.fold fold_jump transitions ""
	in
	let prism_of_automaton a spec str =
		let ls = snd (List.split spec)
		in
		str
		^ "// automaton \""^a^"\"\n"
		^ "module "^modname a^"\n"
		^ "\t// "
			^ (String.concat "; " (List.map 
					(fun (s_i, i) -> string_of_int i^"=\""
										^string_of_sig_state s_i^"\"") spec))
			^"\n"
		^ "\t"^varname a^": [0.."^string_of_int (List.length spec - 1)^"]"
		^ " init "^string_of_int (ISet.choose (SMap.find a ctx))^";\n\n"
		^ (String.concat "\n" (List.map (prism_of_transitions a) ls))
		^ "\n\nendmodule\n\n"
	in
	"mdp\n\n"
	^ Hashtbl.fold prism_of_automaton an.automata ""


let nusmv_of_an ?(map=None) universal an ctx =
	let varname a = "a_"^a
	and updname a = "u_"^a
	and tbd_state = "TBD"
	in
	let automaton_spec a spec specs =
		(a, List.map snd spec)::specs
	in
	let automata_spec = Hashtbl.fold automaton_spec an.automata []
	in
	let automata_spec = List.sort compare automata_spec
	in
	let automata_tbd =
		if universal then SSet.empty else
			SMap.fold (fun a is aset ->
				if ISet.cardinal is > 1 then
					SSet.add a aset else aset) ctx SSet.empty
	in
	let def_automaton (a, is) =
		(match map with None -> ()
		| Some m -> List.iter (fun i ->
				Hashtbl.add m (a,i) (varname a, string_of_int i)) is
		);
		let sis = List.map string_of_int is
		in
		let sis = if SSet.mem a automata_tbd then
			tbd_state::sis else sis
		in
		varname a^": {"^(String.concat "," sis)^"};"
	in
	let nusmv_of_conditions (a, i, j) conds =
		let conds = (a,i)::SMap.bindings conds
		in
		let conds = List.map (fun (a,i) -> varname a^"="^string_of_int i) conds
		in
		String.concat " & " conds
	in
	let nusmv_of_transitions (a, is) =
		let nusmv_of_transition a i j conds =
			"\tu="^updname a^" & "^nusmv_of_conditions (a,i,j) conds
				^": "^string_of_int j^";\n"
		in
		let nusmv_of_transitions a i =
			let transitions = Hashtbl.find an.transitions (a,i)
			in
			let nusmv_trs = List.map (fun j ->
				let conds_list = Hashtbl.find_all an.conditions (a,i,j)
				in
				List.map (nusmv_of_transition a i j) conds_list)
					(ISet.elements transitions)
			in
			List.flatten nusmv_trs
		in
		match is with
		  i1::i2::_ ->
		"next("^varname a^") := case\n"
		^(if SSet.mem a automata_tbd then
			("\t"^varname a^"="^tbd_state^": {"
			^(String.concat "," (List.map string_of_int is))
			^"};\n") else "")
		^(String.concat "" (List.flatten
				(List.map (nusmv_of_transitions a) is)))
		^"\tTRUE: "^varname a^";\nesac;"
		| _ -> ""
	in
	let nusmv_fp_cond =
		let fold tr conds acc =
			nusmv_of_conditions tr conds::acc
		in
		let conds = Hashtbl.fold fold an.conditions []
		in
		let conds = List.map (fun expr -> "!("^expr^")") conds
		in
		String.concat " & " conds
	in
	let nusmv_of_init (a, is) =
		let is = ISet.elements is
		in
		match is with
		  [i] -> varname a^"="^string_of_int i
		| _ -> if universal then
				"("^(String.concat " | " (List.map (fun i ->
					varname a^"="^string_of_int i) is))^")"
			else (varname a^"="^tbd_state)
	in
	"MODULE main\n\n"
	^"IVAR\n\tu: {u_"^(String.concat ", u_" (List.map fst automata_spec)) ^"};\n"
	^"\nVAR\n\t"
	^(String.concat "\n\t" (List.map def_automaton automata_spec))^"\n"
	^"\nASSIGN\n"
	^(String.concat "\n" (List.map nusmv_of_transitions automata_spec))^"\n"
	^"\nTRANS\n\t"
	^(String.concat " |\n\t" (List.map (fun (a,_) ->
		"next("^varname a^") != "^varname a) automata_spec))
	^ " |\n\t("^nusmv_fp_cond^");\n"
	^"\nINIT\n"
	^"\t"^(String.concat " & " (List.map nusmv_of_init (SMap.bindings ctx)))^";\n"
	^"\n"

