
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


module PetriNet =
struct
	type place_t =
		  LS of local_state
		| Custom of string

	type transition_t =
		  LT of (automaton_p * automaton_state * automaton_state * local_state list)
		| TCustom of string

	type arc_t = TP | PT | RA

	type t = {
		pm: (place_t, int) Hashtbl.t;
		tm: (int, transition_t) Hashtbl.t;
		arcs: ((int *int), arc_t) Hashtbl.t;
		tokens: (int, int) Hashtbl.t;
	}

	let create ?(nplaces=20) ?(ntrs=20) ?(narcs=20) ?(ntokens=10) () = {
			pm = Hashtbl.create nplaces;
			tm = Hashtbl.create ntrs;
			arcs = Hashtbl.create narcs;
			tokens = Hashtbl.create ntokens;
		}

	let place pn p =
		try
			Hashtbl.find pn.pm p
		with Not_found ->
			let idx = Hashtbl.length pn.pm + 1
			in
			(Hashtbl.add pn.pm p idx;
			idx)

	let places pn =
		let nodes = Hashtbl.fold (fun p pid l -> (pid,p)::l) pn.pm []
		in
		List.sort compare nodes

	let transition pn name =
		let tid = Hashtbl.length pn.tm + 1
		in
		Hashtbl.add pn.tm tid name;
		tid

	let transitions pn =
		let nodes = Hashtbl.fold (fun tid name l-> (tid,name)::l) pn.tm []
		in
		List.sort compare nodes

	let add_pt pn pid tid =
		Hashtbl.add pn.arcs (pid, tid) PT
	let add_tp pn tid pid =
		Hashtbl.add pn.arcs (tid, pid) TP
	let add_ra pn tid pid =
		Hashtbl.add pn.arcs (tid, pid) RA

	let arcs pn =
		let nodes = Hashtbl.fold (fun (id1,id2) kind l-> (id1,id2,kind)::l) pn.arcs []
		in
		List.sort compare nodes

	let mark pn pid =
		Hashtbl.add pn.tokens pid 1
	let marked pn pid =
		Hashtbl.mem pn.tokens pid

end

let pn_of_an ?(goal=None) contextual an ctx =
	let pn = PetriNet.create ()
	in
	let places_of_conds conds =
		let conds = SMap.bindings conds
		in
		List.map (fun ai -> PetriNet.place pn (PetriNet.LS ai)) conds
	in
	let register_automaton a =
		List.iter (fun (_, i) ->
			ignore(PetriNet.place pn (PetriNet.LS (a,i))))
	and register_transition (a,i,j) conds =
		let conds = SMap.bindings conds
		in
		let t = PetriNet.transition pn (PetriNet.LT (a,i,j,conds))
		in
		PetriNet.add_pt pn (PetriNet.place pn (PetriNet.LS (a,i))) t;
		PetriNet.add_tp pn t (PetriNet.place pn (PetriNet.LS (a,j)));
		let ps = List.map (fun ai -> PetriNet.place pn (PetriNet.LS ai)) conds
		in
		if contextual then
			List.iter (PetriNet.add_ra pn t) ps
		else
			List.iter (fun p -> PetriNet.add_pt pn p t; PetriNet.add_tp pn t p) ps
	and register_ctx a is =
		if ISet.cardinal is > 1 then
			let p = PetriNet.place pn (PetriNet.Custom (a^"_TBD"))
			in
			let register_i i =
				let t = PetriNet.transition pn (PetriNet.TCustom (a^"_TBD_"^string_of_int i))
				in
				PetriNet.add_pt pn p t;
				PetriNet.add_tp pn t (PetriNet.place pn (PetriNet.LS (a,i)))
			in
			ISet.iter register_i is;
			PetriNet.mark pn p
		else
			PetriNet.mark pn (PetriNet.place pn (PetriNet.LS (a,ISet.choose is)))
	in
	Hashtbl.iter register_automaton an.automata;
	(match goal with None -> ()
	| Some (conds, trname) ->
		let t = PetriNet.transition pn (PetriNet.TCustom trname)
		in
		List.iter (fun p -> PetriNet.add_pt pn p t) (places_of_conds conds));
	Hashtbl.iter register_transition an.conditions;
	SMap.iter register_ctx ctx;
	pn


let pep_of_an ?(goal=None) ?(mapfile="") opts an ctx =
	let pn = pn_of_an ~goal opts.contextual_ptnet an ctx
	in
	let places = PetriNet.places pn
	and transitions = PetriNet.transitions pn
	and arcs = PetriNet.arcs pn
	in
	(if mapfile <> "" then
		let mapplaces = List.filter
			(function (_, PetriNet.LS _) -> true | _ -> false) places
		in
		let string_of_map (id, p) =
			match p with PetriNet.LS (a,i) ->
				string_of_int id^" "^a^" "^string_of_int i^"\n"
			| _ -> raise (Invalid_argument "string_of_map")
		in
		let mapdata = String.concat "" (List.map string_of_map mapplaces)
		in
		Util.dump_to_file mapfile mapdata
	);
	let pep_of_place (pid, p) =
		string_of_int pid^"\""
		^ (match p with PetriNet.LS ai -> string_of_localstate ~protect:false an ai
		    | PetriNet.Custom name -> name)^"\""
		^(if PetriNet.marked pn pid then "M1" else "M0")
		^"\n"
	and pep_of_transition (tid, t) =
		let name = match t with
			  PetriNet.LT (a,i,j,conds) ->
				(a ^ " " ^ (string_of_astate ~protect:false an a i)
					^ " -> " ^ (string_of_astate ~protect:false an a j)
					^ (match conds with [] -> "" | _ -> " when "^
						String.concat " and "
							(List.map (string_of_localstate ~protect:false an)
							conds)))
			| PetriNet.TCustom name -> name
		in
		string_of_int tid^"\""^name^"\"\n"
	and pep_of_arc (id1, id2, kind) =
		string_of_int id1
		^(match kind with PetriNet.PT -> ">" | _ -> "<")
		^string_of_int id2^"\n"
	in
	"PEP\nPTNet\nFORMAT_N\n"
	^ "PL\n" ^ (String.concat "" (List.map pep_of_place places))
	^ "TR\n" ^ (String.concat "" (List.map pep_of_transition transitions))
	^ "TP\n" ^ (String.concat "" (List.map pep_of_arc
		(List.filter (function (_,_,PetriNet.TP) -> true | _ -> false) arcs)))
	^ "PT\n" ^ (String.concat "" (List.map pep_of_arc
		(List.filter (function (_,_,PetriNet.PT) -> true | _ -> false) arcs)))
	^ (if opts.contextual_ptnet then
		"RA\n" ^ (String.concat "" (List.map pep_of_arc
		(List.filter (function (_,_,PetriNet.RA) -> true | _ -> false) arcs)))
		else "")

let romeo_of_an ?(map=None) ?(mapfile="") an ctx =
	let pn = pn_of_an true an ctx
	in
	let space = 100
	and margin = 100
	in
	let register_automaton a _ (id, pos) =
		let pos = SMap.add a (id*space) pos
		in
		(id+1, pos)
	in
	let _, a_x = Hashtbl.fold register_automaton an.automata (1, SMap.empty)
	in
	let places = PetriNet.places pn
	and transitions = PetriNet.transitions pn
	and arcs = PetriNet.arcs pn
	in
	let repr_i = string_of_astate ~protect:false an
	in
	let repr_ls (a,i) = a^"_"^repr_i a i
	in
	let romeo_of_place (pid, p) =
		let name = match p with
			  PetriNet.LS ai -> repr_ls ai
			| PetriNet.Custom name -> name
		and x,y = match p with
			  PetriNet.LS (a,i) -> SMap.find a a_x, i*space
			| PetriNet.Custom _ -> 0,0
		in
		"<place id=\""^string_of_int pid^"\" label=\""^name^"\" "
			^" initialMarking=\""^(if PetriNet.marked pn pid then "1" else "0")^"\">\n"
			^"\t<graphics><position x=\""^(string_of_int (x+margin))^"\""
			^" y=\""^string_of_int (y+margin)^"\"/></graphics>\n"
			^"\t<scheduling gamma=\"0\" omega=\"0\"/>\n"
		^"</place>\n"
	and romeo_of_transition (tid, t) =
		let name = match t with PetriNet.TCustom name -> name
			| PetriNet.LT (a,i,j,conds) ->
				a^"_"^(repr_i a i)^"_"^(repr_i a j)
					^(match conds with [] -> "" | _ -> "_"^
						String.concat "__" (List.map repr_ls conds))
		and x, y = match t with PetriNet.TCustom _ -> 0, 200
			| PetriNet.LT (a,_,j,_) ->
				let x = SMap.find a a_x + margin + space / 2
				and y = j * space - space/2 + margin
				in
				x,y
		and sid = string_of_int tid
		in
		"<transition id=\""^sid^"\" label=\""^name^"\"  "
			^"eft=\"0\" lft=\"0\" "
			^"eft_param=\"a"^sid^"\" lft_param=\"b"^sid^"\" >\n"
			^"\t<graphics><position x=\""^string_of_int x^"\" y=\""^string_of_int y^"\"/>"
				^ "<deltaLabel deltax=\"5\" deltay=\"5\"/>"
			^"</graphics>\n"
		^"</transition>\n"
	and romeo_of_arc (id1, id2, kind) =
		let (pid, tid, rtype) = match kind with
		      PetriNet.TP -> (id2, id1, "TransitionPlace")
			| PetriNet.PT -> (id1, id2, "PlaceTransition")
			| PetriNet.RA -> (id2, id1, "read")
		in
		let sp = string_of_int pid
		and st = string_of_int tid
		in
		"<arc place=\""^sp^"\" transition=\""^st^"\" type=\""^rtype^"\" weight=\"1\"/>\n"
	in
	(match map with Some map -> List.iter (fun (pid, p) ->
		match p with PetriNet.LS ai ->
			Hashtbl.add map ai (repr_ls ai, pid) | _ -> ()) places | None -> ());
	(if mapfile <> "" then
		let mapplaces = List.filter
			(function (_, PetriNet.LS _) -> true | _ -> false) places
		in
		let string_of_map (id, p) =
			match p with PetriNet.LS ai ->
				string_of_int id^" "^repr_ls ai^"\n"
			| _ -> raise (Invalid_argument "string_of_map")
		in
		let mapdata = String.concat "" (List.map string_of_map mapplaces)
		in
		Util.dump_to_file mapfile mapdata
	);
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<TPN>\n"
	^ (String.concat "" (List.map romeo_of_place places))
	^ (String.concat "" (List.map romeo_of_transition transitions))
	^ (String.concat "" (List.map romeo_of_arc arcs))
	^ "</TPN>\n"




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

