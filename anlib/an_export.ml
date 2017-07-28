
open PintTypes
open AutomataNetwork

type opts = {
	contextual_ptnet: bool;
}

let protect = true

let dump_of_an an ctx =
	let fold_defs a n buf =
        let aname = Hashtbl.find an.a2sig a
        and def_states = automaton_sigls an a
        in
		(a, "\""^aname^"\" ["^(String.concat ", "
			(List.map string_of_sigls def_states))^"]\n")::buf
	and fold_tr trid tr buf =
		(trid, (string_of_transition an tr)^"\n")::buf
	in
	let defs = Hashtbl.fold fold_defs an.ls []
	and trs = Hashtbl.fold fold_tr an.trs []
	and lss = LSSet.elements (lsset_of_ctx ctx)
	in
	let defs = List.map snd (List.sort compare defs)
	and trs = List.map snd (List.sort compare trs)
	in
	(String.concat "" defs)
	^ "\n"
	^ (String.concat "" trs)
	^ "\n"
	^ match lss with [] -> "" | _ -> (
        (if ctx_is_state ctx then "initial_state " else "initial_context ")
        ^ (String.concat ", " (List.map (string_of_ls ~protect an) lss))^"\n")

let nbjson_of_an ?(output_transitions=true) an ctx =
	let features = []
	in
	let features = if is_async_automata_network an then "synced_transitions"::features
					else features
	in
	let fold_a _ a l = a::l
	and fold_ls a n m =
        let aname = Hashtbl.find an.a2sig a
        and sigls = automaton_sigls an a
        in
		SMap.add aname sigls m
	in
	let automata = Hashtbl.fold fold_a an.a2sig []
	and localstates = Hashtbl.fold fold_ls an.ls SMap.empty
	in
	let automata = List.sort compare automata
	in
	let json_of_smap json_of_value sm =
		let fold k v buf = (if buf <> "" then buf^",\n" else buf)
			^"    \""^k^"\": "^json_of_value v
		in
		"{\n" ^ (SMap.fold fold sm "") ^ "}"
	in
	let json_of_slist = json_of_list json_of_str
	and json_of_siglist = json_of_list json_of_sigls
	in
	"{\n"
	^"\"automata\": "^json_of_slist automata^",\n"
	^"\"local_states\": "^json_of_smap json_of_siglist localstates^",\n"
    ^(if output_transitions then
        "\"local_transitions\": ["
            ^(String.concat ",\n\t"
                (Hashtbl.fold (fun trid tr l ->
                    json_of_transition an tr::l) an.trs []))
            ^"]\n,"
    else "")
	^"\"initial_state\": "^json_of_ctx an ctx^",\n"
	^"\"features\": "^json_of_slist features^"\n"
	^"}\n"


module PetriNet =
struct
	type place_t =
		  LS of sig_local_state
		| Custom of string

	type transition_t =
		  LT of (sig_state * sig_state * sig_state)
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
	let register_automaton aname a =
		List.iter (fun isig ->
			ignore(PetriNet.place pn (PetriNet.LS (aname,isig))))
            (automaton_sigls an a)
	and register_transition trid =
        let tr = Hashtbl.find an.trs trid
        in
        let cond = resolve_state an tr.cond
        and orig = resolve_state an tr.orig
        and dest = resolve_state an tr.dest
        in
		let t = PetriNet.transition pn (PetriNet.LT (orig, dest, cond))
		in
        let _ = SMap.iter (fun a i ->
            let j = SMap.find a dest
            in
			PetriNet.add_pt pn (PetriNet.place pn (PetriNet.LS (a,i))) t;
			PetriNet.add_tp pn t (PetriNet.place pn (PetriNet.LS (a,j)))) orig
        in
		let ps = SMap.fold (fun a i ps -> PetriNet.place pn (PetriNet.LS (a,i))::ps) cond []
		in
		if contextual then
			List.iter (PetriNet.add_ra pn t) ps
		else
			List.iter (fun p -> PetriNet.add_pt pn p t; PetriNet.add_tp pn t p) ps

	and register_ctx a is =
        let aname = label_of_a an a
        in
		if ISet.cardinal is > 1 then
			let p = PetriNet.place pn (PetriNet.Custom (aname^"_TBD"))
			in
			let register_i i =
				let t = PetriNet.transition pn (PetriNet.TCustom (aname^"_TBD_"^string_of_int i))
				in
                let i = snd (Hashtbl.find an.ls2sig (a,i))
                in
				PetriNet.add_pt pn p t;
				PetriNet.add_tp pn t (PetriNet.place pn (PetriNet.LS (aname,i)))
			in
			ISet.iter register_i is;
			PetriNet.mark pn p
		else
            let i = snd (Hashtbl.find an.ls2sig (a, ISet.choose is))
            in
			PetriNet.mark pn (PetriNet.place pn (PetriNet.LS (aname,i)))
	in
	let sorted_automata = Hashtbl.fold (fun a n ->
                            let aname = label_of_a an a
                            in
							SMap.add aname a) an.ls SMap.empty
    and sorted_transitions = Hashtbl.fold (fun trid _ -> ISet.add trid)
                                an.trs ISet.empty
	in
	SMap.iter register_automaton sorted_automata;
	(match goal with None -> ()
	| Some (conds, trname) ->
		let t = PetriNet.transition pn (PetriNet.TCustom trname)
		in
		List.iter (fun p -> PetriNet.add_pt pn p t) (places_of_conds conds));
    ISet.iter register_transition sorted_transitions;
	IMap.iter register_ctx ctx;
	pn


let pep_of_an ?(goal=None) ?(mapfile="") opts an ctx =
	let pn = pn_of_an ~goal opts.contextual_ptnet an ctx
	in
	let places = PetriNet.places pn
	and transitions = PetriNet.transitions pn
	and arcs = PetriNet.arcs pn
	in
    let protect = false
    in
	(if mapfile <> "" then
		let mapplaces = List.filter
			(function (_, PetriNet.LS _) -> true | _ -> false) places
		in
		let string_of_map (id, p) =
			match p with PetriNet.LS (a,i) ->
				string_of_int id^" "^a^" "^(string_of_sigls ~protect i)^"\n"
			| _ -> raise (Invalid_argument "string_of_map")
		in
		let mapdata = String.concat "" (List.map string_of_map mapplaces)
		in
		Util.dump_to_file mapfile mapdata
	);
    let string_of_resolved_ls (a,isig) =
        a^"="^string_of_sigls ~protect isig
    in
	let name_of_set smap =
        string_of_map
            ~lbracket:(if SMap.cardinal smap > 1 then "{" else "")
            ~rbracket:(if SMap.cardinal smap > 1 then "}" else "")
            string_of_resolved_ls
            SMap.fold smap
	in
	let name_of_trs orig dest =
        name_of_set orig ^ " -> " ^name_of_set dest
	in
	let pep_of_place (pid, p) =
		string_of_int pid^"\""
		^ (match p with
              PetriNet.LS ai -> string_of_resolved_ls ai
		    | PetriNet.Custom name -> name)^"\""
		^(if PetriNet.marked pn pid then "M1" else "M0")
		^"\n"
	and pep_of_transition (tid, t) =
		let name = match t with
			  PetriNet.LT (orig,dest,conds) ->
				(name_of_trs orig dest)
                ^ (if SMap.is_empty conds then "" else
                    " when "^string_of_map ~lbracket:"" ~rbracket:""
                                ~delim:" and "
                                string_of_resolved_ls SMap.fold conds)
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
		let pos = SMap.add (label_of_a an a) (id*space) pos
		in
		(id+1, pos)
	in
	let _, a_x = Hashtbl.fold register_automaton an.ls (1, SMap.empty)
	in
	let places = PetriNet.places pn
	and transitions = PetriNet.transitions pn
	and arcs = PetriNet.arcs pn
	in
    let rank_of_ls ai =
        snd (Hashtbl.find an.sig2ls ai)
	and repr_i _ = string_of_sigls ~protect:false
	in
	let repr_ls (a,i) = a^"_"^repr_i a i
	in
	let name_of_state state =
        String.concat "_" (List.map repr_ls (SMap.bindings state))
	in
	let name_of_trs orig dest =
        name_of_state orig ^ "___" ^ name_of_state dest
	in
	let romeo_of_place (pid, p) =
		let name = match p with
			  PetriNet.LS ai -> repr_ls ai
			| PetriNet.Custom name -> name
		and x,y = match p with
			  PetriNet.LS (a,i) -> 
                SMap.find a a_x, (rank_of_ls (a,i))*space
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
			| PetriNet.LT (orig,dest,conds) ->
				name_of_trs orig dest
				^(if SMap.is_empty conds then "" else "___"^name_of_state conds)
		and x, y = match t with PetriNet.TCustom _ -> 0, 200
			| PetriNet.LT (_,dest,_) ->
				let a,j = SMap.choose dest
				in
				let x = SMap.find a a_x + margin + space / 2
				and y = (rank_of_ls (a,j)) * space - space/2 + margin
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
	assert_async_an an;
	let modname a = "A"^string_of_int a
	and varname a = "s"^string_of_int a
	in
	let prism_state (a,i) =
		varname a ^ "=" ^ string_of_int i
	and prism_state' (a,i) =
		varname a ^ "'=" ^ string_of_int i
	in
	let prism_of_transition a trid =
        let tr = Hashtbl.find an.trs trid
        in
        let j = IMap.find a tr.dest
        and pre = IMap.bindings tr.pre
        in
		"\t[] " ^ (String.concat " & " (List.map prism_state pre))
			^ " -> (" ^ prism_state' (a,j)^");\n"
	in
	let prism_of_automaton a n str =
		str
		^ "// automaton \""^label_of_a an a^"\"\n"
		^ "module "^modname a^"\n"
		^ "\t// "
        ^ (String.concat "; "
            (List.mapi
                (fun i isig -> string_of_int i^" is \"" ^string_of_sigls isig^"\"")
                (automaton_sigls an a)))^"\n"
		^ "\t"^varname a^": [0.."^string_of_int (n-1)^"]"
		^ " init "^string_of_int (ISet.choose (IMap.find a ctx))^";\n\n"
        ^ (String.concat ""
            (List.map
                (fun i ->
                    let trs = Hashtbl.find_all an.lsnext (a,i)
                    in
                    String.concat "\n" (List.map (prism_of_transition a) trs))
                (Util.range 0 (n-1))))
		^ "\n\nendmodule\n\n"
	in
	"mdp\n\n"
	^ Hashtbl.fold prism_of_automaton an.ls ""


let nusmv_of_an ?(map=None) universal an ctx =
	assert_async_an an;
	let varname a = "a"^string_of_int a
	and updname a = "u"^string_of_int a
	and tbd_state = "TBD"
	in
	let automaton_spec a n specs = (a, n)::specs
	in
	let automata_spec = Hashtbl.fold automaton_spec an.ls []
	in
	let automata_spec = List.sort compare automata_spec
	in
	let automata_tbd =
		if universal then ISet.empty else
			IMap.fold (fun a is iset ->
				if ISet.cardinal is > 1 then
					ISet.add a iset else iset) ctx ISet.empty
	in
	let def_automaton (a, n) =
        let isigs = automaton_sigls an a
        in
		(match map with None -> ()
		| Some m -> List.iteri (fun i _ ->
				Hashtbl.add m (a,i) (varname a, string_of_int i)) isigs
		);
		let sis = List.mapi (fun i _ -> string_of_int i) isigs
		in
		let sis = if ISet.mem a automata_tbd then
			tbd_state::sis else sis
		in
		varname a^": {"^(String.concat "," sis)^"};"
        ^ " // automaton "^(label_of_a an a)^" with local states "
            ^String.concat ", "
                (List.mapi (fun i isig ->
                        string_of_int i^"="^string_of_sigls ~protect:false isig)
                    isigs)
	in
	let nusmv_of_conditions conds =
		let conds = IMap.bindings conds
		in
		let conds = List.map (fun (a,i) -> varname a^"="^string_of_int i) conds
		in
		String.concat " & " conds
	in
	let nusmv_of_transitions (a,n) =
		let nusmv_of_transition (a,i,j) trid =
            let tr = Hashtbl.find an.trs trid
            in
			"\t\t"^nusmv_of_conditions tr.pre^"?"^string_of_int j^":"^varname a
		in
		let nusmv_of_transitions a i =
            let js = Hashtbl.find_all an.lsnext (a,i)
            in
            List.flatten (List.map (fun j ->
                let trids = Hashtbl.find_all an.change2tr ((a,i),j)
                in
                List.map (nusmv_of_transition (a,i,j)) trids) js)
		in
        if n >= 2 then
            let trs = List.flatten
                (List.map (nusmv_of_transitions a) (Util.range 0 (n-1)))
            in
            "next("^varname a^") := case\n"
            ^(if ISet.mem a automata_tbd then
                let is = ISet.elements (IMap.find a ctx)
                in
                ("\t"^varname a^"="^tbd_state^": {" ^(String.concat "," (List.map string_of_int is)) ^"};\n") else "")
		    ^(match trs with [] -> "" | _ ->
                "\tu="^updname a^": {\n"^(String.concat ",\n" trs)^"};\n")
            ^"\tTRUE: "^varname a^";\nesac;"
        else ""
	in
	let nusmv_fp_cond =
		let fold _ tr acc =
			nusmv_of_conditions tr.pre::acc
		in
		let conds = Hashtbl.fold fold an.trs []
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
	^"IVAR\n\tu: {"^(String.concat ", " (List.map (fun (a,_) -> updname a) automata_spec))^"};\n"
	^"\nVAR\n\t"
	^(String.concat "\n\t" (List.map def_automaton automata_spec))^"\n"
	^"\nASSIGN\n"
	^(String.concat "\n" (List.map nusmv_of_transitions automata_spec))^"\n"
	^"\nTRANS\n\t"
	^(String.concat " |\n\t" (List.map (fun (a,_) ->
		"next("^varname a^") != "^varname a) automata_spec))
	^ " |\n\t("^nusmv_fp_cond^");\n"
	^"\nINIT\n"
	^"\t"^(String.concat " & " (List.map nusmv_of_init (IMap.bindings ctx)))^";\n"
	^"\n"

