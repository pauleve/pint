
open PintTypes

type trid = int
type state = id IMap.t

type ls = id*id

type sig_automaton = string
type sig_automaton_state = StateId of int | StateLabel of string
type sig_local_state = sig_automaton * sig_automaton_state

type sig_state = sig_automaton_state SMap.t

type transition = {
    orig: state;
    dest: state;
    cond: state;
    pre: state; (* orig + cond *)
}

type t = {
    (* signature/internal id mapping *)
    a2sig: (id, sig_automaton) Hashtbl.t;
    sig2a: (sig_automaton, id) Hashtbl.t;
    ls2sig: (ls, sig_local_state) Hashtbl.t;
    sig2ls: (sig_local_state, ls) Hashtbl.t;

    ls: (id, int) Hashtbl.t;

    trs : (trid, transition) Hashtbl.t;

    (* local states graph *)
    lsnext: (ls, id) Hashtbl.t; (* out-going transitions *)

    (* map local changes to transitions *)
    change2tr: ((ls*id), trid) Hashtbl.t;
}


let is_async_automata_network an =
    Hashtbl.fold (fun _ tr async ->
        async && (IMap.cardinal tr.orig = 1)) an.trs true

let assert_async_an an =
    if not (is_async_automata_network an) then
		failwith "Automata networks with synchronous transitions are not supported yet."

let resolve_sig_a an = Hashtbl.find an.sig2a

let resolve_sig_a_set an aname_set =
    SSet.fold (fun a -> ISet.add (resolve_sig_a an a)) aname_set ISet.empty

let resolve_sig_ls an = Hashtbl.find an.sig2ls
let resolve_ls an = Hashtbl.find an.ls2sig

let resolve_sig_state an sig_state =
    SMap.fold (fun aname isig ->
        let a, i = Hashtbl.find an.sig2ls (aname, isig)
        in
        IMap.add a i) sig_state IMap.empty

let resolve_state an state =
    IMap.fold (fun a i ->
        let a, i = Hashtbl.find an.ls2sig (a,i)
        in
        SMap.add a i) state SMap.empty


(** Number of automata *)
let count_automata an =
    Hashtbl.length an.a2sig

(** Number of local transitions *)
let count_transitions an =
	Hashtbl.length an.trs

(** Number of local states in automata [a] *)
let automaton_length an a =
    Hashtbl.find an.ls a

let automaton_sigls an a =
    let n = Hashtbl.find an.ls a
    in
    List.map (fun i -> snd (Hashtbl.find an.ls2sig (a,i)))
        (Util.range 0 (n-1))

let has_automaton an name = Hashtbl.mem an.sig2a name


(** Returns the set of automata having 2 local states *)
let boolean_automata an =
	let folder a n bools =
        if n = 2 then ISet.add a bools else bools
	in
	Hashtbl.fold folder an.ls ISet.empty

let label_of_a an =
    Hashtbl.find an.a2sig

module LSOrd = struct type t = ls let compare = compare end

(** state **)

let state_union =
    IMap.union (fun a i j -> assert (i=j); Some i)

let substate (s1:state) (s2:state) =
    Util.imap_subset (=) s1 s2

let is_emptystate = IMap.is_empty

let compare_state s1 s2 =
	let c = compare (IMap.cardinal s1) (IMap.cardinal s2)
	in
	if c = 0 then IMap.compare compare s1 s2 else c

module StateOrd = struct type t = state let compare = compare_state end

module LSSet = Set.Make (LSOrd)
module LSMap = Map.Make (LSOrd)
module StateSet = Set.Make (StateOrd)

type objective = id*id*id (* a,i,j *)

module ObjOrd = struct type t = objective let compare = compare end
module ObjSet = Set.Make (ObjOrd)
module ObjMap = Map.Make (ObjOrd)

let obj_a (a,_,_) = a

let lsset_of_state s =
	IMap.fold (fun a i s -> LSSet.add (a,i) s) s LSSet.empty

let lsset_of_list = set_of_list LSSet.empty LSSet.add

let state_override state overlay =
    IMap.fold IMap.add overlay state


(** Context *)
type ctx = ISet.t IMap.t

let ctx_equal = IMap.equal ISet.equal

let ctx_empty = IMap.empty

let ctx_get = IMap.find

let ctx_safe_get a ctx =
	try ctx_get a ctx with Not_found -> ISet.empty

let ctx_has_ls (a,i) ctx =
	try
		ISet.mem i (ctx_get a ctx)
	with Not_found -> false

let ctx_add_ls (a,i) ctx =
	let is = ctx_safe_get a ctx
	in
	IMap.add a (ISet.add i is) ctx

let ctx_rm_ls (a,i) ctx =
	try
		let is = ctx_get a ctx
		in
		let is = ISet.remove i is
		in
		IMap.add a is ctx
	with Not_found -> ctx

let ctx_of_state state =
	IMap.map ISet.singleton state

let ctx_of_lslist =
	let group ctx ls =
        ctx_add_ls ls ctx
	in
	List.fold_left group IMap.empty

let ctx_of_lsset ps = ctx_of_lslist (LSSet.elements ps)

let ctx_of_siglocalstates ?(complete=false) an sls =
    let ctx = ctx_of_lslist (List.map (Hashtbl.find an.sig2ls) sls)
    in
	if complete then
	let complete_ctx a _ ctx = if IMap.mem a ctx then ctx
        else IMap.add a (ISet.singleton 0) ctx
	in
	Hashtbl.fold complete_ctx an.ls ctx
	else ctx

(*
let string_of_ctx ctx = 
	let folder a is str =
		str ^ (if str = "" then "" else "; ")
		^ a ^ "="^ string_of_iset is
	in
	"<"^(SMap.fold folder ctx "")^">"
*)

let lsset_of_ctx ctx =
	let register a is ps =
		let register i ps =
			LSSet.add (a,i) ps
		in
		ISet.fold register is ps
	in
	IMap.fold register ctx LSSet.empty

let ctx_is_state =
    IMap.for_all (fun _ is -> ISet.cardinal is == 1)

let state_of_ctx ctx =
	let register a is =
        assert (ISet.cardinal is == 1);
        IMap.add a (ISet.choose is)
    in
	IMap.fold register ctx IMap.empty

(*

let procs_to_ctx ps =
	let group (a,i) ctx =
		let is = try SMap.find a ctx with Not_found -> ISet.empty
		in
		let is = ISet.add i is
		in
		SMap.add a is ctx
	in
	PSet.fold group ps SMap.empty
;;

let ctx_sorts ctx =
	let register_sort a _ sorts =
		SSet.add a sorts
	in
	SMap.fold register_sort ctx SSet.empty
;;


let ctx_override ctx ps =
	ctx_override_by_ctx ctx (procs_to_ctx ps)
;;
*)

let ctx_override_by_ctx ctx ctx' =
	IMap.fold IMap.add ctx' ctx

let ctx_union ctx1 ctx2 =
	let register a is2 ctx1 =
		let is1 = try IMap.find a ctx1 with Not_found -> ISet.empty
		in
		IMap.add a (ISet.union is1 is2) ctx1
	in
	IMap.fold register ctx2 ctx1

(*
let ctx_union_state ctx state =
	let register a i ctx =
		let is = ctx_safe_get a ctx
		in
		SMap.add a (ISet.add i is) ctx
	in
	SMap.fold register state ctx
;;
*)

let ctx_inter ctx1 ctx2 =
	let register a is2 ctx =
        try
			let is1 = IMap.find a ctx1
			in
			IMap.add a (ISet.inter is1 is2) ctx
		with Not_found -> ctx
	in
	IMap.fold register ctx2 IMap.empty

(*
let ctx_diff ctx1 ctx2 =
	let diff a is ctx =
		let is =
			if SMap.mem a ctx2 then ISet.diff is (SMap.find a ctx2)
			else is
		in
		SMap.add a is ctx
	in
	SMap.fold diff ctx1 SMap.empty
;;
*)

let full_ctx an =
	let folder a n = IMap.add a (Util.srange 0 (n-1))
	in
	Hashtbl.fold folder an.ls ctx_empty

let flatten_stateset ss =
	let folder s ls =
		IMap.fold (fun a i -> LSSet.add (a,i)) s ls
	in
	StateSet.fold folder ss LSSet.empty

(*

let copy_an an = {
	automata = Hashtbl.copy an.automata;
	transitions = Hashtbl.copy an.transitions;
	conditions = Hashtbl.copy an.conditions;
	sync_transitions = an.sync_transitions;
}

let automaton_sdomain an a =
	let n = List.length (Hashtbl.find an.automata a)
	in
	Util.srange 0 (n-1)
*)

let new_an ?(nb_a=21) ?(nb_trs=53) () = {
    a2sig = Hashtbl.create nb_a;
    sig2a = Hashtbl.create nb_a;
    ls = Hashtbl.create nb_a;
    ls2sig = Hashtbl.create (nb_a*2);
    sig2ls = Hashtbl.create (nb_a*2);
	trs = Hashtbl.create nb_trs;
    lsnext = Hashtbl.create (nb_a*2);
    change2tr = Hashtbl.create (nb_a*2);
}

let new_an_copy_def ?(nb_trs=53) an =
    let nb_a = count_automata an
    in {
    a2sig = Hashtbl.copy an.a2sig;
    sig2a = Hashtbl.copy an.sig2a;
    ls = Hashtbl.copy an.ls;
    ls2sig = Hashtbl.copy an.ls2sig;
    sig2ls = Hashtbl.copy an.sig2ls;
	trs = Hashtbl.create nb_trs;
    lsnext = Hashtbl.create (nb_a*2);
    change2tr = Hashtbl.create (nb_a*2);
}

let changes_from_orig_dest orig dest =
    let get_changes a i changes =
        let j = IMap.find a dest
        in
        (a,i,j)::changes
    in
    IMap.fold get_changes orig []

let new_transition orig dest cond = {
        orig = orig;
        dest = dest;
        cond = cond;
        pre = state_union orig cond;
    }

let constants an ctx =
	let get_constants a _ cset =
        let is = IMap.find a ctx
        in
        if ISet.cardinal is != 1 || Hashtbl.mem an.lsnext (a,ISet.choose is)
        then cset else ISet.add a cset
	in
	Hashtbl.fold get_constants an.ls ISet.empty



(*
let resolve_siglocalstates an =
	List.map (fun (a,sig_i) -> (a,get_automaton_state_id an a sig_i))

let state_of_lsset ps =
	let fold (a,i) s =
		if SMap.mem a s then
			(if SMap.find a s != i then
				failwith "state_of_lsset: invalid state"
			else s)
		else
			SMap.add a i s
	in
	LSSet.fold fold ps SMap.empty



let lsset_of_ctx ctx =
	let register a is ps =
		let register i ps =
			LSSet.add (a,i) ps
		in
		ISet.fold register is ps
	in
	SMap.fold register ctx LSSet.empty


let an_compl_ctx an ctx =
	let fold a is ctx =
		let js = automaton_sdomain an a
		in
		let js = ISet.diff js is
		in
		SMap.add a js ctx
	in
	SMap.fold fold ctx SMap.empty

let condition_matches ctx cond =
	let ls_matches a i =
		try ISet.mem i (SMap.find a ctx)
		with Not_found -> false
	in
	SMap.for_all ls_matches cond
*)

let declare_automaton an name sigstates =
	assert (not (has_automaton an name));
    let a = new_id ()
    and n = List.length sigstates
    in
    Hashtbl.add an.a2sig a name;
    Hashtbl.add an.sig2a name a;
    List.iteri (fun i isig ->
        let ls = (a,i)
        and sigls = (name, isig)
        in
        Hashtbl.add an.ls2sig ls sigls;
        Hashtbl.add an.sig2ls sigls ls) sigstates;
    Hashtbl.add an.ls a n

let declare_transition an sig_changes sig_cond =
    let trid = new_id ()
	and cond = resolve_sig_state an sig_cond
    and register_change ai j =
        let js = Hashtbl.find_all an.lsnext ai
        in
        if not (List.mem j js) then
            Hashtbl.add an.lsnext ai j
    in
    let fold (orig,dest) (aname, sig_i, sig_j) =
        let a, i = Hashtbl.find an.sig2ls (aname, sig_i)
        and _, j = Hashtbl.find an.sig2ls (aname, sig_j)
        in
        (if IMap.mem a orig then
            failwith ("multiple changes are declared for '"^aname^"'"));
        (if IMap.mem a cond then
            failwith ("enabling condition should not refer to '"^aname^"'"));
        Hashtbl.add an.change2tr ((a,i),j) trid;
        register_change (a,i) j;
        IMap.add a i orig, IMap.add a j dest
    in
    let orig, dest = List.fold_left fold (IMap.empty, IMap.empty) sig_changes
    in
    Hashtbl.add an.trs trid (new_transition orig dest cond)


(** String representations **)

let string_of_a = label_of_a

let protect_str protect s = if protect then "\""^s^"\"" else s

let string_of_sigls ?(protect=true) = function
	  StateId i -> string_of_int i
	| StateLabel n -> protect_str protect n

let string_of_lsid an a i =
    let _, sigi = Hashtbl.find an.ls2sig (a,i)
    in
    string_of_sigls sigi

let string_of_ls ?(protect=false) an ai =
    let sa, sigi = Hashtbl.find an.ls2sig ai
    in
    let si = string_of_sigls ~protect sigi
    in
    protect_str protect sa^"="^si

let string_of_lsids an a =
    string_of_set (string_of_lsid an a) ISet.elements

let string_of_cls an a lsids =
    let sa = string_of_a an a
    and scls = string_of_lsids an a lsids
    in
    sa^"="^scls

let string_of_lsset an =
    string_of_set (string_of_ls an) LSSet.elements

let string_of_state an =
    string_of_map (string_of_ls an) IMap.fold

let string_of_obj an (a,i,j) =
    let sa, i = Hashtbl.find an.ls2sig (a,i)
    and _, j = Hashtbl.find an.ls2sig (a,j)
    in
    let si = string_of_sigls i
    and sj = string_of_sigls j
    in
    sa^" "^si^" "^sj

let string_of_local_transition an (a,i,j) =
    let aname, i = Hashtbl.find an.ls2sig (a,i)
    and _, j = Hashtbl.find an.ls2sig (a,j)
    in
	"\""^aname^"\" "^(string_of_sigls i)^" -> " ^(string_of_sigls j)

let string_of_local_condition ?(protect=true) an cond =
	match IMap.bindings cond with [] -> ""
	| cond -> (" when "^String.concat " and "
				(List.map (string_of_ls ~protect an) cond))

let string_of_transition ?(protect=true) an tr =
    (match changes_from_orig_dest tr.orig tr.dest with
      [obj] -> string_of_local_transition an obj
    | changes -> ("{ "^(String.concat " ; "
			(List.map (string_of_local_transition an) changes))^" }"))
	^ string_of_local_condition ~protect an tr.cond



(** JSON representations **)

let json_of_a an a =
    json_of_str (Hashtbl.find an.a2sig a)

let json_of_sigls = function
      StateId i -> json_of_int i
    | StateLabel l -> json_of_str l

let json_of_state an s =
    let json_of_kv a i =
        let aname, isig = Hashtbl.find an.ls2sig (a,i)
        in
        json_of_str aname, json_of_sigls isig
    in
	json_of_bindings json_of_kv (IMap.bindings s)

let json_of_ctx an ctx =
	let json_of_kv a iset =
        json_of_a an a,
        let sigs = List.map (fun i -> snd (Hashtbl.find an.ls2sig (a,i)))
            (ISet.elements iset)
        in
        match sigs with
          [isig] -> json_of_sigls isig
        | _ -> json_of_list json_of_sigls sigs
	in
	json_of_bindings json_of_kv (IMap.bindings ctx)

let json_of_transition an tr =
	let json_conds = json_of_state an tr.cond
	in
    let json_obj (a,i,j) =
        let aname, isig = Hashtbl.find an.ls2sig (a,i)
        and _, jsig = Hashtbl.find an.ls2sig (a,j)
        in
        [json_of_str aname;json_of_sigls isig;json_of_sigls jsig]
    in
    let jl = match changes_from_orig_dest tr.orig tr.dest with
      [obj] -> json_obj obj
    | changes -> [json_of_list id (List.map (fun obj ->
                json_of_list id (json_obj obj)) changes)]
    in
    json_of_list id (jl @ [json_conds])


