
open Debug

open PintTypes
open AutomataNetwork

let enumerate_acyclic_paths register append discard elt0 set0 an (a,i,goal) =
	let rec walk path i visited results =
		if i = goal then
			register results path
		else
			let visited = ISet.add i visited
			and nexts = Hashtbl.find_all an.lsnext (a,i)
			in
			let visit results j =
				if ISet.mem j visited then results
				else
					let path = append results path ((a,i),j)
					in
					if discard results path then
						results
					else
						walk path j visited results
			in
            List.fold_left visit results nexts
	in
	walk elt0 i ISet.empty set0

(**
    Compute raw local paths, i.e., local paths expressed only in term of local
    state changes and not resolved yet into transitions.
**)
let raw_local_paths =
	let paths = []
	and path0 = []
	and append _ path tr = path@[tr]
	and register paths path = path::paths
	and discard _ _ = false
	in
	enumerate_acyclic_paths register append discard path0 paths

type local_path = trid list

let local_paths an obj =
    let expand lps lp = lps @
        let x = List.map (Hashtbl.find_all an.change2tr) lp
        in
        Util.cross_list x
    and lps = raw_local_paths an obj
    in
    List.fold_left expand [] lps

type 'a lp_collection = {
    reg: 'a reg_t;
    byobj: (objective, id list * 'a list) Hashtbl.t;
}

let new_lp_collection nblp nbobj = {
    reg = new_reg nblp ;
    byobj = Hashtbl.create nbobj
}

let _cache_computation reg f obj =
    try Hashtbl.find reg obj
    with Not_found ->
        let elt = f obj
        in
        (Hashtbl.add reg obj elt; elt)

let persistent_local_paths lpc an =
    _cache_computation lpc.byobj (fun obj ->
        let lps = local_paths an obj
        in
        let ilps = List.map (register_elt lpc.reg) lps
        in
        (ilps, lps))


type abstract_local_path = {
    obj: objective;
    conds: ls list list;
    interm: int list;
    ext_post: ls list;
}

let abstract_local_path an obj trs =
    let a = obj_a obj
    in
    let interm = match trs with [] -> [] | _ ->
        List.map (fun trid ->
            IMap.find a (Hashtbl.find an.trs trid).orig) (List.tl trs)
    and fold (conds, ext_post) trid =
        let tr = Hashtbl.find an.trs trid
        in
        let trconds = IMap.remove a tr.pre
        and trpost = IMap.remove a tr.dest
        in
        (if IMap.is_empty trconds then conds else
        StateSet.add trconds conds),
        (if IMap.is_empty trpost then ext_post else
        IMap.fold (fun a i -> LSSet.add (a,i)) trpost ext_post)
    in
    let conds, ext_post = List.fold_left fold (StateSet.empty, LSSet.empty) trs
    in
    let conds = List.map IMap.bindings (StateSet.elements conds)
    and ext_post = LSSet.elements ext_post
    in
    { obj = obj; conds = conds; interm = interm; ext_post = ext_post }

let string_of_abstract_local_path an alp =
    let string_of_view =
        string_of_list ~lbracket:"" ~rbracket:"" ~delim:","
            (function [ls] -> string_of_ls an ls
                | state -> string_of_list ~lbracket:"<" ~rbracket:">"
                            (string_of_ls an) state)
    in
    "<"^string_of_view alp.conds
        ^"/"^string_of_list string_of_int alp.interm
        ^"/"^string_of_list (string_of_ls an) alp.ext_post
        ^">"

(*
let alp_leq (v1,i1) (v2,i2) =
       ISet.equal i1 i2
    && StateSet.for_all (fun s1 -> StateSet.exists (substate s1) v2) v1

let alp_compare (v1,i1) (v2,i2) =
    let cmp = ISet.compare i1 i2
    in
    if cmp <> 0 then cmp else
    if StateSet.equal v1 v2 then 0 else
    if StateSet.for_all (fun s1 -> StateSet.exists (substate s1) v2) v1 then
        -1 else
    if StateSet.for_all (fun s1 -> StateSet.exists (substate s1) v1) v2 then
        1 else
    StateSet.compare v1 v2
*)

type abstract_collection = {
    lpc: local_path lp_collection;
    lp_filter: (trid list -> bool) option;
    alpc: abstract_local_path lp_collection;
    concrete: (id, id) Hashtbl.t;
}

let new_abstract_collection lpc nblp nbobj = {
    lpc = lpc;
    lp_filter = None;
    alpc = new_lp_collection nblp nbobj;
    concrete = Hashtbl.create nblp
}

let reset_abstract_collection ac =
    let nblp = Hashtbl.length ac.concrete
    and nbobj = Hashtbl.length ac.alpc.byobj
    in
    {ac with
        alpc = new_lp_collection nblp nbobj;
        concrete = Hashtbl.create nblp; }

let abstract_local_paths ac an obj =
    let ilps, lps = persistent_local_paths ac.lpc an obj
    in
    let ilps, lps = match ac.lp_filter with None -> ilps, lps
        | Some filter ->
            let filter2 (ilps, lps) ilp lp =
                if filter lp then (ilp::ilps, lp::lps) else (ilps, lps)
            in
            List.fold_left2 filter2 ([],[]) ilps lps
    in
    let alps = List.map (abstract_local_path an obj) lps
    in
    let register ialps alp ilp =
        let ialp = register_elt ac.alpc.reg alp
        in
        Hashtbl.add ac.concrete ialp ilp;
        ISet.add ialp ialps
    in
    List.fold_left2 register ISet.empty alps ilps

let resolve_indexed_alps ac ialps =
    List.map (Hashtbl.find ac.alpc.reg.id2elt) ialps

let abstract_local_paths ac an =
    _cache_computation ac.alpc.byobj (fun obj ->
        let ialps = abstract_local_paths ac an obj
        in
        let ialps = ISet.elements ialps
        in
        let alps = resolve_indexed_alps ac ialps
        in
        (ialps, alps))

let restricted_abstract_local_paths ac an overlay obj =
    match Hashtbl.find_opt overlay obj with
      Some ialps -> (ialps, resolve_indexed_alps ac ialps)
    | None -> abstract_local_paths ac an obj

let extract_local_states alp =
    List.flatten alp.conds

let extract_transitions_from_ialp ?(trs=ISet.empty) ac ialp =
    let pull_trs trs ilp =
        let lp = Hashtbl.find ac.lpc.reg.id2elt ilp
        in
        List.fold_left (fun trs tri -> ISet.add tri trs) trs lp
    in
    let ilps = Hashtbl.find_all ac.concrete ialp
    in
    List.fold_left pull_trs trs ilps


