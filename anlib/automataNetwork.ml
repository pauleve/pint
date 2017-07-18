
open Big_int

open PintTypes

open Ph_types

type id = int
type idmap = id IMap.t

type lsid = id*id

type sig_automaton = string
type sig_automaton_state = StateId of int | StateLabel of string
type sig_local_state = sig_automaton * sig_automaton_state

let next_trid = ref 0

type t = {

    (* signature/internal id mapping *)
    a2sig: (id, sig_automaton) Hashtbl.t;
    ls2sig: (id, sig_local_state) Hashtbl.t;
    sig2a: (sig_automaton, id) Hashtbl.t;
    sig2ls: (sig_local_state, id) Hashtbl.t;

    (* local transitions *)
    trcond: (id, idmap) Hashtbl.t;
    trdest: (id, idmap) Hashtbl.t;
    trorig: (id, idmap) Hashtbl.t;

    (* local states graph *)
    lsnext: (lsid, id) Hashtbl.t; (* out-going transitions *)
    lsprev: (lsid, id) Hashtbl.t; (* in-going transitions *)
}



type automaton_p = string
type automaton_state = int
type local_state = automaton_p * automaton_state

module LSOrd = struct type t = local_state let compare = compare end

(** state **)

let substate (s1:state) (s2:state) = Util.smap_subset (=) s1 s2

let is_emptystate = SMap.is_empty

let compare_state s1 s2 =
	let c = compare (SMap.cardinal s1) (SMap.cardinal s2)
	in
	if c = 0 then SMap.compare compare s1 s2 else c

module StateOrd = struct type t = state let compare = compare_state end

module LSSet = Set.Make (LSOrd)
module LSMap = Map.Make (LSOrd)
module StateSet = Set.Make (StateOrd)

let lsset_of_state s =
	SMap.fold (fun a i s -> LSSet.add (a,i) s) s LSSet.empty

let lsset_of_list = set_of_list LSSet.empty LSSet.add

let flatten_stateset ss =
	let folder s ls =
		SMap.fold (fun a i ls -> LSSet.add (a,i) ls) s ls
	in
	StateSet.fold folder ss LSSet.empty


type local_transition = (int * (local_state list))

type transition = automaton_p
						* automaton_state
						* automaton_state
type trcond = automaton_state SMap.t

let tr_dest (_,_,j) = j

type t = {
	automata: (automaton_p, (sig_automaton_state * automaton_state) list) Hashtbl.t;
	transitions: (local_state, ISet.t) Hashtbl.t;
	conditions: (transition, trcond) Hashtbl.t;
	sync_transitions: (transition list * trcond) list;
}

let empty_an ?size:(size=(20,50)) () = {
	automata = Hashtbl.create (fst size);
	transitions = Hashtbl.create (snd size);
	conditions = Hashtbl.create (snd size);
	sync_transitions = [];
}

let copy_an an = {
	automata = Hashtbl.copy an.automata;
	transitions = Hashtbl.copy an.transitions;
	conditions = Hashtbl.copy an.conditions;
	sync_transitions = an.sync_transitions;
}

let automata_limits an =
	let folder a def lims =
		(a, List.length def - 1)::lims
	in
	Hashtbl.fold folder an.automata []

let automaton_sdomain an a =
	let n = List.length (Hashtbl.find an.automata a)
	in
	Util.srange 0 (n-1)

let full_ctx an =
	let folder a def ctx =
		let is = List.fold_left (fun is i -> ISet.add i is) ISet.empty (List.map snd def)
		in
		SMap.add a is ctx
	in
	Hashtbl.fold folder an.automata ctx_empty

let get_automaton_state_sig an a i =
	Util.list_lassoc i (Hashtbl.find an.automata a)

let get_automaton_state_id an a sig_i =
	List.assoc sig_i (Hashtbl.find an.automata a)

let string_of_sig_state ?(protect=true) = function
	  StateId i -> string_of_int i
	| StateLabel n -> if protect then "\""^n^"\"" else n

let string_of_astate ?(protect=true) an a i =
	string_of_sig_state ~protect (get_automaton_state_sig an a i)

let string_of_localstate ?(protect=true) an (a,i) =
	(if protect then ("\""^a^"\"") else a)^"="^string_of_astate ~protect an a i

let string_of_localstates an lsset =
	String.concat ", " (List.map (string_of_localstate an) (SMap.bindings lsset))

let string_of_ls (a,i) =
	a^" "^string_of_int i

let string_of_lsset lsset =
	String.concat ", " (List.map string_of_ls (LSSet.elements lsset))

let _string_of_local_transition an (a,i,j) =
	"\""^a^"\" "^(string_of_astate an a i)^" -> " ^(string_of_astate an a j)

let _string_of_local_condition an cond =
	match SMap.bindings cond with [] -> ""
	| cond -> (" when "^String.concat " and "
				(List.map (string_of_localstate an) cond))

let string_of_transition an (a,i,j) cond =
	(_string_of_local_transition an (a,i,j))
	^ _string_of_local_condition an cond

let string_of_sync_transition an trs cond =
	"{ "^(String.concat " ; "
			(List.map (_string_of_local_transition an) trs))^" }"
	^ _string_of_local_condition an cond

let count_automata an = Hashtbl.length an.automata

let count_local_states an =
	let count a ls_defs c =
		List.length ls_defs + c
	in
	Hashtbl.fold count an.automata 0

let count_states an =
	let count a ls_defs c =
		mult_int_big_int (List.length ls_defs) c
	in
	Hashtbl.fold count an.automata unit_big_int

let count_transitions an =
	Hashtbl.length an.conditions

let boolean_automata an =
	let folder a def bools =
		if List.length def = 2 then SSet.add a bools else bools
	in
	Hashtbl.fold folder an.automata SSet.empty

let has_automaton an name = Hashtbl.mem an.automata name

let declare_automaton an a sigstates =
	assert (not (has_automaton an a));
	let register_sig (sigassoc, i) sig_i =
		Hashtbl.add an.transitions (a,i) ISet.empty;
		(sig_i,i)::sigassoc, i+1
	in
	let sigassoc, _ = List.fold_left register_sig ([], 0) sigstates
	in
	Hashtbl.add an.automata a (List.rev sigassoc)

let resolve_sig_transition an (a,sig_i,sig_j) =
	let i = get_automaton_state_id an a sig_i
	and j = get_automaton_state_id an a sig_j
	in
	a,i,j

let resolve_sig_conditions an sig_conds =
	List.fold_left
		(fun cond (b,sig_k) ->
			let k = get_automaton_state_id an b sig_k
			in
			try
				let k' = SMap.find b cond
				in
				if k <> k' then
					failwith ("enabling condition cannot contain "
						^"two different local states of a same automaton")
				else cond
			with Not_found -> SMap.add b k cond) SMap.empty sig_conds

let declare_transition an a sig_i sig_j sig_conds =
	let a,i,j = resolve_sig_transition an (a, sig_i, sig_j)
	and conds = resolve_sig_conditions an sig_conds
	in
	(if SMap.mem a conds then
		failwith ("enabling condition should not refer to '"^a^"'"));
	let trs = Hashtbl.find an.transitions (a,i)
	in
	Hashtbl.replace an.transitions (a,i) (ISet.add j trs);
	Hashtbl.add an.conditions (a, i, j) conds

let is_async_automata_network an =
	an.sync_transitions = []

let assert_async_an an =
	(if an.sync_transitions <> [] then
		failwith "Automata networks with synchronous transition is not supported
		yet.")

let declare_sync_transition an sig_trs sig_conds =
	let trs = List.map (resolve_sig_transition an) sig_trs
	and conds =resolve_sig_conditions an sig_conds
	in
	(List.iter (fun (a,_,_) ->
		(if SMap.mem a conds then
			failwith ("enabling condition should not refer to '"^a^"'"))) trs);
	{an with
		sync_transitions = (trs,conds)::an.sync_transitions
	}

let an_replace_trconditions an tr conds =
	while Hashtbl.mem an.conditions tr do
		Hashtbl.remove an.conditions tr
	done;
	match conds with
	  [] -> (match tr with (a,i,j) ->
	  	let js = Hashtbl.find an.transitions (a,i)
		in
		let js = ISet.remove j js
		in
		Hashtbl.replace an.transitions (a,i) js)
	| _ ->
		let conds = List.sort_uniq (SMap.compare compare) conds
		in
		List.iter (Hashtbl.add an.conditions tr) conds


module TRSet = Set.Make (struct
	type t = transition * automaton_state SMap.t
	let compare (tr, conds) (tr', conds') =
		let ctr = compare tr tr'
		in
		if ctr = 0 then SMap.compare compare conds conds' else ctr
end)

let an_sorted_transitions an =
	Hashtbl.fold (fun t c trs ->
		TRSet.add (t,c) trs) an.conditions TRSet.empty

let partial an sset =
	let an' = empty_an ~size:(SSet.cardinal sset, 50) ()
	in
	let match_lsset =
		SMap.for_all (fun a _ -> SSet.mem a sset)
	in
	let register_localstate a (sigs,i) =
		Hashtbl.add an'.transitions (a,i) ISet.empty
	in
	let register_automaton a def =
		if SSet.mem a sset then
			(Hashtbl.add an'.automata a def;
			List.iter (register_localstate a) def)
	and register_condition (a,i,j) lsset =
		if SSet.mem a sset && match_lsset lsset then
			let trs = Hashtbl.find an'.transitions (a,i)
			in
			(Hashtbl.replace an'.transitions (a,i) (ISet.add j trs);
			Hashtbl.add an'.conditions (a,i,j) lsset)
	in
	Hashtbl.iter register_automaton an.automata ;
	Hashtbl.iter register_condition an.conditions;
	an'

let remove_automata (an,ctx) aset =
    let an' = empty_an ~size:(Hashtbl.length an.automata - SSet.cardinal aset,
            Hashtbl.length an.transitions) ()
	in
	let match_lsset trcond =
		not (SMap.exists (fun a _ -> SSet.mem a aset) trcond)
	in
	let register_localstate a (sigs,i) =
		Hashtbl.add an'.transitions (a,i) ISet.empty
	in
	let register_automaton a def =
		if not (SSet.mem a aset) then (
            Hashtbl.add an'.automata a def;
			List.iter (register_localstate a) def)
	and register_condition (a,i,j) lsset =
		if not (SSet.mem a aset) && match_lsset lsset then
			let trs =  Hashtbl.find an'.transitions (a,i)
			in
			(Hashtbl.replace an'.transitions (a,i) (ISet.add j trs);
			Hashtbl.add an'.conditions (a,i,j) lsset)
	in
	Hashtbl.iter register_automaton an.automata ;
	Hashtbl.iter register_condition an.conditions;
    let match_sync_tr (trs, trcond) =
        List.for_all (fun (a,_,_) -> not (SSet.mem a aset)) trs
            && match_lsset trcond
    in
    let sync_transitions = List.filter match_sync_tr an.sync_transitions
    in
    {automata = an'.automata;
        transitions = an'.transitions;
        conditions = an'.conditions;
        sync_transitions = sync_transitions},
    SMap.filter (fun a _ -> not (SSet.mem a aset)) ctx

let restrict an ctx =
	let an' = empty_an ~size:(count_automata an, 50) ()
	in
	let allow a i =
		try ISet.mem i (SMap.find a ctx) with Not_found -> true
	in
	let match_lsset = SMap.for_all allow
	in
	let register_localstate a (sigs,i) =
		Hashtbl.add an'.transitions (a,i) ISet.empty
	in
	let register_automaton a def =
		(Hashtbl.add an'.automata a def;
		List.iter (register_localstate a) def)
	and register_condition (a,i,j) lsset =
		if allow a i && allow a j && match_lsset lsset then
			let trs = Hashtbl.find an'.transitions (a,i)
			in
			(Hashtbl.replace an'.transitions (a,i) (ISet.add j trs);
			Hashtbl.add an'.conditions (a,i,j) lsset)
	in
	Hashtbl.iter register_automaton an.automata ;
	Hashtbl.iter register_condition an.conditions;
	an'

let simplify an =
	let conditions = Hashtbl.create (Hashtbl.length an.conditions / 2)
	and sd = Hashtbl.fold (fun a def -> SMap.add a (List.map snd def))
				an.automata SMap.empty
	in
	let simplify_transition a i j =
		let vs = Hashtbl.find_all an.conditions (a,i,j)
		in
		let vs = ValSet.simplify_with_bse sd vs
		in
		List.iter (Hashtbl.add conditions (a,i,j)) vs
	in
	let simplify_transitions (a,i) js =
		ISet.iter (simplify_transition a i) js
	in
	Hashtbl.iter simplify_transitions an.transitions;
	{an with conditions = conditions}


let constants an =
	let get_constants a is cset =
		if List.for_all (fun (_,i) ->
			try ISet.is_empty (Hashtbl.find	an.transitions (a,i))
			with Not_found -> true) is then
				SSet.add a cset else cset
	in
	Hashtbl.fold get_constants an.automata SSet.empty

(** [squeeze an ctx] removes constant automata and unused local states.
	The id of local state will probably by modified afterward. *)
let squeeze ?(preserve=SSet.empty) an ctx =
	(* get constant automata *)
	let cset = constants an
	and ukn = SMap.fold (fun a is uset ->
							if ISet.cardinal is > 1 then SSet.add a uset
								else uset) ctx SSet.empty
	in
	let cset = SSet.diff cset ukn
    in
    let cset = SSet.diff cset preserve
	in
	let ctx' = Util.smap_remove_keys ctx cset
	and an' = empty_an ~size:(Hashtbl.length an.automata - SSet.cardinal cset, 50) ()
	in

	(* fetch referenced local states *)
	let register_local_states (a,i,j) lsset smap =
		let smap = ctx_add_proc (a,i) smap
		in
		let smap = ctx_add_proc (a,j) smap
		in
		ctx_union_state smap lsset
	in
	let used = Hashtbl.fold register_local_states an.conditions ctx'
	in
	let used = Util.smap_remove_keys used cset
	in
	(* map for renaming *)
	let sigmamap = SMap.mapi (fun a iset ->
					let register i (n, sigma) =
						(n+1, IMap.add i (if SSet.mem a preserve then i else n)
                                sigma)
					in
					snd(ISet.fold register iset (0, IMap.empty))) used
	in
	let relabel a i = IMap.find i (SMap.find a sigmamap)
	in

	let match_lsset lsset =
		(* ensure that the conditions match with the constant automata init value *)
		SSet.for_all (fun a ->
			try ISet.mem (SMap.find a lsset) (SMap.find a ctx)
			with Not_found -> true) cset
	in
	let register_localstate a (sigs,i) =
		Hashtbl.add an'.transitions (a,i) ISet.empty
	in
	let register_automaton a def =
		if not (SSet.mem a cset) then
            let def =
                if SSet.mem a preserve then def else
                let register def (sig_i, i) =
                    try
                        let i = relabel a i
                        and sig_i = match sig_i with
                              StateId i -> StateId (relabel a i)
                            | _ -> sig_i
                        in
                        (sig_i, i)::def
                    with Not_found -> def
                in
                let def = List.fold_left register [] def
                in
                List.fast_sort (fun a b -> compare (snd a) (snd b)) def
			in
			(Hashtbl.add an'.automata a def;
			List.iter (register_localstate a) def)
	and register_condition (a,i,j) lsset =
		if match_lsset lsset then
			let i = relabel a i
			and j = relabel a j
			and lsset = Util.smap_remove_keys lsset cset
			in
			let trs = Hashtbl.find an'.transitions (a,i)
			and lsset = SMap.mapi relabel lsset
			in
			(Hashtbl.replace an'.transitions (a,i) (ISet.add j trs);
			Hashtbl.add an'.conditions (a,i,j) lsset)
	in
	Hashtbl.iter register_automaton an.automata ;
	Hashtbl.iter register_condition an.conditions;
	let ctx' = SMap.mapi (fun a iset ->
			ISet.fold (fun i iset -> ISet.add (relabel a i) iset) iset ISet.empty) ctx'
	in
	an', ctx'

let string_of_state an s =
	String.concat ", " (List.map (string_of_localstate an) (SMap.bindings s))

let resolve_siglocalstates an =
	List.map (fun (a,sig_i) -> (a,get_automaton_state_id an a sig_i))
(**
	Context
**)

let ctx_of_siglocalstates ?(complete=false) an sls =
	let fold_localstate ctx (a,sig_i) =
		let i = get_automaton_state_id an a sig_i
		in
		Ph_types.ctx_add_proc (a,i) ctx
	in
	let ctx = List.fold_left fold_localstate Ph_types.ctx_empty sls
	in
	if complete then
	let complete_ctx a _ ctx =
		if not (SMap.mem a ctx) then
			SMap.add a (ISet.singleton 0) ctx
		else ctx
	in
	Hashtbl.fold complete_ctx an.automata ctx
	else ctx

let ctx_has_localstate = Ph_types.ctx_has_proc

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

let ctx_of_lslist =
	let group ctx (a,i) =
		let is = try SMap.find a ctx with Not_found -> ISet.empty
		in
		let is = ISet.add i is
		in
		SMap.add a is ctx
	in
	List.fold_left group SMap.empty

let ctx_of_lsset ps = ctx_of_lslist (LSSet.elements ps)

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


(** JSON outputs **)

let json_of_state s =
	json_of_bindings json_of_str json_of_int (SMap.bindings s)

let json_of_ctx ctx =
	let json_of_elt iset =
		if ISet.cardinal iset = 1 then
			json_of_int (ISet.choose iset)
		else
			json_of_list json_of_int (ISet.elements iset)
	in
	json_of_bindings json_of_str json_of_elt (SMap.bindings ctx)

let json_of_conds conds =
    json_of_bindings json_of_str json_of_int (SMap.bindings conds)

let json_of_transition (a,i,j) conds =
	let json_conds = json_of_conds conds
	in
	json_of_list id (json_of_str a::json_of_int i::json_of_int j::json_conds::[])

let json_of_sync_transition (aijs, conds) =
    let json_conds = json_of_conds conds
    in
    let json_aijs = json_of_list id (List.map (fun (a,i,j) ->
            json_of_list id (json_of_str a::json_of_int i::json_of_int j::[]))
                aijs)
    in
    json_of_list id (json_aijs::json_conds::[])


