
open PintTypes

type sig_automaton_p = string
type sig_automaton_state = StateId of int | StateLabel of string
type sig_local_state = sig_automaton_p * sig_automaton_state

type automaton_p = string
type automaton_state = int
type local_state = automaton_p * automaton_state

module LSSet = Set.Make (struct type t = local_state let compare = compare end)
module LSMap = Map.Make (struct type t = local_state let compare = compare end)

let lsset_of_list = set_of_list LSSet.empty LSSet.add

type local_transition = (int * (local_state list))

type transition = automaton_p 
						* automaton_state 
						* automaton_state 

let tr_dest (_,_,j) = j

type t = {
	automata: (automaton_p, (sig_automaton_state * automaton_state) list) Hashtbl.t;
	transitions: (local_state, ISet.t) Hashtbl.t;
	conditions: (transition, LSSet.t) Hashtbl.t;
}

let empty_an ?size:(size=(20,50)) () = {
	automata = Hashtbl.create (fst size);
	transitions = Hashtbl.create (snd size);
	conditions = Hashtbl.create (snd size);
}

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
	String.concat ", " (List.map (string_of_localstate an) (LSSet.elements lsset))

let string_of_ls (a,i) =
	a^" "^string_of_int i

let string_of_lsset lsset =
	String.concat ", " (List.map string_of_ls (LSSet.elements lsset))

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

let declare_transition an a sig_i sig_j sig_conds =
	let i = get_automaton_state_id an a sig_i
	and j = get_automaton_state_id an a sig_j
	and conds = List.fold_left
					(fun lsset (b,sig_k) -> 
							let k = get_automaton_state_id an b sig_k
							in
							LSSet.add (b,k) lsset) LSSet.empty sig_conds
	in
	let trs = Hashtbl.find an.transitions (a,i)
	in
	Hashtbl.replace an.transitions (a,i) (ISet.add j trs);
	Hashtbl.add an.conditions (a, i, j) conds


let partial an sset =
	let an' = empty_an ~size:(SSet.cardinal sset, 50) ()
	in
	let match_lsset =
		LSSet.for_all (fun (a,_) -> SSet.mem a sset)
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

let simplify an =
	let conditions = Hashtbl.create (Hashtbl.length an.conditions / 2)
	and sd = Hashtbl.fold (fun a def -> SMap.add a (List.map snd def))
				an.automata SMap.empty
	in
	let state_of_cond lsset =
		LSSet.fold (fun (a,i) s -> SMap.add a i s) lsset SMap.empty
	and cond_of_state s =
		SMap.fold (fun a i lsset -> LSSet.add (a,i) lsset) s LSSet.empty
	in
	let simplify_transition a i j =
		let vs = Hashtbl.find_all an.conditions (a,i,j)
		in
		let vs = List.map state_of_cond vs
		in
		let vs = ValSet.simplify sd vs
		in
		let vs = List.map cond_of_state vs
		in
		List.iter (Hashtbl.add conditions (a,i,j)) vs
	in
	let simplify_transitions (a,i) js =
		ISet.iter (simplify_transition a i) js
	in
	Hashtbl.iter simplify_transitions an.transitions;
	{an with conditions = conditions}


let string_of_state an s =
	String.concat ", " (List.map (string_of_localstate an) (SMap.bindings s))

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

let ctx_of_lsset ps =
	let group (a,i) ctx =
		let is = try SMap.find a ctx with Not_found -> ISet.empty
		in
		let is = ISet.add i is
		in
		SMap.add a is ctx
	in
	LSSet.fold group ps SMap.empty

let lsset_of_ctx ctx =
	let register a is ps =
		let register i ps =
			LSSet.add (a,i) ps
		in
		ISet.fold register is ps
	in
	SMap.fold register ctx LSSet.empty

