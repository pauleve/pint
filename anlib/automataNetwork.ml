
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

let string_of_sig_state = function
	  StateId i -> string_of_int i
	| StateLabel n -> "\""^n^"\""

let string_of_astate an a i =
	string_of_sig_state (get_automaton_state_sig an a i)

let string_of_localstate ?(protect=true) an (a,i) =
	(if protect then ("\""^a^"\"") else a)^"="^string_of_astate an a i

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

