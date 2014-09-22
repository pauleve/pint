
open PintTypes

type sig_automaton_p = string
type sig_automaton_state = StateId of int | StateLabel of string

type automaton_p = string
type automaton_state = int
type local_state = automaton_p * automaton_state

type local_transition = (int * (local_state list))

module LSMap = Map.Make (struct type t = local_state let compare = compare end)

type t = {
	automata: ((sig_automaton_state * automaton_state) list) SMap.t;
	transitions: (local_transition list) LSMap.t
}

let get_automaton_state_sig an a i =
	Util.list_lassoc i (SMap.find a an.automata)

let get_automaton_state_id an a sig_i =
	List.assoc sig_i (SMap.find a an.automata)

let string_of_sig_state = function
	  StateId i -> string_of_int i
	| StateLabel n -> n

let string_of_astate an a i =
	string_of_sig_state (get_automaton_state_sig an a i)

let string_of_localstate an (a,i) =
	a^"="^string_of_astate an a i

let empty_an () = {
	automata = SMap.empty;
	transitions = LSMap.empty;
}

let has_automaton an name = SMap.mem name an.automata

let declare_automaton an a sigstates =
	assert (not (has_automaton an a));
	let register_sig (sigassoc, tr, i) sig_i =
		(sig_i,i)::sigassoc, LSMap.add (a,i) [] tr, i+1
	in
	let sigassoc, tr, _ = List.fold_left register_sig ([], an.transitions, 0)
								sigstates
	in
	{(*an with*)
		automata = SMap.add a sigassoc an.automata;
		transitions = tr;
	}

let declare_transition an a sig_i sig_j sig_conds =
	let i = get_automaton_state_id an a sig_i
	and j = get_automaton_state_id an a sig_j
	and conds = List.map (fun (b,k) -> (b, get_automaton_state_id an b k)) sig_conds
	in
	let trs = LSMap.find (a,i) an.transitions
	in
	{an with transitions = LSMap.add (a,i) ((j,conds)::trs) an.transitions}


let ctx_has_localstate = Ph_types.ctx_has_proc


