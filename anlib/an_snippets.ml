
open PintTypes
open InteractionGraph
open AutomataNetwork

let an_of_ig ig limits =
	let an = empty_an ()
	in
	let register_node (a,l) =
		let sigstates = List.map (fun i -> StateId i) (Util.range 0 l)
		in
		declare_automaton an a sigstates
	in
	let register_transition b j j' a i =
		if a <> b || j = i then (
			let trs = Hashtbl.find an.transitions (b,j)
			in
			Hashtbl.replace an.transitions (b,j) (ISet.add j' trs);
			Hashtbl.add an.conditions (b,j,j')
				(if a <> b then SMap.singleton a i else SMap.empty))
	in
	let register_transitions b targets jump a i =
		List.iter (fun j -> register_transition b j (j+jump) a i) targets
	in
	let register_interaction b (a, i, s) =
		let la = List.assoc a limits
		and lb = List.assoc b limits
		in
		let a_top = Util.range i la
		and a_bot = Util.range 0 (i-1)
		and b_incr = Util.range 0 (lb-1)
		and b_decr = Util.range 1 lb
		in
		let a_act, a_inh =
			match s with Positive -> a_top, a_bot
				| Negative -> a_bot, a_top
		in
		List.iter (register_transitions b b_incr 1 a) a_act;
		List.iter (register_transitions b b_decr (-1) a) a_inh
	in
	let register_interaction b ins =
		List.iter (register_interaction b) ins
	in
	List.iter register_node limits;
	SMap.iter register_interaction ig;
	an


