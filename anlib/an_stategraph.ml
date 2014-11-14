
open Big_int

open PintTypes
open AutomataNetwork


module BigISet = Set.Make (struct type t = big_int let compare = compare_big_int end);;

let log2 = log 2.

let bits n = int_of_float (ceil (log (float_of_int n) /. log2))

let required_bits_per_automaton an =
	let fold_automata _ states b =
		max (bits (List.length states)) b
	in
	SMap.fold fold_automata an.automata 0

let automata_index an = 
	let fold_automata a _ (i, index) =
		let index = SMap.add a i index
		in
		(i+1, index)
	in
	snd (SMap.fold fold_automata an.automata (0, SMap.empty))

(*
let index_automata aindex =
	let fold a i indexa =
		IMap.add i a indexa 
	in
	SMap.fold fold aindex IMap.empty
*)


let single_sid base i ai =
	shift_left_big_int (big_int_of_int ai) (base*i)

let encode_transitions base aindex an =
	let single_sid = single_sid base
	and prepare_cond (b,j) =
		let id = SMap.find b aindex
		in
		let j = big_int_of_int j
		in
		(fun sid -> extract_big_int sid (base*id) base), eq_big_int j
	in
	let fold_transitions (a,i) transitions etransitions =
		let id = SMap.find a aindex
		in
		let encode_transition (j, conds) =
			let conds = List.map prepare_cond ((a,i)::conds)
			in
			let precond sid = List.for_all 
				(fun (get_comp, test_comp) -> test_comp (get_comp sid))
					conds
			and shift = single_sid id (j-i)
			in
			precond, shift
		in
		List.map encode_transition transitions @ etransitions
	in
	LSMap.fold fold_transitions an.transitions []

let sid_of_state base aindex state =
	let fold a i sid =
		let ai = SMap.find a state
		in
		if ai > 0 then
			let aid = single_sid base i ai
			in
			or_big_int aid sid
		else sid
	in
	SMap.fold fold aindex zero_big_int
		
let reachable_states an state =
	let base = required_bits_per_automaton an
	and aindex = automata_index an
	in
	let etrs = encode_transitions base aindex an
	and sid0 = sid_of_state base aindex state
	in
	let next_sids sid =
		let fold_etr sids (precond, shift) =
			if precond sid then
				add_big_int sid shift::sids
			else sids
		in
		List.fold_left fold_etr [] etrs
	in
	let rec explore sid ((counter, known), todo) =
		if BigISet.mem sid known then
			((counter, known), todo)
		else (
			let counter = succ_big_int counter
			and known = BigISet.add sid known
			in
			let nexts = next_sids sid
			in
			let todo = List.fold_left (fun bis bi ->
					if BigISet.mem bi known then bis else BigISet.add bi bis) 
					 	todo nexts
			in
			if BigISet.is_empty todo then
				((counter, known), todo)
			else (
				let sid = BigISet.choose todo
				in
				let todo = BigISet.remove sid todo
				in
				explore sid ((counter, known), todo)
			)
		)
	in
	fst (explore sid0 ((zero_big_int, BigISet.empty), BigISet.empty))



