
open Big_int_Z

open PintTypes
open AutomataNetwork


module BigISet = Set.Make (struct type t = big_int let compare = compare_big_int end);;

let log2 = log 2.

let bits n = int_of_float (ceil (log (float_of_int n) /. log2))

let required_bits_per_automaton an =
	let fold_automata _ n b = max (bits n) b
	in
	Hashtbl.fold fold_automata an.ls 0

let automata_index an =
	let fold_automata a _ (i, index) =
		let index = IMap.add a i index
		in
		(i+1, index)
	in
	snd (Hashtbl.fold fold_automata an.ls (0, IMap.empty))

(*
let index_automata aindex =
	let fold a i indexa =
		IMap.add i a indexa
	in
	SMap.fold fold aindex IMap.empty
*)


let single_sid base i ai =
	shift_left_big_int (big_int_of_int ai) (base*i)

let extract_local_state base sid i =
	(* extract_big_int sid (base*i) base*)
	let sid = shift_right_big_int sid (base*i)
	in
	extract_big_int sid 0 base


let encode_transitions base aindex an =
	let single_sid = single_sid base
	and prepare_cond (b,j) =
		let id = IMap.find b aindex
		in
		let j = big_int_of_int j
		in
		(fun sid -> extract_local_state base sid id), eq_big_int j
	in
	let make_precond conds =
		let conds = List.map prepare_cond (IMap.bindings conds)
		in
		let precond sid = List.for_all
				(fun (get_comp, test_comp) -> test_comp (get_comp sid))
					conds
		in
		precond
	and make_shift orig dest =
        let fold a i shift =
            let j = IMap.find a dest
            and id = IMap.find a aindex
            in
            add_big_int shift (single_sid id (j-i))
        in
        IMap.fold fold orig zero_big_int
	in
	let fold_transition trid tr etransitions =
		let precond = make_precond tr.pre
		and shifts = make_shift tr.orig tr.dest
		in
		(precond, shifts)::etransitions
	in
    Hashtbl.fold fold_transition an.trs []

let sid_of_state base aindex state =
	let fold a i sid =
		let ai = IMap.find a state
		in
		if ai > 0 then
			let aid = single_sid base i ai
			in
			or_big_int aid sid
		else sid
	in
	IMap.fold fold aindex zero_big_int

let state_of_sid base aindex sid =
	let folder a id state =
		let i = extract_local_state base sid id
		in
		let i = int_of_big_int i
		in
		IMap.add a i state
	in
	IMap.fold folder aindex IMap.empty

module BigHashtbl = Hashtbl.Make(struct
		type t = big_int
		let equal = eq_big_int
		let hash = Hashtbl.hash
end)

let prepare_sts an state =
	let base = required_bits_per_automaton an
	and aindex = automata_index an
	in
	let etrs = encode_transitions base aindex an
	and sid0 = sid_of_state base aindex state
	and state_of_sid = state_of_sid base aindex
	in
	let next_sids sid =
		let fold_etr sids (precond, shift) =
			if precond sid then
				BigISet.add (add_big_int sid shift) sids
			else sids
		in
		List.fold_left fold_etr BigISet.empty etrs
	in
	sid0, next_sids, state_of_sid

let reachable_states an state =
	let sid0, next_sids, sid2state = prepare_sts an state
	and known = BigHashtbl.create 10240
	in
	let rec explore sid (counter, todo) =
		if BigHashtbl.mem known sid then
			(counter, todo)
		else (
			let counter = succ_big_int counter
			in
			BigHashtbl.add known sid ();
			let nexts = next_sids sid
			in
			let todo = BigISet.fold (fun bi bis ->
					if BigHashtbl.mem known bi then bis else BigISet.add bi bis)
					 	nexts todo
			in
			if BigISet.is_empty todo then
				(counter, todo)
			else (
				let sid = BigISet.choose todo
				in
				let todo = BigISet.remove sid todo
				in
				explore sid (counter, todo)
			)
		)
	in
	fst (explore sid0 (zero_big_int, BigISet.empty)), known, sid2state

let reachable_stategraph an state =
	let sid0, next_sids, sid2state = prepare_sts an state
	and sg = BigHashtbl.create 10240
	in
	let rec explore sid =
		if not(BigHashtbl.mem sg sid) then (
			let nexts = next_sids sid
			in
			BigHashtbl.add sg sid (BigISet.elements nexts);
			BigISet.iter explore nexts
		)
	in
	explore sid0;
	sg, sid0, sid2state

let attractors an state =
	let sid0, next, sid2state = prepare_sts an state
	and index = BigHashtbl.create 10240
	and lowlink = BigHashtbl.create 10240
	and nid = ref 0
	and stack = Stack.create ()
	and stackc = BigHashtbl.create 1024
	in
	let stack_push sid =
		Stack.push sid stack;
		BigHashtbl.add stackc sid ()
	and stack_pop () =
		let sid = Stack.pop stack
		in
		BigHashtbl.remove stackc sid;
		sid
	in
	let stack_pop_until sid =
		let c = ref 1
		in
		while not (eq_big_int (stack_pop ()) sid) do
			incr c
		done;
		!c
	in
	let rec bsccs v =
		BigHashtbl.add index v !nid;
		BigHashtbl.add lowlink v !nid;
		stack_push v;
		incr nid;
		let sccs = List.fold_left (handle_child v) []
			(BigISet.elements (next v))
		in
		let l_v = BigHashtbl.find lowlink v
		and i_v = BigHashtbl.find index v
		in
		let sccs = if l_v = i_v then (
			let size = stack_pop_until v
			in
			match sccs with [] -> [Some (v, size)]
				| _ -> sccs) else sccs
		in
		let has_none = List.mem None sccs
		in
		let sccs = List.filter (function None -> false | _ -> true) sccs
		in
		if has_none then None::sccs else sccs
	and handle_child v sccs w =
		if not(BigHashtbl.mem index w) then (
			let sccs = sccs @ bsccs w
			in
			let l_v = BigHashtbl.find lowlink v
			and l_w = BigHashtbl.find lowlink w
			in
			let l_v' = min l_v l_w
			in
			(if l_v' <> l_v then BigHashtbl.replace lowlink v l_v');
			sccs
		) else (
			if BigHashtbl.mem stackc w then (
				let l_v = BigHashtbl.find lowlink v
				and i_w = BigHashtbl.find index w
				in
				let l_v' = min l_v i_w
				in
				(if l_v' <> l_v then BigHashtbl.replace lowlink v l_v');
				sccs
			) else
				None::sccs
		)
	in
	let bsccs = bsccs sid0
	in
	let bsccs = match bsccs with None::bsccs -> bsccs | _ -> bsccs
	in
	let bsccs = List.map (function Some bscc -> bscc
					| None -> failwith "invalid value returned by bsccs") bsccs
	in
	bsccs, sid2state


