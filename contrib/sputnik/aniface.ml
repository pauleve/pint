
open PintTypes
open InteractionGraph
open Ph_types
open AutomataNetwork

let test () =
	print_endline ("Pint - Sputnik");
	true

let ig_of_interactions =
	let fold ig (a, i, s, b) =
		let ins = try SMap.find b ig with Not_found -> []
		in
		SMap.add b ((a,i,s)::ins) ig
	in
	List.fold_left fold SMap.empty

let an_of_grn ig limits =
	(* TODO *)
	empty_an ()

let dump_an outf an ctx =
	let data = An_export.dump_of_an an ctx
	in
	let cout = open_out outf
	in
	output_string cout data;
	close_out cout

let init_stack =
	Stack.create

let push_an stack an lcg =
	let state = copy_an an, lcg
	in
	Stack.push state stack

let pop_an = Stack.pop

let thomas_constrain ig an ctx a bound min_bound =
	let ps = automata_limits an
	in
	let actx = IG.extended_local_ctx ig ps a ctx
	and is = if min_bound then Util.srange bound (List.assoc a ps)
				else Util.srange 0 bound
	in
	An_focal.restrict_focal an a actx is

let reduced_an an ctx goal =
	let env = An_reach.init_env an ctx goal
	in
	An_reach.reduced_an env

let worth_lcg an ctx goal =
	let env = An_reach.init_env an ctx goal
	in
	An_reach.worth_lcg env

let is_transition_worth = An_reach.is_localstate_worth

