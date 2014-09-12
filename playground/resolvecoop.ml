

open Ph_types;;

let (ph, ctx), args = Ui.simple_input ()
in
let resolve_cooperativity ai =
	let conds = Ph_cooperativity.local_fixed_points !Ph_instance.cooperativities
		ph ai
	in
	print_endline (string_of_proc ai^": "^
			String.concat "\t" (List.map string_of_state conds))
and pl = Ui.proclist_from_stringlist args
in
List.iter resolve_cooperativity pl


