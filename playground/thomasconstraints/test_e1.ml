
open PintTypes;;

let dump_ph outf ph ctx =
	let data = Ph_translator.dump_of_ph ph ctx
	in
	let cout = open_out outf
	in
	output_string cout data;
	close_out cout

in
let model = "e1.ph"
in
let ph, ctx = Ph_util.parse (open_in model)
in

let ph = Ph_thomas.constrained_ph ph "c" (SSet.singleton "a") [0]
in
dump_ph "gen/e1-1.ph" ph ctx;

let ph = Ph_thomas.constrained_ph ph "c" (SSet.singleton "b") [0]
in
dump_ph "gen/e1-2.ph" ph ctx;

