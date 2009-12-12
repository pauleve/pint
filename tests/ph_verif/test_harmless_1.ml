
open Ui;;

let test_model phname arg =
	let ph = ph_load (phname^".ph")
	in
	print_endline ("#<<<<<< test_model "^phname);
	let harmless = Ph_verif.harmless ph arg
	in
	ignore(harmless);
	print_endline ("#>>>>>> ["^phname^"] harmless_noresolv ready")
in

test_model "simple" ("z", [1]);
test_model "coop1" ("z", [1]);
test_model "coop2" ("z", [1]);
test_model "loop1" ("z", [1]);




