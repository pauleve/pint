
open Ui;;

let test_model phname arg =
	let ph = ph_load (phname^".ph")
	in
	print_endline ("#<<<<<< test_model "^phname);
	let harmful = Ph_verif.harmful ph arg
	in
	ignore(harmful);
	print_endline ("#>>>>>> ["^phname^"] harmful ready")
in

let zl = ("z",1)
in
(**test_model "simple" zl;**)
(**test_model "coop1" zl;**)
(**test_model "coop2" zl;**)
(**test_model "loop1" zl;**)
(**)test_model "incr" ("z",3);(**)

