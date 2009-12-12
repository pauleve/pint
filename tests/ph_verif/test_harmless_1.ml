
open Ui;;

let test_model phname arg =
	print_endline ("#<<<<<< test_model "^phname);
	let ph = ph_load (phname^".ph")
	in
	let ret = Ph_verif.harmless ph arg
	in
	print_endline ("#>>>>>> ["^phname^"] harmless("^
		Ph_verif.string_of_sortdomain arg^
		") = " ^ Ph_verif.BS.string_of_dnf ret);
	print_endline ("#<<<<<< test_model "^phname);
	let ret = Ph_verif.harmless_noresolv ph arg
	in
	print_endline ("#>>>>>> ["^phname^"] harmless_noresolv ready")
in

test_model "simple" ("z", [1]);
test_model "coop1" ("z", [1]);
test_model "coop2" ("z", [1]);
test_model "loop1" ("z", [1]);




