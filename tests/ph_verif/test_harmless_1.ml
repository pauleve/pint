
open Ui;;

let test_model phname arg =
	print_endline ("#<<<<<< test_model "^phname);
	let ph = ph_load (phname^".ph")
	in
	let ret = Ph_verif.harmless ph arg
	in
	print_endline ("#>>>>>> ["^phname^"] harmless("^
		Ph_verif.string_of_sortdomain arg^
		") = " ^ Ph_verif.BS.string_of_dnf ret)
in

test_model "simple" ("z", [1])




