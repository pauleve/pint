
open Ui;;

let test_model phname (z, lz') =
	print_endline ("#test_model "^phname);
	let ph = ph_load (phname^".ph")
	in
	let ret = Ph_verif.harmless ph (z,lz')
	in
	ret
in

test_model "simple" ("z", [1])




