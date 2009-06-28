
let exponential r =
	log (1. /. Random.float 1.) /. r
;;

let _ =
	let rate = float_of_string (Sys.argv.(1))
	and count = int_of_string (Sys.argv.(2))
	in
	let rec iter acc n = if n > 0 then (
		let acc = acc +. exponential rate
		in
		print_endline (string_of_float acc);
		iter acc (n-1)
	)
	in
	iter 0.0 count
;;


