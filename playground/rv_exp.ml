
let exponential r =
	log (1. /. Random.float 1.) /. r
;;

let _ =
	Random.self_init();
	let rate = float_of_string (Sys.argv.(1))
	and count = int_of_string (Sys.argv.(2))
	in
	let rec iter acc n = if n <= count then (
		let acc = acc +. exponential rate
		in
		if n mod 5 = 0 then (
			print_endline ("\\draw<3> ("^string_of_float acc^",0) -- ("^string_of_float acc^",1);");
			print_endline ("\\draw<4->[hl] ("^string_of_float acc^",0) -- ("^string_of_float acc^",1);");
		) else
			print_endline ("\\draw<3> ("^string_of_float acc^",0) -- ("^string_of_float acc^",1);");
		iter acc (n+1)
	)
	in
	iter 0.0 1
;;


