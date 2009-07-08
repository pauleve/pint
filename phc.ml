(***
	Process Hitting Compiler
***)

open Ph_translator;;

let round_float v = (* assume v >= 0 *)
	let f = floor v
	in
	int_of_float (if v -. f > 0.5 then ceil v else f)
;;

let round_fi_closest = function dmin,dmax -> round_float dmin, round_float dmax
;;
let round_fi_ex = function dmin,dmax ->
	int_of_float (floor dmin), int_of_float (ceil dmax)
;;
let round_fi_in = function dmin,dmax ->
	int_of_float (ceil dmin), int_of_float (floor dmax)
;;

let _ =
	let language, filename, output = match Array.length Sys.argv with
		4 -> Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
		| _ -> failwith "Usage: phc <-spim|-prism > <source.ph> <output>"
	in
	let opts = {
		alpha = 0.05;
		round_fi = round_fi_ex
	}
	in

	let translator = match language with
		  "-dump" -> dump_of_ph
		| "-spim" -> spim_of_ph
		| "-prism" -> prism_of_ph
		| "-prism2" -> prism2_of_ph
		| "-romeo" -> romeo_of_ph opts
		| _ -> failwith ("Unknown language "^language)
	in

	let channel_in = open_in filename
	in
	let ph, (properties,init_state) = Ph_util.parse channel_in
	in
	let data = translator ph init_state properties
	in
	let fd_out = match output with "-" -> stdout
					| _ -> open_out output
	in
	output_string fd_out data;
	close_out fd_out
;;

