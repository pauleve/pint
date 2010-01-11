(***
	Process Hitting Compiler
***)

open Ph_translator;;


let _ =
	let language, filename, output = match Array.length Sys.argv with
		4 -> Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
		| _ -> failwith "Usage: phc <-spim|-prism|...> <source.ph> <output>"
	in
	let opts = {
		alpha = 0.05;
		round_fi = Param.round_fi_ex
	}
	in
	let languages = [
		("dump", dump_of_ph);
		("spim", spim_of_ph);
		("prism", prism_of_ph);
		("prism_mdp", prism_mdp_of_ph);
		("romeo", romeo_of_ph opts)
	]
	in
	let translator = List.assoc (Str.string_after language 1) languages
	in

	let channel_in = open_in filename
	in
	let ph, init_state = Ph_util.parse channel_in
	in
	let data = translator ph init_state
	in
	let fd_out = match output with "-" -> stdout
					| _ -> open_out output
	in
	output_string fd_out data;
	close_out fd_out
;;

