(***
	Process Hitting Compiler
***)

let _ =
	let language, filename, output = match Array.length Sys.argv with
		4 -> Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
		| _ -> failwith "Usage: phc <-spim|-prism > <source.ph> <output>"
	in

	let translator = match language with
		  "-dump" -> Ph_translator.dump_of_ph
		| "-spim" -> Ph_translator.spim_of_ph
		| "-prism" -> Ph_translator.prism_of_ph
		| "-prism2" -> Ph_translator.prism2_of_ph
		| "-romeo" -> Ph_translator.romeo_of_ph
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

