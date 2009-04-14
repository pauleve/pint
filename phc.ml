(***
	Process Hitting Compiler
***)

let _ =
	let default_properties = [
		("sample", "1000.0");
		("stochasticity_absorption", "1")
	]
	in
	let language, filename, output = match Array.length Sys.argv with
		4 -> Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
		| _ -> failwith "Usage: phc <-pint|-prism > <source.ph> <output>"
	in

	let translator = match language with
		  "-pint" -> Ph_translator.spim_of_ph
		| "-prism" -> Ph_translator.prism_of_ph
		| _ -> failwith ("Unknown language "^language)
	in


	let lexbuf = Lexing.from_channel (open_in filename)
	in

	let show_position pos =
		  "Line " ^ string_of_int pos.Lexing.pos_lnum ^
		  " char " ^ string_of_int 
		  		(pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ^ ": "
	in
	try 
		let properties, ph, init_state = Ph_parser.main Ph_lexer.lexer lexbuf
		in
		let init_state = List.map 
			(fun (p,_) -> p, try List.assoc p init_state with Not_found -> 0)
			(fst ph)
		and properties = properties@default_properties
		in
		let data = translator ph init_state properties
		in
		let fd_out = match output with "-" -> stdout | _ -> open_out output
		in
		output_string fd_out data;
		close_out fd_out
	with Parsing.Parse_error ->
		failwith (show_position (Lexing.lexeme_start_p lexbuf) ^
						"Syntax error");
;;

