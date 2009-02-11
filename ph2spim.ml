
let make_spim output ph properties =
	let procs = fst ph
	in
	let init_state = List.map (fun _ -> 0) (fst procs @ snd procs)
	in
	let spim = Ph.spim_of_ph ph init_state properties
	in
	Util.dump_to_file output spim
;;


let _ =
	let default_properties = [
		("sample", "1000.0");
		("stochasticity_absorption", "1")
	]
	in
	let filename = Sys.argv.(1)
	in

	let lexbuf = Lexing.from_channel (open_in filename)
	in

	let show_position pos =
		  "Line " ^ string_of_int pos.Lexing.pos_lnum ^
		  " char " ^ string_of_int 
		  		(pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ^ ": "
	in
	try 
		let properties, ph = Ph_parser.main Ph_lexer.lexer lexbuf
		in
		make_spim (filename^".spi") ph
					(properties@default_properties)
	with Parsing.Parse_error ->
		failwith (show_position (Lexing.lexeme_start_p lexbuf) ^
						"Syntax error");
;;

