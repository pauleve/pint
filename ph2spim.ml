
let make_spim output ph properties init_state =
	let init_state = List.map 
		(fun (p,l) -> try List.assoc p init_state with Not_found -> 0)
		(fst ph)
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
		let properties, ph, init_state = Ph_parser.main Ph_lexer.lexer lexbuf
		in
		make_spim (filename^".spi") ph
					(properties@default_properties) init_state
	with Parsing.Parse_error ->
		failwith (show_position (Lexing.lexeme_start_p lexbuf) ^
						"Syntax error");
;;

