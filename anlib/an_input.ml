
open Debug

let parse channel_in =
	let lexbuf = Lexing.from_channel channel_in
	in
	let show_position () =
		let pos = Lexing.lexeme_start_p lexbuf
		in
		"Line " ^ string_of_int pos.Lexing.pos_lnum ^
		  " char " ^ string_of_int 
				(pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ^ ": "
	in
	try
		let t0 = Sys.time ()
		in
		let an, ctx = An_parser.main An_lexer.lexer lexbuf
		in
		dbg ("Parsing took "^string_of_float (Sys.time() -. t0)^"s");
		close_in channel_in;
		an, ctx
	with Parsing.Parse_error ->
		failwith (show_position () ^ "Syntax error")
	| Failure msg ->
		failwith (show_position () ^ msg)
	| e -> (
		failwith (show_position () ^ Printexc.to_string e)
	)

let parse_string entry data =
	Parsing.clear_parser ();
	(*Parsing.set_trace true;*)
	let lexing = Lexing.from_string data
	in
	try
		entry An_lexer.lexer lexing
	with Parsing.Parse_error ->
		failwith ("Error while parsing '"^data^"'")


