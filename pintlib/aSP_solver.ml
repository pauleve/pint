
open Debug

let cached_has_clingo = ref false
let clingo_checked = ref false

let pint_asp_path = Filename.concat Distenv.pint_share_path "asp"

let pint_asp_abspath relpath =
	Filename.concat pint_asp_path relpath

let has_clingo () =
	try
		let cin = Unix.open_process_in "clingo -v"
		in
		let line = input_line cin
		in
        ignore(Unix.close_process_in cin);
		Str.string_partial_match
			(Str.regexp "clingo version 4\\.") line 0
	with _ -> false

let has_clingo () =
	(if not !clingo_checked then
		(cached_has_clingo := has_clingo ();
		clingo_checked := true));
	!cached_has_clingo

let check_clingo () =
	if not (has_clingo ()) then
		failwith "Clingo version 4 is required (http://sourceforge.net/projects/potassco/files/clingo/)"


let solver ?(opts="") ?(inputs=["-"]) () =
	check_clingo ();
	dbg ~level:2 "Invoking clingo...";
	Unix.open_process ("clingo --verbose=0 "
			^opts^" "^String.concat " " inputs)

let decl asp pred =
	let s = pred^".\n"
	in
	dbg_noendl ~level:5 s;
	output_string (snd asp) s;
	asp

let decls asp preds =
	let decl = decl asp
	in
	let decl p = ignore(decl p)
	in
	List.iter decl preds;
	asp

let sat (cin, cout) =
	close_out cout;
	let rec get_last_line last_line =
		try
			let last_line = input_line cin
			in
			dbg ~level:2 last_line;
			get_last_line last_line
		with End_of_file -> last_line
	in
	let last_line = get_last_line ""
	in
	let ret = last_line = "SATISFIABLE"
	in
	ignore(Unix.close_process (cin, cout));
	ret

let all_solutions (cin, cout) =
	close_out cout;
	let rec readlines cin =
		try
			let line = input_line cin
			in
			dbg ~level:2 line;
			if line = "SATISFIABLE" || line = "UNSATISFIABLE" then []
			else line::readlines cin
		with End_of_file -> []
	in
	readlines cin

let solutions (cin, cout) handler =
	close_out cout;
	let rec readlines cin =
		try
			let line = input_line cin
			in
			dbg ~level:2 line;
			if line = "SATISFIABLE" || line = "UNSATISFIABLE" then 0
			else (handler line; 1+readlines cin)
		with End_of_file -> 0
	in
	readlines cin

let parse_answerset data =
	Parsing.clear_parser ();
	(*Parsing.set_trace true;*)
	let lexing = Lexing.from_string data
	in
	try
		ASP_parser.solution ASP_lexer.lexer lexing
	with Parsing.Parse_error ->
		failwith ("Error while parsing '"^data^"'")

