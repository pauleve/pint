
open Debug

let clingo_checked = ref false

let check_clingo () =
	if not !clingo_checked then
		try
			let cin = Unix.open_process_in "clingo -v"
			in
			let line = input_line cin
			in
			if Str.string_partial_match (Str.regexp "clingo version 4\\.") line 0 then
				clingo_checked := true
			else
				raise Not_found
		with _ -> failwith "Clingo version 4 is required (http://sourceforge.net/projects/potassco/files/clingo/)"


let solver () =
	check_clingo ();
	dbg ~level:2 "Invoking clingo...";
	Unix.open_process "clingo --verbose=0 -"

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

