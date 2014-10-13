
let commands = [
	("share-path", fun () -> print_endline (Distenv.pint_share_path));
	("version", fun () -> print_endline (Distenv.version));
]
in
let usage () = 
	prerr_endline (Sys.argv.(0) ^ " ["^
					(String.concat "|" (fst (List.split commands))^"]"));
	exit 1
in
if Array.length Sys.argv != 2 then
	usage ()
else
	let cmd = 
		try List.assoc Sys.argv.(1) commands
		with Not_found-> usage ()
	in
	cmd ()


