
let dump_to_file filename content = 
	let fd = open_out filename in
	output_string fd content;
	close_out fd
;;


