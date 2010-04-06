open Big_int;;

open Ui;;

let _ = 
	let filename = match Array.length Sys.argv with
		  2 -> Sys.argv.(1)
		| _ -> failwith "Usage: ph-stable <source.ph>"
	in
	let ph = ph_load filename
	in
	let nb_states = ph_count_states ph
	and nb_sorts = ph_count_sorts ph
	and nb_processes = ph_count_processes ph
	and nb_actions = ph_count_actions ph
	and larger_sort = 
		let sps = List.sort (fun (_,l) (_,l') -> compare l' l) (fst ph)
		in
		snd (List.hd sps) + 1
	in
	print_endline filename;
	print_endline (string_of_int nb_sorts^" sorts");
	print_endline (string_of_int nb_processes^" processes");
	print_endline ("largest sort: "^string_of_int larger_sort);
	print_endline (string_of_int nb_actions^" actions");
	print_endline (string_of_big_int nb_states^" states");
;;


