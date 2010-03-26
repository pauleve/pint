
open Big_int;;

let ph_load filename = fst (Ph_util.parse (open_in filename));;
let ph_load2 filename = Ph_util.parse (open_in filename);;
let ph_stable_states = Ph_verif.stable_states;;
let ph_count_states (ps,hits) =
	let counter c (_,la) = mult_int_big_int (la+1) c
	in
	List.fold_left counter unit_big_int ps
;;

let string_of_process (a,i) = a^"_"^string_of_int i;;
let string_of_state state =
	"[ "^
	String.concat " " (List.sort compare (List.map string_of_process state))
	^" ]"
;;
let string_of_states states =
	String.concat "\n" (List.sort compare (List.map string_of_state states))
;;


let _ = 
	let filename = match Array.length Sys.argv with
		  2 -> Sys.argv.(1)
		| _ -> failwith "Usage: ph-stable <source.ph>"
	in
	let ph = ph_load filename
	in
	let nb_states = ph_count_states ph
	in
	output_string stderr ("["^filename^"] total: "^string_of_big_int nb_states^" states\n"); flush stderr;
	let stable_states = ph_stable_states ph
	in
	print_endline (string_of_states stable_states)
;;


