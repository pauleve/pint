(****
	SHORTCUTS
 ****)

open Big_int;;

let ph_load filename = fst (Ph_util.parse (open_in filename));;
let ph_load2 filename = Ph_util.parse (open_in filename);;

let ph_stable_states = Ph_static.stable_states;;

let ph_count_states (ps,hits) = 
	let counter c (_,la) = mult_int_big_int (la+1) c
	in
	List.fold_left counter unit_big_int ps
;;
let ph_count_sorts (ps, _) = List.length ps
;;
let ph_count_processes (ps, _) =
	let c acc (_,l) = acc + l + 1
	in
	List.fold_left c 0 ps
;;
let ph_count_actions (_, hits) = Hashtbl.length hits;;

let common_cmdopts = [
	("--no-debug", Arg.Clear Debug.dodebug, "Disable debugging");
	("--debug", Arg.Set Debug.dodebug, "Enable debugging");
];;


