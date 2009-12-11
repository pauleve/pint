
open Big_int;;


(**
 * Process Hitting
 **)
let ph_load filename = fst (Ph_util.parse (open_in filename));;
let ph_load2 filename = Ph_util.parse (open_in filename);;
let ph_stable_states = Ph_verif.stable_states;;
let ph_count_states (ps,hits) = 
	let counter c (_,la) = mult_int_big_int (la+1) c
	in
	List.fold_left counter unit_big_int ps
;;



