
(* Thomas' logicial parameters: gene * activators * value *)
type parameter_thomas = Decision.metaproc * Decision.metaproc list * int
;;

let decisions_of_parameter_thomas grn (a, activators, value) =
	let folder b ((t,e),a') infos =
		if a = a' then (
			let levels = (if List.mem b activators && e = Brg.Activation
				|| not (List.mem b activators) && e = Brg.Inhibition
			then Util.range t (Brg.level_max grn b) else (Util.range 0 (t-1)))
			in
			Util.cross infos (List.map (fun l -> b, l) levels)
		) else infos
	in
	let infos = Brg.fold folder grn [[]]
	in
	assert (List.length infos > 0);
	let infos = if not (List.mem a (fst (List.split (List.hd infos)))) then
					let levels = Util.range 0 (Brg.level_max grn a)
					in
					Util.cross infos (List.map (fun l -> a, l) levels)
				else infos
	in
	let make_decision state =
		let _,x = Decision.get_proc_by_meta a state
		and sgn diff = if diff < 0 then Decision.Dec
						else if diff > 0 then Decision.Inc
						else Decision.Dis
		and info_from_p p =
			Decision.L (fst p, string_of_int (snd p))
		in
		a, (List.map info_from_p state), sgn (value - x)
	in
	let decisions = List.map make_decision infos
	in
	List.filter (fun (m,i,a) -> a != Decision.Dis) decisions
;;


