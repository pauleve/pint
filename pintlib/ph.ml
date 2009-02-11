
open Ph_types;;

let spim_of_ph ((ps,aps),hits) init_state properties =
	let name_of_process (p,l) = p^string_of_int l^"()"
	in
	let register_metaproc piprocs (p,l) =
		piprocs @ List.map (fun l -> (name_of_process (p,l),[])) (Util.range 0 l)
	in
	let piprocs = List.fold_left register_metaproc [] (ps@aps)
	in
	let add_pichoice piprocs piproc pi =
		let pis = List.assoc piproc piprocs
		in
		(piproc,pi::pis)::List.remove_assoc piproc piprocs
	in
	let register_hit p2 ((p1,r),l) (piprocs,channels,counter) =
		let cid = "hit"^string_of_int counter
		and p2' = (fst p2,l)
		in
		let np1 = name_of_process p1
		and np2 = name_of_process p2
		and np2' = name_of_process p2'
		in
		let piprocs = 
			if p1 = p2 then (
				(* delay *)
				let pi = if r = RateInf then np2' else ("delay@"^cid^";"^np2')
				in
				add_pichoice piprocs np2 pi
			) else (
				let pi1 = "!"^cid^";"^np1
				and pi2 = "?"^cid^";"^np2'
				in
				let piprocs = add_pichoice piprocs np1 pi1
				in
				add_pichoice piprocs np2 pi2
			)
		in
		let c = (cid, r, p2 <> p1)
		in
		(piprocs,c::channels,counter+1)
	in
	let piprocs,channels,counter = Hashtbl.fold register_hit hits (piprocs,[],0)
	in
	let string_of_channel (cid, rate, ischan) = match ischan with
		  true -> "new "^cid^(match rate with 
		  				Rate f -> "@"^Spim.string_of_rate f
						| RateInf -> "")^":chan"
		| false -> (match rate with
						Rate f -> "val "^cid^"="^Spim.string_of_rate f
						| RateInf -> "")
	and string_of_piproc (piproc, choices) =
		piproc ^ " = " ^ match choices with
				  [] -> "!dead"
				| [pi] -> pi
				| pis -> "do "^String.concat " or " pis
	in
	
	let defs = "new dead:chan\n" ^ 
		(String.concat "\n" (List.map string_of_channel channels))
	and body = "let "^
		String.concat "\nand " (List.map string_of_piproc piprocs)

	and directives = String.concat "\n" [
		"directive sample "^Spim.string_of_rate (float_of_string (List.assoc "sample" properties));
		"directive plot "^String.concat ";" (fst (List.split piprocs))]

	and run = "run ("^(String.concat " | " 
					(List.map (fun ((n,_),l) -> name_of_process (n,l))
						(List.combine (ps@aps) init_state))) ^ ")\n"
	in
	directives ^ "\n\n" ^ defs ^ "\n\n" ^ body ^ "\n\n" ^ run
;;

