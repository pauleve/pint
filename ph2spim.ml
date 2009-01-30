
let _ =
	let filename = Sys.argv.(1)
	and time = float_of_string Sys.argv.(2)
	in
	let ph = Ph.from_channel (open_in filename)
	in
	let procs = fst ph
	in
	let init_state = List.map (fun _ -> 0) (fst procs @ snd procs)
	in
	let spim = Ph.spim_of_ph ph init_state time
	in
	Util.dump_to_file (filename^".spi") spim
;;


