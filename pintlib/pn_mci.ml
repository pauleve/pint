
type t_unf = {
	ev2tr: (int, int) Hashtbl.t;
	co2pl: (int, int) Hashtbl.t;
	preco: (int, int) Hashtbl.t;
	postco: (int, int) Hashtbl.t;
	cutoffs: (int, int) Hashtbl.t;
	mutable plname: string array;
	mutable trname: string array;
}

let load_mci filename =
	let mci = open_in_bin filename
	in
	let numco = input_binary_int mci
	and numev = input_binary_int mci
	in
	let unf = {
		ev2tr = Hashtbl.create numev;
		co2pl = Hashtbl.create numco;
		preco = Hashtbl.create numco;
		postco = Hashtbl.create numco;
		cutoffs = Hashtbl.create (numev/100);
		plname = Array.make 0 "";
		trname = Array.make 0 "";
	} in
	let rec read_ev2trs i = if i <= numev then (
		Hashtbl.add unf.ev2tr i (input_binary_int mci);
		read_ev2trs (i+1)
	) in
	let rec read_conditions i = if i <= numco then (
		Hashtbl.add unf.co2pl i (input_binary_int mci);
		let pre_ev = input_binary_int mci
		in
		(if pre_ev > 0 then (Hashtbl.add unf.preco pre_ev i));
		let rec read_post () =
			let post_ev = input_binary_int mci
			in
			if post_ev > 0 then (
				Hashtbl.add unf.postco i post_ev;
				read_post ())
		in
		read_post ()
	) in
	let rec read_cutoffs () =
		let cutoff = input_binary_int mci
		in
		if cutoff > 0 then (
			let orig = input_binary_int mci
			in
			Hashtbl.add unf.cutoffs cutoff orig;
			read_cutoffs ()
		)
	in
	let rec read_until_zero () =
		if input_binary_int mci > 0 then
			read_until_zero ()
	in
	let read_string mci =
		let b = Buffer.create 16
		in
		let read_until_zero () =
			let c = input_char mci
			in
			if c > '\000' then (
				Buffer.add_char b c;
				read_until_zero ()
			)
		in
		read_until_zero ();
		Buffer.contents b
	in
	read_ev2trs 1;
	read_conditions 1;
	read_cutoffs ();
	read_until_zero ();
	let numpl = input_binary_int mci
	and numtr = input_binary_int mci
	and _ = input_binary_int mci
	in
	let plname = List.map (fun _ -> read_string mci) (Util.range 0 numpl)
	and _ = input_char mci
	and trname = List.map (fun _ -> read_string mci) (Util.range 0 numtr)
	and _ = input_char mci
	in
	unf.plname <- Array.of_list plname;
	unf.trname <- Array.of_list trname;
	close_in mci;
	unf



