
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
	let mci = BatFile.open_in filename
	in
	let read_int mci =
		BatIO.read_i32 mci
	in
	let numco = read_int mci
	and numev = read_int mci
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
	for i = 1 to numev do
		Hashtbl.add unf.ev2tr i (read_int mci)
	done;
	for i = 1 to numco do
		Hashtbl.add unf.co2pl i (read_int mci);
		let pre_ev = read_int mci
		in
		(if pre_ev > 0 then (Hashtbl.add unf.preco pre_ev i));
		let rec read_post () =
			let post_ev = read_int mci
			in
			if post_ev > 0 then (
				Hashtbl.add unf.postco i post_ev;
				read_post ())
		in
		read_post ()
	done;
	let rec read_cutoffs () =
		let cutoff = read_int mci
		in
		if cutoff > 0 then (
			let orig = read_int mci
			in
			Hashtbl.add unf.cutoffs cutoff orig;
			read_cutoffs ()
		)
	in
	let rec read_until_zero () =
		if read_int mci > 0 then
			read_until_zero ()
	in
	read_cutoffs ();
	read_until_zero ();
	let numpl = read_int mci
	and numtr = read_int mci
	and _ = read_int mci
	in
	let plname = List.map (fun _ -> BatIO.read_string mci) (Util.range 1 numpl)
	and _ = BatIO.read_byte mci
	and trname = List.map (fun _ -> BatIO.read_string mci) (Util.range 1 numtr)
	and _ = BatIO.read_byte mci
	in
	unf.plname <- Array.of_list plname;
	unf.trname <- Array.of_list trname;
	BatIO.close_in mci;
	unf



