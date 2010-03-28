
open Debug;;

open Ph_types;;

type bounce_path = sort * sortidx * ISet.t
type bounce_sequence = action list

let bp_sort (a, _, _) = a;;
let bp_bounce (_, _, aj) = aj;;

module BounceSequenceOT = struct type t = bounce_sequence let compare = compare end
module BS = Set.Make (BounceSequenceOT)
module BPSet = Set.Make (struct type t = bounce_path let compare = compare end)

let string_of_bounce_path (a, i, js) =
	string_of_process (a,i) ^ " " ^ string_of_iset js
;;
let string_of_bounce_sequence bs =
	"["^(String.concat "; " (List.map string_of_action bs))^"]"
;;
let string_of_BS = string_of_set string_of_bounce_sequence BS.elements;;

type env = {
	sorts : process list;
	t_hits : hits;
	_BS : (bounce_path, BS.t) Hashtbl.t;
}

let create_env (ps,hits) = 
	{
		sorts = ps;
		t_hits = hits;
		_BS = Hashtbl.create 50;
	}
;;

let compute_BS env bp =
	let a, i, reachset = bp
	in
	let prepend_action action seqs =
		BS.fold (fun seq seqs -> BS.add (action::seq) seqs) seqs BS.empty
	in
	let rec walk j visited =
		if ISet.mem j reachset then
			BS.singleton []
		else
			let visited = ISet.add j visited
			and actions = Hashtbl.find_all env.t_hits (a,j)
			in
			let folder seqs = function (hitter,_),k ->
				if ISet.mem k visited then
					BS.empty
				else
					let k_seqs = walk k visited
					in
					BS.union (prepend_action (Hit (hitter,(a,j),k)) k_seqs) seqs
			in
			List.fold_left folder BS.empty actions
	in
	(*DEBUG*) dbg_noendl ("- computing BS("^string_of_bounce_path bp^")..."); (**)
	let seqs = walk i ISet.empty
	in
	(*DEBUG*) dbg (" "^string_of_BS seqs); (**)
	Hashtbl.add env._BS bp seqs;
	seqs
;;
let _BS env bp =
	try Hashtbl.find env._BS bp
	with Not_found -> compute_BS env bp
;;

exception ExecuteCrash
exception ExecuteNoCrash of state
let rec execute env bp s stack =
	(if BPSet.mem bp stack then raise ExecuteCrash);
	let stack = BPSet.add bp stack
	and a = bp_sort bp
	in
	let rec execute_seq s = function 
		  [] -> s
		| action::seq -> 
			let b,j = hitter action
			in
			let sb = state_value s b
			in
			let s = execute env (b, sb, ISet.singleton j) s stack
			in
			let sa = state_value s a
			in
			if sa <> snd (target action) then
				execute env (a, sa, bp_bounce bp) s stack
			else 
				let s = SMap.add a (bounce action) s
				in
				execute_seq s seq
	in
	let try_seq seq =
		try
			let s = execute_seq s seq
			in
			raise (ExecuteNoCrash s)
		with ExecuteCrash -> ()
	in
	try
		BS.iter try_seq (_BS env bp);
		raise ExecuteCrash
	with ExecuteNoCrash s -> s
;;

let process_reachability env (z,l) s =
	let bpzl = (z, state_value s z, ISet.singleton l)
	in
	dbg ("process_reachability "^string_of_bounce_path bpzl);

	(* Can not statically conclude. *)
	dbg "- can not statically conclude.";
	dbg "+ running execute...";
	try
		let s = execute env bpzl s BPSet.empty
		in
		(*DEBUG*) 
			dbg "execute successful.";
			dbg (string_of_state s);
		(**)
		true
	with ExecuteCrash -> (
		dbg "execute failed.";
		false
	)
;;


