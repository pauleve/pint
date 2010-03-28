
open Debug;;

open Ph_types;;

type bounce_path = sort * sortidx * ISet.t
type bounce_sequence = action list

let bp_sort (a, _, _) = a;;
let bp_bounce (_, _, aj) = aj;;
let bp_reach s (z,l) = (z, state_value s z, ISet.singleton l);;

module BounceSequenceOT = struct type t = bounce_sequence let compare = compare end
module BS = Set.Make (BounceSequenceOT)
module BPSet = Set.Make (struct type t = bounce_path let compare = compare end)
module BPMap = Map.Make (struct type t = bounce_path let compare = compare end)

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
	aBS : (bounce_path, BPSet.t list) Hashtbl.t;
	process_equivalence : sort -> process -> ISet.t;
}

let create_env (ps,hits) = 
	let equivalences = Ph_static.processes_equivalences (ps, hits)
	in
	{
		sorts = ps;
		t_hits = hits;
		_BS = Hashtbl.create 50;
		aBS = Hashtbl.create 50;
		process_equivalence = Ph_static.get_process_equivalence equivalences;
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
			let reach = env.process_equivalence a (b,j)
			in
			let s = execute env (b, sb, reach) s stack
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

let process_reachability_using_execute env bpzl s =
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

let abstr_seq env a seq = 
	let folder hitters action =
		let (b,j) = hitter action
		in
		if b <> a then 
			let js = env.process_equivalence a (b,j)
			in
			Ph_static.ProcEqSet.add (b,js) hitters
		else hitters
	in
	List.fold_left folder Ph_static.ProcEqSet.empty seq
;;
let dep s aseq =
	let fold (a,js) bps =
		BPSet.add (a, state_value s a, js) bps
	in
	Ph_static.ProcEqSet.fold fold aseq BPSet.empty
;;

module BPSS = Set.Make (struct type t = BPSet.t let compare = BPSet.compare end)
let rec compute_aBS env s bp =
	let a = bp_sort bp
	and seqs = _BS env bp
	in
	let fold seq aBS =
		let aseq = abstr_seq env a seq 
		in
		BPSS.add (dep s aseq) aBS
	in
	let aBS = BS.fold fold seqs BPSS.empty
	in
	(* remove sur-sets *)
	let keep_dep bps =
		BPSS.for_all (function bps' -> 
			BPSet.equal bps bps' || not (BPSet.subset bps' bps))
				aBS
	in
	let aBS = BPSS.elements (BPSS.filter keep_dep aBS)
	in
	Hashtbl.add env.aBS bp aBS;
	(* resolv dependences *)
	let resolv_bp bp =
		if not (Hashtbl.mem env.aBS bp) then
			ignore (compute_aBS env s bp)
	in
	List.iter (BPSet.iter resolv_bp) aBS;
	aBS
;;

let cleanup_aBS env =
	(* build reversed aBS *)
	let raBS = Hashtbl.create (Hashtbl.length env.aBS)
	in
	let folder bp bps_list (trues,keys) =
		let register bps =
			let register bp' =
				Hashtbl.add raBS bp' bp
			in
			BPSet.iter register bps
		in
		List.iter register bps_list;
		(if bps_list = [BPSet.empty] then
			BPSet.add bp trues
		else trues), bp::keys
	in
	let green, keys = Hashtbl.fold folder env.aBS (BPSet.empty, [])
	in
	(* colourise *)
	let rec colourise visited currents =
		if BPSet.is_empty currents then
			visited
		else 
			let visited = BPSet.union visited currents
			in
			let fold bp coloured =
				let parents = Hashtbl.find_all raBS bp
				in
				let fold_parent coloured pbp =
					if BPSet.mem pbp visited then coloured
					else
						let bps_list = Hashtbl.find env.aBS pbp
						in
						if List.exists (fun bps -> BPSet.subset bps visited) bps_list then
							BPSet.add pbp coloured
						else
							coloured
				in
				List.fold_left fold_parent coloured parents
			in
			let coloured = BPSet.fold fold currents BPSet.empty
			in
			colourise visited coloured
	in
	let green = colourise BPSet.empty green
	in
	(* remove uncoloured nodes *)
	let cleanup key =
		if BPSet.mem key green then 
			(* remove uncoloured choices *)
			let aBS = Hashtbl.find env.aBS key
			in
			let aBS = List.filter (fun bps -> BPSet.subset bps green) aBS
			in
			Hashtbl.replace env.aBS key aBS
		else
			(* remove key *)
			Hashtbl.remove env.aBS key
	in
	List.iter cleanup keys
;;

let dot_from_aBS env =
	let id_from_bp (a,i,js) =
		"\"" ^ a^" "^string_of_int i^"->"^string_of_iset js^"\""
	in
	let idx = ref 0
	in
	let folder bp bpss str =
		let fold_bps str bps = (
			idx := !idx + 1;
			let child_id = "child"^string_of_int (!idx)
			in
			let fold_bp bp' str =
				str ^
				"  " ^ child_id ^" -> "^id_from_bp bp'^"\n"
			in
			str ^ 
			"  " ^ child_id^"[label=ALL shape=box]\n" ^
			"  " ^ id_from_bp bp^ " -> "^child_id^"\n" ^
			(BPSet.fold fold_bp bps "")
		) in
		List.fold_left fold_bps str bpss
	in
	Hashtbl.fold folder env.aBS "digraph aBS {\n" ^ "}"
;;

let fold_concretions (handler, merger, stopper) env root =
	let rec link_choices _D = function _,[] | [],_ -> _D
		| (k::tk, v::tv) -> 
			let _D = BPMap.add k v _D
			in link_choices _D (tk,tv)
	in
	let rec fold_concretions _D visited bps =
		let bps = BPSet.diff bps visited
		in
		if BPSet.is_empty bps then
			handler (visited, _D)
		else
			let visited = BPSet.union bps visited
			in
			let bps = BPSet.elements bps
			in
			let selectors = List.map (fun bp -> Hashtbl.find env.aBS bp) bps
			in
			let handler choices =
				let _D = link_choices _D (bps,choices)
				in
				let bps' = List.fold_left BPSet.union BPSet.empty choices
				in
				fold_concretions _D visited bps'
			in
			Util.cross_forward (handler, merger, stopper) selectors
	in
	fold_concretions BPMap.empty BPSet.empty (BPSet.singleton root)
;;

let concretion_has_cycle (_,_D) root =
	let rec walk stack bp =
		(if BPSet.mem bp stack then raise ExecuteCrash);
		let stack = BPSet.add bp stack
		in
		let childs = BPMap.find bp _D
		in
		BPSet.iter (walk stack) childs
	in
	try
		walk BPSet.empty root;
		false
	with ExecuteCrash ->
		true
;;

let process_reachability env zl s =
	let bpzl = bp_reach s zl
	in
	(* Under-approximate ExecuteCrash *)
	dbg "+ under-approximating ExecuteCrash...";
	ignore (compute_aBS env s bpzl);
	(* TODO: Lemma 1 ? *)
	Util.dump_to_file "dbg-reach_aBS-init.dot" (dot_from_aBS env);
	dbg "- cleanup...";
	cleanup_aBS env;
	if not (Hashtbl.mem env.aBS bpzl) then (
		dbg "+ early decision: false";
		false
	) else (
		dbg "+ no conclusion.";
		Util.dump_to_file "dbg-reach_aBS-clean.dot" (dot_from_aBS env);

		(* Over-approximate ExecuteCrash *)
		dbg "+ over-approximating ExecuteCrash...";
		let handler (bps,_D) =
			dbg "  - handling a concretion...";
			(* 1. check for cycle-free concretion *)
			if concretion_has_cycle (bps,_D) bpzl then (
				dbg "    => cycle";
				false
			) else (
			(* 2. independence *)
			(* 3. scheduling saturation *)
				dbg "    => inconclusive";
				false
			)
		and merger r l = r || l
		and valid v = v
		in
		let ret = fold_concretions (handler, merger, valid) env bpzl
		in
		if valid ret then (
			dbg "+ early decision: true";
			true
		) else (
			(* Can not statically conclude. *)
			dbg "+ can not statically decide.";
			process_reachability_using_execute env bpzl s
		)
	)
;;


