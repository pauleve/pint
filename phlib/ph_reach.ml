(*
Copyright or © or Copr. Loïc Paulevé, Morgan Magnin, Olivier Roux (2010)

loic.pauleve@irccyn.ec-nantes.fr
morgan.magnin@irccyn.ec-nantes.fr
olivier.roux@irccyn.ec-nantes.fr

This software is a computer program whose purpose is to provide Process
Hitting related tools.

This software is governed by the CeCILL license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the
CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author, the holder of the
economic rights, and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL license and that you accept its terms.
*)

open Debug;;

open Ph_types;;

type bounce_path = sort * sortidx * ISet.t
type bounce_sequence = action list

let bp_sort (a, _, _) = a;;
let bp_bounce (_, _, js) = js;;
let bp_target (_, i, _) = i;;
let bp_reach s (z,l) = (z, state_value s z, ISet.singleton l);;

module BounceSequenceOT = struct type t = bounce_sequence let compare = compare end
module BS = Set.Make (BounceSequenceOT)
module BPSet = Set.Make (struct type t = bounce_path let compare = compare end)
module BPMap = Map.Make (struct type t = bounce_path let compare = compare end)

let string_of_bounce_path (a, i, js) =
	string_of_process (a,i) ^ " " ^ string_of_iset js
;;
let string_of_bp_set = string_of_set string_of_bounce_path BPSet.elements;;
let string_of_bounce_sequence bs =
	(*"["^(String.concat "; " (List.map (function action -> string_of_process (hitter action)) bs))^"]"*)
	"["^(String.concat "; " (List.map string_of_action bs))^"]"
;;
let string_of_BS = string_of_set string_of_bounce_sequence BS.elements;;

type env = {
	sorts : process list;
	t_hits : hits;
	aBS : (bounce_path, Ph_static.ProcEqSet.t list) Hashtbl.t;
	aDep : (bounce_path, BPSet.t list) Hashtbl.t;
	process_equivalence : sort -> process -> ISet.t;
}

let create_env (ps,hits) = 
	let equivalences = Ph_static.EqMap.empty
	(*let equivalences = Ph_static.processes_equivalences (ps, hits)*)
	in
	{
		sorts = ps;
		t_hits = hits;
		aBS = Hashtbl.create 50;
		aDep = Hashtbl.create 50;
		process_equivalence = Ph_static.get_process_equivalence equivalences;
	}
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

let string_of_aBS aBS = "[ "^(String.concat "; " (List.map Ph_static.string_of_proceqset aBS))^" ]"
;;
let compute_aBS env bp =
	let a, i, reachset = bp
	in
	let rec walk history results j visited =
		if ISet.mem j reachset then
			history::List.filter (fun bps -> not(Ph_static.ProcEqSet.subset history bps)) results
		else
			let visited = ISet.add j visited
			and actions = Hashtbl.find_all env.t_hits (a,j)
			in
			let folder results = function ((b,j),_), k ->
				if ISet.mem k visited then
					results
				else 
					let history = 
						if b <> a then 
							let js = env.process_equivalence a (b,j)
							in
							let peq = (b, js)
							in
							Ph_static.ProcEqSet.add peq history
						else
							history
					in
					if List.exists (fun bps -> Ph_static.ProcEqSet.subset bps history) results then
						results
					else
						walk history results k visited
			in
			List.fold_left folder results actions
	in
	(*DEBUG*) dbg_noendl ("- computing aBS("^string_of_bounce_path bp^")..."); (**)
	let aBS = walk Ph_static.ProcEqSet.empty [] i ISet.empty
	in  
	(*DEBUG*) dbg (" "^string_of_aBS aBS); (**)
	Hashtbl.add env.aBS bp aBS;
	aBS
;;
let get_aBS env bp =
	try Hashtbl.find env.aBS bp
	with Not_found -> compute_aBS env bp
;;

let string_of_aDep bps_list = 
	"[ "^ (String.concat "; " (List.map (string_of_set string_of_bounce_path BPSet.elements) bps_list))^" ]"
;;

let reach_bounce_path s (a,i) =
	(a, state_value s a, ISet.singleton i)
;;

let rec compute_aDep env s bp =
	let aBS = get_aBS env bp
	in
	let aDep = List.map (fun peqs -> 
		let tr (b, js) bps =
			BPSet.add (b, state_value s b, js) bps
		in
		Ph_static.ProcEqSet.fold tr peqs BPSet.empty) aBS
	in
	(*DEBUG*) dbg ("- aDep(s, "^string_of_bounce_path bp^") = "^string_of_aDep aDep); (**)
	Hashtbl.add env.aDep bp aDep;
	(* resolv dependences *)
	let resolv_bp bp =
		if not (Hashtbl.mem env.aDep bp) then
			ignore (compute_aDep env s bp)
	in
	List.iter (BPSet.iter resolv_bp) aDep;
	aDep
;;
(* TODO: support multiple s *)
let get_aDep env s bp =
	try Hashtbl.find env.aDep bp
	with Not_found -> compute_aDep env s bp
;;

let cleanup_aDep env =
	(* build reversed aDep *)
	let raDep = Hashtbl.create (Hashtbl.length env.aDep)
	in
	let folder bp bps_list (trues,keys) =
		let register bps =
			let register bp' =
				Hashtbl.add raDep bp' bp
			in
			BPSet.iter register bps
		in
		List.iter register bps_list;
		(if bps_list = [BPSet.empty] then
			BPSet.add bp trues
		else trues), bp::keys
	in
	let green, keys = Hashtbl.fold folder env.aDep (BPSet.empty, [])
	in
	(* colourise *)
	let rec colourise visited currents =
		if BPSet.is_empty currents then
			visited
		else 
			let visited = BPSet.union visited currents
			in
			let fold bp coloured =
				let parents = Hashtbl.find_all raDep bp
				in
				let fold_parent coloured pbp =
					if BPSet.mem pbp visited then coloured
					else
						let bps_list = Hashtbl.find env.aDep pbp
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
			let aDep = Hashtbl.find env.aDep key
			in
			let aDep = List.filter (fun bps -> BPSet.subset bps green) aDep
			in
			Hashtbl.replace env.aDep key aDep
		else
			(* remove key *)
			Hashtbl.remove env.aDep key
	in
	List.iter cleanup keys
;;

let dot_idx_from_bp (a,i,js) =
	"\"" ^ a^" "^string_of_int i^"->"^string_of_iset js^"\""
;;

let dot_from_aDep env =
	let idx = ref 0
	in
	let string_of_bps =
		string_of_set (fun (a,i,js) -> Ph_static.string_of_proceq (a,js)) BPSet.elements
	in
	let folder bp bpss str =
		let fold_bps str bps = (
			idx := !idx + 1;
			let child_id = "child"^string_of_int (!idx)
			in
			let fold_bp bp' str =
				str ^
				"  " ^ child_id ^" -> "^dot_idx_from_bp bp'^"\n"
			in
			let label = string_of_bps bps
			in
			str ^ 
			"  " ^ child_id^"[label=\""^label^"\" shape=box]\n" ^
			"  " ^ dot_idx_from_bp bp^ " -> "^child_id^"\n" ^
			(BPSet.fold fold_bp bps "")
		) in
		List.fold_left fold_bps str bpss
	in
	Hashtbl.fold folder env.aDep "digraph aDep {\n" ^ "}"
;;

let rec link_choices _D = function _,[] | [],_ -> _D
	| (k::tk, v::tv) -> 
		let _D = BPMap.add k v _D
		in link_choices _D (tk,tv)
;;


let fold_concretions2 (bps,_D) (handler, merger, stopper) env s root =
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
			let selectors = List.map (fun bp -> get_aDep env s bp) bps
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
	fold_concretions _D bps (BPSet.singleton root)
;;
let fold_concretions = fold_concretions2 (BPSet.empty, BPMap.empty)
;;

let dot_from_concretion (bps,_D) =
	let fold bp bps str = 
		let fold_bp bp' str = str ^
			"  " ^ dot_idx_from_bp bp ^" -> " ^dot_idx_from_bp bp'^"\n"
		in
		BPSet.fold fold_bp bps str
	in
	BPMap.fold fold _D "digraph concretion {\n" ^ "}"
;;

exception ExecuteCrash
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

let concretion_sort_independence (bps, _D) =
	let fold bp sorts =
		let a = bp_sort bp
		in
		if SSet.mem a sorts then
			raise Not_found
		else
			SSet.add a sorts
	in
	try
		ignore (BPSet.fold fold bps SSet.empty);
		true
	with Not_found ->
		false
;;

(* TODO: handle equivalent processes *)
let bp_singlebounce (_,_,js) = ISet.min_elt js;;

let top a _D root =
	let rec top root = match root with (b, j, js) ->
		if b = a then 
			ISet.singleton (bp_singlebounce root)
		else
			let fold bp res =
				ISet.union res (top bp)
			in
			BPSet.fold fold (BPMap.find root _D) ISet.empty
	in
	top root
;;

module IS2 = Set.Make(struct type t = ISet.t let compare = ISet.compare end)

let order_bps (bps,_D) ignore =
	let rec order_bps root bps =
		if BPMap.mem root ignore then
			bps
		else
			let bps = root::List.filter (fun bp -> bp <> root) bps
			in
			let childs = BPMap.find root _D
			in
			BPSet.fold order_bps childs bps
	in
	order_bps
;;

let concretion_saturation_valid (bps, _D) env s bpzl =
	let missing_bps (bps, _D) satured root =
		if BPMap.mem root satured then 
			[], BPMap.find root satured
		else (
			(* fetch childs' tops *)
			let fold_child bp map =
				let fold_sort a top map =
					let atops = try SMap.find a map with Not_found -> IS2.empty
					in
					let atops' = 
						if a = bp_sort bp && a = bp_sort root then
							atops (* ignore direct child of same sort *)
						else
							let atops, atopsinter = IS2.partition (fun top' -> ISet.is_empty (ISet.inter top' top)) atops
							in
							if IS2.is_empty atopsinter then
								IS2.add top atops
							else
								let atop' = IS2.fold (fun top' atop -> ISet.union top' atop) atopsinter top
								in
								IS2.add atop' atops
					in
					SMap.add a atops' map
				in
				SMap.fold fold_sort (BPMap.find bp satured) map
			in
			let childs = BPMap.find root _D
			in
			let map = BPSet.fold fold_child childs SMap.empty
			in
			(* links between childs' tops *)
			let get_missing a tops (missing, topm) =
				let fold top (st1, missing, top') =
					let t2 = ISet.min_elt top
					and top' = ISet.union top' top
					in
					match st1 with
					  None -> (Some t2, missing, top')
					| Some t1 -> (
						st1, 
						(a, t1, ISet.singleton t2)
							::(a, t2, ISet.singleton t1)
							::missing,
						top'
					)
				in
				let _, missing, atop = IS2.fold fold tops (None, missing, ISet.empty)
				in
				let missing = List.filter (fun bp -> not (BPSet.mem bp childs)) missing
				in
				missing, SMap.add a atop topm
			in
			let missing, futuretopmap = SMap.fold get_missing map ([], SMap.empty)
			in
			(* forward between childs and root sort *)
			let a = bp_sort root
			in
			let missing = try
					(* find direct child of same sort *)
					let has_direct_child bp =
						if bp_sort bp = a && bp_singlebounce bp = bp_singlebounce root then
							raise Not_found
					in
					BPSet.iter has_direct_child childs;
					(* check if forwarding is needed *)
					let atop = SMap.find a futuretopmap
					in
					if ISet.mem (bp_target root) atop
							|| ISet.mem (bp_singlebounce root) atop then
						raise Not_found
					else
						(a, ISet.min_elt atop, bp_bounce root)::missing
				with Not_found -> missing
			in
			let futuretopmap = SMap.add a (ISet.singleton (bp_singlebounce root)) futuretopmap
			in
			missing, futuretopmap
		)
	in
	let flip = ref 0
	in
	let rec sature (bps, _D) satured = function [] -> true, (bps, _D)
		| root::tosature ->
			if !Debug.dodebug then (
				flip := 1 - !flip;
				Util.dump_to_file ("dbg-current-concretion-"^string_of_int (!flip)^".dot") (dot_from_concretion (bps,_D));
			);
			let missing, topm = missing_bps (bps, _D) satured root
			in
			if missing = [] then (* saturation done *)
				let satured = BPMap.add root topm satured
				in
				sature (bps, _D) satured tosature
			else (
				dbg_noendl ("[adding "^(String.concat " + " (List.map string_of_bounce_path missing))^"] ");
				let rec push_missing (bps, _D) tosature = function
					  [] -> sature (bps, _D) satured tosature
					| bp::missing ->
						(* merge concretion *)
						let handler (bps, _D) =
							(* link with root *)
							let _D = BPMap.add root (BPSet.add bp (BPMap.find root _D)) _D
							in
							(* ensure loop-free solution *)
							if concretion_has_cycle (bps,_D) bpzl then
								(false, (bps, _D))
							else (
								(* prepend new nodes to tosature *)
								let tosature = order_bps (bps,_D) satured bp tosature
								in
								(* push next missing *)
								push_missing (bps, _D) tosature missing
							)
						and stopper (ret, _) = ret
						and merger _ d2 = d2
						in
						try fold_concretions2 (bps,_D) (handler,merger,stopper) env s bp
						with Not_found -> (false, (bps,_D))
				in
				push_missing (bps, _D) (root::tosature) missing
			)
	in

	let get_satured bp satured =
		if not (BPMap.mem bp _D) || BPSet.is_empty (BPMap.find bp _D) then
			let b = bp_singlebounce bp
			in
			let topmap = SMap.add (bp_sort bp) (ISet.singleton b) SMap.empty
			in
			BPMap.add bp topmap satured
		else
			satured
	in
	let satured = BPSet.fold get_satured bps BPMap.empty
	in
	let tosature = order_bps (bps,_D) satured bpzl []
	in
	let success, concretion = sature (bps, _D) satured tosature
	in
	if success && !Debug.dodebug then Util.dump_to_file "dbg-concretion.dot" (dot_from_concretion concretion);
	success
;;

let process_reachability_old ph zl s =
	let env = create_env ph
	and bpzl = bp_reach s zl
	in
	(* Under-approximate ExecuteCrash *)
	dbg "+ under-approximating ExecuteCrash...";
	ignore (compute_aDep env s bpzl);
	(if !Debug.dodebug then Util.dump_to_file "dbg-reach_aDep-init.dot" (dot_from_aDep env));
	dbg "- cleanup...";
	cleanup_aDep env;
	if not (Hashtbl.mem env.aDep bpzl) then (
		dbg "+ early decision: false";
		False
	) else (
		dbg "+ no conclusion.";
		(if !Debug.dodebug then Util.dump_to_file "dbg-reach_aDep-clean.dot" (dot_from_aDep env));

		(* Over-approximate ExecuteCrash *)
		dbg "+ over-approximating ExecuteCrash...";
		let handler (bps,_D) =
			dbg_noendl "  - handling a concretion... ";
			if !Debug.dodebug then Util.dump_to_file "dbg-current-concretion.dot" (dot_from_concretion (bps,_D));
			(* 1. check for cycle-free concretion *)
			if concretion_has_cycle (bps,_D) bpzl then (
				dbg "cycle.";
				false
			(* 2. independence *)
			) else if concretion_sort_independence (bps, _D) then (
				dbg "sort independence.";
				true
			(* 3. scheduling saturation *)
			) else if concretion_saturation_valid (bps, _D) env s bpzl then (
				dbg "valid saturation.";
				dbg "WARNING: incomplete implementation!!!";
				false
			) else (
				dbg "inconclusive.";
				false
			)
		and merger r l = r || l
		and valid v = v
		in
		let ret = fold_concretions (handler, merger, valid) env s bpzl
		in
		if valid ret then (
			dbg "+ early decision: true";
			True
		) else (
			(* Can not statically conclude. *)
			dbg "+ can not statically decide.";
			Inconc
		)
	)
;;

(*** NEW IMPLEMENTATION ***)

type objective = sort * sortidx * sortidx
module ObjOrd = struct 
	type t = objective 
	let compare = compare
end

module ObjSet = Set.Make (ObjOrd)
module ObjMap = Map.Make (ObjOrd)

type abstr_struct = {
	mutable _Sol : (PSet.t list) ObjMap.t;
	mutable procs : PSet.t;
	mutable objs : ObjSet.t;
	mutable _Req : (objective list) PMap.t;
}

let copy_abstr_struct aS = {
	_Sol = aS._Sol;
	procs = aS.procs;
	objs = aS.objs;
	_Req = aS._Req;
}

type env_ng = {
	sorts : process list;
	t_hits : hits;
	r_obj : objective;
	zl : process;
	s : state;
	aBS : (objective, PSet.t list) Hashtbl.t;
	a : abstr_struct;
}

(** Objectives **)

let obj_sort (a, _, _) = a;;
let obj_bounce (_, _, j) = j;;
let obj_target (_, i, _) = i;;
let obj_reach s (a,i) = (a, state_value s a, i);;
let string_of_obj (a,i,j) =
		a^" "^string_of_int i^" "^string_of_int j;;
let string_of_objs = string_of_set string_of_obj ObjSet.elements;;
let string_of_obj_list objs = "[ "^(String.concat "; "
					(List.map string_of_obj objs))^" ]";;

(** BS **)

let string_of_aBS aBS_obj = "[ "^(String.concat "; " 
		(List.map string_of_procs' aBS_obj))^" ]"
;;

let compute_aBS env obj =
	let a, i, __to_reach = obj
	in
	let rec walk history results i visited =
		if i = __to_reach then
			(* we reached our objective, remove larger results *)
			history::List.filter 
				(fun ps -> not(PSet.subset history ps)) results
		else
			let visited = ISet.add i visited
			and actions = Hashtbl.find_all env.t_hits (a,i)
			in
			let folder results = function ((b,j),_), k ->
				if ISet.mem k visited then (* ignore loops *)
					results
				else 
					let history = 
						(* register hitter iff has different sort *)
						if b <> a then 
							PSet.add (b,j) history
						else
							history
					in
					(* ensure we are the shortest known path *)
					if List.exists (fun ps -> PSet.subset ps history) results then 
						results
					else
						walk history results k visited
			in
			List.fold_left folder results actions
	in
	dbg_noendl ("- computing aBS("^string_of_obj obj^")...");
	let aBS_obj = walk PSet.empty [] i ISet.empty
	in  
	dbg (" "^string_of_aBS aBS_obj); (**)
	Hashtbl.add env.aBS obj aBS_obj;
	aBS_obj
;;
let get_aBS env obj =
	try Hashtbl.find env.aBS obj
	with Not_found -> compute_aBS env obj
;;

(** Req, Sol **)

let new_abstr_struct s zl = {
	_Req = PMap.add zl [obj_reach s zl] PMap.empty;
	_Sol = ObjMap.empty;
	procs = PSet.singleton zl;
	objs = ObjSet.empty;
}
let init_env (ps,hits) s zl = 
	{
		sorts = ps;
		t_hits = hits;
		r_obj = obj_reach s zl;
		zl = zl;
		s = s;
		aBS = Hashtbl.create 50;
		a = new_abstr_struct s zl;
	}
;;

let __all_ObjMap m obj = try ObjMap.find obj m with Not_found -> [];;
let __all_PMap m p = try PMap.find p m with Not_found -> [];;

let dbg_aS aS =
	if !dodebug then
		let fold obj ps_list buf =
			buf
			^" - Sol("^string_of_obj obj^") = [ "^
				(String.concat "; " (List.map string_of_procs ps_list))
			^" ]\n"
		in
		let buf = ObjMap.fold fold aS._Sol ""
		in
		let fold p obj_list buf =
			buf
			^" - Req("^string_of_proc p^") = [ "^
				(String.concat "; " (List.map string_of_obj obj_list))
			^" ]\n"
		in
		let buf = PMap.fold fold aS._Req buf
		in
		let buf = buf ^ " - procs = " ^ string_of_procs aS.procs ^ "\n"
				^ " - objs = " ^ string_of_objs aS.objs ^ "\n"
		in
		dbg buf
	else ()
;;

let rec register_obj env s obj =
	if not (ObjSet.mem obj env.a.objs) then (
		let register_proc p new_objs = 
			let obj' = obj_reach s p
			in
			env.a._Req <- PMap.add p (obj'::__all_PMap env.a._Req p)
							env.a._Req;
			if ObjSet.mem obj' env.a.objs then
				new_objs
			else
				ObjSet.add obj' new_objs
		in
		let aBS = get_aBS env obj
		in
		env.a.objs <- ObjSet.add obj env.a.objs;
		env.a._Sol <- ObjMap.add obj aBS env.a._Sol;
		let procs = List.fold_left PSet.union PSet.empty aBS
		in
		let new_procs = PSet.diff procs env.a.procs
		in
		env.a.procs <- PSet.union new_procs env.a.procs;
		let new_objs = PSet.fold register_proc new_procs ObjSet.empty
		in
		ObjSet.iter (register_obj env s) new_objs
	)
;;

let cleanup_abstr env =
(* colorise (eq. concretizable) procs and objs
- a process is colorised => a related objectif is colorised
- an objective is colorised => exists a procset fully colorised
*)
	let rec colorize keep_procs test_procs keep_objs test_objs =
		if ObjSet.is_empty keep_objs then (
			keep_procs, keep_objs
		) else (
			let keep_procs', test_procs = PSet.partition (fun p ->
					ObjSet.exists (fun (a,i,j) -> (a,j) = p) keep_objs)
						test_procs
			in
			if not (PSet.is_empty keep_procs') then (
				let keep_procs = PSet.union keep_procs keep_procs'
				in
				let keep_objs', test_objs = ObjSet.partition 
					(fun obj -> 
					let ps_list = __all_ObjMap env.a._Sol obj
					in
					List.exists (fun ps -> PSet.subset ps keep_procs)
						ps_list)
						test_objs
				in
				let keep_procs, keep_objs' = colorize 
												keep_procs test_procs
												keep_objs' test_objs
				in
				keep_procs, ObjSet.union keep_objs keep_objs'
			) else (
				keep_procs, keep_objs
			)
		)
	in

	(* start with trivial objectives *)
	let keep_objs, test_objs = ObjSet.partition (fun (a,i,j) ->
			i = j) env.a.objs
	in
	let green_procs, green_objs = colorize PSet.empty env.a.procs 
										keep_objs test_objs
	in

	(* remove non-colorised elements *)
	let remove_procs = PSet.diff env.a.procs green_procs
	and remove_objs = ObjSet.diff env.a.objs green_objs
	in
	let cleanup_Req_proc p =
		env.a._Req <- PMap.remove p env.a._Req
	and cleanupobj obj =
		env.a._Sol <- ObjMap.remove obj env.a._Sol
	in
	PSet.iter cleanup_Req_proc remove_procs;
	ObjSet.iter cleanupobj remove_objs;
	env.a.procs <- green_procs;
	env.a.objs <- green_objs;

	let cleanupproc obj ps_list =
		let ps_list' = List.filter (fun ps -> 
		 		PSet.is_empty (PSet.inter ps remove_procs)) ps_list
		in
		assert (ps_list' <> []);
		if (List.length ps_list <> List.length ps_list') then (
			env.a._Sol <- ObjMap.add obj ps_list' env.a._Sol
		)
	in
	ObjMap.iter cleanupproc env.a._Sol
;;

let sature_loops_Req env aS =
	let group_procs (a,i) groups =
		SMap.add a (i::(try SMap.find a groups with Not_found->[]))
				groups
	in
	let groups = PSet.fold group_procs aS.procs SMap.empty
	in
	let sature_group a levels new_objs =
		new_objs @
		let sature_bounce i =
			let cur_objs = PMap.find (a,i) aS._Req
			in
			let known_targets = i::List.map obj_target cur_objs
			in
			let targets = List.filter (fun j -> 
					not (List.mem j known_targets) && (a,j) <> env.zl
				) levels
			in
			let new_objs = List.map (fun j -> (a,j,i)) targets
			in
			aS._Req <- PMap.add (a,i) (cur_objs@new_objs) aS._Req;
			new_objs
		in
		List.flatten (List.map sature_bounce levels)
	in
	SMap.fold sature_group groups []
;;

let rec sature_loops env aS =
	let new_objs = sature_loops_Req env aS
	in
	if !dodebug then
	dbg ("- sature_loops: new objectives "^string_of_obj_list new_objs);
	let register_new_obj objs obj =
		register_obj env env.s obj;
		ObjSet.add obj objs
	in
	List.fold_left register_new_obj ObjSet.empty new_objs;
;;

exception Found
let has_inconcretizable_obj aS =
	let test_req obj ps_list =
		if ps_list = [] then raise Found
	in
	try 
		ObjMap.iter test_req aS._Sol;
		false
	with Found ->
		true
;;
	

exception HasCycle
let is_cycle_free aS obj_root =
	let rec walk_obj stacks obj =
		(if ObjSet.mem obj (fst stacks) then raise HasCycle);
		let stacks = (ObjSet.add obj (fst stacks)), snd stacks
		in
		let ps_list = ObjMap.find obj aS._Sol
		in
		List.iter (fun ps -> PSet.iter (walk_proc stacks) ps) ps_list

	and walk_proc stacks p =
		(if PSet.mem p (snd stacks) then raise HasCycle);
		let stacks = fst stacks, PSet.add p (snd stacks)
		in
		let objs = PMap.find p aS._Req
		in
		List.iter (walk_obj stacks) objs
	in
	try 
		dbg_noendl "- checking cycle freeness... ";
		walk_obj (ObjSet.empty, PSet.empty) obj_root;
		dbg "cycle free!";
		true
	with HasCycle -> (
		dbg "failure =(";
		false
	) | Not_found -> (dbg "error"; false)
;;



exception Decision of ternary

let over_approximation_1 env =
	(* remove inconcretizable objectives *)
	cleanup_abstr env;
	dbg_aS env.a;
	if not (PMap.mem env.zl env.a._Req) then (
		dbg "+ over-approximation (1) failure";
		raise (Decision False)
	) else
		dbg "+ over-approximation (1) success"
;;

let under_approximation_1 env =
	let bind_choices aS =
		let rec bind_choices = function 
			  [],[] -> []
			| _,[] | [],_ -> invalid_arg "bind_choices"
			| (obj::objs, ps::choices) -> (
				aS._Sol <- ObjMap.add obj [ps] aS._Sol;
				let new_procs = PSet.diff ps aS.procs
				in
				aS.procs <- PSet.union aS.procs new_procs;
				let register_proc p objs =
					let objs' = PMap.find p env.a._Req
					in
					aS._Req <- PMap.add p objs' aS._Req;
					objs@objs'
				in
				(PSet.fold register_proc new_procs [])
				@ bind_choices (objs,choices)
			)
		in
		bind_choices
	in
	let rec make_concretions aS new_objs =
		if ObjSet.is_empty new_objs then
			(* concretion is built, handle it *)
			handle_concretion aS
		else (
			aS.objs <- ObjSet.union aS.objs new_objs;
			let new_objs = ObjSet.elements new_objs
			in
			let selectors = List.map 
				(fun obj -> ObjMap.find obj env.a._Sol) new_objs
			in
			let handler choices =
				let aS = copy_abstr_struct aS
				in
				let new_objs = bind_choices aS (new_objs, choices)
				in
				(* convert obj list into ObjSet *)
				let new_objs = List.fold_left 
						(fun objs obj -> ObjSet.add obj objs)
									ObjSet.empty new_objs
				in
				let new_objs = ObjSet.diff new_objs aS.objs
				in
				make_concretions aS new_objs
			in
			let merger v1 v2 = v1 || v2
			and stopper v = v
			in
			try 
			Util.cross_forward (handler, merger, stopper) selectors
			with Not_found -> (
				(* no choice available: do nothing *)
				dbg "# aborting concretion";
				false )
			| x -> raise x
		)

	and handle_concretion aS =
		dbg "# handling concretion";
		let new_objs = sature_loops env aS
		in
		if not (ObjSet.is_empty new_objs) then (
			dbg "# continue concretizing";
			make_concretions aS new_objs
		) else (
			dbg_aS aS;
			if not (has_inconcretizable_obj aS) && is_cycle_free aS env.r_obj then (
				dbg "+ under-approximation (1) success";
				true
			) else (
				dbg "+ under-approximation (1) failure";
				false
			)
		)
	in

	let aS = new_abstr_struct env.s env.zl
	in
	if make_concretions aS (ObjSet.singleton env.r_obj) then
		raise (Decision True);
;;
	
let process_reachability ph zl s =
	let env = init_env ph s zl
	in
	(* fill Req and Sol *)
	register_obj env env.s env.r_obj;
	try
		over_approximation_1 env;
		under_approximation_1 env;
		Inconc
	with 
	  Decision d -> d
	| x -> raise x
;;
(*** ***)

