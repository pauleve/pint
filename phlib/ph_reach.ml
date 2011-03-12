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

type objective_seq = objective list

type abstr_struct = {
	mutable procs : PSet.t;
	mutable objs : ObjSet.t;
	mutable _Sol : (PSet.t list) ObjMap.t;
	mutable _Req : (objective list) PMap.t;
	mutable _Cont : ObjSet.t ObjMap.t;
}

let copy_abstr_struct aS = {
	procs = aS.procs;
	objs = aS.objs;
	_Sol = aS._Sol;
	_Req = aS._Req;
	_Cont = aS._Cont;
}

type env = {
	ph : ph;
	s : state;
	w : objective_seq;
	bs_cache : Ph_bounce_seq.bs_cache;
	a : abstr_struct;
}

(** Objectives **)

let string_of_objs = string_of_set string_of_obj ObjSet.elements;;
let string_of_objseq = Util.string_of_list string_of_obj;;

let rec objseq_from_procseq s = function
	  [] -> []
	| (a,i)::pl ->
		let s' = SMap.add a i s
		in
		(obj_reach s (a,i))::objseq_from_procseq s' pl
;;

(** Req, Sol **)

let new_abstr_struct s w = 
	let aS = {
		_Req = PMap.empty;
		_Sol = ObjMap.empty;
		_Cont = ObjMap.empty;
		procs = PSet.empty;
		objs = ObjSet.empty;
	}
	in
	let register_obj obj =
		let bounce = obj_bounce_proc obj
		in (
		let objs = try PMap.find bounce aS._Req
			with Not_found -> []
		in
		let objs = if List.mem obj objs then objs
			else (obj::objs)
		in
		aS._Req <- PMap.add bounce objs aS._Req;
		aS.procs <- PSet.add bounce aS.procs;
		)
	in
	List.iter register_obj w;
	aS
;;

let init_env ph s w = 
	{
		ph = ph;
		s = s;
		w = w;
		bs_cache = Ph_bounce_seq.new_bs_cache ();
		a = new_abstr_struct s w;
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
		let fold obj objs buf =
			buf
			^" - Cont("^string_of_obj obj^") = "^string_of_objs objs
			^"\n"
		in
		let buf = ObjMap.fold fold aS._Cont buf
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
		let aBS = Ph_bounce_seq.get_aBS env.ph env.bs_cache obj
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
			let known_targets = List.map obj_target cur_objs
			in
			let targets = List.filter (fun j -> 
					not (List.mem j known_targets)
					&& (List.length env.w > 1 
						|| (a,j) <>	obj_bounce_proc (List.hd env.w))
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

let register_objs env objs =
	if !dodebug then
		dbg ("- new objectives "^string_of_objseq objs);
	let register_new_obj objs obj =
		if not (ObjSet.mem obj env.a.objs) then (
			register_obj env env.s obj;
			ObjSet.add obj objs
		) else objs
	in
	List.fold_left register_new_obj ObjSet.empty objs;
;;

let rec sature_loops env aS =
	let new_objs = sature_loops_Req env aS
	in
	register_objs env new_objs
;;

(** returns ObjSet *)
let min_cont env aS obj =
	dbg_noendl ("- minCONT("^string_of_obj obj^") = ");
	let a = obj_sort obj
	in
	let rec min_cont_obj knownps obj =
		let fold_p p levels =
			ISet.union levels (min_cont_p (PSet.add p knownps) p)
		in
		let make_sol ps =
			PSet.fold fold_p (PSet.diff ps knownps) ISet.empty
		in
		let fold_sol levels ps =
			if ISet.is_empty levels then levels
			else (ISet.inter levels (make_sol ps))
		in
		let sols = ObjMap.find obj aS._Sol
		in
		match sols with
		  [] -> ISet.empty
		| ps::altps ->
			let levels = make_sol ps
			in
			List.fold_left fold_sol levels altps
	and min_cont_p knownps (b,i) =
		if a = b then ISet.singleton i
		else (
			let fold_req levels obj =
				if ISet.is_empty levels then levels
				else (ISet.inter levels (min_cont_obj knownps obj))
			in
			let reqs = PMap.find (b,i) aS._Req
			in
			match reqs with
			  [] -> ISet.empty
			| obj::objs ->
				let levels = min_cont_obj knownps obj
				in
				List.fold_left fold_req levels objs
		)
	in
	let bounce = obj_bounce obj
	in
	let cont_levels = ISet.remove bounce (min_cont_obj PSet.empty obj)
	in
	let fold_level i objs = 
		ObjSet.add (a, i, bounce) objs
	in
	let objs = ISet.fold fold_level cont_levels ObjSet.empty
	in
	if !dodebug then dbg (string_of_objs objs);
	objs
;;
	

(* returns map ai -> ISet where ai in Sol(obj) *)
let max_contmap env aS obj =
	let a = obj_sort obj
	in
	let rec max_cont_obj knownps obj =
		let fold_sol levels ps =
			let fold_p p levels =
				ISet.union levels (max_cont_p (PSet.add p knownps) p)
			in
			PSet.fold fold_p (PSet.diff ps knownps) levels
		in
		List.fold_left fold_sol ISet.empty (try ObjMap.find obj aS._Sol with Not_found -> [])
	and max_cont_p knownps (b,j) =
		if a = b then ISet.singleton j
		else (
			let fold_req levels obj =
				ISet.union levels (max_cont_obj knownps obj)
			in
			List.fold_left fold_req ISet.empty (PMap.find (b,j) aS._Req)
		)
	in
	let fold_sol map ps =
		let fold_p p map = 
			let mc = max_cont_p PSet.empty p
			in
			if ISet.is_empty mc then
				map
			else
				let levels = try PMap.find p map
					with Not_found -> ISet.empty
				in
				PMap.add p (ISet.union levels mc) map
		in
		PSet.fold fold_p ps map
	in
	dbg ("maxCont for "^string_of_obj obj);
	let r = List.fold_left fold_sol PMap.empty (ObjMap.find obj aS._Sol)
	in
	dbg "done.";
	r
;;

let get_tBS env aS obj =
	let choices = ObjMap.find obj aS._Sol
	in
	dbg ("restricted choices: "^(String.concat ";" (List.map string_of_procs choices)));
	let restr_procs = List.fold_left PSet.union PSet.empty choices
	in
	Ph_bounce_seq.tBS env.ph env.bs_cache obj restr_procs
;;

let sature_cont env aS =
	dbg_aS aS;
	dbg "- continuity saturation...";
	let sature_cont obj objs =
		let b = obj_sort obj
		in
		let contmap = max_contmap env aS obj
		in
		let fold ai levels maxcont =
			ISet.union maxcont (ISet.remove (obj_target obj) levels)
		in
		let levels = PMap.fold fold contmap ISet.empty
		in
		let fold i maxcont =
			ObjSet.add (b, i, obj_bounce obj) maxcont
		in
		let maxcont = ISet.fold fold levels ObjSet.empty
		in
		aS._Cont <- ObjMap.add obj maxcont aS._Cont;

		(* reversed cont *)
		let fold ai levels objs =
			let fold j objs =
				let bj = (b,j)
				in
				let fold_target (b,k) objs =
					let obj' = (b, k, j)
					in
					dbg ("reverse continuity from "
						^ string_of_proc ai ^ ": "
						^ string_of_obj obj');
					let cur_req = PMap.find bj aS._Req
					in
					if not (List.mem obj' cur_req) then (
						aS._Req <- PMap.add bj (obj'::cur_req) aS._Req;
						ObjSet.add obj' objs
					) else
						objs
				in
				let targets = get_tBS env aS obj ai
				in
				PSet.fold fold_target targets objs
			in
			ISet.fold fold levels objs
		in
		PMap.fold fold contmap maxcont
	in
	let objs = ObjSet.fold sature_cont aS.objs ObjSet.empty
	in
	let new_objs = ObjSet.diff objs aS.objs
	in
	register_objs env (ObjSet.elements new_objs)
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

let fill_min_cont env =
	let rec new_min_cont objs =
		let fold_obj obj new_objs =
			let cont_objs = min_cont env env.a obj
			in (
			env.a._Cont <- ObjMap.add obj cont_objs env.a._Cont;
			ObjSet.union new_objs cont_objs 
		) in
		let new_objs = ObjSet.fold fold_obj objs ObjSet.empty
		and cur_objs = env.a.objs
		in
		ObjSet.iter (register_obj env env.s) new_objs;
		let objs = ObjSet.diff env.a.objs cur_objs
		in
		if not (ObjSet.is_empty objs) then
			new_min_cont objs
	in
	new_min_cont env.a.objs
;;

exception HasCycle
let is_cycle_free aS obj_root =
	let rec walk_obj stacks obj =
		(if ObjSet.mem obj (fst stacks) then raise HasCycle);
		let stacks = (ObjSet.add obj (fst stacks)), snd stacks
		in
		try
			let objs = ObjMap.find obj aS._Cont
			in
			ObjSet.iter (walk_obj stacks) objs
		with Not_found -> ();
		let ps_list = ObjMap.find obj aS._Sol
		in
		List.iter (fun ps -> PSet.iter (walk_proc stacks) ps) ps_list;

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
	) | x -> raise x
;;



exception Decision of ternary

let over_approximation_1 env =
	(* inital abstract structure *)
	ignore(register_objs env env.w);
	(* remove inconcretizable objectives *)
	cleanup_abstr env;
	dbg_aS env.a;
	let obj_ok obj = 
		ObjMap.mem obj env.a._Sol (*
		&& (
			let conts = ObjMap.find obj env.a._Cont
			in
			ObjSet.for_all obj_ok conts
		)*)
	in
	if List.for_all obj_ok env.w then
		dbg "+ over-approximation (1) success"
	else (
		dbg "+ over-approximation (1) failure";
		raise (Decision False)
	)
;;

let over_approximation_mincont env =
	fill_min_cont env;
	cleanup_abstr env;
	dbg_aS env.a;
	let rec obj_ok obj = 
		ObjMap.mem obj env.a._Sol
		&& (
			let conts = ObjMap.find obj env.a._Cont
			in
			ObjSet.for_all obj_ok conts
		)
	in
	if List.for_all obj_ok env.w then
		dbg "+ over-approximation (minCONT) success"
	else (
		dbg "+ over-approximation (minCONT) failure";
		raise (Decision False)
	)
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
				let new_objs = List.fold_right ObjSet.add new_objs ObjSet.empty
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
			with Util.Empty -> (
				(* no choice available: do nothing *)
				dbg "# aborting concretion";
				false )
			| x -> raise x
		)

	and handle_concretion aS =
		dbg "# handling concretion";
		let new_objs = ObjSet.union (sature_loops env aS)
							(sature_cont env aS)
		in
		if not (ObjSet.is_empty new_objs) then (
			dbg "# continue concretizing";
			make_concretions aS new_objs
		) else (
			dbg_aS aS;
			if not (has_inconcretizable_obj aS) && 
					List.for_all (is_cycle_free aS) env.w then (
				dbg "+ under-approximation (1) success";
				true
			) else (
				dbg "+ under-approximation (1) failure";
				false
			)
		)
	in

	let aS = new_abstr_struct env.s env.w
	in
	let new_objs = List.fold_right ObjSet.add env.w ObjSet.empty
	in
	if make_concretions aS new_objs then
		raise (Decision True);
;;

let process_reachability ph s w =
	let env = init_env ph s w
	in
	try
		over_approximation_1 env;
		under_approximation_1 env;
		over_approximation_mincont env;
		Inconc
	with 
	  Decision d -> d
	| x -> raise x
;;

(**
  EXPERIMENTATIONS
*)

let overapprox_order env =
	(* 1. Compute full saturation *)
	dbg "\nComputing saturated structure";
	let rec full_saturation () = 
		let new_objs = ObjSet.union (sature_loops env env.a) (sature_cont env env.a)
		in
		if not (ObjSet.is_empty new_objs) then (
			env.a.objs <- ObjSet.union env.a.objs new_objs;
			full_saturation ()
		)
	in
	full_saturation ();
	dbg "Fully saturated structure:";
	dbg_aS env.a;

	(* 2. Compute pre-order *)
	let fetch_impossible obj sol stack =
		if sol == [] then obj::stack else stack
	in
	let iobjs = ObjMap.fold fetch_impossible env.a._Sol []
	in
	dbg ("Impossible objectives: "^(String.concat "; " (List.map string_of_obj iobjs)))
;;

let test_order ph s w =
	let env = init_env ph s w
	in
	try
		over_approximation_1 env;
		under_approximation_1 env;
		over_approximation_mincont env;
		overapprox_order env;
		Inconc
	with 
	  Decision d -> d
	| x -> raise x
;;

open Ph_abstr_struct;;

let init_obj env gaS obj = 
	let rec init_obj obj =
		if not (gaS#has_obj obj) then (
			let nobj = NodeObj obj
			and aBS = Ph_bounce_seq.get_aBS env.ph env.bs_cache obj
			in
			let register_sol allps ps =
				gaS#add_child nobj (NodeSol (obj, ps));
				PSet.union allps ps
			in
			let allps = List.fold_left register_sol PSet.empty aBS
			in
			PSet.iter init_proc allps
		)
	and init_proc p =
		if not (gaS#has_proc p) then (
			let np = NodeProc p
			in
			let obj = obj_reach env.s p
			in
			init_obj obj;
			gaS#add_child np (NodeObj obj)
		)
	in
	init_obj obj
;;


let max_cont (gaS : #graph) objs =
	let init = function
		  NodeProc (a,i) -> SMap.add a (PSet.singleton (a,i)) SMap.empty
		| _ -> SMap.empty
	and push n v n' v' = if SMap.is_empty v' then (v, false) else
		match n, n' with
		  NodeSol _, NodeProc _ 
		| NodeObj _, NodeSol _ | NodeProc _, NodeObj _ ->
		  	(* union between childs *)
			let merge a ps my_v =
				let ps' = try SMap.find a my_v
						with Not_found -> PSet.empty
				in
				SMap.add a (PSet.union ps ps') my_v
			in
			let my_v = SMap.fold merge v' v
			in
			my_v, my_v <> v

		| NodeObj _, NodeObj _ -> v, false (* ignore Cont rels *)
		| _ -> failwith "invalid graph"
	in
	let fold_obj obj ns = NodeSet.add (NodeObj obj) ns
	in
	let ns = ObjSet.fold fold_obj objs NodeSet.empty
	in
	let values = gaS#rflood init push ns
	in
	let dbg_val n v = match n with
		  NodeObj (a,i,j) -> 
		  	let ps = try SMap.find a v
				with Not_found -> PSet.empty
			in
			dbg ("maxCONT("^string_of_obj (a,i,j)^")="
					^string_of_procs ps)
		| _ -> ()
	in
	if !dodebug then Hashtbl.iter dbg_val values;
	values
;;

let sature_gaS env gaS =
	let sature_req () =
		let group_procs (a,i) groups =
			SMap.add a (i::(try SMap.find a groups with Not_found->[]))
					groups
		in
		let groups = PSet.fold group_procs gaS#procs SMap.empty
		in
		let sature_group a levels =
			let sature_bounce i =
				let cur_objs = gaS#childs (NodeProc (a,i))
				in
				let known_targets = List.map (function NodeObj obj -> obj_target obj
									| _ -> failwith "invalid graph") cur_objs
				in
				let new_targets = List.filter (fun j -> 
						not (List.mem j known_targets)
						&& (List.length env.w > 1 
							|| (a,j) <>	obj_bounce_proc (List.hd env.w))
						) levels
				in
				let register_target j =
					let obj = (a,j,i)
					in
					init_obj env gaS obj;
					gaS#add_child (NodeProc (a,i)) (NodeObj obj)
				in
				List.iter register_target new_targets
			in
			List.iter sature_bounce levels
		in
		SMap.iter sature_group groups

	and sature_cont () =
		let maxCont = max_cont gaS gaS#objs
		in
		let register_cont n v = match n with
			  NodeObj (a,i,j) -> 
				let ps = try SMap.find a v with Not_found -> PSet.empty
				in
				let known_conts = List.filter (function NodeObj _ -> true | _ -> false)
									(gaS#childs n)
				in
				let register_target (a,k) =
					if k <> i then
						let obj' = (a,k,j) 
						in
						let nobj' = NodeObj obj'
						in
						if not (List.mem nobj' known_conts) then (
							init_obj env gaS obj';
							gaS#add_child n nobj'
						)
				in
				PSet.iter register_target ps
				(* TODO: reversed continuity *)
			| _ -> ()
		in
		Hashtbl.iter register_cont maxCont

	in
	let rec sature () =
		let nb_procs = gaS#count_procs ()
		in
		sature_req ();
		sature_cont ();
		if gaS#count_procs () <> nb_procs then
			sature ()
	in
	sature ()
;;

let min_proc (gaS : #graph) ctx objs =

	let union_values (ps,nm) n' (ps',_) =
		(* union between childs *)
		(* 1. update cache map *)
		let nm = NodeMap.add n' ps' nm
		in
		if PSet.is_empty ps' then
			((ps,nm), false)
		else 
			(* 2. compute union *)
			let new_ps = NodeMap.fold (fun _ -> PSet.union) nm PSet.empty
			in
			((new_ps,nm), new_ps <> ps)

	and inter_values (ps,nm) n' (ps',_) =
		(* intersection between childs *)
		(* 1. update cache map *)
		let nm = NodeMap.add n' ps' nm
		in
		(* 2. compute intersection *)
		let inter_ps _ ps = function None -> Some ps
				| Some ps' -> Some (PSet.inter ps ps')
		in
		let new_ps = match NodeMap.fold inter_ps nm None with
			  None -> PSet.empty
			| Some x -> x
		in
		(new_ps,nm), new_ps <> ps
	in

	let init = function
		  NodeObj obj -> (PSet.singleton (obj_bounce_proc obj), NodeMap.empty)
		| _ -> (PSet.empty, NodeMap.empty)
	and push n v n' v' =
		match n, n' with
		  NodeSol _, NodeProc _ -> union_values v n' v'

		| NodeProc _, NodeObj obj ->
			(* ignore if not in context *)
			if not (List.mem (obj_target obj) (SMap.find (obj_sort obj) ctx)) then
				(v,false)
			else 
				(inter_values v n' v')

		| NodeObj obj, NodeSol _  ->
			if SMap.find (obj_sort obj) ctx = [obj_target obj] then
				inter_values v n' v'
			else
				failwith "TODO"
				(* inter_values v n' v' + apply new context *)

		| NodeObj _, NodeObj _ -> inter_values v n' v'

		| _ -> failwith "wrong abstract structure graph."
	in
	let fold_obj obj ns = NodeSet.add (NodeObj obj) ns
	in
	let ns = ObjSet.fold fold_obj objs NodeSet.empty
	in
	let values = gaS#rflood init push ns
	in
	let string_of_value (ps,_) = string_of_procs ps
	in
	let dbg_val n v = match n with
		  NodeObj obj -> dbg ("minPROC^Obj("^string_of_obj obj^")="
		  		^string_of_value v)
		| NodeProc p -> dbg ("minPROC^Proc("^string_of_proc p^")="
				^string_of_value v)
		| NodeSol (obj,ps) -> dbg ("minPROC^Sol("^string_of_procs ps^")="
				^string_of_value v)
	in
	if !dodebug then Hashtbl.iter dbg_val values
;;

(*
let test_gaS env gaS =
	(*fill_min_cont env*)
	min_cont gaS env.a.objs;

	(* test saturation *)
	sature_gaS env gaS;
	gaS#debug ();
;;
*)

let min_conts (gA : #graph) objs =

	let union_value (ctx,nm) =
		NodeMap.fold (fun _ -> ctx_union) nm ctx
	and inter_value (ctx,nm) =
		NodeMap.fold (fun _ -> ctx_inter) nm ctx
	in
	let update_value n (ctx,nm) = match n with
		  NodeSol _ -> union_value (ctx,nm)
		| NodeObj _ -> inter_value (ctx,nm)
		| NodeProc (a,i) -> 
			let ctx' = inter_value (ctx,nm)
			in
			SMap.add a (ISet.singleton i) ctx'
	in
	let update n (v,nm) n' (v',_) =
		(* 1. update cache map *)
		let nm = NodeMap.add n' v' nm
		in
		(* 2. update value *)
		let v' = update_value n (v,nm)
		in
		(v',nm), v<>v'
	in

	(** each node is associated to a couple
			(ctx, nm) 
		where nm is the cached value of childs *)
	
	let init n =
		let nm = NodeMap.empty 
		in
		update_value n (SMap.empty, nm), nm

	(* the node n with value v receive update from node n' with value v' *)
	and push n v n' v' = if SMap.is_empty (fst v') then (v, false) else
		match n, n' with
		  NodeSol _, NodeProc _
		| NodeObj _, NodeSol _
		| NodeProc _, NodeObj _ -> (update n v n' v')
		| NodeObj _, NodeObj _ -> (v, false) (* ignore Cont rels *)
		| _ -> failwith "wrong abstract structure graph."
	in
	let fold_obj ns obj = NodeSet.add (NodeObj obj) ns
	in
	let ns = List.fold_left fold_obj NodeSet.empty objs
	in
	let values = gA#rflood init push ns
	in
	if !dodebug then (
		let dbg_val n (v,_) = match n with
			  NodeObj (a,i,j) -> 
				let ais = try SMap.find a v with Not_found -> ISet.empty
				in
				dbg ("minCONT("^string_of_obj (a,i,j)^")="
						^a^"_{"^string_of_iset ais^ "} ("^string_of_ctx v^")")
			| _ -> ()
		in
		Hashtbl.iter dbg_val values;
	);
	values
;;


class cwA ctx w get_Sols =
object(self) inherit graph

	val mutable new_objs = []

	method start_mod () = 
		new_objs <- []
	
	method end_mod () =
		(* update min_cont for new objects *)
		let cont = min_conts self new_objs
		in
		ignore(cont)

	method init_obj obj nobj =
		let aBS = get_Sols obj
		in
		let register_sol allps ps =
			self#add_child nobj (NodeSol (obj, ps));
			PSet.union allps ps
		in
		let allps = List.fold_left register_sol PSet.empty aBS
		in
		PSet.iter self#init_proc allps;
		new_objs <- obj::new_objs

	method init_proc (a,i) =
		if not (self#has_proc (a,i)) then (
			let np = NodeProc (a,i)
			in
			let objs = ISet.fold (fun j objs -> (a,j,i)::objs) (SMap.find a ctx) []
			in
			List.iter (fun obj ->
				let nobj = NodeObj obj
				in
				self#add_child np nobj;
				self#init_obj obj nobj) objs
		)
end;;



let gA_init env ctx w =
	let gA = new cwA ctx w (Ph_bounce_seq.get_aBS env.ph env.bs_cache)
	in
	gA#start_mod ();
	List.iter (fun (a,j,i) -> gA#init_proc (a,j)) w;
	gA#end_mod ();
	gA
;;

let test_new_abstr ph s w =
	let ctx = ctx_of_state s
	in
	let env = init_env ph s w
	in
	let gA = gA_init env ctx w
	in
	gA#debug ();
	try
(*		test_gaS env gaS; *)
		Inconc
	with
	  Decision d -> d
	| x -> raise x
;;

let test = test_new_abstr;;

