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

type objective = sort * sortidx * sortidx
module ObjOrd = struct 
	type t = objective 
	let compare = compare
end

module ObjSet = Set.Make (ObjOrd)
module ObjMap = Map.Make (ObjOrd)

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
	_Cont = ObjMap.empty;
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
	dbg_aS aS;
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

let max_cont env aS obj =
	let bounce = obj_bounce obj
	and a = obj_sort obj
	in
	let rec max_cont_obj obj =
		let fold_sol levels ps =
			let fold_p p levels =
				ISet.union levels (max_cont_p p)
			in
			PSet.fold fold_p ps levels
		in
		List.fold_left fold_sol ISet.empty (ObjMap.find obj aS._Sol)
	and max_cont_p (b,j) =
		if a = b then ISet.singleton j
		else (
			let fold_req levels obj =
				ISet.union levels (max_cont_obj obj)
			in
			List.fold_left fold_req ISet.empty (PMap.find (b,j) aS._Req)
		)
	in
	let folder i objs = ObjSet.add (a, i, bounce) objs
	in
	ISet.fold folder (ISet.remove (obj_target obj) (max_cont_obj obj)) ObjSet.empty
;;

let sature_cont env aS =
	dbg_noendl "- computing maxCont...";
	let sature_cont obj =
		aS._Cont <- ObjMap.add obj (max_cont env aS obj) aS._Cont;
	in
	ObjSet.iter sature_cont aS.objs;
	dbg " done.";
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
			with Util.No_choice -> (
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
			sature_cont env aS;
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

