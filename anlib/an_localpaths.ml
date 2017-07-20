
open Debug

open PintTypes
open AutomataNetwork

open Ph_types

let enumerate_acyclic_paths register append discard elt0 set0 an (a,i,goal) =
	let rec walk path i visited results =
		if i = goal then
			register results path
		else
			let visited = ISet.add i visited
			and nexts = Hashtbl.find an.transitions (a,i)
			in
			let visit j results =
				if ISet.mem j visited then results
				else
					let path = append results path (a,i,j)
					in
					if discard results path then
						results
					else
						walk path j visited results
			in
			ISet.fold visit nexts results
	in
	walk elt0 i ISet.empty set0


type cache = {
	csol: (transition, transition list list) Hashtbl.t;
	asol: (transition, (LSSet.t * ISet.t) list) Hashtbl.t;
	asyncsol: (transition, (StateSet.t * ISet.t) list) Hashtbl.t;
}
let create_cache ?size:(size=50) () = {
	csol = Hashtbl.create size;
	asol = Hashtbl.create size;
	asyncsol = Hashtbl.create size;
}
let _cache_computation func subcache an obj =
	try Hashtbl.find subcache obj
	with Not_found -> (
		let res = func an obj
		in
		Hashtbl.add subcache obj res;
		res)


(*** concrete solutions ***)

let simple_acyclic_paths =
	let paths = []
	and path0 = []
	and append _ path tr = path@[tr]
	and register paths path = path::paths
	and discard _ _ = false
	in
	enumerate_acyclic_paths register append discard path0 paths

let simple_acyclic_paths cache =
	_cache_computation simple_acyclic_paths cache.csol


let intermediates cache an obj =
	let fold_paths ps = function [] -> ps
		| _::path ->
		let fold_tr ps (a,i,j) = LSSet.add (a,i) ps
		in
		List.fold_left fold_tr ps path
	in
	let csols = simple_acyclic_paths cache an obj
	in
	List.fold_left fold_paths LSSet.empty csols


let push_abstract_transition an goal (conds_list, interm) tr =
	let conds = Hashtbl.find_all an.conditions tr
	in
	let push_conds prod conds =
		let conds = lsset_of_state conds
		in
		List.map (fun conds' -> LSSet.union conds conds') conds_list @ prod
	in
	let conds_list = List.fold_left push_conds [] conds
	in
	conds_list,
	let j = tr_dest tr
	in
	(if j <> goal then ISet.add j interm else interm)


let abstract_path an goal path =
	let fold_tr = push_abstract_transition an goal
	in
	List.fold_left fold_tr ([LSSet.empty], ISet.empty) path


let complete_abstract_solutions cache an obj =
	let goal = tr_dest obj
	in
	let abstract_path = abstract_path an goal
	in
	let fold_paths apaths path =
		let conds_list, interm = abstract_path path
		in
		List.map (fun conds -> (conds,interm)) conds_list
		@ apaths
	in
	let csols = simple_acyclic_paths cache an obj
	in
	List.fold_left fold_paths [] csols


let full_paths an ?(filter_conds = fun x -> x) path =
	let full_tr tr =
		let conds = Hashtbl.find_all an.conditions tr
		in
		let conds = filter_conds conds
		in
		List.map (fun cond -> (tr, cond)) conds
	in
	Util.cross_list (List.map full_tr path)


let concrete_solutions cache an obj (conds, interm) =
	let interm_of_path = function [] -> ISet.empty
		| _::path ->
			let fold_tr interm (_,i,_) = ISet.add i interm
			in
			List.fold_left fold_tr ISet.empty path
	in
	let filter_conds = List.filter (fun cond ->
		let cond = lsset_of_state cond in LSSet.subset cond conds)
	in
	let full_paths = full_paths an ~filter_conds
	and merge_conds =
		List.fold_left (fun conds (_, cond) ->
				let cond = lsset_of_state cond
				in
				LSSet.union conds cond)
			LSSet.empty
	in
	let filter_fpath fpath =
		LSSet.equal conds (merge_conds fpath)
	in
	let fold_csol msols path =
		let interm' = interm_of_path path
		in
		if ISet.equal interm interm' then
			List.filter filter_fpath (full_paths path) @ msols
		else msols
	in
	let csols = simple_acyclic_paths cache an obj
	in
	List.fold_left fold_csol [] csols


module type UnorderedAbstractTraceType = sig
	type t

	val empty : t
	(** Total ordering between traces. [leq e1 e2] if and only if the trace e1 is
		smaller or equal to e2 *)
	val leq : t -> t -> bool
	(** Extend the trace with transition condition ([state]).
		It is assumed that [leq e1 (extend s e1)] for any [s]. *)
	val extend : state ->  t -> t

	(** It is assumed that [leq t1 t2] only if [quick_compare t1 t2] is negative. *)
	val quick_compare : t -> t -> int

	val subcache : cache -> (transition, (t * ISet.t) list) Hashtbl.t

	type abstr
	val abstr : t -> abstr
	val match_abstr : abstr list -> t -> bool
end

module UnordUnsyncTrace = struct
	type t = LSSet.t

	let empty = LSSet.empty
	let quick_compare t1 t2 = compare (LSSet.cardinal t1) (LSSet.cardinal t2)
	let leq = LSSet.subset
	let extend state =
		let lss = lsset_of_state state
		in
		LSSet.union lss
	let subcache cache = cache.asol

	type abstr = Universe
	let abstr _ = Universe
	let match_abstr a e = true
end

module UnordTrace = struct
	type t = StateSet.t

	let empty = StateSet.empty
	let quick_compare t1 t2 =
		let lss1, lss2 = flatten_stateset t1, flatten_stateset t2
		in
		LSSet.compare lss1 lss2
	let leq t1 t2 =
		StateSet.for_all (fun s1 -> StateSet.exists (substate s1) t2) t1
	let extend s t = if is_emptystate s then t else StateSet.add s t
	let subcache cache = cache.asyncsol

	type abstr = UnordUnsyncTrace.t
	let abstr = flatten_stateset
	let match_abstr lss_l states =
		let astates = abstr states
		in
		List.exists (fun lss -> UnordUnsyncTrace.leq lss astates) lss_l
end

exception Nothing

module MinimalUnorderedAbstractTraces (Uat: UnorderedAbstractTraceType) = struct

	type t = Uat.t list

	let extend_trace an goal (conds_list, interm) tr =
		let conds = Hashtbl.find_all an.conditions tr
		in
		let push_conds prod conds =
			List.map (fun conds' -> Uat.extend conds conds') conds_list @ prod
		in
		let conds_list = List.fold_left push_conds [] conds
		in
		conds_list,
		let j = tr_dest tr
		in
		(if j <> goal then ISet.add j interm else interm)

	let solutions an obj =
		let goal = tr_dest obj
		in
		let push_tr = extend_trace an goal
		in
		let sols = []
		and sol0 = [Uat.empty], ISet.empty
		and append sols (conds_list,interm) tr =
			let conds_list, interm = push_tr (conds_list,interm) tr
			in
			let conds_list = List.sort Uat.quick_compare conds_list
			in
			let fold_conds sol conds =
				if List.exists (fun conds' -> Uat.leq conds' conds) sol then
					(* conds already in sol *)
					sol
				else
				if List.exists (fun (conds',_) -> Uat.leq conds' conds) sols then
					(* conds already registered *)
					sol
				else
					conds::sol
			in
			let conds_list = List.fold_left fold_conds [] conds_list
			in
			conds_list, interm
		and register sols (conds_list,interm) =
			List.map (fun conds -> (conds,interm)) conds_list
			@
			List.filter (fun (conds',_) ->
					List.for_all
						(fun conds -> not(Uat.leq conds conds'))
						conds_list) sols
		and discard _ (conds_list,_) = [] = conds_list
		in
		enumerate_acyclic_paths register append discard sol0 sols an obj

	let solutions cache =
		_cache_computation solutions (Uat.subcache cache)

	let filtered_solutions cache abstrdom an obj =
		try
			let abstrfilter =
				try
					match ObjMap.find obj abstrdom with
					  [] -> raise Nothing
					| asols -> Some asols
				with Not_found -> None
			in
			let sols = solutions cache an obj
			in
			match abstrfilter with
			  None -> sols
			| Some asols -> List.filter (fun (sol,_) ->
								Uat.match_abstr asols sol) sols
		with Nothing -> []

end

module MinUnordUnsyncSol = MinimalUnorderedAbstractTraces(UnordUnsyncTrace)

module MinUnordSol = MinimalUnorderedAbstractTraces(UnordTrace)


(*

let get_matching_BS ph cache obj ps interm =
	let paths = get_BS ph cache obj
	in
	let g = obj_bounce obj
	and b = obj_sort obj
	in
	let causes =
		let register_action (ps,interm) = function Hit ((a,i),_,k) ->
			(if a <> b then PSet.add (a,i) ps else ps),
			(if g <> k then ISet.add k interm else interm)
		in
		List.fold_left register_action (PSet.empty, ISet.empty)
	in
	List.filter (fun path -> 
					let p_ps, p_interm = causes path
					in
					PSet.equal p_ps ps && ISet.equal p_interm interm)
						paths
;;


let lasthitters cache ph ?filter:(filter = fun _ -> true) obj =
	let fold_actions ps = function [] -> PSet.empty
		| actions -> 
			PSet.add (hitter (List.nth actions (List.length actions - 1))) ps
	in
	let bs = List.filter filter (get_BS ph cache obj)
	in
	List.fold_left fold_actions PSet.empty bs
;;



*)
