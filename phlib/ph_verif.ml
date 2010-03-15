
open Big_int;;

open Ph_types;;
open Ph_op;;
open Ph_util;;
open Ph_sortgraph;;

module EMap = Map.Make (struct type t = process * string
	let compare = compare end);;

let stable_states (ps,hits) =
	(*DEBUG*) print_endline ". hitless graph";
	let v, e = hitless_graph (ps,hits)
	and sigma = fst (List.split ps)
	in
	(*DEBUG*) print_endline " OK";
	(* Fill E *)
	let register_couple ((a,i),(b,j)) _E =
		let register_couple _E ((a,i),(b,j)) =
			let key = ((a,i),b)
			in
			let prev = try EMap.find key _E with Not_found -> []
			in
			EMap.add key ((b,j)::prev) _E
		in
		let _E = register_couple _E ((a,i),(b,j))
		in
		register_couple _E ((b,j),(a,i))
	in
	(*DEBUG*) print_endline ". fill E";
	let _E = PCSet.fold register_couple e (EMap.empty);
	in
	(*DEBUG*) print_endline " OK";
	(* Prune E *)
	let _E_remove _E ai =
		(* remove E_ai *)
		let folder _E b =
			EMap.remove (ai,b) _E
		in
		let _E = List.fold_left folder _E sigma
		in
		(* remove (a,i) from all others *)
		EMap.map (fun eb -> Util.list_remove ai eb) _E
	in
	let rec prune v _E =
		(*DEBUG*) print_endline ". prune";
		let has_empty (a,i) =
			let check b =
				if b <> a then
					let v = EMap.find ((a,i),b) _E
					in
					if v = [] then raise Not_found
			in
			try List.iter check sigma; false
			with Not_found -> true
		in
		let to_rm, v = List.partition has_empty v
		in 
		match to_rm with [] -> v,_E | _ -> (
			let _E = List.fold_left _E_remove _E to_rm
			in
			(*DEBUG*) print_endline " OK (recur)";
			prune v _E
		)
	in
	let v,_E = prune v _E
	in
	(*DEBUG*) print_endline " DONE";

	(* Choose the smallest E_a *)
	let count ((a,i),b) eb cE =
		let prev = try SMap.find a cE with Not_found -> 0
		in
		SMap.add a (prev + List.length eb) cE
	in
	let cE = EMap.fold count _E SMap.empty
	in
	let smaller a' c' (a,c) =
		if c' < c || c < 0 then (a',c') else (a,c)
	in
	let a = fst (SMap.fold smaller cE ("",-1))
	in
	(*DEBUG*) print_endline (". using "^a); 

	(* Cross product E_a and test for cliques *)
	let folder stable_states ai =
		let get_Eaib b = if b = a then [ai]
			else EMap.find (ai,b) _E
		in
		let to_test = List.rev (List.map get_Eaib sigma)
		in
		(*DEBUG*) print_endline (". testing "^string_of_big_int 
				(List.fold_left (fun c l -> mult_int_big_int (List.length l) c)
					unit_big_int to_test)^" states");

		let test_surclique clique bj =
			List.for_all (fun ai -> PCSet.mem (ai,bj) e) clique
		in
		let rec build_cliques clique = function
			  [] -> [clique]
			| bjs::parts ->
				let mapper bj =
					if test_surclique clique bj then
						build_cliques (bj::clique) parts
					else []
				in
				List.flatten (List.map mapper bjs)
		in
		stable_states @ build_cliques [] to_test
	in
	let ais = List.filter (fun (b,j) -> a = b) v
	in
	List.fold_left folder [] ais
;;


(****************************)
(*      REACHABILITY        *)
(****************************)

(*
	Computes keyactions predicates (offline operation).
	Returns a (hit_t list option) PMap preds:
		aj-keyactions(ai) = List.nth (PMap.find ai preds) j
		Some([]) = False
		None = True
*)
let keyactions_predicates (ps,hits) zl =
	let rec build_keyactions (a,i) preds =
		let build_aj_keyactions j =
			if i = j then None
			else (
				let actions = Hashtbl.find_all hits (a,j)
				in
				let actions = List.filter (function ((hitter,_),_) ->
								hitter <> zl) actions
				in
				Some (List.map (function ((hitter,_),j') -> 
						Hit (hitter,(a,j),j')) actions)
			)
		in
		let range = Util.range 0 (List.assoc a ps)
		in
		let switch = List.map build_aj_keyactions range
		in

		(* Propagate false predicates *)
		let get_bots switch bots =
			List.filter (fun i -> List.nth switch i = Some [] && 
					not (List.mem i bots)) range
		in
		let rec propagate_bots switch bots = 
			match get_bots switch bots with
			  [] -> switch
			| bots' -> 
				let filter_bot = function None -> None
					| Some actions ->
						let actions' = List.filter (function Hit (ai,bj,j') ->
								not(List.mem j' bots')) actions
						in
						Some actions'
				in
				let switch = List.map filter_bot switch
				in
				propagate_bots switch (bots@bots')
		in
		let switch = propagate_bots switch []
		in

		let preds = PMap.add (a,i) switch preds
		in
		(* Compute new predicates *)
		let resolve_aj_keyactions preds = function
			Hit (hitter,_,_) ->
				if not(PMap.mem hitter preds) then 
					build_keyactions hitter preds
				else
					preds
		in
		let resolve_keyactions preds = function
			  None -> preds
			| Some actions -> 
				List.fold_left resolve_aj_keyactions preds actions
		in
		List.fold_left resolve_keyactions preds switch
	in
	build_keyactions zl (PMap.empty)
;;

(*
	Test if a set of action is schedulable in a given state.
	Returns a ternary (True/False/Inconc).
*)
let actions_schedulability_in_state state actions =
	(*DEBUG*)
		print_endline ("Testing solution "^string_of_actions actions);
	(**)

	(* build Sort-Graphs *)
	let sortgraphs = sortgraphs_of_actions actions
	in

	(* prepare the check for action replay relevance (default answer) 
		simple algo: max sortgraph_reeulerisable 
	*)
	let is_action_replay_relevant () =
		let folder a sortgraph = function 
			  True -> True
			| Inconc -> Inconc (* Inconc is the max for now *)
			| False -> sortgraph_reeulerisable sortgraph
		in
		match SMap.fold folder sortgraphs False with
		  True | Inconc -> true
		| False -> false
	in
	let default_answer () =
		if is_action_replay_relevant () then
			Inconc
		else
			False
	in

	(* check for Eulerian paths *)
	try
		(* every Sort-Graph has to be Eulerian + the initial node of the
			Eulerian path have to match with the state
		*)
		let test_sortgraph a sortgraph =
			let first = fst (sortgraph_eulerian_extrem sortgraph)
			in
			match first with
			  None -> ()
			| Some (a,i) -> if SMap.find a state <> i then raise Non_eulerian
		in
		SMap.iter test_sortgraph sortgraphs;

		(*DEBUG*)
			print_endline "All Sort-Graphs are Eulerian.";
		(**)

		(* check for schedulability *)
		if sortgraphs_schedulable sortgraphs then
			True
		else 
			(default_answer ())

	with Non_eulerian -> (
		print_endline "There exists a non-Eulerian Sort-Graph";
		default_answer ()
	)
;;

(* 
	Check for process reachability from state.
	Returns ternary (True/False/Inconc).
*)

type pred_t = process * sortidx;;

let string_of_pred (ai, j) =
	"P("^string_of_process ai^","^string_of_int j^")"
;;

module KeyActions = Map.Make (struct type t = pred_t let compare = compare end);;

module PredSet = Set.Make(struct type t = pred_t let compare = compare end);;
let string_of_predset predset =
	let content = String.concat ", " 
		(List.map string_of_pred (PredSet.elements predset))
	in
	"{ " ^ content ^" }"
;;

let string_of_predgraph predgraph =
	let string_of_child (action, pred1, pred2) =
		"("^string_of_action action ^","^string_of_pred pred1^","^string_of_pred pred2^")"
	in
	let string_of_value = function
		  None -> "T"
		| Some [] -> "F"
		| Some childs -> 
			String.concat "; " (List.map string_of_child childs)
	in
	let folder pred value str =
		str^"  ["^string_of_pred pred^"] = "^string_of_value value^"\n"
	in
	KeyActions.fold folder predgraph ""
;;
let string_of_revgraph revgraph =
	let folder pred parents str =
		str^"  ["^string_of_pred pred^"] is a child of "^string_of_predset parents^"\n"
	in
	KeyActions.fold folder revgraph ""
;;

module Tree = Set.Make (struct type t = pred_t * hit_t let compare = compare end);;
module FlatSat = Set.Make (struct type t = Tree.t let compare = compare end);;

let process_reachability keyactions zl state = 
	let root_pred = (zl, SMap.find (fst zl) state)
	in
	print_endline ("root_pred is "^string_of_pred root_pred);

	(* pre1.1. Compute predicates hyper-graph with reverse dependencies *)

	let merge_rev pred =
		let merge_rev revgraph predchild =
			let current = try KeyActions.find predchild revgraph
						with Not_found -> PredSet.empty
			in
			KeyActions.add predchild (PredSet.add pred current) revgraph
		in
		let folder revgraph (_,pred1,pred2) =
			merge_rev (merge_rev revgraph pred2) pred1
		in
		List.fold_left folder
	in

	let rec register (predgraph, revgraph, coloured) (ai,j) = 
		let pred = (ai,j)
		in
		(* register aj-keyactions(ai) *)
		if not (KeyActions.mem pred predgraph) then
			match List.nth (PMap.find ai keyactions) j with
			  None -> KeyActions.add pred None predgraph, revgraph, PredSet.add pred coloured
			| Some actions -> 
				let preds_of_action action = match action with
					Hit (bk,_,j') ->
						let pred1 = (bk,SMap.find (fst bk) state)
						and pred2 = (ai,j')
						in
						action,pred1,pred2
				in
				let childs = List.map preds_of_action actions
				in
				let predgraph = KeyActions.add pred (Some childs) predgraph
				and revgraph = merge_rev pred revgraph childs
				in
				let resolver args (_,pred1,pred2) =
					register (register args pred1) pred2
				in
				List.fold_left resolver (predgraph, revgraph, coloured) childs
		else (predgraph, revgraph, coloured)
	in
	let predgraph, revgraph, coloured = register 
			(KeyActions.empty, KeyActions.add root_pred PredSet.empty KeyActions.empty, PredSet.empty) root_pred
	in

	(*DEBUG* (
		print_endline "=== PREDICATE HYPERGRAPH BEFORE COLOURATION PRUNING ===";
		print_endline (string_of_predgraph predgraph);
		print_endline (string_of_revgraph revgraph);
	); **)
	print_endline "computing colouration...";

	(* pre1.2. Predicates coloration *)
	let child_coloured coloured (_, pred1, pred2) =
		PredSet.mem pred1 coloured && PredSet.mem pred2 coloured
	in
	let colour_parent parent (coloured, newcoloured) =
		if not (PredSet.mem parent coloured) then 
			match KeyActions.find parent predgraph with
			  Some childs -> 
					if List.exists (child_coloured coloured) childs then
						PredSet.add parent coloured, PredSet.add parent newcoloured
					else
						coloured, newcoloured
			| _ -> invalid_arg "colour_parent"
		else
			coloured, newcoloured
	in
	let colour_parents pred (coloured, newcoloured) =
		let parents = KeyActions.find pred revgraph
		in
		PredSet.fold colour_parent parents (coloured, newcoloured)
	in
	let rec colour_newcoloured (coloured, newcoloured) =
		let coloured, newcoloured = PredSet.fold colour_parents newcoloured (coloured, PredSet.empty)
		in
		if not(PredSet.is_empty newcoloured) then
			colour_newcoloured (coloured,newcoloured)
		else
			coloured
	in
	let coloured = colour_newcoloured (coloured, coloured)
	in

	(*DEBUG*) (
		print_endline ("=== COLOURATION IS " ^ string_of_predset coloured)
	); (**)


	(* 1. test if root predicate is coloured *)
	if not(PredSet.mem root_pred coloured) then (
		(*DEBUG*) print_endline "root predicate is not coloured => FALSE"; (**)
		False
	) else (
		(*DEBUG*) print_endline "root predicate is coloured => testing solutions"; (**)

		(* pre2. remove uncoloured predicated *)
		let remove_uncoloured pred assoc (predgraph,revgraph) =
			if PredSet.mem pred coloured then 
				(* remove uncolored childs *)
				let predgraph = match assoc with
					  None -> predgraph
					| Some childs ->
						let assoc = Some (List.filter (child_coloured coloured) childs)
						in
						KeyActions.add pred assoc predgraph
				in
				predgraph, revgraph
			else
				(* remove node *)
				let predgraph = KeyActions.remove pred predgraph
				and revgraph = KeyActions.remove pred revgraph
				in
				predgraph, revgraph
		in
		let predgraph, revgraph = KeyActions.fold remove_uncoloured predgraph (predgraph,revgraph)
		in

		(*DEBUG*) (
			print_endline "=== PREDICATES HYPERGRAPH ===";
			print_endline (string_of_predgraph predgraph);
			print_endline (string_of_revgraph revgraph);
		); (**)

		(* 2. test solutions *)

		let cross_sat sat2 sat1 =
			let folder tree2 crossed =
				let folder1 tree1 crossed =
					let tree12 = Tree.union tree1 tree2
					in
					FlatSat.add tree12 crossed
				in
				FlatSat.fold folder1 sat1 crossed
			in
			FlatSat.fold folder sat2 FlatSat.empty
		and prepend_sat elt sat =
			let folder tree sat =
				let tree = Tree.add elt tree
				in
				FlatSat.add tree sat
			in
			FlatSat.fold folder sat FlatSat.empty
		(*
		let rec cross_sat sat2 = function
			  [] -> []
			| h::t -> List.map (List.append h) sat2 @ cross_sat sat2 t
		*)
		in
		let rec sat_flat pred (computing, computed) =
			(*
			if KeyActions.mem pred computed then [[]], (computing, computed)
			else
			*)
			try KeyActions.find pred computed, (computing, computed) 
			with Not_found -> (
			match KeyActions.find pred predgraph with
			  None -> FlatSat.singleton Tree.empty, (computing, computed)
			| Some [] -> FlatSat.empty, (computing, computed)
			| Some childs ->
				let computing = 
					if PredSet.mem pred computing then
						raise Not_found
					else
						PredSet.add pred computing
				in
				let folder (sat, stack) (action,pred1,pred2) =
					try 
						let sat2, stack = sat_flat pred2 stack
						in
						let sat1, stack = sat_flat pred1 stack
						in
						print_endline ("  sat_flat "^string_of_pred pred);
						print_endline ("    "^string_of_action action^" "^string_of_pred pred1^" x "^string_of_pred pred2);
						print_endline ("    cross_sat "^string_of_int (FlatSat.cardinal sat1) ^" x "^string_of_int (FlatSat.cardinal sat2)^"...");
						let sat12 = cross_sat sat2 sat1
						in
						print_endline "    done.";
						(*let sat_child = List.map (fun tree -> (pred,action)::tree) sat12*)
						let sat_child = prepend_sat (pred,action) sat12
						in
						FlatSat.union sat_child sat, stack
					with Not_found -> sat, stack
				in
				let sat, (computing, computed) = List.fold_left folder (FlatSat.empty,(computing,computed)) childs
				in
				let computing = PredSet.remove pred computing
				and computed = KeyActions.add pred sat computed
				in
				sat, (computing, computed)
			)
		in
		let sat_flat pred = fst (sat_flat pred (PredSet.empty, KeyActions.empty))
		in
		try
			let flat_trees = sat_flat root_pred
			in
			print_endline (string_of_int (FlatSat.cardinal flat_trees)^" flat solutions");
			(*DEBUG* (
				let string_of_tree tree =
					String.concat " - " (
						List.map (fun (pred, action) -> string_of_pred pred^ " "^string_of_action action)
							tree
					)
				in
				print_endline (String.concat "\n---\n" (List.map string_of_tree flat_trees));
			); **)

			let solution_of_tree tree =
				let folder (_,action) actions =
					action::actions
				in
				Tree.fold folder tree []
			and test_solution = actions_schedulability_in_state state
			in
			let walk_solutions tree best_answer =
				if best_answer = True then True
				else 
					let sol = solution_of_tree tree
					in
					match test_solution sol with
					  True -> True
					| False -> best_answer
					| Inconc -> (
						print_endline ("** INCONCLUSIVE SOLUTION "^string_of_actions sol);
						Inconc)
			in
			FlatSat.fold walk_solutions flat_trees False
		with Not_found -> False
	)
;;

