
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

type proceq_t = sort * ISet.t;;
let string_of_proceq (a,is) = a^"_"^string_of_iset is
;;

module ProcEqSet = Set.Make (struct type t = proceq_t let compare = compare end);;
let string_of_proceqset = string_of_set string_of_proceq ProcEqSet.elements
;;

module TargetBounceSet = Set.Make (struct type t = process * sortidx let compare = compare end);;
module TargetBounceMap = Map.Make (struct type t = process * sortidx let compare = compare end);;

module S2Set = Set.Make (struct type t = sort * sort let compare = compare end);;
module I2Set = Set.Make (struct type t = int * int let compare = compare end);;

module EqMap = Map.Make (struct type t = sort * sort * sortidx let compare = compare end);;

let processes_equivalences (ps, hits) =
	let htbl = Hashtbl.create (List.length ps)
	in
	let folder (b,j) (((a,i),_),k) keys =
		Hashtbl.add htbl (a,b) (i,j,k);
		S2Set.add (a,b) keys
	in
	let keys = Hashtbl.fold folder hits S2Set.empty
	in
	let folder (a,b) equivalences =
		let hits = Hashtbl.find_all htbl (a,b)
		in
		(* group by i *)
		let group (groups,keys) (i,j,k) =
			let g = try IMap.find i groups with Not_found -> I2Set.empty
			in
			IMap.add i (I2Set.add (j,k) g) groups, ISet.add i keys
		in
		let groups, keys = List.fold_left group (IMap.empty, ISet.empty) hits
		in
		(* check for groups equality *)
		let rec check_eq keys equivalences =
			if ISet.cardinal keys < 2 then
				equivalences
			else
				let i = ISet.min_elt keys
				in
				let keys = ISet.remove i keys
				and g = IMap.find i groups
				in
				let folder i' eqs =
					let g' = IMap.find i' groups
					in
					if I2Set.equal g g' then ISet.add i' eqs else eqs
				in
				let eqs = ISet.fold folder keys (ISet.singleton i)
				in
				let equivalences = 
					if ISet.cardinal eqs > 1 then (
						(*DEBUG*) print_endline ("* process equivalence mod "^b^": "^a^"_"^string_of_iset eqs); (**)
						let register i equivalences =
							EqMap.add (b,a,i) eqs equivalences
						in
						ISet.fold register eqs equivalences
					) else
						equivalences
				and keys = ISet.diff keys eqs
				in
				check_eq keys equivalences
		in
		check_eq keys equivalences
	in
	S2Set.fold folder keys EqMap.empty
;;


let get_process_equivalence equivalences b (a,i) =
	try 
		a, EqMap.find (b,a,i) equivalences
	with Not_found ->
		a, ISet.singleton i
;;



type result_t = (ActionSet.t * ProcEqSet.t * sortidx);;
let string_of_result (actions, proceqs, l) =
	"("^string_of_actionset actions^", "^string_of_proceqset proceqs^", "^string_of_int l^")"
;;
module ProcEqSetSet = Set.Make (struct type t = ProcEqSet.t let compare = ProcEqSet.compare end);;

module ResultSet = Set.Make (struct type t = result_t let compare = compare end);;
let string_of_resultset = string_of_set string_of_result ResultSet.elements
;;

module IndexSet = Set.Make (struct type t = proceq_t * sortidx let compare = compare end);;

let process_reachability_prepare (ps,hits) (z,l) state =

	let equivalences = processes_equivalences (ps,hits)
	in
	let get_process_equivalence = get_process_equivalence equivalences
	in

	let hbounces = Hashtbl.create 1
	and pred_index (a, reachset) j = (a,reachset), j
	in

	(* returns the set of action lists to make bounce s_a to a_i, i \in reachset
		(without any cycle *)
	let bounce_paths (a, reachset) j =

		let prepend_action action results =
			(* add action and hitter to every choices *)
			let target_sort = fst (target action)
			in
			let hittereq = get_process_equivalence target_sort (hitter action)
			in
			let folder (actions, proceqs, l) rs =
				let actions = ActionSet.add action actions
				and proceqs = ProcEqSet.add hittereq proceqs
				in
				ResultSet.add (actions, proceqs, l) rs
			in
			ResultSet.fold folder results ResultSet.empty
		in
		let push_results action k_results results =
			let k_results = prepend_action action k_results
			in
			let results = ResultSet.union results k_results
			in
			let keep_result (_, proceqs, _) =
				ResultSet.for_all (function (_, proceqs', _) -> 
					ProcEqSet.equal proceqs proceqs' || not (ProcEqSet.subset proceqs' proceqs))
						results
			in
			ResultSet.filter keep_result results
		in

		let rec walk j visited =
			if ISet.mem j reachset then
				ResultSet.singleton (ActionSet.empty, ProcEqSet.empty, j)
			else
				let visited = ISet.add j visited
				and actions = Hashtbl.find_all hits (a,j)
				in
				let folder results = function (hitter,_),k ->
					if ISet.mem k visited then
						results
					else
						let k_results = walk k visited
						in
						push_results (Hit (hitter,(a,j),k)) k_results results
				in
				List.fold_left folder ResultSet.empty actions
		in
		(*DEBUG*)
			print_endline ("- computing bounce paths from "^
							string_of_process (a,j)^" to "^
							string_of_iset reachset ^ "...");
		(**)
		let rs = walk j ISet.empty
		in
		(*DEBUG*) print_endline ("+ computation result: "^string_of_resultset rs); (**)
		Hashtbl.add hbounces (pred_index (a, reachset) j) rs;
		rs
	in
	let get_bounce_paths ((a, reachset), j) =
		try
			Hashtbl.find hbounces ((a, reachset), j)
		with Not_found ->
			bounce_paths (a, reachset) j
	in
	let hdepend = Hashtbl.create 1
	in
	(* compute dependencies graph *)
	let rec dependencies index =
		if not (Hashtbl.mem hdepend index) then
			let bounce_paths = get_bounce_paths index
			in
			let get_proceqs (_, proceqs, _) childs =
				ProcEqSetSet.add proceqs childs
			in
			let childs = ResultSet.fold get_proceqs bounce_paths ProcEqSetSet.empty
			in
			let iter_child proceqs =
				let fold_proceq proceq indexes =
					let j = SMap.find (fst proceq) state
					in
					IndexSet.add (pred_index proceq j) indexes
				in
				let indexes = ProcEqSet.fold fold_proceq proceqs IndexSet.empty
				in (
					Hashtbl.add hdepend index indexes;
					IndexSet.iter (fun index -> dependencies index) indexes
				)
			in
			ProcEqSetSet.iter iter_child childs
	in
	let root_index = pred_index (z,(ISet.singleton l)) (SMap.find z state)
	in
	dependencies root_index;
	hdepend	
;;


let dot_from_hdepend hdepend =
	let id_from_index ((a,reachset),j) =
		"\"" ^ a^" "^string_of_int j^"->"^string_of_iset reachset^"\""
	in
	let idx = ref 0
	in
	let folder index indexes str = (
		idx := !idx + 1;
		let child_id = "child"^string_of_int (!idx)
		in
		let fold_index index' str =
			str ^
			"  " ^ child_id ^" -> "^id_from_index index'^"\n"
		in
		str ^ 
		"  " ^ child_id^"[label=ALL shape=box]\n" ^
		"  " ^ id_from_index index^ " -> "^child_id^"\n" ^
		(IndexSet.fold fold_index indexes "")
	) in
	Hashtbl.fold folder hdepend "digraph hdepend {\n" ^ "}"
;;


