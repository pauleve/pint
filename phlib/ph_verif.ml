
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

		(* check for schedulability *)
		if sortgraphs_schedulable sortgraphs then
			True
		else 
			(default_answer ())

	with Non_eulerian -> default_answer ()
;;

(* 
	Check for process reachability from state.
	Returns ternary (True/False/Inconc).
*)

type pred_t = process * sortidx;;

module KeyActions = Map.Make (struct type t = pred_t let compare = compare end);;
module PredSet = Set.Make(struct type t = pred_t let compare = compare end);;

let process_reachability keyactions zl state = 
	let root_pred = (zl, SMap.find (fst zl) state)
	in

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
			(KeyActions.empty, KeyActions.empty, PredSet.empty) root_pred
	in

	(* pre1.2. Predicates coloration *)
	let child_coloured (_, pred1, pred2) =
		PredSet.mem pred1 coloured && PredSet.mem pred2 coloured
	in
	let colour_parent parent (coloured, newcoloured) =
		if not (PredSet.mem parent coloured) then 
			match KeyActions.find parent predgraph with
			  Some childs -> 
					if List.exists child_coloured childs then
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
						let assoc = Some (List.filter child_coloured childs)
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
		ignore(predgraph);
		ignore(revgraph);
				

		(* 2. test solutions *)
		(*
		let test_solution = actions_schedulability_in_state state
		in
		let walk_solutions inconc_answers
			(* generate a new solution *)
			try
				let sol = []

				in
				match test_solution (uniqise_actions sol) with
				  True -> True
				| False -> walk_solutions inconc_answers
				| Inconc -> walk_solutions (sol::inconc_answers)

			with Not_found -> 
				if inconc_answers = [] then False else Inconc
		in
		walk_solutions
		*)
		Inconc
	)
;;

(********** TRASH ***********
	let rec register (predgraph, revgraph, falsepreds) (ai,j) = 
		(* register aj-keyactions(ai) *)
		if not (KeyActions.mem (ai,j) predgraph) then
			match List.nth (PMap.find ai keyactions) j with
			  None -> KeyActions.add (ai,j) None predgraph, revgraph, falsepreds
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
				let predgraph = KeyActions.add (ai,j) (Some childs) predgraph
				and falsepreds, revgraph = 
					if childs = [] then
						(PredSet.add (ai,j) falsepreds, revgraph)
					else
						(falsepreds, merge_rev (ai,j) revgraph childs)
				in
				let resolver args (_,pred1,pred2) =
					register (register args pred1) pred2
				in
				List.fold_left resolver (predgraph, revgraph, falsepreds) childs
		else (predgraph, revgraph, falsepreds)
	in
	let predgraph, revgraph, falsepreds = register 
			(KeyActions.empty, KeyActions.empty, PredSet.empty) (zl,SMap.find (fst zl) state)
	in

	(* 2. remove false leafs *)
	let rec remove_falsepreds (predgraph, revgraph) falsepreds =
		let remove_pred pred (predgraph, revgraph, falsepreds) =
			let parents = KeyActions.find pred revgraph
			and predgraph = KeyActions.remove pred predgraph
			and revgraph = KeyActions.remove pred revgraph
			in
			(* update parents, register if false *)
			let update_parent parent (predgraph, falsepreds) =
				match KeyActions.find parent predgraph with
				  Some childs -> 
				  	let childs = List.filter (function (_,pred1,pred2) -> pred1 <> pred && pred2 <> pred) childs
					in
					KeyActions.add parent (Some childs) predgraph,
						if childs = [] then PredSet.add parent falsepreds else falsepreds
				| _ -> invalid_arg "update_parent"
			in
			let predgraph, falsepreds = PredSet.fold update_parent parents (predgraph, falsepreds)
			in
			predgraph, revgraph, falsepreds
		in
		let predgraph, revgraph, falsepreds = PredSet.fold remove_pred falsepreds
												(predgraph, revgraph, PredSet.empty)
		in
		if not(PredSet.is_empty falsepreds) then
			remove_falsepreds (predgraph, revgraph) falsepreds
		else
			predgraph, revgraph
	in
	let predgraph, revgraph = remove_falsepreds (predgraph, revgraph) falsepreds
	in
	ignore(predgraph);


***************************)


(********************)
(**** DEPRECATED ****)
(********************)

type pharmful_t = PTrue | PClauses of (process * sortidx) list;;
let string_of_pclause (hitter,bounce_idx) =
	"harmful("^string_of_process hitter^") & p_harmful("^string_of_int bounce_idx^")"
;;
let string_of_pharmful = function
	  PTrue -> "True"
	| PClauses c -> String.concat " | " (List.map string_of_pclause c)
;;

module HarmfulLiteral = 
struct 
	type t = Harmful of process
	let to_string = function Harmful ai -> "harmful("^string_of_process ai^")"
end;;

module HarmfulNF = Bool.NormalForm (HarmfulLiteral);;

let string_of_harmful (a,nfs) =
	String.concat " ; " (List.map (fun j -> 
		(string_of_process (a,j)) ^ ": " ^ (HarmfulNF.to_string (List.nth nfs j))) (Util.range 0 (List.length nfs - 1)))
;;

let harmful (ps,hits) zl =
	let computing = Hashtbl.create (List.length ps)
	and register = Hashtbl.create (List.length ps)
	in
	let rec harmful arg = 
		if not(Hashtbl.mem register arg) && not(Hashtbl.mem computing arg) then (
			Hashtbl.add computing arg true;
			let value = _harmful arg
			in
			Hashtbl.add register arg value;
			(* DEBUG *) print_endline ("# harmful("^
							string_of_process arg^") = "^string_of_harmful value);(**)
			Hashtbl.remove computing arg
		)
	and _harmful (a,i) =
		let la = Util.range 0 (List.assoc a ps)
		in
		(* I. compute p-harmless *)
		(* I.a symbolic *)
		let p_harmful j =
			if i = j then PTrue
			else (
				let actions = Hashtbl.find_all hits (a,j)
				in
				let clause_of_action ((hitter,_),bounce_idx) = (hitter, bounce_idx)
				in
				PClauses (List.map clause_of_action actions)
			)
		in
		let p_harmfuls = List.map p_harmful la
		in
		(*DEBUG* List.iter (fun j -> print_endline ("("^string_of_process (a,i)
					^") p_harmful("^string_of_int j^") = "
					^string_of_pharmful (List.nth p_harmfuls j))) la;**)
		(* I.b resolve *)
		let resolve j =
			let p_register = Hashtbl.create (List.assoc a ps)
			and p_computing = Hashtbl.create (List.assoc a ps)
			in
			let rec resolve_next j' =
				if Hashtbl.mem p_computing j' then (
					HarmfulNF.val_false
				) else (
					if not(Hashtbl.mem p_register j') then (
						Hashtbl.add p_computing j' true;
						let next = _resolve_next j'
						in
						Hashtbl.add p_register j' next;
						Hashtbl.remove p_computing j';
						next
					) else (
						Hashtbl.find p_register j'
					)
				)

			and _resolve_next j' =
				match List.nth p_harmfuls j' with
				  PTrue -> HarmfulNF.val_true
				| PClauses c ->
					let folder nf (hitter,j'') =
						let nf_j'' = resolve_next j''
						in
						let nf' = HarmfulNF.cross_literal (HarmfulLiteral.Harmful hitter) nf_j''
						in
						HarmfulNF.union nf nf'
					in
					List.fold_left folder HarmfulNF.val_false c
			in
			resolve_next j
		in
		let p_harmfuls' = List.map resolve la
		in
		(*DEBUG* List.iter (fun j -> print_endline ("("^string_of_process (a,i)
					^") p_harmful'("^string_of_int j^") = "
					^(HarmfulNF.to_string (List.nth p_harmfuls' j)))) la;**)
		(* II. compute harmless *) 
		let resolve_harmful = function HarmfulLiteral.Harmful hitter -> harmful hitter
		in
		List.iter (fun nf -> HarmfulNF.iter (fun clause -> HarmfulNF.Clause.iter resolve_harmful clause) nf)
			p_harmfuls';
		(a,p_harmfuls')
	in
	(harmful zl);
	(zl,register)
;;

let solve_harmful (arg,harmfuls) state_value = (* returns true if state is harmful for arg *)
	let register = Hashtbl.create 10
	and solving = Hashtbl.create 10
	in
	let rec solve_harmful arg =
		try false, Hashtbl.find register arg with Not_found -> (
			if Hashtbl.mem solving arg then (
				(*DEBUG* print_endline ("#! assume solve("^string_of_process arg^")=False"); **)
				true, false
			) else (
				Hashtbl.add solving arg true;
				let has_loop, value = satisfy_switch (Hashtbl.find harmfuls arg)
				in
				Hashtbl.remove solving arg;
				if not has_loop then Hashtbl.add register arg value;
				has_loop, value
			)
		)
	and satisfy_lit has_loop = function
		HarmfulLiteral.Harmful ai -> 
			let has_loop', ret = solve_harmful ai
			in
			has_loop := has_loop';
			ret
	and satisfy_dnf = function
		  HarmfulNF.True -> false, true
		| HarmfulNF.Clauses [] -> false, false
		| HarmfulNF.Clauses clauses ->
			let has_loop = ref false 
			in
			let ret = List.exists (HarmfulNF.Clause.for_all (satisfy_lit has_loop)) clauses
			in
			!has_loop, ret
	and satisfy_switch (a, nfs) =
		satisfy_dnf (List.nth nfs (state_value a))
	in
	snd (solve_harmful arg)
;;

