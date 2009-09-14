
open Big_int;;

open Ph_types;;
open Ph_op;;
open Ph_util;;

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


let key_actions (ps,hits) zk =
	let make_black i (black,white) =
		i::black, Util.list_remove i white
	in
	let update_map map = function Hit((a,i),(b,j),j') ->
		(* make bj black *)
		let map = try
			let bw = SMap.find b map
			in
			SMap.add b (make_black j bw) map
			with Not_found -> map
		in
		(* make ai black *)
		let bw = try SMap.find a map with Not_found ->
						([], Util.range 0 (List.assoc a ps))
		in
		SMap.add a (make_black i bw) map
	in
	let matching map = function Hit(_,(b,j),j') ->
		try
			let black,white = SMap.find b map
			in
			List.mem j white && List.mem j' black
		with Not_found -> false
	in
	let rec build map actions =
		let resp, actions = List.partition (matching map) actions
		in
		match resp with
		  [] -> map, resp
		| _ -> (
			let map = List.fold_left update_map map resp
			in
			let map, resp' = build map actions
			in
			map, resp@resp'
		)
	in
	let map = update_map SMap.empty (Hit (zk,("",-1),-2))
	and actions = ph_actions (ps,hits)
	in
	build map actions
;;

let reach_decisive_process_levels ph zk =
	let _, actions = key_actions ph zk
	in
	let is_target_specific actions ai bj =
		not (List.exists (function Hit ((a',_),bj',_) -> 
				bj=bj' && fst ai <> a') actions)
	in
	let has_specific_targets actions ai =
		let hits = List.filter (function Hit(ai',_,_) -> ai'=ai) actions
		in
		let targets = List.map (function Hit(_,bj,_) -> bj) hits
		in
		List.exists (is_target_specific actions ai) targets
	in
	let rec build actions hitters =
		let hitters' = List.filter (has_specific_targets actions) hitters
		in
		if List.length hitters' = List.length hitters then
			actions, hitters
		else (
			let hitters = hitters'
			in
			let procs = Util.list_uniq (fst (List.split hitters))
			in
			let actions = List.filter (function Hit ((a,_),(b,_),_) -> 
				List.mem a procs && (List.mem b procs || b = fst zk))
				actions
			in
			build actions hitters
		)
	and hitters = Util.list_uniq (List.map (function Hit (ai,_,_) -> ai)
					actions)
	in
	build actions hitters
;;

