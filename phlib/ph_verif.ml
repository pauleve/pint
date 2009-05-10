
open Ph_types;;

let hitless_graph (ps, hits) =
	let has_hit ai bj =
		let hs = Hashtbl.find_all hits bj
		in
		let pred ((ai',p),j') = ai' = ai
		in
		List.exists pred hs
	in
	let folder v (a,la) = 
		let folder v i =
			let ai = (a,i)
			in
			if has_hit ai ai then v else (ai::v)
		in
		List.fold_left folder v (Util.range 0 la)
	in
	let v = List.fold_left folder [] ps
	in
	let v2 = Util.cross_list [v;v]
	in
	let folder set c =
		let (ai,bj) = List.nth c 0, List.nth c 1
		in
		if ai <> bj && not (has_hit ai bj || has_hit bj ai) then
			PCSet.add (ai,bj) set
		else
			set
	in
	v, List.fold_left folder PCSet.empty v2
;;

module EMap = Map.Make (struct type t = process * string
	let compare = compare end);;

let stable_states (ps,hits) =
	let v, e = hitless_graph (ps,hits)
	and sigma = fst (List.split ps)
	in
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
	let _E = PCSet.fold register_couple e (EMap.empty);
	in
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
			prune v _E
		)
	in
	let v,_E = prune v _E
	in
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
	(* Cross product E_a and test for clique *)
	let is_clique state = 
		let pairs = Util.cross_list [state;state]
		in
		List.for_all (fun pair -> PCSet.mem (List.nth pair 0,List.nth pair 1) e) pairs
	in
	let folder stable_states ai =
		let get_Eaib b = if b = a then [ai]
			else EMap.find (ai,b) _E
		in
		let to_test = List.map get_Eaib sigma
		in
		stable_states @
		List.filter is_clique (Util.cross_list to_test)
	in
	let ais = List.filter (fun (b,j) -> a = b) v
	in
	List.fold_left folder [] ais
;;



