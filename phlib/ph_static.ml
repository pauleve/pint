
open Big_int;;

open Debug;;

open Ph_types;;
open Ph_op;;
open Ph_util;;
open Ph_sortgraph;;

module EMap = Map.Make (struct type t = process * string
	let compare = compare end);;

let stable_states (ps,hits) =
	(*DEBUG*) dbg ". hitless graph";
	let v, e = hitless_graph (ps,hits)
	and sigma = fst (List.split ps)
	in
	(*DEBUG*) dbg " OK";
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
	(*DEBUG*) dbg ". fill E";
	let _E = PCSet.fold register_couple e (EMap.empty);
	in
	(*DEBUG*) dbg " OK";
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
		(*DEBUG*) dbg ". prune";
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
			(*DEBUG*) dbg " OK (recur)";
			prune v _E
		)
	in
	let v,_E = prune v _E
	in
	(*DEBUG*) dbg " DONE";

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
	(*DEBUG*) dbg (". using "^a); 

	(* Cross product E_a and test for cliques *)
	let folder stable_states ai =
		let get_Eaib b = if b = a then [ai]
			else EMap.find (ai,b) _E
		in
		let to_test = List.rev (List.map get_Eaib sigma)
		in
		(*DEBUG*) dbg (". testing "^string_of_big_int 
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


type proceq_t = sort * ISet.t;;
let string_of_proceq (a,is) = a^"_"^string_of_iset is
;;

module ProcEqSet = Set.Make (struct type t = proceq_t let compare = compare end);;
let string_of_proceqset = string_of_set string_of_proceq ProcEqSet.elements
;;
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
	try EqMap.find (b,a,i) equivalences
	with Not_found -> ISet.singleton i
;;

