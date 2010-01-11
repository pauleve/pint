
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

type pharmful_t = PTrue | PClauses of (process * sortidx) list;;
let string_of_pclause (hitter,bounce_idx) =
	"harmful("^string_of_process hitter^") & p_harmful("^string_of_int bounce_idx^")"
;;
let string_of_pharmful = function
	  PTrue -> "True"
	| PClauses c -> String.concat " | " (List.map string_of_pclause c)
;;

(*
module HarmfulLiteral = 
struct 
	type t = Harmful of process
	let to_string = function Harmful ai -> "harmful("^string_of_process ai^")"
end;;
*)
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


type sortdomain = sort * sortidx list
type bsm_t =
	  Proc of process 
	| Harmless of sortdomain

let string_of_sortdomain (z,lz') =
		z^"_{"^(String.concat "," (List.map string_of_int lz'))^"}"
;;

module BHarmless = Bool.Manipulator (struct
	type t = bsm_t
	let to_string = function 
		  Proc x -> string_of_process x
		| Harmless x -> "harmless("^string_of_sortdomain x^")"
	let tautology x1 x2 = match x1, x2 with
		  (_,Proc _),(_,Harmless _) | (_,Harmless _),(_,Proc _) -> false
		| (h,Proc (a,i)),(h',Proc (a',i')) ->
			a = a' && (h==h' && i!=i' || h!=h' && i==i)
		| (h1,Harmless x1), (h2,Harmless x2) -> false
end);;

let harmless (ps,hits) (z,lz') = 
	let register = Hashtbl.create (List.length ps)
	and computing = Hashtbl.create (List.length ps)
	in
	let rec harmless arg = (* caching harmless results *)
		if not(Hashtbl.mem register arg) && not(Hashtbl.mem computing arg) then (
			Hashtbl.add computing arg true;
			(* DEBUG * print_endline ("# computing harmless("^
							string_of_sortdomain arg^")");**)
			let value = _harmless arg
			in
			Hashtbl.add register arg value;
			(* DEBUG *) print_endline ("# harmless("^
							string_of_sortdomain arg^") = "^
							BHarmless.string_of_dnf value);(**)
			Hashtbl.remove computing arg
		)

	and _harmless (z,lz') =
		(********************)
		(* I. local key actions for reaching lz' *)

		(* index actions on z by their bounce *)
		(* zhits : (bounce idx, (hitter, target idx)) Hashtbl *)
		let zhits = Hashtbl.create (List.assoc z ps + 1)
		in
		let reindex (b,j) ((ai,p),j') =
			if b = z then Hashtbl.add zhits j' (ai,j)
		in
		Hashtbl.iter reindex hits;

		(* compute local key actions *)
		(* keyactions: (hitter, target idx, bounce idx) list list *)
		let rec build_keyactions omap lz' cur_order =
			(* set lz' order to cur_order *)
			let omap = List.fold_left (fun omap i -> set_order omap (z,i) cur_order)
						omap lz'
			in
			(* get actions making z bounce to a process in lz' *)
			let actions = List.flatten (List.map (fun j' -> 
					List.map (fun (ai,j) -> (ai,j,j'))
						(Hashtbl.find_all zhits j')
				) lz')
			in 
			(* keep actions having target with bottom order (key actions) *)
			let actions = List.filter (function (_,j,_) -> order omap (z,j) = Bot) actions
			in
			(* extract concerned targets *)
			let lz' = Util.list_uniq (List.map (function (_,j,_) -> j) actions)
			in
			match actions with [] -> omap, []
				| _ -> let omap, keyactions = build_keyactions omap lz' (cur_order+1)
					in omap, actions::keyactions
		in
		let omap, keyactions = build_keyactions omap_empty lz' 0
		in
		
		(* DEBUG *
		print_endline "- keyactions";
		let string_of_action (ai,j,j') = string_of_process ai^"->"^
				string_of_process (z,j)^" "^string_of_int j'
		in
		let rec debug_keyactions n = function [] -> ()
			| actions::keyactions -> (
				print_endline ("K^"^string_of_int n^" : ["^
					(String.concat "; " (List.map string_of_action actions))^"]");
				debug_keyactions (n+1) keyactions )
		in
		debug_keyactions 0 keyactions;
		* END DEBUG *)

		(********************)
		(* II. compute p-harmless for each z_i \in L_z *)

		(* p_harmless : (idx, (sort,idx list) Bool.expr) Hashtbl *)
		let p_harmless = Hashtbl.create (List.assoc z ps+1)
		in

		(* sort z_is along their order *)
		let lz = List.sort (fun i i' -> compare (order omap (z,i)) (order omap (z,i')))
					(Util.range 0 (List.assoc z ps))
		in
		let compute_p_harmless i = 
			let expr = match order omap (z,i) with
				  Bot -> Bool.T
				| Order 0 -> Bool.F
				| Order n ->  
					(* group hitters by sort*bounce *)
					let group sg ((a,k),j,j') = if i <> j then sg else
						let la' = try PMap.find (a,j') sg with Not_found -> []
						in
						PMap.add (a,j') (k::la') sg
					in
					let sg = List.fold_left group PMap.empty (List.nth keyactions (n-1))
					in
					let folder (a,j') la' expr =
						let expr' = BHarmless.expr_of_dnf (Hashtbl.find p_harmless j')
						in
						harmless (a,la'); (* compute it *)
						Bool.And (expr, Bool.Or (Bool.L (Harmless (a,la')), expr'))
					in
					PMap.fold folder sg Bool.T
			in
			(* DEBUG * print_endline "## computing dnfs..."; **)
			let dnf = BHarmless.dnf expr
			in
			(* DEBUG * print_endline "## computing dnfs...OK"; **)
			Hashtbl.add p_harmless i dnf
		in
		List.iter compute_p_harmless lz;
		(* DEBUG *
		let iterator i =
			let dnf = Hashtbl.find p_harmless i
			in
			print_endline ("p-harmless("^z^string_of_int i^") = " ^ BHarmless.string_of_dnf dnf)
		in
		List.iter iterator lz;
		* END DEBUG *)

		(********************)
		(* III. compute harmless *)
		
		let harmless_i dnf i =
			(* TODO: optimize *)
			BHarmless.dnf_disj dnf (BHarmless.dnf_conj (Hashtbl.find p_harmless i)
									(Some [BHarmless.LSet.singleton (true,Proc (z,i))]))
		in
		List.fold_left harmless_i None lz
	in

	harmless (z,lz');
	((z,lz'),register)
;;

let solve_harmless (arg,harmless) state_contains = (* returns true if state is harmless for arg *)
	let register = Hashtbl.create 10
	and solving = Hashtbl.create 10
	in
	let rec solve_harmless arg =
		try false, Hashtbl.find register arg with Not_found -> (
			if Hashtbl.mem solving arg then (
				(* TODO check that!!! *)
				(*DEBUG* print_endline ("#! assume solve("^string_of_sortdomain arg^")=True"); **)
				true, true
			) else (
				Hashtbl.add solving arg true;
				let has_loop, value = satisfy_dnf (Hashtbl.find harmless arg)
				in
				Hashtbl.remove solving arg;
				if not has_loop then Hashtbl.add register arg value;
				has_loop, value
			)
		)
	and satisfy_lit has_loop = function
		  (pos, Proc ai) -> pos == state_contains ai
		| (pos, Harmless arg) -> 
			let has_loop', ret = solve_harmless arg
			in
			has_loop := has_loop';
			pos == ret
	and satisfy_dnf = function 
		  None -> false, false
		| Some [] -> false, true
		| Some lsets ->	 
			let has_loop = ref false 
			in
			let ret = List.exists (BHarmless.LSet.for_all (satisfy_lit has_loop)) lsets
			in
			!has_loop, ret
	in
	snd (solve_harmless arg)
;;

(* DEAD CODE: resolve harmless *
		let p_harmless = Hashtbl.create (List.assoc z ps+1)
		in
		let iterator key dnf =
			let dnf = match dnf with None -> None | Some [] -> Some [] | Some lsets -> (
				let resolve lset = 
					let folder (harm,arg) dnf =
						let dnf' = match harm with
							  true -> failwith "harmful not implemented!"
							| false ->
								if Hashtbl.mem computing arg then (
									(* already computing, prevent infinite recursion!!
										assume True ???? XXX TODO *)
									(*DEBUG*) print_endline ("#! assume harmless("^string_of_sortdomain arg^")=True"); (**)
									Some []
								) else harmless arg
						in
						BS.dnf_conj dnf dnf'
					in
					BM.LSet.fold folder lset (Some [])
				in
				let folder dnf lset =
					BS.dnf_disj dnf (resolve lset)
				in
				List.fold_left folder None lsets
			)
			in
			Hashtbl.add p_harmless key dnf
		in
		Hashtbl.iter iterator p_harmless_dnf;
*)
