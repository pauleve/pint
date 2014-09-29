(*** Inférer les paramètres de Thomas à partir d'un Process Hitting
      et d'un Graphe des Interactions ***)

open Ph2thomas_asp;;

let input_clauses clauses entree =
	(** Lecture effective des clauses **)
	(input_clauses entree clauses ["ecs" ; "cooperation" ; "cannot_be_cs" ; "error"]);
	close_in entree;
	check_errors clauses
;;

let cooperative_sorts clauses =
	List.map (fun c -> let s1 = parse_for_string c in fst s1)
				(Hashtbl.find_all clauses "ecs")
;;

let asp_of_clauses clauses cooperative_sorts =
	(** Traitement des clauses **)
	let cooperations =
	  let get_cooperation s =
		let cs = parse_for_string s in
		  let a = parse_for_string_at s (after_s cs) in
			let i = parse_for_word_at s (after_s a) in
			  let j = parse_for_word_at s (after_w i) in
				(fst cs, fst a, int_of_string (fst i), int_of_string (fst j))
	in
    List.map get_cooperation (Hashtbl.find_all clauses "cooperation")
	in

	(* Sortes coopératives et coopérations du PH *)
	"\n% Cooperations from the Process Hitting\n"
	^ (String.concat "" (List.map 
			(fun cs -> ("cooperative_sort(\"" ^ cs ^ "\").\n"))
				cooperative_sorts))
	^ (String.concat "" (List.map
			(fun coop -> let (cs, a, i, k) = coop in
			  	("cooperation(\"" ^ cs ^ "\",\"" ^ a ^ "\"," ^ (string_of_int i) ^ "," ^ (string_of_int k) ^ ").\n"))
				cooperations))
;;

