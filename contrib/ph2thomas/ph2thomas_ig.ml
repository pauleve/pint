(*** Récupérer les résultats de l'inférence du Graphe des Interactions ***)
(*** Les traduire au format .lp pour l'inférence des paramètres ***)

(*  Syntaxe : ./pint phinferK-v5.ml
  Lit l'entrée standard pour en extraire les clauses résultant de l'exécution du programme ASP inf-PH-Thomas-v5.lp
  permettant de reconstruire le Graphe des Interactions du modèle de Thomas, et écrire ces informations au format ".lp" sur la sortie standard.
    Entrée : entrée standard
    Sortie : sortie standard
*)

exception Parsing_error of string ;;    (* Error while parsing *)
exception Result_error ;;               (* Error in the resolution result, f.i.: undefined sign *)

(* (* Fonctions devenues inutiles *)
(* Fonction de lecture par mots *)
let input_word input =
  let not_sep c input =
    let separators = [' ' ; '\n' ; ',' ; '(' ; ')'] in
    let comments = ['#'] in
      if List.mem c comments
        then (ignore (input_line input); false)
        else not (List.mem c separators)
in
  let rec read_word input start_read =
    let c = input_char input in
      if not_sep c input
        then (Char.escaped c) ^ (read_word input true)
        else if start_read
          then ""
          else (read_word input false)
in
  read_word input false
;;

(* Fonction de lecture de chaînes *)
let input_string input =
  let string_delimitor c =
    let delimitors = ['\""'] in
      List.mem c delimitors
in
  let rec read_string input start_read =
    let c = input_char input in
      if string_delimitor c
        then if start_read
          then ""
          else read_string input true
        else if start_read
          then (Char.escaped c) ^ (read_string input true)
          else (read_string input false)
in
  read_string input false
;;
*)

(** Fonctions de lecture des clauses **)
(* Fonction de lecture du nom d'une clause *)
let input_clause_name input =
  let is_separator c =
    let separators = [' ' ; '\n'] in
      List.mem c separators
in
  let is_delimitor c =
    let delimitors = ['(' ; ' ' ; '\n'] in
      List.mem c delimitors
in
  let fst_concat c r =
    let (fst, snd) = r in
      (c ^ fst, snd)
in
  let rec read_clause_name input start_read =
    let c = input_char input in
      if start_read
        then if is_delimitor c
          then if is_separator c then ("", false) else ("", true)
          else (fst_concat (Char.escaped c) (read_clause_name input true))
        else if is_separator c
          then (read_clause_name input false)
          else (fst_concat (Char.escaped c) (read_clause_name input true))
in
  read_clause_name input false
;;

(* Fonction de lecture des arguments d'une clause *)
let input_clause_args input =
  let rec read_clause_args input level =
    let c = input_char input in
      match c with
      | '(' -> if level = 0
                 then read_clause_args input 1
                 else (Char.escaped c) ^ (read_clause_args input (level + 1))
      | ')' -> (match level with
                | 0 -> raise (Parsing_error "input_clause_args: Invalid clause argument")
                | 1 -> ""
                | _ -> (Char.escaped c) ^ (read_clause_args input (level - 1))
               )
      |  _  -> if level = 0
                 then (read_clause_args input 0)
                 else (Char.escaped c) ^ (read_clause_args input level)
in
  read_clause_args input 1
;;

(* Lecture de toutes les clauses de la sortie de clasp *)
let input_clause input =
  let (clause_name, some_args) = input_clause_name input in
    let clause_args =
      if some_args
        then input_clause_args input
        else ""
in
  (clause_name, clause_args)
;;

let table_mem t k a = List.mem a (Hashtbl.find_all t k) ;;

let rec input_clauses input res lc =
  let c = ref ("", "") in
    try
      while true do
      c := input_clause input;
      if List.mem (fst !c) lc
        then if not (table_mem res (fst !c) (snd !c))
          then Hashtbl.add res (fst !c) (snd !c)
      done
    with End_of_file ->
      ()
;;



(** Fonctions d'analyse des clauses lues **)
(* Lecture d'une chaîne de carctères (délimitée par des guillemets) *)
let end_string str start =
  String.sub str start ((String.length str) - start)
;;

let parse_for_string str =
  let string_delimitor c =
    let delimitors = ['\"'] in
      List.mem c delimitors
in
  let fst_concat c r =
    let (fst, snd) = r in
      (c ^ fst, snd)
in
  let rec read_string str start_read position =
    let c = str.[0] in
      if string_delimitor c
        then if start_read
          then ("", position)
          else read_string (end_string str 1) true position
        else if start_read
          then fst_concat (Char.escaped c) (read_string (end_string str 1) true position)
          else (read_string (end_string str 1) false (position + 1))
in
  read_string str false 0
;;

let parse_for_string_at str start =
  let (s, p) = parse_for_string (end_string str start) in
    (s, p + start)
;;

(*
(* Lecture d'un mot (non délimité par des guillemets) *)
let parse_for_word str =
  let delimitor c =
    let delimitors = [',' ; ' ' ; '(' ; ')' ; '\""'] in
      List.mem c delimitors
in
  let fst_concat c r =
    let (fst, snd) = r in
      (c ^ fst, snd)
in
  let rec read_string str start_read position =
    let c = str.[0] in
      if delimitor c
        then if start_read
          then ("", position)
          else read_string (end_string str 1) true position
        else if start_read
          then fst_concat (Char.escaped c) (read_string (end_string str 1) true position)
          else (read_string (end_string str 1) false (position + 1))
in
  read_string str false 0
;;
*)

(* Lecture d'un mot (non délimité par des guillemets) *)
let parse_for_word str =
  let delimitor c =
    let delimitors = [',' ; ' ' ; '(' ; ')' ; '\"'] in
      List.mem c delimitors
in
  let fst_concat c r =
    let (fst, snd) = r in
      (c ^ fst, snd)
in
  let rec read_string str start_read position =
    if String.length str = 0 then ("", position) else
      let c = str.[0] in
        if delimitor c
          then if start_read
            then ("", position)
            else (read_string (end_string str 1) false (position + 1))
          else fst_concat (Char.escaped c) (read_string (end_string str 1) true position)
in
  read_string str false 0
;;

let parse_for_word_at str start =
  let (s, p) = parse_for_word (end_string str start) in
    (s, p + start)
;;

(*
let parse_for_int str =
  let (s, p) = parse_for_word str in
    (int_of_string s, p)
;;

let parse_for_int_at str start =
  let (s, p) = parse_for_word (end_string str start) in
    (int_of_string s, p + start)
;;
*)

let after c =
  let (c1, c2) = c in
    c2 + (String.length c1) + 2
;;

let after_w c =
  let (c1, c2) = c in
    c2 + (String.length c1)
;;


let input_graph entree =

	(** Lecture effective des clauses **)
	let clauses = Hashtbl.create 4
	in
	input_clauses entree clauses ["ea" ; "eb" ; "gi_edge" ; "error"];

	(* Vérification de l'absence d'erreurs *)
	if Hashtbl.mem clauses "error" then (
	  let err_args = Hashtbl.find clauses "error" in
		let err_text = parse_for_string_at err_args 0 in
		  let err_compl = end_string err_args ((after err_text) + 1) in
			prerr_endline ("Error in the result of the resolution: \"" ^ (fst err_text) ^ "\", with args: " ^ err_compl) ;
			raise Result_error
	);


	(** Traitement des clauses **)
	(*
	let node_from_clause cargs =
	  let a = parse_for_string cargs in
		(fst a) ^ "\n"
	in
	*)

	(* Table des nœuds *)
	let nodupes l = List.fold_left (fun l1 a -> if (List.mem a l1) then l1 else a :: l1) [] l
	in
	let nodes = (nodupes (List.map (fun a -> fst (parse_for_string a))
	  (List.append (Hashtbl.find_all clauses "ea") (Hashtbl.find_all clauses "eb"))))
	in

	(* Tables des arcs positifs et négatifs *)
	let edges_act = Hashtbl.create (2 * (List.length nodes))
	and edges_inh = Hashtbl.create (2 * (List.length nodes))
	and edges_nos = Hashtbl.create (2 * (List.length nodes))
	in

	let update_edge e =
	  let (ea, sign, threshold, eb) = e in
		match sign with
		| "+" -> Hashtbl.add edges_act (ea, eb) threshold
		| "-" -> Hashtbl.add edges_inh (ea, eb) threshold
		| "?" -> Hashtbl.add edges_nos (ea, eb) 0
		| _ -> raise (Parsing_error ("update_edge: Unknown edge sign: " ^ sign))
	in

	let edge_from_clause cargs =
	  let ea = parse_for_string_at cargs 0 in
		let sign = parse_for_string_at cargs (after ea) in
		  let threshold = parse_for_word_at cargs (after sign) in
			let eb = parse_for_string_at cargs (after_w threshold) in
			((fst ea), (fst sign), (int_of_string (fst threshold)), (fst eb))
	in

	List.iter (fun a -> update_edge (edge_from_clause a)) (Hashtbl.find_all clauses "gi_edge");

	(* Relecture des arcs et rangement final dans la table edges, et mise à jour du seuil *)
	let edges = Hashtbl.create ((Hashtbl.length edges_act) + (Hashtbl.length edges_inh))
	in

	let iterator sign ee threshold =
	  let (ea, eb) = ee in
		if Hashtbl.mem edges (ea, eb)
		  then (
			let old_edge = Hashtbl.find edges (ea, eb) in
			  if (fst (old_edge)) != sign
				then (
				  Hashtbl.replace edges (ea, eb) ("?", 0)
				  (* prerr_endline ("Error: Edge " ^ ea ^ " -> " ^ eb ^ " has both signs (+ and -)") ;
				  raise Result_error *)
				) else
				  Hashtbl.replace edges (ea, eb) (sign, min threshold (snd old_edge))
		  ) else
			Hashtbl.add edges (ea, eb) (sign, threshold)
	in
	Hashtbl.iter (iterator "+") edges_act ;
	Hashtbl.iter (iterator "-") edges_inh ;

	let iterator ee _ =
	  let (ea, eb) = ee in
		Hashtbl.replace edges (ea, eb) ("?", 0)
	in
	Hashtbl.iter iterator edges_nos;
	close_in entree;
	(nodes, edges)
;;

let string_of_edge ee st =
  let (ea, eb) = ee in
	let (sign, threshold) = st in
	  "edge(\"" ^ ea ^ "\",\"" ^ sign ^ "\"," ^ 
	  ( if (String.compare sign "?") != 0
		  then (string_of_int threshold) ^ ","
		  else "" ) ^
	  "\"" ^ eb ^ "\").\n"
;;

let string_of_edge_DOT ee st =
  let (ea, eb) = ee in
    let (sign, threshold) = st in
      ea ^ " -> " ^ eb ^ "[label=\"" ^ sign ^ 
      ( if (String.compare sign "?") != 0
          then string_of_int threshold
          else "" ) ^
      "\"];\n"
;;

let asp_of_graph (nodes, edges) =
	"\n"
(*
List.iter (fun n -> output_string sortie (node_from_clause n)) nodes ;;
output_string sortie "\nEND_NODES\n\n" ;;
*)
 	^ "% Edges of Thomas' modelling\n"
	^ Hashtbl.fold (fun e c b -> b ^ (string_of_edge e c)) edges ""
;;

let dot_of_graph (nodes, edges) =
      "digraph ig {\n"
      ^ (String.concat "" (List.map (fun n -> "  " ^ n ^ ";\n") nodes))
	  ^ (Hashtbl.fold (fun e c b -> b ^ string_of_edge_DOT e c) edges "")
	  ^ "}\n" 
;;

