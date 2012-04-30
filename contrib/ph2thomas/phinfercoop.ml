(*** Inférer les paramètres de Thomas à partir d'un Process Hitting
      et d'un Graphe des Interactions ***)



let fichier_sortie =
  if Array.length Sys.argv > 1
    then Array.get Sys.argv 1
    else ""
;;

let entree = stdin ;;
let sortie =
  if String.length fichier_sortie != 0
    then open_out_gen [Open_append ; Open_creat] 700 fichier_sortie
    else stdout
;;

exception Parsing_error of string ;;    (* Error while parsing *)



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

let after_s c =
  let (c1, c2) = c in
    c2 + (String.length c1) + 2
;;

let after_w c =
  let (c1, c2) = c in
    c2 + (String.length c1)
;;



(** Lecture effective des clauses **)
let clauses = Hashtbl.create 3 ;;
input_clauses entree clauses ["ecs" ; "cooperation" ; "cannot_be_cs"] ;;

(*
(* Vérification de l'absence d'erreurs *)
if Hashtbl.mem clauses "error" then
  let err_args = Hashtbl.find clauses "error" in
    let err_text = parse_for_string_at err_args 0 in
      let err_compl = end_string err_args ((after err_text) + 1) in
        prerr_endline ("Error in the result of the resolution: \"" ^ (fst err_text) ^ "\", with args: " ^ err_compl) ;
        raise Result_error
;;
*)

(** Traitement des clauses **)
(* Liste des sortes coopératives *)
let cannot_be_cs = List.map (fun c -> fst (parse_for_string c)) (Hashtbl.find_all clauses "cannot_be_cs") ;;

let cooperative_sorts =
  let possible_sorts = List.map
    (fun c -> let s1 = parse_for_string c in fst s1)
    (Hashtbl.find_all clauses "ecs")
in
  List.filter (fun a -> not (List.mem a cannot_be_cs)) possible_sorts ;;

let fst4 a = let (a1, _, _, _) = a in a1 ;;

let cooperations =
  let get_cooperation s =
    let cs = parse_for_string s in
      let a = parse_for_string_at s (after_s cs) in
        let i = parse_for_word_at s (after_s a) in
          let j = parse_for_word_at s (after_w i) in
            (fst cs, fst a, int_of_string (fst i), int_of_string (fst j))
in
  let possible_cooperations = List.map get_cooperation (Hashtbl.find_all clauses "cooperation")
in
  List.filter (fun a -> not (List.mem (fst4 a) cannot_be_cs)) possible_cooperations ;;



(** Fichier de sortie **)
(* Sortes coopératives et coopérations du PH *)
output_string sortie "\n% Cooperations from the Process Hitting\n" ;;
List.iter (fun cs -> output_string sortie ("cooperative_sort(\"" ^ cs ^ "\").\n")) cooperative_sorts ;;
List.iter (fun coop -> let (cs, a, i, k) = coop in
  output_string sortie ("cooperation(\"" ^ cs ^ "\",\"" ^ a ^ "\"," ^ (string_of_int i) ^ "," ^ (string_of_int k) ^ ").\n")) cooperations ;;

flush sortie ;;
close_out sortie ;;

