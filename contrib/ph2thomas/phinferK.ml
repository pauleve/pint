(*** Récupérer les résultats de l'inférence de la Paramétrisation ***)
(*** Lecture et sortie texte ***)

(*  Syntaxe : ./pint phinferK.ml
  Lit l'entrée standard pour en extraire les clauses résultant de l'exécution du programme ASP phinfK.lp.
    Entrée : entrée standard
    Sortie : sortie standard
*)

(*open List;;*)
let hd = List.hd ;;
let tl = List.tl ;;

let fichier_ASP = Array.get Sys.argv 1 ;;
let fichier_entree = Array.get Sys.argv 2 ;;
let output_format = Array.get Sys.argv 3 ;;

let sortie_ASP = open_out_gen [Open_append ; Open_creat] 700 fichier_ASP ;;
let entree_IG = open_in fichier_entree ;;
let entree = stdin ;;
let sortie = stdout ;;

exception Parsing_error of string ;;    (* Error while parsing *)
exception Result_error ;;               (* Error in the resolution result, f.i.: undefined sign *)



(** Lecture des données IG **)
let nodes : string list = input_value entree_IG ;;
let edges : ((string * string), (string * int)) Hashtbl.t = input_value entree_IG ;;
close_in entree_IG ;;

(* Hashtbl.iter (fun (a, b) (s, t) -> print_endline (a ^ " =(" ^ s ^ (string_of_int t) ^ ")=> " ^ b)) edges ;; *)

(* Table de référence des prédécesseurs *)
let edgesb : (string, string) Hashtbl.t = Hashtbl.create (Hashtbl.length edges) ;;
Hashtbl.iter (fun e _ -> let (e1, e2) = e in Hashtbl.add edgesb e2 e1) edges ;;

(* Paramétrisation *)
let param = Hashtbl.create (Hashtbl.length edges) ;;



(** Fonctions de récupération des clauses **)
let end_string str start =
  String.sub str start ((String.length str) - start)
;;

let fst_concat c r =
  let (fst, snd) = r in
    (c ^ fst, snd)
;;

let fst_concat_3 c r =
  let (fst, snd, thrd) = r in
    (c ^ fst, snd, thrd)
;;

(* Lit la première clause rencontrée et ses arguments dans une chaîne de caractères *)
let parse_for_clause str =
  let is_string_delimitor c =
    let string_delimitors = ['\"'] in
      List.mem c string_delimitors
in
  let is_separator c =
    let separators = [' ' ; '\n'] in
      List.mem c separators
in
  let is_delimitor c =
    let delimitors = ['('] in
      List.mem c delimitors
in
  let rec read_clause_args str depth in_string position =
    if String.length str = 0 then raise (Parsing_error "parse_for_clause: Missing \")\"") else
      let c = str.[0] in
        if is_string_delimitor c
          then fst_concat (Char.escaped c) (read_clause_args (end_string str 1) depth (not in_string) (position + 1))
          else if not in_string
            then match c with
                 | '(' -> fst_concat (Char.escaped c) (read_clause_args (end_string str 1) (depth + 1) in_string (position + 1))
                 | ')' -> (match depth with
                           | 0 -> raise (Parsing_error "parse_for_clause: Invalid \")\"")
                           | 1 -> ("", position + 1)
                           | _ -> fst_concat (Char.escaped c) (read_clause_args (end_string str 1) (depth - 1) in_string (position + 1))
                          )
                 |  _  -> fst_concat (Char.escaped c) (read_clause_args (end_string str 1) depth in_string (position + 1))
            else fst_concat (Char.escaped c) (read_clause_args (end_string str 1) depth in_string (position + 1))
in
  let rec read_clause_name str start_read position =
    if String.length str = 0 then ("", "", position) else
      let c = str.[0] in
        if (is_separator c) || (is_delimitor c)
          then if start_read
            then if is_delimitor c
              then let (a, p) = read_clause_args (end_string str 1) 1 false (position + 1) in ("", a, p)
              else ("", "", position)
            else (read_clause_name (end_string str 1) false (position + 1))
          else fst_concat_3 (Char.escaped c) (read_clause_name (end_string str 1) true (position + 1))
in
  read_clause_name str false 0
;;

let parse_for_clause_at str start = parse_for_clause (end_string str start) ;;

let rec get_all_clauses str nclause =
  if (String.length str) = 0
    then []
    else let (c, a, l) = (parse_for_clause str) in
      if String.compare c nclause = 0
        then a :: (get_all_clauses (end_string str l) nclause)
        else get_all_clauses (end_string str l) nclause
;;



(** Fonctions d'analyse de chaînes de caractères **)
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



(** Lecture des lignes, rangement et traitement **)
let rec string_of_list sep converter l =
  match l with
  | [] -> ""
  | h::[] -> converter h
  | h::t -> (converter h) ^ sep ^ (string_of_list sep converter t)
;;

let id x = x ;;

let rec input_lines input table key_clause =
  let line = ref "" in let clause = ref "" in let args = ref "" in let pos = ref 0 in
    try
      while true do
        line := input_line input ;
        clause := "" ;
        args := "" ;
        pos := 0 ;
        while (String.compare !clause key_clause) != 0 && !pos < (String.length !line) do
          let (c, a, p) = parse_for_clause_at !line !pos in
            ( clause := c;
              args := a;
              pos := !pos + p ) ;
        done ;
        if (String.compare !clause key_clause) = 0
          then (
            (* Traitement ici si trouvé ! *)
            let lres = get_all_clauses !line "result" in
              if fst (parse_for_string (hd lres)) = "ok"
                (* Traitement ici si bon résultat *)
                then let lactivation = List.map
                                            (fun i -> fst (parse_for_string i))
                                            (List.sort compare (get_all_clauses !line "kA")) in
                  let lparameters = (if List.length (get_all_clauses !line "error") > 0
                    then ( prerr_endline ("Error: parameter is not an attractor for k_ " ^ fst (parse_for_string (hd (get_all_clauses !line "error")))
                                          ^ ", {" ^ (string_of_list ", " id lactivation) ^ "}") ;
                      []
                    ) else         List.map (fun i -> int_of_string (fst (parse_for_word i)))
                                            (List.sort compare (get_all_clauses !line "sL_foc")) ) in
                      List.iter (Hashtbl.add table (fst (parse_for_string !args), lactivation)) lparameters ;
          ) else (
            (* Traitement ici si non trouvé *)
(*            print_endline "NOT FOUNDS HERE =(";*)
          )
      done
    with End_of_file ->
      ()
;;

input_lines entree param "ka" ;;



(** Affichage de sortie **)
let rec powerset l =
  match l with
   | [] -> [[]]
   | (x::xs) -> let xss = powerset xs in xss @ (List.map (fun xs -> x::xs) xss)
;;

let (|||) a b = (not (a && b)) && (a || b) ;;

let active l a =
  let rec active_rec predec l a =
    match predec with
    | [] -> []
    | h::tpredec ->
      let (sign, _) = Hashtbl.find edges (h, a) in
        if (List.mem h l) ||| ((String.compare sign "-") = 0)
          then h :: (active_rec tpredec l a)
          else active_rec tpredec l a
in
  active_rec (Hashtbl.find_all edgesb a) l a
;;

(*
let print_nodes_list l =
  let rec print_nodes_list_rec l =
    match l with
    | [] -> ""
    | h::[] -> h
    | h::t -> h ^ " " ^ (print_nodes_list_rec t)
in
  print_string ("{" ^ (print_nodes_list_rec l ^ "}"))
;;

let print_param_list l =
*)

if String.compare output_format "AB" = 0 then (
  let iterator n c =
    output_string sortie ("k_ " ^ n ^ ", {") ;
    output_string sortie (string_of_list ";" id c) ;
    output_string sortie "}  =  {" ;
    output_string sortie (string_of_list " " string_of_int (Hashtbl.find_all param (n, c))) ;
    output_string sortie "}\n" ;
  in
    List.iter (fun n -> List.iter (fun c -> iterator n (List.sort compare c)) (powerset (Hashtbl.find_all edgesb n))) nodes
) else if String.compare output_format "active" = 0 then (
  let iterator n c =
    output_string sortie ("k_ " ^ n ^ ", {") ;
    output_string sortie (string_of_list ";" id (active c n)) ;
    output_string sortie "}  =  {" ;
    output_string sortie (string_of_list " " string_of_int (Hashtbl.find_all param (n, c))) ;
    output_string sortie "}\n" ;
  in
    List.iter (fun n -> List.iter (fun c -> iterator n (List.sort compare c)) (powerset (Hashtbl.find_all edgesb n))) nodes

) else if String.compare output_format "iter" = 0 then (

  let iterator =
    fun a lparams -> let (gene, activators) = a in
      output_string sortie ("k_ " ^ gene ^ ", A =");
      List.iter (fun s -> output_string sortie (" " ^ s)) activators;
      output_string sortie (" = {");
      output_string sortie (" " ^ (string_of_int lparams)) ;
      output_string sortie " }\n"
  in
    Hashtbl.iter iterator param

) ;;

close_out sortie ;;



(** Sortie fichier ASP **)
let rec pow n e =
  if e = 0
    then 1
    else n * (pow n (e - 1))
;;

if String.compare output_format "enum" = 0 || String.compare output_format "enumerate" = 0 then (
  output_string sortie_ASP ("\n% Labelling for parameter enumeration\n") ;
(*  List.iter (fun n -> (output_string sortie_ASP ("max_param(\"" ^ n ^ "\"," ^ (string_of_int (pow 2 (List.length (Hashtbl.find_all edgesb n)))) ^ ").\n"))) nodes ; *)
  let rec rec_node n predec =
    match predec with
    | [] -> ()
    | h :: t -> (output_string sortie_ASP ("param_label(\"" ^ n ^ "\"," ^ (string_of_int (List.length predec)) ^ ").\n") ; rec_node n t)
  in
    List.iter (fun n -> rec_node n (powerset (Hashtbl.find_all edgesb n))) nodes ;
  let iterator_predec n i a = output_string sortie_ASP ("param_label(\"" ^ n ^ "\"," ^ (string_of_int i) ^ ",\"" ^ a ^ "\").\n") in
    let rec rec_node n predec =
      match predec with
      | [] -> ()
      | h :: t -> (List.iter (iterator_predec n (List.length predec)) h ; rec_node n t)
  in
    List.iter (fun n -> rec_node n (powerset (Hashtbl.find_all edgesb n))) nodes ;
  output_string sortie_ASP ("\n% Infered parameters\n") ;
  let iterator_param n i p = output_string sortie_ASP ("param(\"" ^ n ^ "\"," ^ (string_of_int i) ^ "," ^ (string_of_int p) ^ ").\n") in
    let rec rec_node n predec =
      match predec with
      | [] -> ()
      | h :: t -> (List.iter (iterator_param n (List.length predec)) (Hashtbl.find_all param (n, (List.sort compare h))) ; rec_node n t)
  in
    List.iter (fun n -> rec_node n (powerset (Hashtbl.find_all edgesb n))) nodes
) ;;

