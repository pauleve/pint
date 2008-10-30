
type metaproc = string
type proc = (string * string)

type action = Inc | Dec | Dis

type substitution = proc * action
type context = proc list
type transition = substitution * context

type dyninfo_bit = 
	L of proc

type dyninfo = dyninfo_bit list

type decision = metaproc * dyninfo * action
;;

(*
 * meta-procs and procs
 *)
let string_of_proc p = (fst p)^(snd p)
;;
let string_of_action = function
	Inc -> "+" | Dec -> "-" | Dis -> "x"
;;

(*
 * substitutions
 *)
let string_of_substitution subst = 
	"["^(string_of_action (snd subst))^"/"^(string_of_proc (fst subst))^"]"
;;
let actant = fst
;;

(*
 * context
 *)
let string_of_context ctx = "("^(String.concat "" (List.map string_of_proc ctx))^")"
;;
let string_of_context_list ctxs = "["^(String.concat ";" 
	(List.map string_of_context ctxs))^"]"
;;

(*
 * transitions
 *)
let string_of_transition t =
	(string_of_substitution (fst t))^(string_of_context (snd t))
;;
let state_match_transition t s =
	Util.subset s (snd t)
;;
let t_appliers t = List.filter (state_match_transition t)
;;
let t_extend states t =
	List.map (fun s -> (fst t),s) (t_appliers t states)
;;
	

(*
 * dynamical information
 *)
let string_of_dyninfo i =
	let string_of_bit = function
		L p -> string_of_proc p
	in
	String.concat ";" (List.map string_of_bit i)
;;
let state_match_dyninfo i s =
	let match_bit = function
		  L p -> List.mem p s
	in
	let folder t k = t && match_bit k
	in
	List.fold_left folder true i
;;
let d_appliers (m, i, a) = List.filter (state_match_dyninfo i)
;;
let drivers states decisions t =
	let states = t_appliers t states
	and decisions = List.filter (fun (m,i,a) -> a = snd t) decisions
	in
	List.filter (fun d -> d_appliers d states <> []) decisions
;;
let drivers_e decisions ((p,a),s) =
	List.filter (fun (m,i,a') -> a = a' && state_match_dyninfo i s) decisions
;;

(*
 * decision
 *)
let string_of_decision (m, i, a) =
	"\\delta_{"^m^"}("^(string_of_dyninfo i)^")"^string_of_action a
;;

let solve_conflict d pctxs cctxs =
	[d]
;;

let solve states decisions constraints properties =
	let extend tl =
		List.flatten (List.map (t_extend states) tl)
	in
	let constraints = extend constraints
	and properties = extend properties
	in

	let cdsm = List.map (fun t -> t, drivers_e decisions t) constraints
	and pdsm = List.map (fun t -> t, drivers_e decisions t) properties
	in

	let ds_from_map m = Util.list_uniq (List.flatten (snd (List.split m)))
	and h_from_map m pred =
		let h = Hashtbl.create (List.length m)
		in
		let register_assoc (t,ds) =
			List.iter (fun d -> if pred d then Hashtbl.add h d (snd t)) ds
		in
		List.iter register_assoc m;
		h
	in

	let cds = ds_from_map cdsm
	in

	(* 1. filter free properties *)
	let p_not_free (t, ds) = not (Util.subset cds ds)
	in
	let pdsm = List.filter p_not_free pdsm
	in

	(* 2. disable free decisions *)
	let pds = ds_from_map pdsm
	in
	let fd = Util.list_sub cds pds
	in
	let decisions = (Util.list_sub decisions fd) @
			List.map (fun (m,i,_) -> (m,i,Dis)) fd

	(* 3. resolve conflicts *)
	and cdsh = h_from_map cdsm (fun d -> not (List.mem d fd))
	and pdsh = h_from_map pdsm (fun d -> true)
	in
	let dconflicts = List.map 
		(fun d -> d,(Hashtbl.find_all pdsh d),(Hashtbl.find_all cdsh d))
		pds
	in
	let dresols = List.map
		(fun (d, pctxs, cctxs) -> d, (solve_conflict d pctxs cctxs))
		dconflicts
	in

	(** DEBUG **)
	let string_of_conflict_resol (conflict, resol) =
		let string_of_conflict (d, pctxs, cctxs) =
			"Conflict: "^(string_of_decision d)^" with "^
			(string_of_context_list pctxs)^" and "^
			(string_of_context_list cctxs)^"."
		and string_of_resol (d, ds') =
			"replace "^(string_of_decision d)^" by "^
			String.concat ", " (List.map string_of_decision ds')
		in
		(string_of_conflict conflict)^"\n\tsolution: "^
		string_of_resol resol
	in
	let crs = List.map string_of_conflict_resol (List.combine dconflicts dresols)
	in
	print_endline (String.concat "\n" crs);
	(** FIN DEBUG *)

	(*
	let pds = snd (List.split pdsm)
	in
	let all_solving_ds = List.map Util.list_uniq (Util.cross_list pds)
	in*)
	decisions
;;

