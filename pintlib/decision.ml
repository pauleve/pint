
type metaproc = string
type proc = (string * string)

type action = Inc | Dec | Dis

type substitution = proc * action
type context = proc list
type transition = substitution * context

type dyninfo_bit = 
	  L of proc
	| Neg of dyninfo_bit

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
let get_proc_by_meta m state =
	let matching = List.filter (fun (m',v) -> m = m') state
	in
	assert (List.length matching = 1);
	List.hd matching
;;

(*
 * substitutions
 *)
let string_of_substitution subst = 
	"["^(string_of_action (snd subst))^"/"^(string_of_proc (fst subst))^"]"
;;

let apply_substitution state ((m,sv), a) = 
	let v = int_of_string sv
	in
	let v' = match a with Inc -> v+1 | Dec -> v-1 
				| Dis -> raise (Invalid_argument "disabled action")
	in
	Util.list_replace (m,sv) (m,string_of_int v') state
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
	let rec string_of_bit = function
		  L p -> string_of_proc p
		| Neg b -> "!"^string_of_bit b
	in
	String.concat ";" (List.map string_of_bit i)
;;
let state_match_dyninfo i s =
	let rec match_bit = function
		  L p -> List.mem p s
		| Neg b -> not (match_bit b)
	in
	let folder t k = t && match_bit k
	in
	List.fold_left folder true i
;;
let d_appliers (m, i, a) = List.filter (state_match_dyninfo i)
;;
let drivers_e decisions (((m,l),a),s) =
	List.filter (fun (m',i,a') -> m = m' && a = a' && state_match_dyninfo i s) decisions
;;

(*
 * decision
 *)
let string_of_decision (m, i, a) =
	"\\delta_{"^m^"}("^(string_of_dyninfo i)^")"^string_of_action a
;;

let apply_decision state (m,i,a) =
	let subst = (get_proc_by_meta m state, a)
	in
	apply_substitution state subst
;;

let next decisions state =
	let decisions = List.filter 
		(fun (m,i,a) -> state_match_dyninfo i state) decisions
	in
	List.map (fun d -> d, apply_decision state d) decisions
;;

(*
let extend_states decisions states =
	let rec _extend_states known = function
		  [] -> known
		| todo -> 
			let known = known @ todo
			and todo = List.flatten (List.map (next decisions) todo)
			in
			let todo = Util.list_uniq (Util.list_sub todo known)
			in
			_extend_states known todo
	in
	_extend_states [] states
;;
*)

let dynamic_of_decisions decisions states =
	let dyn = Graph.create (List.length states)
	in
	let register_assoc s1 acc (d,s2) =
		Graph.add dyn s1 (d,s2);
		Util.list_prepend_if_new s2 acc
	in
	let rec from_states known = function [] -> ()
		| states ->
			let push_state acc state = 
				List.fold_left (register_assoc state) acc (next decisions state)
			in
			let known = known @ states
			and states = List.fold_left push_state [] states;
			in
			let states = Util.list_sub states known
			in
			from_states known states
	in
	from_states [] states;
	dyn
;;

let solve_conflict (m,i,a) pctxs cctxs =
	let solve_conflict_1 pctx =
		List.map (fun cctx -> Util.list_sub pctx cctx) cctxs
	in
	let sols = List.flatten (List.map solve_conflict_1 pctxs)
	in
	let compare_sol s1 s2 = compare (List.length s1) (List.length s2)
	in
	let sols = Util.list_uniq (List.map Util.list_uniq (Util.cross_list sols))
	in
	let rec decisions_of_sol = function [] -> []
		| p::q -> (m,(L p)::i,a)::(m,(Neg (L p))::i,a)::decisions_of_sol q
	in
	let sols = List.sort compare_sol sols
	in
	decisions_of_sol (List.hd sols)
;;

let solve states decisions (constraints,properties) =
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
	let p_not_free (t, ds) = Util.subset cds ds
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
	print_endline (String.concat "\n" (List.map string_of_decision decisions));
	print_endline (String.concat "\n" crs);
	(** FIN DEBUG *)

	(*
	let pds = snd (List.split pdsm)
	in
	let all_solving_ds = List.map Util.list_uniq (Util.cross_list pds)
	in*)
	decisions
;;

