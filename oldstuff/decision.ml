
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

let dyninfo_compare =
	let proc_compare (m1,l1) (m2,l2) =
		let dm = compare m1 m2
		in
		if dm <> 0 then dm else compare l1 l2
	in
	let rec bit_compare b1 b2 = match b1,b2 with
		  L p, Neg b -> 1
		| Neg b, L p -> -1
		| L p, L p' -> proc_compare p p'
		| Neg b, Neg b' -> bit_compare b b'
	in
	let rec list_compare l1 l2 = match l1,l2 with
		  [],[] -> 0 | [],_ -> -1 | _,[] -> 1
		| h::q, h'::q' ->
			let dh = bit_compare h h'
			in
			if dh <> 0 then dh else list_compare q q'
	in
	list_compare
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
let driver decisions (((m,l),a),s) =
	List.filter (fun (m',i,a') -> 
		m = m' && a = a' && state_match_dyninfo i s
	) decisions
;;
let wdriver decisions (((m,l),a),s) =
	List.filter (fun (m',i,a') -> 
		not(m = m' && a = a') && state_match_dyninfo i s
	) decisions
;;

let involved_metaproc_dyninfo =
	let rec folder genes = function
		  L (a,x) -> a::genes
		| Neg b -> folder genes b
	in
	List.fold_left folder []
;;

let involved_procs_dyninfo =
	let rec folder procs = function
		  L p -> p::procs
		| Neg b -> folder procs b
	in
	List.fold_left folder []
;;

(*
 * decision
 *)
let string_of_decision (m, i, a) =
	"\\delta_{"^m^"}^"^(string_of_action a)^"("^(string_of_dyninfo i)^")"
;;

let action_compare a1 a2 =
	let int_of_action = function
		Dec -> -1 | Dis -> 0 | Inc -> 1
	in
	(int_of_action a1) - (int_of_action a2)
;;

let decision_compare (m1,i1,a1) (m2,i2,a2) =
	let dm = compare m1 m2
	in
	if dm <> 0 then dm
	else (
		let di = dyninfo_compare i1 i2
		in
		if di <> 0 then di
		else action_compare a1 a2
	)
;;

let select metaproc = List.filter (fun (m,_,_) -> m = metaproc)
;;

let transitions_matching_decision (m,i,a) =
	List.filter (fun (_,s) -> state_match_dyninfo i s)
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

let state_graph_of_dynamic dyn =
	let sg = Hashtbl.create 1
	in
	let iter s (_,s') =
		let s, s' = string_of_context s, string_of_context s'
		in
		if not (List.mem s' (Hashtbl.find_all sg s)) then
			Hashtbl.add sg s s'
	in
	Hashtbl.iter iter dyn;
	sg
;;


let rec free_transition (m,i,a) (t,s) constraints =
	let im = involved_procs_dyninfo i
	in
	let remove_root s = Util.list_sub s im
	in
	let r = remove_root s
	in
	let cr = List.flatten (List.map 
		(fun (_,s) -> Util.list_intersection r s) constraints)
	in
	let p = Util.list_sub r cr
	in
	if List.length p > 0 then (m,(L (List.hd p))::i,a)
	else (
		let i = (L (List.hd cr))::i
		in
		let constraints = List.filter (fun (_,s) ->
			state_match_dyninfo i s) constraints
		in
		free_transition (m,i,a) (t,s) constraints
	)
;;

let solve states decisions (constraints,properties) =
	let rec free_properties decisions constraints cds = function
		  [] -> decisions
		| t::q -> let ds = driver decisions t in
			if Util.subset cds ds then
				let d = List.hd ds
				in
				let nd = free_transition d t (transitions_matching_decision d constraints)
				in
				free_properties (nd::decisions) constraints cds q
			else free_properties decisions constraints cds q
	in

	let extend tl =
		List.flatten (List.map (t_extend states) tl)
	in
	let constraints = extend constraints
	and properties = extend properties
	in
	let cds = Util.list_uniq (List.flatten (List.map
		(fun t -> driver decisions t) constraints))
	in

	let decisions = free_properties decisions constraints cds properties
	in
	Util.list_sub decisions cds
;;


module Polynome = Polynome.Make
	(struct 
		type t = decision
		let compare = decision_compare
		let to_string = string_of_decision
	end)
;;

let path_rate decisions (rf,ro) state substs =
	assert (rf <= ro);
	let rec make_eq state = function [] -> []
		| subst::q ->
			let dok, dko = driver decisions (subst,state), wdriver decisions (subst,state)
			in
			let r = if List.length dko > 0 then [(dok,dok@dko)] else []
			in
			r @ make_eq (apply_substitution state subst) q
	in
	Polynome.init (make_eq state substs) (Num.div_num (Num.Int rf) (Num.Int ro))
;;

let map_decisions decisions =
	let rec string_from_i i =
		"x("^(string_of_int i)^")"
	in
	let folder (map,i) decision =
		let varname = string_from_i i
		in
		(decision, varname)::map, i+1
	in
	fst (List.fold_left folder ([],1) decisions)
;;


