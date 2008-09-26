
type pi_pname = string (* process name *)
type pi_rname = string (* rate name *)
type pi_proc = pi_pname
type pi_edge = Delay of pi_rname
			| Call of pi_rname
			| Take of pi_rname

type t_state = pi_proc list
type t_transition = Transition of pi_rname

type t = (pi_proc, pi_edge) Graph.t
;;

let create = Graph.create;;
let add (spig:t) = Graph.add spig;;
let get (spig:t) = Graph.get spig;;

let string_of_pi_edge = function
	  Delay(rname) -> rname
	| Call(rname) -> "?"^rname
	| Take(rname) -> "!"^rname
;;
let string_of_pi_proc proc = proc
;;
let string_of_transition = function Transition(name) -> name;;
let string_of_state = String.concat ",";;
let string_of_rname x = x;;

let to_dot (spig:t) = Graph.to_dot spig string_of_pi_proc string_of_pi_edge;;

let pi_edge_complement = function
	  Call(rname) -> Take(rname)
	| Take(rname) -> Call(rname)
	| _ -> invalid_arg "Delay(rname)"
;;
let transition_of_pi_edge = function
	  Call(rname) | Take(rname) | Delay(rname) -> Transition(rname)
;;

let next spig state = 
	let edges = List.map (fun p1 -> (p1, get spig p1)) state
	in
	let has_pi_edge mpe = List.exists (fun (p, p_edges) -> List.mem (mpe,p) p_edges)
	in
	let rec build_transitions p1 = function [] -> []
		| (Delay(rname),p2)::q -> (p1,Transition(rname),p2)::(build_transitions p1 q)
		| (pe,p2)::q -> 
			if p1 != p2 && has_pi_edge (pi_edge_complement pe) edges then
				(p1,transition_of_pi_edge(pe),p2)::(build_transitions p1 q)
			else
				build_transitions p1 q
	in
	let transitions = List.flatten (List.map (fun (p1, p1_edges) -> build_transitions p1 p1_edges) edges)
	in
	let rec replace p1 p2 = function [] -> []
		| p::q -> if p = p1 then p2::q else p::(replace p1 p2 q)
	in
	List.map (fun (p1, tr, p2) -> (tr, replace p1 p2 state)) transitions
;;

let stateg_of_spig spig states =
	let stateg = Graph.create (List.length states)
	in
	let push_state state = List.iter (fun edge -> Graph.add stateg state edge) (next spig state)
	in
	List.iter push_state states;
	stateg
;;
let stateg_to_dot stateg = Graph.to_dot stateg string_of_state string_of_transition
;;

