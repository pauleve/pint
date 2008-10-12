
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
let fold f (spig:t) = Graph.fold f spig;;
let procs (spig:t) = Graph.vertices spig;;

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

let dot_of_sgc stateg =
	let dot_of_state state =
		let id = string_of_state state
		in
		"\""^id^"\"[label=\""^id^"\"]"
	and dot_of_transition transition =
		let id = string_of_transition transition
		in
		"\""^id^"\"[label=\""^id^"\",style=filled,fillcolor=green,shape=diamond,fontsize=10]"
	and dot_of_edge s1 (tr, s2) acc =
		let sid1 = string_of_state s1
		and sid2 = string_of_state s2
		and trid = string_of_transition tr
		in
		("\""^sid1^"\" -> \""^trid^"\" \""^trid^"\" -> \""^sid2^"\"")::acc
	and states = Graph.vertices stateg
	and transitions = Graph.edges stateg
	in
	"digraph G { node[fontsize=15] edge[fontsize=15,fontname=times]\n" ^ 
		(String.concat "\n" (List.map dot_of_state states))^"\n"^
		(String.concat "\n" (List.map dot_of_transition transitions))^"\n"^
		(String.concat "\n" (Graph.fold dot_of_edge stateg []))^"\n"^
		"}\n"

let spi_of_spig (spig:t) valuation default_rate init_state =

	let string_of_rate rate = 
		let s = string_of_float rate
		in
		s ^ if s.[String.length s - 1] = '.' then "0" else ""
	in

	(* 1. declare channels and delays rate *)
	let rate rname = try List.assoc rname valuation with Not_found -> default_rate
	in
	let declare_delay delay = "val "^(string_of_rname delay)^" = "^
		string_of_rate (rate delay)
	and declare_channel channel = "new "^(string_of_rname channel)^"@"^
		(string_of_rate (rate channel))^":chan"
	in
	let register_rates _ (edge, _) (channels, delays) =
		match edge with
			  Call channel | Take channel -> 
			  	Util.list_prepend_if_new channel channels, delays
			| Delay delay ->
				channels, Util.list_prepend_if_new delay delays
	in
	let channels, delays = fold register_rates spig ([],[])
	in

	(* 2. program *)
	let source_of_edge = function
		  Delay delay -> "delay@"^delay
		| Call channel -> "?"^channel
		| Take channel -> "!"^channel
	in
	let source_of_action (edge, dest) =
		(source_of_edge edge)^"; "^(string_of_pi_proc dest)^"()"
	in
	let source_of_proc proc =
		(string_of_pi_proc proc) ^ "() = " ^
		(match List.map source_of_action (get spig proc) with
			  [] -> failwith ("no action for proc "^string_of_pi_proc proc)
			| [action] -> action
			| actions -> "do "^String.concat " or " actions)
	in
	let program = "\tlet\n"^String.concat "\n\tand\n" 
		(List.map source_of_proc (procs spig))
	in

	(* 3. run *)
	let run = "run ("^(String.concat " | "
		(List.map (fun proc -> (string_of_pi_proc proc)^"()") init_state))^")"
	in

	(String.concat "\n" (
		(List.map declare_delay delays)
		@ (List.map declare_channel channels)
		@ [program; run]
	))^"\n"

