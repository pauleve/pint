
type pi_pname = string * int (* process name *)
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
let iter f (spig:t) = Graph.iter f spig;;
let procs (spig:t) = Graph.vertices spig;;

let string_of_pi_edge = function
	  Delay(rname) -> rname
	| Call(rname) -> "?"^rname
	| Take(rname) -> "!"^rname
;;
let string_of_pi_proc (proc, level) = proc ^ string_of_int level
;;
let string_of_transition = function Transition(name) -> name;;
let string_of_state state = String.concat "," (List.map string_of_pi_proc state);;
let string_of_rname x = x;;
let id_from_transition = function Transition(name) -> name;;

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

let sign value = if value < 0 then -1 else if value > 0 then 1 else 0
;;

let restrict spig state (p, p') = assert (List.mem p state);
	let transitions = List.filter (fun (_,state') -> List.mem p' state') (next spig state)
	in
	fst (List.split transitions)
;;

let stable_subst spig state (p, p') = assert (List.mem p state);
	let ps = procs spig
	in
	let all_p' (a,x) = List.filter (fun (b,y) -> b = a && x <> y) ps
	in
	let stable_proc q = List.flatten (List.map 
		(fun q' -> if q' <> p' then restrict spig state (q, q') else [])
		(all_p' q))
	in
	List.flatten (List.map stable_proc state)
;;

let stable_state spig state =
	let ps = procs spig
	in
	let all_p' (a,x) = List.filter (fun (b,y) -> b = a && x <> y) ps
	in
	let no_subst_of_p p = List.flatten
		(List.map (fun p' -> stable_subst spig state (p,p')) (all_p' p))
	in
	List.flatten (List.map no_subst_of_p state)
;;

let rec stable spig state = function [] -> []
	| subst::q ->
		let transitions = stable_subst spig state subst
		in
		transitions @ stable spig (Dynamic.apply_subst state subst) q
;;

let remove_channels spig rnames =
	let nspig = create 0
	in
	let copy_matching p (e, q) =
		if not (List.mem (match e with Delay(n) | Call(n) | Take(n) -> n) rnames)
		then
			add nspig p (e, q)
	in
	iter copy_matching spig;
	nspig
;;
let remove_transitions spig trs = remove_channels spig (List.map id_from_transition trs);;

let apply_constraints spig cs =
	let null_channels, cs = Util.list_separate
		(fun c -> match c with Constraint.Null _ -> true | _ -> false) cs
	in
	let null_channels = List.map 
		(fun x -> match x with Constraint.Null n -> n 
			| _ -> raise (Invalid_argument "null_channels")) null_channels
	in
	remove_channels spig null_channels, cs
;;

let dynamic spig states =
	let rec from_states stateg known = function [] -> ()
		| states ->
			let push_state acc state = acc @ List.map
				(fun (tr, s') -> Graph.add stateg state (tr,s'); s')
				(next spig state);
			in
			let dests = List.fold_left push_state [] states
			in
			let known = known @ states
			in
			from_states stateg known (Util.list_sub dests known)
	in
	let stateg = Graph.create (List.length states)
	in
	from_states stateg [] states;
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

