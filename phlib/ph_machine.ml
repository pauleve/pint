
open Debug;;
open Ph_types;;

(** Process Hitting **)

module P2Map = Map.Make (struct type t = process * process let compare = compare end);;

let get_key ai bj = if ai < bj then (ai,bj) else (bj,ai)
;;

(*
let string_of_reaction (j,f,j') = string_of_processes j^ " -> "^string_of_processes j;;
let string_of_reactions l = "{ "^String.concat ", " (List.map string_of_reaction l)^"}";;
*)

let create_env (ps,hits) =
	let index bj ((ai,f),k) idx =
		let i = get_key ai bj
		in
		let reactions = try P2Map.find i idx with Not_found -> []
		in
		let j = PSet.add bj (PSet.singleton ai)
		and j' = PSet.add (fst bj, k) (PSet.singleton ai)
		in
		(*
		dbg (string_of_process (fst i)^","^string_of_process (snd i)^": adding reaction "^
			string_of_reaction (j,f,j'));
		*)
		let reactions = (j,f,j')::reactions
		in
		P2Map.add i reactions idx
	in
	Hashtbl.fold index hits P2Map.empty
;;

let reactions env ai s =
	let fold b j reactions =
		let i = get_key ai (b,j)
		in
		try P2Map.find i env@reactions
		with Not_found -> reactions
	in
	SMap.fold fold s []
;;

(** NM-NRM **)

let init l (t,s,r) =
	let init (j,f,j') =
		let t' = match f with
			  None -> t
			| Some (r,sa) ->
				let step = Param.random_firing_time r sa
				in
				(*dbg ("t' = "^string_of_float t^" + "^string_of_float step);*)
				t +. step
		in
		Hashtbl.add r (j,f,j') t'
	in
	List.iter init l;
	(t,s,r)
;;

let next (_,_,r) =
	let get_next o t = function
		  None -> Some (o,t)
		| Some (o',t') -> if t < t' then Some (o,t) else Some (o',t')
	in
	Hashtbl.fold get_next r None
;;


(** generic abstract machine **)

let species_init env (a,i) (t,s,r) = 
	let i' = try SMap.find a s with Not_found -> -1
	in
	assert (i' <> i);
	let s = SMap.add a i s
	in
	let l = reactions env (a,i) s
	in
	(*dbg ("initialising new reactions "^string_of_reactions l);*)
	init l (t,s,r)
;;

let get_affected_reactions i (t,s,r) =
	let folder (j,f,j') _ aff =
		if PSet.mem i j then (j,f,j')::aff else aff
	in
	Hashtbl.fold folder r []
;;
let species_remove species (t,s,r) =
	let l = get_affected_reactions species (t,s,r)
	in
	(*dbg ("removing reactions "^string_of_reactions l);*)
	List.iter (Hashtbl.remove r) l;
	(t,s,r)
;;

let initial env state =
	let fold a i term =
		species_init env (a,i) term
	in
	SMap.fold fold state (0., SMap.empty, Hashtbl.create 20)
;;

exception Halt;;

let reduce env term =
	match next term with
	  None -> raise Halt
	| Some ((j,f,j'), t) ->
		let target = PSet.min_elt (if PSet.cardinal j == 1 then j else PSet.diff j j')
		and bounce = PSet.min_elt (PSet.diff j' j)
		in
		let (_,s,r) = species_remove target term
		in
		let term = species_init env bounce (t,s,r)
		in
		term, (target,bounce)
;;

let execute env state duration plotter =
	let term = initial env state
	in
	let rec execute (t,s,r) =
		try
			let (t,s,r), (bj,bk) = reduce env (t,s,r)
			in
			(if t > duration then raise Halt);
			prerr_string (string_of_float t^"\r"); flush stderr;
			plotter t bk;
			execute (t,s,r)
		with Halt -> s
	in
	execute term
;;

