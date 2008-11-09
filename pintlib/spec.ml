
(* return constraints, properties *)

let get_substitution s1 s2 =
	let d1 = Util.list_sub s1 s2
	and d2 = Util.list_sub s2 s1
	in
	assert (List.length d1 = 1);
	assert (List.length d2 = 1);
	let m,v = List.hd d1
	and m',v' = List.hd d2
	in
	assert (m = m');
	let l = int_of_string v
	and l' = int_of_string v'
	in
	let a = if l > l' then Decision.Dec else Decision.Inc
	in
	((m,v), a)
;;

let cycle = function s1::q -> s1::q @ [s1] 
	| _ -> raise (Invalid_argument "cycle")
;;

let merge_spec (c,p) (c',p') = c@c', p@p';;

let fold_specs (specs:('a*'b) list)  = List.fold_left merge_spec ([],[]) specs;;

let rec stable dyn = function [] | [_] -> [],[] 
	| s1::s2::q -> 
		let next = List.filter (fun s -> s <> s2) (Graph.next_vertices dyn s1)
		in
		let c = List.map (fun s2' -> get_substitution s1 s2', s1) next
		and p = if s1 <> s2 then [get_substitution s1 s2, s1] else []
		in
		merge_spec (c,p) (stable dyn (s2::q))
;;

let rec required_path dyn init = function [] -> [],[]
	| subst::q -> 
		let next = Decision.apply_substitution init subst
		in
		[], (subst,init)::snd (required_path dyn next q)
;;

