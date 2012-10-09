
module type KSetsCfg =
sig
	type t
	val compare: t -> t -> int
	val max_h: int
	val string_of_elt: t -> string
end

module Make (Cfg: KSetsCfg) =
struct

	module SubsetMap = Map.Make (struct type t = Cfg.t let compare a b = - Cfg.compare a b end)

	type elt = SubsetMap.key
	type t = Zero | Nodes of (t SubsetMap.t * int) | One

	let string_of_elt = Cfg.string_of_elt

	let to_dot a =
		let rec string_of_tree i = function
		  One -> 
			[i], "n" ^ string_of_int i ^ "[shape=box,label=1];\n", i+1
		| Zero -> 
			[i], "n" ^ string_of_int i ^ "[shape=box,label=0];\n", i+1
		| Nodes (nm, _) ->
			let fold n a (is, buf, i) =
				let nis, nbuf, i = string_of_tree i a
				in
				let nbuf = nbuf ^ "n"^string_of_int i^"[label=\""^string_of_elt n^"\"];\n"
					^ String.concat "\n" 
						(List.map (fun j -> "n"^string_of_int i^" -> n"^string_of_int j^";") nis)
				in
				i::is, buf^nbuf, i+1
			in
			SubsetMap.fold fold nm ([], "", i)
		in
		let _, buf, _ = string_of_tree 0 a
		in
		"digraph{\n" ^ buf ^ "}\n"

	let empty = Zero
	let full = One

	let singleton e = Nodes (SubsetMap.singleton e One, 1)

	let rec cardinal = function Zero -> 0 | One -> 1
		| Nodes (nm, _) -> SubsetMap.fold (fun _ b n -> n + cardinal b) nm 0
	
	let clean_nodes_of_nodemap nm h =
			let nm = SubsetMap.filter (fun n a -> match a with Zero -> false | _ -> true) nm
			in
			if SubsetMap.is_empty nm then Zero else Nodes (nm, h)

	(* cleanup a: remove paths leading to Zero *)
	let rec cleanup = function Zero -> Zero | One -> One
		| Nodes (nm, h) ->
			let nm = SubsetMap.map cleanup nm
			in
			clean_nodes_of_nodemap nm h

	let rec level_up = function Zero -> Zero | One -> One
		| Nodes (nm, h) -> if h >= Cfg.max_h then Zero else Nodes (SubsetMap.map level_up nm, h+1)

	let rec clean_level_up = function Zero -> Zero | One -> One
		| Nodes (nm, h) -> if h >= Cfg.max_h then Zero else
			let nm = SubsetMap.map clean_level_up nm
			in
			clean_nodes_of_nodemap nm (h+1)

	let rec union a b =
		match (a,b) with
		  (One, _) | (_, One) -> One
		| (Zero, a) | (a, Zero) -> a
		| (Nodes (nma, ha), Nodes (nmb, hb)) ->
			assert (ha = hb);
			let fold n a nm =
				let a = try union a (SubsetMap.find n nm) with Not_found -> a
				in
				SubsetMap.add n a nm
			in
			let nm = SubsetMap.fold fold nmb nma
			in
			Nodes (nm, ha)

	let rec product a b =
		match (a,b) with
		  (Zero, _) | (_, Zero) -> Zero
		| (One, a) | (a, One) -> a
		| (Nodes (nma, ha), Nodes (nmb, hb)) ->
			assert (ha = hb);
			let fold n a c =
				let nm2, a', nm = SubsetMap.split n nma
				in
				let nm = if SubsetMap.is_empty nm then nm else
							let a = clean_level_up (Nodes (SubsetMap.singleton n a, ha))
							in
							SubsetMap.map (product a) nm
				and a' = match a' with None -> Zero | Some a' -> product a a'
				and b = if SubsetMap.is_empty nm2 then Zero else
							product a (clean_level_up (Nodes (nm2, ha)))
				in
				let a = union a' b
				in
				let nm = match a with Zero -> nm | a -> SubsetMap.add n a nm
				in
				union c (Nodes (nm, ha))
			in
			SubsetMap.fold fold nmb Zero
	
	let rec rm_sursets p a =
		match (a,p) with
		  (_, One) -> Zero
		| (_, Zero) | (One, _) | (Zero, _) -> a
		| (Nodes (nma, ha), Nodes (nmp, hp)) ->
			let fold np p' nma =
				let up' = clean_level_up (Nodes (SubsetMap.singleton np p', hp))
				in
				let apply n a' =
					let ord = Cfg.compare n np
					in
					if ord < 0 then
						rm_sursets up' a'
					else if ord = 0 then
						rm_sursets p' a'
					else a'
				in
				SubsetMap.mapi apply nma
			in
			let nma =  SubsetMap.fold fold nmp nma
			in
			clean_nodes_of_nodemap nma ha

	let rec simplify = function Zero -> Zero | One -> One
		| Nodes (nm, h) ->
			let nm = SubsetMap.map simplify nm
			in
			let fold n b = function
				  Zero -> Nodes (SubsetMap.singleton n b, h)
				| One -> One
				| p ->
					let a = rm_sursets p (Nodes (SubsetMap.singleton n b, h))
					in
					union p a
			in
			SubsetMap.fold fold nm Zero

	let rec equal a b = 
		match (a,b) with
		  (One,One) | (Zero,Zero) -> true
		| (One,_) | (_,One) | (Zero,_) | (_,Zero) -> false
		| Nodes (nma, _), Nodes (nmb, _) -> SubsetMap.equal equal nma nmb

	let rec compare a b = 
		match (a,b) with
		  (One,One) | (Zero,Zero) -> 0
		| (Zero,_) -> -1
		| (_,Zero) -> 1
		| (One,_) -> 1
		| (_,One) -> -1
		| Nodes (nma, _), Nodes (nmb, _) -> SubsetMap.compare compare nma nmb
	
	let rec elements = function
		  Zero -> []
		| One -> [[]]
		| Nodes (nm, _) ->
			let fold n a elts =
				List.fold_left (fun elts e -> (n::e)::elts) elts (elements a)
			in
			SubsetMap.fold fold nm []

end

