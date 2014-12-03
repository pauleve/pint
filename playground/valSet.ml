

open PintTypes
open AutomataNetwork

type uid = int
type var = string

type t = Top | Bot | N of node
and node = {uid: uid; var: var; children: t IMap.t}

let uid = function
	  Bot -> 0
	| Top -> 1
	| N n -> n.uid

let next_uid = ref 2

let node_equal n1 n2 =
	compare n1.var n2.var = 0
    && IMap.for_all (fun i n -> uid n = uid (IMap.find i n2.children))
		n1.children

module H = struct
	type t = node
    let equal = node_equal
	let hash n = 
		Hashtbl.hash (n.var, 
			List.map (fun _ n' -> uid n') (IMap.bindings n.children))
end

module W = Weak.Make(H)

let table = W.create 20

let mknode var children =
	let child = snd (IMap.choose children)
	in
	let uchild = uid child
	in
	if IMap.for_all (fun _ n -> uid n = uchild) children then
		child
	else (
		let n = {uid = !next_uid; var; children}
		in
		let n' = W.merge table n
		in
		(if n == n' then incr next_uid);
		N n')

let rec valset sd vs d =
	match vs with [] -> Bot
  	| vs ->  
		match d with [] -> Top
		| a::d ->
			let folder i im =
				let vs = List.filter (fun v -> LSMap.mem (a,i) v) vs
				in
				IMap.add i (valset sd vs d) im
			in
			let is = SMap.find a sd
			in
			let children = ISet.fold folder is IMap.empty
			in
			mknode a children



(**
http://stackoverflow.com/questions/20196133/the-ideal-way-to-write-permutation-function-in-ocaml
*)

