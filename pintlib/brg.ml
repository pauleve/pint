
type gene = string
type threshold = int
type epsilon = Activation | Inhibition

type t = (gene, (threshold * epsilon)) Graph.t
;;

let create = Graph.create;;
let add (brg:t) = Graph.add brg;;
let get (brg:t) = Graph.get brg;;
let iter f (brg:t) = Graph.iter f brg;;

let string_of_edge (t, e) = 
	string_of_int(t)^","^(match e with Activation -> "+" | Inhibition -> "-")
;;
let string_of_gene x = x;;

let to_dot (brg:t) = Graph.to_dot brg string_of_gene string_of_edge;;

let not_epsilon = function Activation -> Inhibition | Inhibition -> Activation;;

let spig_of_brg (brg:t) = let spig = Spig.create 1 in
	let proc_of_gene_level gene level = gene^string_of_int level
	and rate_name_of_gene_action g1 l1 g2 l2 =
		g1^(string_of_int l1)^g2^(string_of_int l2)
	in
	let rec xiter func a b = if b >= a then (func b; xiter func a (b-1))
	in
	let highest_t last ((t, epsilon), j) = max last t
	in
	let apply_action i ((t,epsilon), j) =
		let bi = List.fold_left highest_t 0 (get brg i)
		and bj = List.fold_left highest_t 0 (get brg j)
		and epsilon' e = if e < t then not_epsilon epsilon else epsilon
		in
		let op e = match epsilon' e with Activation -> 1 | Inhibition -> -1
		and apply_op op e maxe = min maxe (max 0 (e + op))
		in
		let make e f =
			let f' = apply_op (op e) f bj
			in
			print_endline (i^(string_of_int e)^"->"^j^(string_of_int f)^" = "^j^(string_of_int f'));
			let pie = proc_of_gene_level i e
			and pjf = proc_of_gene_level j f
			and pjf' = proc_of_gene_level j f'
			and rname = rate_name_of_gene_action i e j f
			in
			if pie = pjf && pie = pjf' then 
				Spig.add spig pie (Spig.Delay rname, pjf)
			else 
				(Spig.add spig pie (Spig.Take rname, pie);
				Spig.add spig pjf (Spig.Call rname, pjf'))
		in
		if i <> j then
			let minlevel e = match epsilon' e with Activation -> 0
												 | Inhibition -> 1
			and maxlevel e = match epsilon' e with Activation -> bj-1
												 | Inhibition -> bj
			in
			xiter (fun e -> xiter (make e) (minlevel e) (maxlevel e)) 0 bi
		else 
			xiter (fun e -> make e e) t bi
	in
	iter apply_action brg;
	spig


