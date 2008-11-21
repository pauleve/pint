
type gene = Decision.metaproc
type threshold = int
type epsilon = Activation | Inhibition

type t = (gene, (threshold * epsilon)) Graph.t
;;

let create = Graph.create;;
let add (brg:t) = Graph.add brg;;
let get (brg:t) = Graph.get brg;;
let genes (brg:t) = Graph.vertices brg;;
let iter f (brg:t) = Graph.iter f brg;;
let fold f (brg:t) = Graph.fold f brg;;

let string_of_edge (t, e) = 
	string_of_int(t)^","^(match e with Activation -> "+" | Inhibition -> "-")
;;
let string_of_gene x = x;;

let to_dot (brg:t) = Graph.to_dot brg string_of_gene string_of_edge;;

let not_epsilon = function Activation -> Inhibition | Inhibition -> Activation;;

let level_max (brg:t) gene = 
	let highest_t last ((t, epsilon), j) = max last t
	in
	List.fold_left highest_t 0 (get brg gene)
;;

let spig_of_brg (brg:t) = let spig = Spig.create 1 in
	let proc_of_gene_level gene level = (gene, level)
	and rate_name_of_gene_action g1 l1 g2 l2 =
		g1^(string_of_int l1)^g2^(string_of_int l2)
	in
	let rec xiter func a b = if b >= a then (func b; xiter func a (b-1))
	in
	let apply_action i ((t,epsilon), j) =
		let bi = level_max brg i
		and bj = level_max brg j
		and epsilon' e = if e < t then not_epsilon epsilon else epsilon
		in
		let op e = match epsilon' e with Activation -> 1 | Inhibition -> -1
		and apply_op op e maxe = min maxe (max 0 (e + op))
		in
		let make e f =
			let f' = apply_op (op e) f bj
			in
			let pie = proc_of_gene_level i e
			and pjf = proc_of_gene_level j f
			and pjf' = proc_of_gene_level j f'
			and rname = rate_name_of_gene_action i e j f
			in
			if i <> j then
				(Spig.add spig pie (Spig.Take rname, pie);
				Spig.add spig pjf (Spig.Call rname, pjf'))
			else
				if e <> f' then
					Spig.add spig pie (Spig.Delay ("d"^(Spig.string_of_pi_proc pie)), pjf')
		in
		if i <> j then
			let minlevel e = match epsilon' e with Activation -> 0
												 | Inhibition -> 1
			and maxlevel e = match epsilon' e with Activation -> bj-1
												 | Inhibition -> bj
			in
			xiter (fun e -> xiter (make e) (minlevel e) (maxlevel e)) 0 bi
		else 
			xiter (fun e -> make e e) 0 bi
	in
	iter apply_action brg;
	spig
;;

let constraints_of_k (brg:t) (spig:Spig.t) x rx k =
	let levels y =
		match List.filter (fun ((t,e), x') -> x = x') (get brg y) with
			  [] -> Util.range 0 (level_max brg y)
			| [((t,e),x')] ->
				let min_l, max_l = (match List.mem y rx, e with
					  true, Activation | false, Inhibition -> (t, level_max brg y)
					| true, Inhibition | false, Activation -> (0, t-1)
				) in
				Util.range min_l max_l
			| _ -> failwith "Invalid BRG (more than one action)"
	in
	let all_procs = List.map
		(fun y -> List.map (fun l -> y,l) (levels y))
		(genes brg)
	in
	let states = Util.cross_list all_procs
	in
	let level_from_proc (p,l) = l
	in
	let proc_from_state state = 
		match List.filter (fun (y,_) -> x = y) state with
			  [] -> raise Not_found
			| [p] -> p
			| _ -> failwith "Invalid state (duplicated gene type)"
	in
	let pred orig dest = 
		let lx = level_from_proc (proc_from_state orig)
		and lx' = level_from_proc (proc_from_state dest)
		in
		lx <= k && lx' < lx || lx >= k && lx' > lx
	in
	Util.list_uniq (List.flatten (List.map
		(Inference.never_transition_matching_from_state spig pred)
		states))
;;


