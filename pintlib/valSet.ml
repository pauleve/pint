

open PintTypes

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
	

let match_cond a i v = not (SMap.mem a v) || SMap.find a v = i

let rec mdd_of_valset sd vs d =
	match vs with [] -> Bot
  	| vs ->  
		match d with [] -> Top
		| a::d ->
			let folder im i =
				let vs = List.filter (match_cond a i) vs
				in
				IMap.add i (mdd_of_valset sd vs d) im
			in
			let is = SMap.find a sd
			in
			let children = List.fold_left folder IMap.empty is
			in
			mknode a children

let rec valset_of_mdd = function
	  Bot -> []
	| Top -> [SMap.empty]
	| N n ->
		let a = n.var
		and nontops = IMap.filter (fun _ ni -> uid ni <> uid Top) n.children
		in
		let folder i n vs = vs @
			let vs = valset_of_mdd n
			in
			if IMap.cardinal nontops = 1 && IMap.mem i nontops then
				vs
			else
				List.map (fun v -> SMap.add a i v) vs
		in
		IMap.fold folder n.children []

let string_of_valset vs =
	"{" ^ (String.concat " ; " (List.map (string_of_map
		(fun (a,i) -> a^"="^string_of_int i) SMap.fold) vs))^"}"


module CCs = struct
	type t = {reprs: (string, string) Hashtbl.t;
				ccs: (string, SSet.t) Hashtbl.t}
	
	let create ?(n=10) ?(m=5) () =
		{reprs = Hashtbl.create n; ccs = Hashtbl.create m}

	let add t g =
		let crs = SSet.fold (fun a crs -> 
				try SSet.add (Hashtbl.find t.reprs a) crs 
					with Not_found -> crs) g SSet.empty
		in
		let r = SSet.min_elt g
		in
		let r = if SSet.is_empty crs then r else min r (SSet.min_elt crs)
		in
		let ug = SSet.fold (fun r ug ->
					SSet.union ug (Hashtbl.find t.ccs r)) 
						crs g
		and crs = SSet.remove r crs
		in
		Hashtbl.replace t.ccs r ug;
		SSet.iter (fun a -> Hashtbl.replace t.reprs a r) ug;
		SSet.iter (Hashtbl.remove t.ccs) crs
	
	let cardinal t =
		Hashtbl.length t.ccs
	
	let repr t a =
		Hashtbl.find t.reprs a
	
	let get t r =
		Hashtbl.find t.ccs r
end

let domain v = SMap.fold (fun a _ d -> SSet.add a d) v SSet.empty

let group_by_domain vs =
	let ccs = CCs.create ()
	in
	List.iter (fun v -> CCs.add ccs (domain v)) vs;
	let groups = Hashtbl.create (CCs.cardinal ccs)
	in
	let rs = List.fold_left (fun rs v -> 
				let a = fst (SMap.choose v)
				in
				let r = CCs.repr ccs a
				in
				Hashtbl.add groups r v;
				SSet.add r rs) SSet.empty vs;
	in
	List.map (fun r -> (CCs.get ccs r, Hashtbl.find_all groups r)) (SSet.elements rs)


module ValSetMap = Map.Make(struct type t = int SMap.t let compare = SMap.compare compare end)


let simplify ?(max_ite=1024) sd vs = match vs with [] | [_] -> vs | _ ->
	let rec simplify_group (d,vs) = if SSet.is_empty d then vs else
		match vs with [] | [_] -> vs | _ ->
		let perms = Util.stream_permutations (SSet.elements d)
		in
		let rec simplify_valset ?(c=0) vs = if c = max_ite then vs else
			try
				let d = Stream.next perms
				in
				let mdd = mdd_of_valset sd vs d
				in
				let vs = valset_of_mdd mdd
				in
				match vs with [] | [_] -> vs | _ ->
				match group_by_domain vs with
				  [_] -> simplify_valset ~c:(c+1) vs
				| gvs -> List.flatten (List.map simplify_group gvs)
			with Stream.Failure -> vs
		in
		simplify_valset vs
	in
	let gvs = group_by_domain vs
	in
	let vs = List.flatten (List.map simplify_group gvs)
	in
	let eliminate a ais vs =
		let vsa, vs = List.partition (SMap.mem a) vs
		in
		let project vm v =
			let i = SMap.find a v
			and v = SMap.remove a v
			in
			let is = try ValSetMap.find v vm with Not_found -> ISet.empty
			in
			let is = ISet.add i is
			in
			ValSetMap.add v is vm
		in
		let vm = List.fold_left project ValSetMap.empty vsa
		in
		let ni = List.length ais
		in
		let expand v is vs =
			if ISet.cardinal is = ni then
				v::vs
			else
				ISet.fold (fun i vs -> (SMap.add a i v)::vs) is vs
		in
		ValSetMap.fold expand vm vs
	in
	SMap.fold eliminate sd vs

let simplify_with_bse sd vs = match vs with [] | [_] -> vs | _ ->
	let rec simplify_group (d,vs) = if SSet.is_empty d then vs else
		match vs with [] | [_] -> vs | _ ->
		(* determine Boolean variables *)
		let fold_a a vars =
			let dom = SMap.find a sd
			in
			vars @
			if List.length dom = 2 then (
				assert (dom = [0;1] || dom = [1;0]);
				[(a,true,1)])
			else
				(List.map (fun i -> (a,false,i)) dom)
		in
		let vars = SSet.fold fold_a d []
		in
		(* build Bes.dnf_expression *)
		let bool_of_var cond (a,is_bool,i) =
			try
				let j = SMap.find a cond
				in
				if i = j then `True else `False
			with Not_found ->
				`Dontcare
		in
		let minterm_of_cond cond =
			List.map (bool_of_var cond) vars
		in
		let dnf = List.map minterm_of_cond vs
		in
		let dnf' = fst (Bes.auto_optimize dnf)
		in
		let dbg () =
			prerr_endline ("## simplify_with_bse");
			prerr_endline ("vs = "^(String.concat " OR "
					(List.map (string_of_map (fun (a,i) -> a^"="^string_of_int i) SMap.fold) vs)));
			prerr_endline ("vars = "^(String.concat "|"
					(List.map (fun (a,is_bool,i) ->
						if is_bool then a else (a^"="^string_of_int i)) vars)));
			prerr_endline (Bes.string_of_dnf_expression dnf);
			prerr_endline ("----");
			prerr_endline (Bes.string_of_dnf_expression dnf');
		in
		(* extract conds *)
		let cond_of_minterm mt =
			let fold_var cond (a,is_bool,i) = function
				  `Dontcare -> cond
				| `True ->
					if SMap.mem a cond then (dbg ();
						failwith (a^" is already set in cond"))
					else SMap.add a i cond
				| `False ->
					if is_bool then
						SMap.add a 0 cond
					else cond
					(*
					else (dbg ();
						failwith ("discrete variable set to false..."))*)
			in
			List.fold_left2 fold_var SMap.empty vars mt
		in
		List.map cond_of_minterm dnf'
	in
	let gvs = group_by_domain vs
	in
	List.flatten (List.map simplify_group gvs)



