
module IntSet = Set.Make(struct type t = int let compare = compare end);;
module SMap = Map.Make(struct type t = string let compare = compare end);;

module Domain =
struct
	type value = Empty | Full | S of IntSet.t
	type t = IntSet.elt * value

	let full ml = ml,Full
	let empty ml = ml,Empty
	let one l (ml,v) = assert (l <= ml);
		ml, if ml = 0 then Full else S (IntSet.add l IntSet.empty)

	let _fulllist ml = Util.range 0 ml
	let _fullset ml = 
		List.fold_right IntSet.add (_fulllist ml) IntSet.empty
	
	let mem l (ml,v) = match v with
		  Empty -> false
		| Full -> true
		| S set -> IntSet.mem l set
	
	let add l (ml,v) = ml, match v with
		  Empty -> if ml = 0 then Full else S (IntSet.singleton l)
		| Full -> Full
		| S set -> let set = IntSet.add l set in
			if IntSet.cardinal set = ml+1 then Full else S set
	
	let remove l (ml,v) = ml, match v with
		  Empty -> Empty
		| Full -> if ml = 0 then Empty else S (IntSet.remove l (_fullset ml))
		| S set -> let set = IntSet.remove l set
			in if IntSet.is_empty set then Empty else S set
	
	let elements (ml,v) = match v with
		  Empty -> []
		| Full -> _fulllist ml
		| S set -> IntSet.elements set
	
	let map f (ml,v) = match v with
		  Empty -> []
		| Full -> List.map f (_fulllist ml)
		| S set -> List.map f (IntSet.elements set)

	let to_string (ml,v) = match v with
		  Empty -> "/"
		| Full -> "*"
		| S set -> String.concat "," 
			(List.map string_of_int (IntSet.elements set))

	let subset (ml',v') (ml,v) = match v',v with
		  Empty,_ | _,Full -> true | Full,_ | _,Empty -> false
		| S s', S s -> IntSet.subset s' s
	
	let cardinal (ml,v) = match v with
		  Empty -> 0
		| Full -> ml+1
		| S s -> IntSet.cardinal s

	let is_empty (ml,v) = v = Empty

	let inter (ml',v') (ml,v) = assert (ml=ml'); ml, match v,v' with
		  Empty,_ | _,Empty -> Empty
		| Full,x | x,Full -> x
		| S s,S s' -> S (IntSet.inter s s')

end
;;

type metaproc = string
type action = Inc | Dec
type state = int SMap.t

type t_mvar = Domain.t SMap.t
type t_mvars = t_mvar list
;;

module RuleMap = Map.Make(struct type t = metaproc*int*action
	let compare (m,l,t) (m',l',t') =
		let c = compare m m' in
		if c <> 0 then c else (
		let c = compare l l' in
		if c <> 0 then c else (match t,t' with
			  Inc,Inc | Dec,Dec -> 0
			| Inc,Dec -> 1 | Dec,Inc -> -1
		)
		)
end)
;;

module ImplyMap = Map.Make(
struct
	type t = bool * state
	let compare (b,s) (b',s') =
		let c = compare b b' in if c <> 0 then c
		else SMap.compare compare s s'
end)
;;

exception Not_satisfied;;

let string_of_state state = 
	let folder m l sl =
		sl@[m^string_of_int l]
	in
	"("^(String.concat "," (SMap.fold folder state []))^")"
;;
let string_of_vardom var dom = var^":"^Domain.to_string dom
;;
let string_of_mvar mvar = 
	let folder var dom sl = sl@[string_of_vardom var dom]
	in
	String.concat ";" (SMap.fold folder mvar [])
;;
let string_of_dismvars mvars = String.concat " OR " (List.map
	(fun mvar -> "("^(string_of_mvar mvar)^")") mvars)
;;
let string_of_rule (m,l,a) mvars =
	m^(string_of_int l) ^
	(match a with Inc -> "+" | Dec -> "-") ^ " => " ^
	string_of_dismvars mvars
;;
let string_of_rules mrules =
	let folder k mvars sl =
		sl@[string_of_rule k mvars]
	in
	String.concat "\n" (RuleMap.fold folder mrules [])
;;
let string_of_implication (reach, goal) mvals =
	let string_of_reach = function
		true -> "Reach" | false -> "NotReach"
	in
	(string_of_dismvars mvals)^" => "^(string_of_reach reach)^" "^
		string_of_state goal
;;
let string_of_implications mimpl =
	let folder k v sl =
		sl@[string_of_implication k v]
	in
	String.concat "\n" (ImplyMap.fold folder mimpl [])
;;

let mvar_is_useless mvar =
	let is_surmvar mvar' =
		if mvar = mvar' then false else
		let folder var dom res =
			if res then
				try
					let dom' = SMap.find var mvar
					in
					Domain.subset dom' dom
				with Not_found -> false
			else false
		in
		SMap.fold folder mvar' true
	in
	let folder res mvar' =
		if not res then is_surmvar mvar' else true
	in
	List.fold_left folder false
;;

let simplify_mvars mvars =
	let folder mvars' mvar =
		if mvar_is_useless mvar mvars then mvars'
		else mvar::mvars'
	in
	List.fold_left folder [] (Util.list_uniq mvars)
;;

let simplify_rules mrules =
	RuleMap.map simplify_mvars mrules
;;

let rules_of_decision mdom (m,i,a) =
	let mya = match a with Decision.Inc -> Inc | Decision.Dec -> Dec
			| _ -> failwith "decision cannot be Dis"
	in
	let rec folder mvar = function
		  Decision.L (a,i) -> let i = int_of_string i in
		  	let dom = Domain.one i (SMap.find a mdom)
			in
			SMap.add a dom mvar
		| Decision.Neg (Decision.L (a,i)) -> let i = int_of_string i in
			let dom = Domain.remove i (SMap.find a mdom)
			in
			SMap.add a dom mvar
		| Decision.Neg b -> folder mvar b
	in
	let mvar = List.fold_left folder SMap.empty i
	in
	let myd = try SMap.find m mvar with Not_found -> SMap.find m mdom
	and mvar = SMap.remove m mvar
	in
	Domain.map (fun l -> (m,l,mya),mvar) myd
;;

let rules_of_decisions mdom ds =
	let folder mrules (m,i,a) =
		let folder mrules (act, mvar) =
			let mvars = try RuleMap.find act mrules with Not_found -> []
			in
			RuleMap.add act (mvar::mvars) mrules
		and rules = rules_of_decision mdom (m,i,a)
		in
		List.fold_left folder mrules rules
	in
	let mrules = List.fold_left folder RuleMap.empty ds
	in
	simplify_rules mrules
;;


exception Found;;
let match_rule (m,l,a) state mrules =
	try
		let mvals = RuleMap.find (m,l,a) mrules
		in
		let match_mval mval =
			let match_domain var dom =
				if not (Domain.mem (SMap.find var state) dom) then
					raise Not_found
			in
			try SMap.iter match_domain mval;
				raise Found
			with Not_found -> ()
		in
		try List.iter match_mval mvals;
			raise Not_found
		with Found -> true
	with Not_found -> false
;;

let matching_rules rk state mrules =
	let mvals = RuleMap.find rk mrules
	in
	let rules = RuleMap.add rk [] RuleMap.empty
	in
	let folder rules mval =
		let restrict_mval m l rval =
			if SMap.mem m mval then
				let dom = SMap.find m mval
				in
				(if Domain.mem l dom then SMap.add m (Domain.one l dom) rval
				else raise Not_found)
			else rval 
		in
		try
			let rval = SMap.fold restrict_mval state SMap.empty
			in
			RuleMap.add rk (rval::RuleMap.find rk rules) rules
		with Not_found -> rules
	in
	List.fold_left folder rules mvals
;;

let merge_rules r1 r2 =
	let folder rk mvals r1 =
		let mvals = mvals @ try RuleMap.find rk r1 with Not_found -> []
		in
		RuleMap.add rk (simplify_mvars mvals) r1
	in
	RuleMap.fold folder r2 r1
;;




let rec filter_valuation_mvars valuation = function [] -> []
	| mvar::q ->
		let folder m l mvar =
			try
				let dom = SMap.find m mvar
				in
				if Domain.mem l dom then SMap.remove m mvar
				else raise Not_satisfied
			with Not_found -> mvar
		in
		try 
			let mvar = SMap.fold folder valuation mvar
			in
			mvar::filter_valuation_mvars valuation q
		with Not_satisfied -> filter_valuation_mvars valuation q
;;

let smap_intersection s s' =
	let folder k v s =
		try
			assert (SMap.find k s' = v);
			SMap.add k v s
		with _ -> s
	in
	SMap.fold folder s SMap.empty
;;
let smap_keys s = 
	let folder k v l =
		k::l
	in
	SMap.fold folder s []
;;

let satisfy_reach reach mimpl dest orig =
	try
		let mvars = ImplyMap.find (reach,dest) mimpl
		in
		filter_valuation_mvars orig mvars <> []
	with Not_found -> false
;;

let state_predecessors mdom mrules state =
	let pstate_if_match (m,l',a) state =
		let pstate = SMap.add m l' state
		in
		if match_rule (m,l',a) pstate mrules
		then [pstate,(m,l',a)] else []
	in
	let folder m l preds =
		preds @ 
		(if l > 0 then pstate_if_match (m,l-1,Inc) state else []) @
		if l < fst (SMap.find m mdom) then pstate_if_match (m,l+1,Dec) state else []
	in
	SMap.fold folder state []
;;

let filter_mvars state mvars =
	let folder mvars mvar =
		let folder m l mvar =
			try
				let dom = SMap.find m mvar
				in
				if Domain.mem l dom then
					let dom = Domain.one l dom
					in
					SMap.add m dom mvar
				else raise Not_satisfied
			with Not_found -> raise Not_satisfied
		in
		try
			let mvar = SMap.fold folder state mvar
			in
			mvar::mvars
		with Not_satisfied -> mvars
	in
	List.fold_left folder [] mvars
;;

let implications_add k lv' mimpl =
	let lv = try ImplyMap.find k mimpl with Not_found -> []
	in
	ImplyMap.add k (simplify_mvars (lv@lv')) mimpl
;;

let mvar_replace_proc_by_mvar (m,l) mv mvar =
	(* 1. remove (m,l) from mvar, check it *)
	let mvar = try
		let dom = SMap.find m mvar
		in
		if Domain.cardinal dom = 1 && Domain.mem l dom then
			SMap.remove m mvar
		else raise Not_satisfied
	with Not_found -> raise Not_satisfied
	in

	(* 2. conjonction with mv, check for empty domains *)
	let folder m dom mvar =
		let dom = try
				let dom = Domain.inter (SMap.find m mvar) dom
				in
				if Domain.is_empty dom then raise Not_satisfied
				else dom
			with Not_found -> dom
		in
		SMap.add m dom mvar
	in
	SMap.fold folder mv mvar
;;

let implications_saturate_1 mdom mimpl =
	let saturate mimpl m =
		let max_l = fst (SMap.find m mdom)
		in
		if max_l < 2 then mimpl else (
		let folder incr mimpl l =
			let l1 = if incr then l-1 else l+1
			in
			let v1 = SMap.add m l1 SMap.empty
			and v2 = SMap.add m l SMap.empty
			in
			let c1 = filter_mvars v1 (ImplyMap.find (true,v2) mimpl)
			in
			let folder mimpl l0 =
				let c0 = filter_mvars (SMap.add m l0 SMap.empty) 
					(ImplyMap.find (true,v1) mimpl)
				in
				let folder mimpl = function
					[c;c'] -> (try
							let c = mvar_replace_proc_by_mvar (m,l1) c' c
							in
							implications_add (true,v2) [c] mimpl
						with Not_satisfied -> mimpl)
					| _ -> raise (Invalid_argument "saturate1/folder")
				in
				List.fold_left folder mimpl (Util.cross_list [c0;c1])
			in
			let r = if incr then Util.range 0 (l-2) else Util.range 2 max_l
			in
			List.fold_left folder mimpl r
		in
		let mimpl = List.fold_left (folder true) mimpl (Util.range 2 max_l)
		in
		List.fold_left (folder false) mimpl (Util.rrange 0 (max_l-2))
		)
	in
	List.fold_left saturate mimpl (smap_keys mdom)
;;

let implications_from_rules mdom mrules =
	let folder (m,l,a) mvars impl =
		let mvars = List.map (fun mvar -> 
				SMap.add m (Domain.one l (SMap.find m mdom)) mvar) mvars
		and state = SMap.add m (match a with Inc -> l+1 | Dec -> l-1) SMap.empty
		in
		ImplyMap.add (true,state) (mvars@try ImplyMap.find (true,state) impl with Not_found->[]) impl
	in
	let mimpl = RuleMap.fold folder mrules ImplyMap.empty
	in
	implications_saturate_1 mdom mimpl
;;

let implications_for_single_context mdom mrules mimpl dest context =
	let mdom_add_value m l md =
		let curdom = try SMap.find m md
			with Not_found -> Domain.empty (fst (SMap.find m mdom))
		in
		let curdom = Domain.add l curdom
		in
		SMap.add m curdom md
	in
	let r1 ctx m l (md_ok,l_conds) = (* reach (m,l) from any (m,l') *)
		let r1' ctx =
			let conds = try ImplyMap.find (true, (SMap.add m l SMap.empty)) mimpl
				with Not_found -> []
			in
			let conds = simplify_mvars (filter_valuation_mvars ctx conds)
			in
			match conds with
				  [] -> false, []
				| h::t ->
					if SMap.is_empty h then true, []
					else true, h::t
		in
		let folder (md_ok,l_conds) l' =
			if l = l' then
				mdom_add_value m l md_ok, l_conds
			else
				match r1' (SMap.add m l' ctx) with
					  false, _ -> md_ok, l_conds
					| true, [] -> mdom_add_value m l' md_ok, l_conds
					| true, conds -> md_ok, (m,(l',conds))::l_conds
		in
		let m_dom = SMap.find m mdom
		in
		List.fold_left folder (md_ok,l_conds) (Domain.elements m_dom)
	in
	let fold_common m l mimpl =
		let ms = Util.list_remove m (smap_keys dest)
		and ctx = SMap.add m l SMap.empty
		in
		let folder (md_ok,l_conds) m = 
			r1 ctx m (SMap.find m dest) (md_ok,l_conds)
		in
		let md_ok,l_conds = List.fold_left folder (SMap.empty,[]) ms
		in
		if not (SMap.is_empty md_ok) then
			let md_ok = mdom_add_value m l md_ok
			in
			implications_add (true,dest) [md_ok] mimpl
		else
			mimpl
	in
	SMap.fold fold_common context mimpl
;;

let check_reachability mdom mrules mimpl dest orig =
	print_endline ("check_reachability of "^(string_of_state dest)^" from "^string_of_state orig);
	if satisfy_reach true mimpl dest orig then (
		print_endline ("-- found an implication saying yes!");
		mimpl, true
	) else if satisfy_reach false mimpl dest orig then (
		print_endline ("-- found an implication saying no!");
		mimpl, false
	) else (
	(*
	let common = smap_intersection dest orig
	in
	print_endline ("- common parts = "^string_of_state common);
	if not (SMap.is_empty common) then (
	*)
		let mimpl = implications_for_single_context mdom mrules mimpl dest dest
		in
		if satisfy_reach true mimpl dest orig then (
			print_endline ("-- found an implication saying yes!");
			mimpl, true
		) else if satisfy_reach false mimpl dest orig then (
			print_endline ("-- found an implication saying no!");
			mimpl, false
		) else (
			print_endline ("!! found no implication... giving up.");
			mimpl, false
		)
		(*
	) else failwith "not implemented: check_reachability without commmon parts"
	*)
	)
;;

let responsible_rules =
let rec responsible_rules known mdom mrules impl goal (init:state) goal' =
	let preds = state_predecessors mdom mrules goal
	in
	let folder (impl,reach,nreach) (pred,rk) =
		let impl, doesreach = check_reachability mdom mrules impl goal' pred
		in
		if doesreach then
			impl, (pred,rk)::reach, nreach
		else
			impl, reach, (pred,rk)::nreach
	in
	let impl, preds_ok, preds_ko = List.fold_left folder (impl,[],[]) preds
	in
	let folder rrules (state,rk) =
		merge_rules rrules (matching_rules rk state mrules)
	in
	let rrules = List.fold_left folder RuleMap.empty preds_ok
	in
	let folder ((impl,rrules),known) (state,_) =
		if List.mem state known then
			(impl,rrules),known
		else
			(* -- disabled until rest is done
			let known = state::known
			in
			let impl, rrules' = responsible_rules known mdom mrules impl state init goal'
			in
			(impl, merge_rules rrules rrules'), known
			*)
			(impl,rrules),known
	in
	fst (List.fold_left folder ((impl,rrules),goal::known) preds_ko)
in
responsible_rules []
;;

