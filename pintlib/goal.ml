
module IntSet = Set.Make(struct type t = int let compare = compare end);;
module StringMap = Map.Make(struct type t = string let compare = compare end);;

module Domain =
struct
	type value = Empty | Full | S of IntSet.t
	type t = IntSet.elt * value

	let full ml = ml,Full
	let one l (ml,v) = assert (l <= ml);
		ml, if ml = 0 then Full else S (IntSet.add l IntSet.empty)

	let _fulllist ml = Util.range 0 ml
	let _fullset ml = 
		List.fold_right IntSet.add (_fulllist ml) IntSet.empty
	
	let mem l (ml,v) = match v with
		  Empty -> false
		| Full -> true
		| S set -> IntSet.mem l set
	
	let remove l (ml,v) = ml, match v with
		  Empty -> Empty
		| Full -> if ml = 0 then Empty else S (IntSet.remove l (_fullset ml))
		| S set -> let set = IntSet.remove l set
			in if IntSet.is_empty set then Empty else S set
	
	let map f (ml,v) = match v with
		  Empty -> []
		| Full -> List.map f (_fulllist ml)
		| S set -> List.map f (IntSet.elements set)

	let to_string (ml,v) = match v with
		  Empty -> "/"
		| Full -> "*"
		| S set -> String.concat "," 
			(List.map string_of_int (IntSet.elements set))

end
;;

type metaproc = string
type action = Inc | Dec
type rule = (int * action) * Domain.t StringMap.t list
type state = (metaproc * int) list
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

let string_of_vardom var dom = var^":"^Domain.to_string dom
;;
let string_of_mvar mvar = 
	let folder var dom sl = sl@[string_of_vardom var dom]
	in
	String.concat ";" (StringMap.fold folder mvar [])
;;
let string_of_dismvars mvars = String.concat " OR " (List.map
	(fun mvar -> "("^(string_of_mvar mvar)^")") mvars)
;;
let string_of_rule (m,l,a) mvars =
	m^(string_of_int l) ^
	(match a with Inc -> "+" | Dec -> "-") ^ " => " ^
	string_of_dismvars mvars
;;

let rules_of_decision mdom (m,i,a) =
	let mya = match a with Decision.Inc -> Inc | Decision.Dec -> Dec
			| _ -> failwith "decision cannot be Dis"
	in
	let rec folder mvar = function
		  Decision.L (a,i) -> let i = int_of_string i in
		  	let dom = Domain.one i (StringMap.find a mdom)
			in
			StringMap.add a dom mvar
		| Decision.Neg (Decision.L (a,i)) -> let i = int_of_string i in
			let dom = Domain.remove i (StringMap.find a mdom)
			in
			StringMap.add a dom mvar
		| Decision.Neg b -> folder mvar b
	in
	let mvar = List.fold_left folder StringMap.empty i
	in
	let myd = try StringMap.find m mvar with Not_found -> StringMap.find m mdom
	and mvar = StringMap.remove m mvar
	in
	Domain.map (fun l -> (m,l,mya),mvar) myd
;;

let rules_of_decisions mdom ds =
	let folder mrules (m,i,a) =
		let folder mrules (act, mvar) =
			let rules = try mvar::RuleMap.find act mrules
						with Not_found -> [mvar]
			in
			RuleMap.add act rules mrules
		and rules = rules_of_decision mdom (m,i,a)
		in
		List.fold_left folder mrules rules
	in
	List.fold_left folder RuleMap.empty ds
;;

let string_of_rules mrules =
	let folder k mvars sl =
		sl@[string_of_rule k mvars]
	in
	String.concat "\n" (RuleMap.fold folder mrules [])
;;


exception Found;;
let match_rule (m,l,a) state mrules =
	try
		let mvals = RuleMap.find (m,l,a) mrules
		in
		let match_mval mval =
			let match_domain var dom =
				if not (Domain.mem (List.assoc var state) dom) then
					raise Not_found
			in
			try StringMap.iter match_domain mval;
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
		let restrict_mval rval (m,l) =
			if StringMap.mem m mval then
				let dom = StringMap.find m mval
				in
				(if Domain.mem l dom then StringMap.add m (Domain.one l dom) rval
				else raise Not_found)
			else rval 
		in
		try
			let rval = List.fold_left restrict_mval StringMap.empty state
			in
			RuleMap.add rk (rval::RuleMap.find rk rules) rules
		with Not_found -> rules
	in
	List.fold_left folder rules mvals
;;

let merge_rules r1 r2 =
	let folder rk mvals r1 =
		RuleMap.add rk (mvals @ try RuleMap.find rk r1 with Not_found -> []) r1
	in
	RuleMap.fold folder r2 r1
;;


let check_reachability mdom mrules dest orig =
	(* TODO *)
	true
;;

let string_of_state state = 
	"("^(String.concat "," (List.map (fun (m,l) -> m^string_of_int l) state))^")"
;;
let responsible_rules =
let rec responsible_rules known mdom mrules goal (init:state) (goal':state) =
	let pstate_if_match (m,l',a) state =
		let pstate = (m,l')::List.remove_assoc m state
		in
		if match_rule (m,l',a) pstate mrules
		then [pstate,(m,l',a)] else []
	in
	let preds = List.flatten (List.map (fun (m,l) -> 
					(if l > 0 then pstate_if_match (m,l-1,Inc) goal else []) @
					if l < fst (StringMap.find m mdom) then pstate_if_match (m,l+1,Dec) goal else [])
					goal)
	in
	(** DEBUG **)
	print_endline "==== 1 ==== preds are : ";
	List.iter (fun (state,_) -> print_endline (string_of_state state)) preds;
	(** END DEBUG **)

	let folder (reach,nreach) (pred,rk) =
		if check_reachability mdom mrules goal' pred then
			(pred,rk)::reach, nreach
		else
			reach, (pred,rk)::nreach
	in
	let preds_ok, preds_ko = List.fold_left folder ([],[]) preds
	in
	let folder rrules (state,rk) =
		merge_rules rrules (matching_rules rk state mrules)
	in
	let rrules = List.fold_left folder RuleMap.empty preds_ok
	in
	let folder (rrules,known) (state,_) =
		if List.mem state known then
			(rrules,known)
		else
			let known = state::known
			in
			let rrules' = responsible_rules known mdom mrules state init goal'
			in
			merge_rules rrules rrules', known
	in
	fst (List.fold_left folder (rrules,goal::known) preds_ko)
in
responsible_rules []
;;

