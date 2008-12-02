
module IntSet = Set.Make(struct type t = int let compare = compare end);;
module StringMap = Map.Make(struct type t = string let compare = compare end);;

module Domain =
struct
	type value = Empty | Full | S of IntSet.t
	type t = int * value

	let full ml = ml,Full
	let one l (ml,v) = assert (l <= ml);
		ml, if ml = 0 then Full else S (IntSet.add l IntSet.empty)

	let _fulllist ml = Util.range 0 ml
	let _fullset ml = 
		List.fold_right IntSet.add (_fulllist ml) IntSet.empty
	
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

