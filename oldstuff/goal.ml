(*
Copyright or © or Copr. Loïc Paulevé, Morgan Magnin, Olivier Roux (2010)

loic.pauleve@irccyn.ec-nantes.fr
morgan.magnin@irccyn.ec-nantes.fr
olivier.roux@irccyn.ec-nantes.fr

This software is a computer program whose purpose is to provide Process
Hitting related tools.

This software is governed by the CeCILL license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the
CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author, the holder of the
economic rights, and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL license and that you accept its terms.
*)

open Types;;

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
	(match a with Inc -> "+" | Dec -> "-") ^ " <= " ^
	string_of_dismvars mvars
;;
let string_of_rules mrules =
	let folder k mvars sl =
		sl@[string_of_rule k mvars]
	in
	String.concat "\n" (RuleMap.fold folder mrules [])
;;
let string_of_reach = function
	true -> "Reach" | false -> "NotReach"
;;
let string_of_implication (reach, goal) mvals =
	(string_of_dismvars mvals)^" => "^(string_of_reach reach)^" "^
		string_of_state goal
;;
let string_of_implications mimpl =
	let folder k v sl =
		sl@[string_of_implication k v]
	in
	String.concat "\n" (ImplyMap.fold folder mimpl [])
;;
let string_of_impl (init, (reach, dest)) =
	(string_of_mvar init)^" => "^(string_of_reach reach)^" "^
		string_of_mvar dest
;;
let string_of_limpl limpl =
	String.concat "\n" (List.map string_of_impl limpl)
;;

let string_of_mlevel mlevel =
	let folder l mvars sl =
		let s = (string_of_int l)^
			if mvars = [] then "" else ("["^(string_of_dismvars mvars)^"]")
		in
		sl@[s]
	in
	String.concat "," (IMap.fold folder mlevel [])
;;

let string_of_mcond mcond =
	let folder m mlevel sl =
		let s = m^":"^string_of_mlevel mlevel
		in
		sl@[s]
	in
	"{"^(String.concat ";" (SMap.fold folder mcond []))^"}"
;;

let string_of_reach_conds (reach, goal) mconds =
	(string_of_reach reach)^" "^(string_of_state goal)^" <= \n\t\t"^
	String.concat "\n\t\t" (List.map string_of_mcond mconds)
;;
let string_of_mreach mreach =
	let folder k v sl =
		sl@[string_of_reach_conds k v]
	in
	String.concat "\n" (ImplyMap.fold folder mreach [])
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

let rec factorize_mvars mvars =
(**	print_endline ("### factorize "^string_of_dismvars mvars);	*)
	let folder (known,fmvars) mvar =
		if List.mem mvar known then known,fmvars else (
		let f_by_var var dom (known,fmvars) =
			let rvar = SMap.remove var mvar
			in
			let matching_mvar mvar' =
				if SMap.mem var mvar' then
					SMap.equal Domain.equal (SMap.remove var mvar') rvar
				else false
			in
			let matching = List.filter matching_mvar mvars
			in
			let folder mvar mvar' =
				let dom, dom' = SMap.find var mvar, SMap.find var mvar'
				in
				SMap.add var (Domain.union dom' dom) mvar
			in
			(*print_endline ("    "^(string_of_dismvars matching)^" are matching "^
				var^","^string_of_mvar rvar);*)
			match matching with 
				  [] | [_] -> known, fmvars
				| _ -> let nvar = List.fold_left folder mvar matching
					in
					(*print_endline ("      -> "^string_of_mvar nvar);*)
					known@matching, nvar::fmvars
		in
		SMap.fold f_by_var mvar (known,fmvars))
	in
	let known,fmvars = List.fold_left folder ([],[]) mvars
	in
	match fmvars with [] -> (*print_endline "*** factorize empty"; *)mvars
		| _ -> 
			let mvars' = fmvars@Util.list_sub mvars known
			in
			(*print_endline ("*** factorize "^(string_of_dismvars mvars)^" : "^string_of_dismvars mvars');*)
			factorize_mvars mvars'
;;
let implications_factorize =
	ImplyMap.map factorize_mvars
;;
let simplify_rules =
	RuleMap.map simplify_mvars 
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

let union_mvar mvar mvar' =
	let folder var dom mvar =
		try
			let dom' = SMap.find var mvar
			in
			SMap.add var (Domain.union dom dom') mvar
		with Not_found -> SMap.add var dom mvar
	in
	SMap.fold folder mvar' mvar
;;
let mvar_add_proc_level mdom (m,l) mvar =
	try
		SMap.add m (Domain.add l (SMap.find m mvar)) mvar
	with Not_found ->
		SMap.add m (Domain.one l (SMap.find m mdom)) mvar
;;

(** restrict mvar using rvar **)
let mvar_restrict rvar mvar =
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
	SMap.fold folder rvar mvar
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
	mvar_restrict mv mvar
	(*
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
	SMap.fold folder mv mvar*)
;;


(*
 * mvar
 *)

let mvar_matches state mvar =
	let folder m dom res = res &&
		let l = SMap.find m state
		in
		Domain.mem l dom
	in
	SMap.fold folder mvar true
;;

let substate sub state =
	let folder m l res = res &&
		(try SMap.find m state = l with Not_found -> false)
	in
	SMap.fold folder sub true
;;

(*
 * mvars
 *)

let mvars_matches state mvars =
	let folder res mvar =
		res || mvar_matches state mvar
	in
	List.fold_left folder false mvars
;;


(*
 * mcond
 *)

let mcond_matches state mcond =
	let folder m l res = res &&
		try
			let mlevel = SMap.find m mcond
			in
			(try 
				let mvars = IMap.find l mlevel
				in
				(match mvars with
					  [] -> true
					| _ -> mvars_matches state mvars)
			with Not_found -> false)
		with Not_found -> true
	in
	SMap.fold folder state true
;;



(*
 *
 *)
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

let nimplications_from_rules mdom mrules mimpl =
	let mrules = RuleMap.map factorize_mvars mrules
	in
	let folder k v msingles =
		match v with
		  [mvar] -> if smap_size mvar = 1 then RuleMap.add k v msingles
					else msingles
		| _ -> msingles
	in
	let msingles = RuleMap.fold folder mrules RuleMap.empty
	in

	let get_single_m_domain = function
		[mvar] -> List.hd (smap_elements mvar)
		| _ -> raise (Invalid_argument "get_single_m_domain on non-single rule")
	in

	let folder (m,l,a) mvars mimpl =
		let m',dm' = get_single_m_domain mvars
		and ml = fst (SMap.find m mdom)
		in
		let borders = Domain.borders dm'
		in
		let folder mimpl (l',a') =
			try
				let m'',dm = get_single_m_domain (RuleMap.find (m',l',a') msingles)
				in
				(if m'' <> m then raise Not_found);
				if Domain.mem l dm then
					mimpl
				else (
					let dm = Domain.one l dm
					and dm' = Domain.one l' dm'
					in
					let init = SMap.add m dm (SMap.add m' dm' SMap.empty)
					and df = match a with
						  Inc -> Util.range (l+1) ml
						| Dec -> Util.range 0 (l-1)
					in
					let folder mimpl l =
						implications_add (false,SMap.add m l SMap.empty)
							[init] mimpl
					in
					List.fold_left folder mimpl df
				)
			with Not_found -> mimpl
		in
		List.fold_left folder mimpl borders
	in
	RuleMap.fold folder msingles mimpl
;;

let implications_from_rules mdom mrules =
	let mimpl = nimplications_from_rules mdom mrules ImplyMap.empty
	in
	let folder (m,l,a) mvars impl =
		let mvars = List.map (fun mvar -> 
				SMap.add m (Domain.one l (SMap.find m mdom)) mvar) mvars
		and state = SMap.add m (match a with Inc -> l+1 | Dec -> l-1) SMap.empty
		in
		ImplyMap.add (true,state) (mvars@try ImplyMap.find (true,state) impl with Not_found->[]) impl
	in
	let mimpl = RuleMap.fold folder mrules mimpl
	in
	let mimpl = implications_saturate_1 mdom mimpl
	in
	implications_factorize mimpl
;;




let cond_set m l mvars mcond =
	let ml = try SMap.find m mcond with Not_found -> IMap.empty
	in
	let ml = IMap.add l mvars ml
	in
	SMap.add m ml mcond
;;

let mcond_for_reach_level mdom mimpl ctx m l mcond =
	let matching_mvars ctx =
		let conds = try ImplyMap.find (true, (SMap.add m l SMap.empty)) mimpl
			with Not_found -> []
		in
		let conds = simplify_mvars (filter_valuation_mvars ctx conds)
		in
		match conds with 
			  [] -> None
			| h::t -> if SMap.is_empty h then Some [] else Some conds
	in
	let folder mlevel l' =
		if l = l' then
			IMap.add l' [] mlevel
		else
			match matching_mvars (SMap.add m l' ctx) with
				  None -> mlevel
				| Some conds -> IMap.add l' conds mlevel
	in
	let levels = Domain.elements (SMap.find m mdom)
	in
	let mlevel = List.fold_left folder IMap.empty levels
	in
	SMap.add m mlevel mcond
;;

let mcond_for_last_step mdom mimpl dest ctx =
	(* convert context to mcond *)
	let folder m l mcond =
		SMap.add m (IMap.add l [] IMap.empty) mcond
	in
	let mcond = SMap.fold folder ctx SMap.empty
	in
	(* find resting meta-processes *)
	let ms = Util.list_sub (smap_keys dest) (smap_keys ctx)
	in
	let folder mcond m =
		let l = SMap.find m dest (* destination level *)
		in
		mcond_for_reach_level mdom mimpl ctx m l mcond
	in
	List.fold_left folder mcond ms
;;

let reachability dest orig mreach =
	try
		let mconds = ImplyMap.find (true,dest) mreach
		in
		let folder res mcond = res || mcond_matches orig mcond
		in
		let found = List.fold_left folder false mconds
		in
		if found then Reach else Inconc
	with Not_found -> Inconc
;;

let mreach_from_last_steps mdom (mimpl,mreach) state =
	let folder m l mconds =
		let ctx = SMap.add m l SMap.empty
		in
		mcond_for_last_step mdom mimpl state ctx::mconds
	in
	let mconds = SMap.fold folder state []
	in
	let mconds' = try ImplyMap.find (true,state) mreach with Not_found -> []
	in
	let mconds = Util.list_uniq (mconds@mconds')
	in
	ImplyMap.add (true,state) mconds mreach
;;

let check_reachability mdom (mimpl,mreach) dest orig =
	print_endline ("check_reachability of "^(string_of_state dest)^" from "^string_of_state orig);

	(* search for NotReach dest implication *)
	let has_notreach (reach,goal) mvars res = res ||
		not reach && substate goal dest && mvars_matches orig mvars
	in
	let notreach = ImplyMap.fold has_notreach mimpl false
	in
	if notreach then (
		print_endline ("-- found an implication saying no!");
		(mimpl,mreach), false
	) else (
		match reachability dest orig mreach with
		  Reach -> print_endline ("-- found a reachability saying yes!");
			(mimpl,mreach), true
		| NotReach -> print_endline ("-- found a reachability saying no!");
			(mimpl,mreach), false
		| Inconc ->
			print_endline "####### implications #######";
			print_endline (string_of_implications mimpl);
			print_endline "####### reachabilities #######";
			print_endline (string_of_mreach mreach);
			failwith "missing implications"
	)
;;

let responsible_rules =
let rec responsible_rules known mdom mrules (mimpl,mreach:t_mimpl*t_mreach) goal goal' =
	let mreach = mreach_from_last_steps mdom (mimpl,mreach) goal'
	in
	let mimpls = mimpl,mreach
	in
	let preds = state_predecessors mdom mrules goal
	in
	let folder (mimpls,reach,nreach) (pred,rk) =
		match check_reachability mdom mimpls goal' pred with
			  mimpls, true -> mimpls, (pred,rk)::reach, nreach
			| mimpls, false -> mimpls, reach, (pred,rk)::nreach
	in
	let mimpls,preds_ok,preds_ko = List.fold_left folder (mimpls,[],[]) preds
	in
	let folder rrules (state,rk) =
		merge_rules rrules (matching_rules rk state mrules)
	in
	let rrules = List.fold_left folder RuleMap.empty preds_ok
	in
	(*mimpls, rrules*)
	(* -- disabled until rest is done *)
	let folder ((mimpls,rrules),known) (state,_) =
		if List.mem state known then
			(mimpls,rrules),known
		else
			let known = state::known
			in
			let (mimpls,rrules'), known = 
				responsible_rules known mdom mrules mimpls state goal'
			in
			(mimpls, merge_rules rrules rrules'), known
	in
	List.fold_left folder ((mimpls,rrules),goal::known) preds_ko
	(**)
in
responsible_rules []
;;

(*
 * VERSION COLORIAGE DE GRAPHE
 *)
module StateMap = Map.Make(
struct
	type t = state
	let compare = SMap.compare compare
end
);;

let color_reachability mdom mrules goal mcolor =
	let rec color_reachability known state mcolor =
		if List.mem state known then known,mcolor
		else (
			let mcolor = StateMap.add state true mcolor
			in
			let preds = state_predecessors mdom mrules state
			in
			let folder (known,mcolor) (pred,rk) =
				color_reachability known pred mcolor
			in
			List.fold_left folder (state::known, mcolor) preds
		)
	in
	snd (color_reachability [] goal mcolor)
;;

let responsible_rules_by_graph mdom mrules goal goal' =
	let mcolor = color_reachability mdom mrules goal' StateMap.empty
	in
	let rec responsible_rules known state rules =
		if List.mem state known then known,rules
		else (
			let preds = state_predecessors mdom mrules state
			in
			let folder (known,rules) (pred,rk) =
				if StateMap.mem pred mcolor then
					let rules = merge_rules rules (matching_rules rk pred mrules)
					in
					known, rules
				else
					responsible_rules known pred rules
			in
			List.fold_left folder (state::known,rules) preds
		)
	in
	snd (responsible_rules [] goal RuleMap.empty)
;;


