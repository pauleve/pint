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
(**
 Generic types
 TODO: move to commonlib
**)

module SSet = Set.Make (struct type t = string let compare = compare end);;
module SMap = Map.Make (struct type t = string let compare = compare end);;
module ISet = Set.Make (struct type t = int let compare = compare end);;
module IMap = Map.Make (struct type t = int let compare = compare end);;

let string_of_set
		?lbracket:(lb="{ ") ?rbracket:(rb=" }") ?delim:(dl=", ")
		string_of_element elements_getter set =
	let content = String.concat dl
		(List.map string_of_element (elements_getter set))
	in
	lb^content^rb
;;

let string_of_iset = string_of_set string_of_int ISet.elements
;;
	

(**
 Process Hitting types
**)

type ternary = True | False | Inconc;;
let string_of_ternary = function True -> "True" | False -> "False" | Inconc -> "Inconc";;

type sort = string
type sortidx = int
type process = sort * sortidx (* name * level *)
module PMap = Map.Make (struct type t = process let compare = compare end);;
module PSet = Set.Make (struct type t = process let compare = compare end);;

let string_of_proc (a,i) = a^"_"^string_of_int i;;
let pintstring_of_proc (a,i) = a^" "^string_of_int i;;

let string_of_procs = string_of_set string_of_proc PSet.elements;;

type stochatime = 
	  Instantaneous
	| RateSA of (float * int)
	| FiringInterval of (float*float*float) 

type rate = (float * int) option

let rsa_of_stochatime = function
	  Instantaneous -> None
	| RateSA (r, sa) -> Some (r,sa)
	| FiringInterval (d1, d2, cc) ->
		Some (Param.rsa_of_firinginterval (d1,d2) cc)
;;

type hits = (process, ((process * stochatime) * int)) Hashtbl.t

let procs_of_ps = 
	List.fold_left (fun procs ai -> PSet.add ai procs) PSet.empty 
;;

type ph = process list * hits

(* (a,i) * (b,j) * j' *)
type action = Hit of (process * process * int)

let hitter = function Hit (ai,_,_) -> ai;;
let target = function Hit (_,bj,_) -> bj;;
let bounce = function Hit (_,_,k) -> k;;
let bounce2 = function Hit (_,(b,_),k) -> (b,k);;

let string_of_action = function
	Hit (ai, bj, k) -> string_of_proc ai ^"->"^string_of_proc bj^" "^string_of_int k
;;
let pintstring_of_action = function
	Hit (ai, bj, k) -> pintstring_of_proc ai ^" -> "^pintstring_of_proc bj^" "^string_of_int k
;;

let string_of_actions actions =
	"{ "^(String.concat ", " (List.map string_of_action actions))^" }"
;;


(* STATE *)
type state = int SMap.t

let string_of_state ?show_zero:(show_zero=false) s =
	let folder a i str =
		if show_zero || i <> 0 then
			str ^ (if str <> "" then ";" else "")
			^ string_of_proc (a,i)
		else str
	in
	"["^(SMap.fold folder s "")^"]"
;;
let state0 = List.fold_left (fun s (a,_) -> SMap.add a 0 s) SMap.empty
;;

let merge_states dest orig =
	let apply a i dest =
		SMap.add a i dest
	in
	SMap.fold apply orig dest
;;
	
let merge_state_with_ps state =
	let apply state (a,i) =
		SMap.add a i state
	in
	List.fold_left apply state
;;
let state_value state a = try SMap.find a state with Not_found -> invalid_arg ("Unknown sort '"^a^"'")
;;
let list_of_state state =
	let folder a i ls = (a,i)::ls
	in
	SMap.fold folder state []
;;

(** Context *)
type ctx = ISet.t SMap.t

let ctx_equal = SMap.equal ISet.equal;;

let ctx_empty = SMap.empty;;

let ctx_get = SMap.find;;

let ctx_safe_get a ctx =
	try ctx_get a ctx with Not_found -> ISet.empty
;;

let ctx_has_proc (a,i) ctx = 
	try
		ISet.mem i (ctx_get a ctx)
	with Not_found -> false
;;

let ctx_add_proc (a,i) ctx =
	let is = ctx_safe_get a ctx
	in
	SMap.add a (ISet.add i is) ctx
;;

let ctx_rm_proc (a,i) ctx =
	try
		let is = ctx_get a ctx
		in
		let is = ISet.remove i is
		in
		SMap.add a is ctx
	with Not_found -> ctx
;;

let string_of_ctx ctx = 
	let folder a is str =
		str ^ (if str = "" then "" else "; ")
		^ a ^ "="^ string_of_iset is
	in
	"<"^(SMap.fold folder ctx "")^">"
;;

let procs_of_ctx ctx =
	let register a is ps =
		let register i ps =
			PSet.add (a,i) ps
		in
		ISet.fold register is ps
	in
	SMap.fold register ctx PSet.empty
;;

let procs_to_ctx ps =
	let group (a,i) ctx =
		let is = try SMap.find a ctx with Not_found -> ISet.empty
		in
		let is = ISet.add i is
		in
		SMap.add a is ctx
	in
	PSet.fold group ps SMap.empty
;;

let state_of_ctx ctx =
	let register a is state = 
		if ISet.cardinal is <> 1 then
			raise (Invalid_argument "state_of_ctx: given ctx is not a state")
		else
			SMap.add a (ISet.choose is) state
	in
	SMap.fold register ctx SMap.empty
;;

let ctx_override_by_ctx ctx ctx' =
	SMap.fold SMap.add ctx' ctx
;;

let ctx_override ctx ps =
	ctx_override_by_ctx ctx (procs_to_ctx ps)
;;

let ctx_union ctx1 ctx2 =
	let register a is2 ctx1 =
		let is1 = try SMap.find a ctx1 with Not_found -> ISet.empty
		in
		SMap.add a (ISet.union is1 is2) ctx1
	in
	SMap.fold register ctx2 ctx1
;;

let ctx_inter ctx1 ctx2 =
	let register a is2 ctx =
		try 
			let is1 = SMap.find a ctx1
			in
			SMap.add a (ISet.inter is1 is2) ctx
		with Not_found -> ctx
	in
	SMap.fold register ctx2 SMap.empty
;;

let ctx_of_state state =
	SMap.map ISet.singleton state
;;


(* Proc level couple set *)
module PCSet = Set.Make (struct type t = (process * process) 
		let compare (ai,bj) (ai',bj') = 
			let order_couple (x,y) =
				if compare x y <= 0 then (x,y) else (y,x)
			in
			let ai,bj = order_couple (ai,bj)
			and ai',bj' = order_couple (ai',bj')
			in
			let c = compare ai ai'
			in
			if c = 0 then compare bj bj' else c
		end)
;;

let ph_sigma = function (ps,_) -> List.map fst ps
;;
	
type t_directive = {
	mutable default_rate : float option;
	mutable default_sa : int;
	mutable sample : float
};;

let directive = {
	default_rate = None;
	default_sa = 1;
	sample = 1000.0
};;

(* Objectives *)

type objective = sort * sortidx * sortidx

module ObjOrd = struct 
	type t = objective 
	let compare = compare
end
module ObjSet = Set.Make (ObjOrd)
module ObjMap = Map.Make (ObjOrd)

let string_of_obj (a,i,j) = a^" "^string_of_int i^" "^string_of_int j;;

let obj_sort (a, _, _) = a;;
let obj_bounce (_, _, j) = j;;
let obj_target (_, i, _) = i;;
let obj_bounce_proc (a, _, j) = (a,j);;

let obj_reach s (a,i) = (a, state_value s a, i);;


