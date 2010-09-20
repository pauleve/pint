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

type action = Inc | Dec
;;

module ISet = Set.Make(struct type t = int let compare = compare end);;

module SMap = Map.Make(struct type t = string let compare = compare end);;
let smap_size smap = 
	let folder k v counter = counter + 1
	in
	SMap.fold folder smap 0
;;
let smap_elements smap =
	let folder k v els = (k,v)::els
	in
	SMap.fold folder smap []
;;

module IMap = Map.Make(struct type t = int let compare = compare end);;

module Domain =
struct
	type value = Empty | Full | S of ISet.t
	type t = ISet.elt * value

	let full ml = ml,Full
	let empty ml = ml,Empty
	let one l (ml,v) = assert (l <= ml);
		ml, if ml = 0 then Full else S (ISet.add l ISet.empty)

	let _fulllist ml = Util.range 0 ml
	let _fullset ml = 
		List.fold_right ISet.add (_fulllist ml) ISet.empty
	
	let mem l (ml,v) = match v with
		  Empty -> false
		| Full -> true
		| S set -> ISet.mem l set
	
	let add l (ml,v) = ml, match v with
		  Empty -> if ml = 0 then Full else S (ISet.singleton l)
		| Full -> Full
		| S set -> let set = ISet.add l set in
			if ISet.cardinal set = ml+1 then Full else S set
	
	let remove l (ml,v) = ml, match v with
		  Empty -> Empty
		| Full -> if ml = 0 then Empty else S (ISet.remove l (_fullset ml))
		| S set -> let set = ISet.remove l set
			in if ISet.is_empty set then Empty else S set
	
	let elements (ml,v) = match v with
		  Empty -> []
		| Full -> _fulllist ml
		| S set -> ISet.elements set
	
	let map f (ml,v) = match v with
		  Empty -> []
		| Full -> List.map f (_fulllist ml)
		| S set -> List.map f (ISet.elements set)

	let to_string (ml,v) = match v with
		  Empty -> "/"
		| Full -> "*"
		| S set -> String.concat "," 
			(List.map string_of_int (ISet.elements set))

	let subset (ml',v') (ml,v) = match v',v with
		  Empty,_ | _,Full -> true | Full,_ | _,Empty -> false
		| S s', S s -> ISet.subset s' s
	
	let cardinal (ml,v) = match v with
		  Empty -> 0
		| Full -> ml+1
		| S s -> ISet.cardinal s

	let is_empty (ml,v) = v = Empty

	let inter (ml',v') (ml,v) = assert (ml=ml'); ml, match v,v' with
		  Empty,_ | _,Empty -> Empty
		| Full,x | x,Full -> x
		| S s,S s' -> S (ISet.inter s s')
	
	let union (ml',v') (ml,v) = assert (ml=ml'); ml, match v,v' with
		  Empty,x | x,Empty -> x
		| Full,_ | _,Full -> Full
		| S s,S s' -> let set = ISet.union s s' in
			if ISet.cardinal set = ml+1 then Full else S set

	let equal (ml',v') (ml,v) = ml = ml' && (match v,v' with
		  Empty,Empty | Full,Full -> true
		| S s,S s' -> s = s'
		| _ -> false)
	
	let borders (ml,v) = match v with
		  Empty -> raise (Invalid_argument "Domain.borders with empty domain");
		| Full -> []
		| S s -> 
			let mark v = (v,ISet.mem v s)
			in
			let ms = List.map mark (_fulllist ml)
			in
			let rec build_border prec = function
				  [] -> []
				| (v,true)::q -> 
					(if prec then [] else [v-1,Inc]) @ build_border true q
				| (v,false)::q ->
					(if prec then [v,Dec] else []) @ build_border false q
			in
			build_border (snd (List.hd ms)) (List.tl ms)
	
	let from_list ml l = ml,
		if List.length l = ml+1 then Full
		else if List.length l = 0 then Empty
		else let s = List.fold_right ISet.add l ISet.empty in S s

end
;;

type metaproc = string
type state = int SMap.t

type t_reachability = Reach | NotReach | Inconc
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

type t_mdom = (ISet.elt * Domain.value) SMap.t
type t_mimpl = t_mdom list ImplyMap.t 
type t_mreach = t_mdom list IMap.t SMap.t list ImplyMap.t
;;

exception Not_satisfied;;

