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

open PintTypes
open Ph_types

type opts = {
	alpha: float; (* 1-confidence *)
	round_fi: float * float -> int * int; (* firing interval rounding *)
	coop_priority: bool;
}

let prism_of_ph (ps,hits) ctx =
	let init_state = state_of_ctx ctx
	in
	let modname p = "proc_"^p
	and statemod p = p
	and hitcounter hitid = "c_"^string_of_int hitid
	and raz hitid = "d_"^string_of_int hitid
	and r_const hitid = "r_"^string_of_int hitid
	and sa_const hitid = "sa_"^string_of_int hitid
	in

	let module_of_proc (a,l_a) =
		let decl = (statemod a)^": [0.."^(string_of_int (max 1 l_a))^"] init "^
					(string_of_int (state_value init_state a))
					^"; // state"
		in
		(a, ([decl],[],[],[]))
	in
	let modules = List.map module_of_proc ps
	in

	let module_update modules (id,(decls,actions,chs,dhs)) =
		let _decls,_actions,_chs,_dhs = List.assoc id modules
		in
		(id, (_decls@decls,_actions@actions,_chs@chs,_dhs@dhs))
		::List.remove_assoc id modules
	in
	let modules_update = List.fold_left module_update
	in
	let string_of_module (a, (decls, actions,chs,dhs)) =
		let raz_c = List.map (fun ch -> "("^ch^"'=1)") chs
		and raz_d = List.map (fun h -> "("^raz h^"'="^hitcounter h^">1)") dhs
		in
		let reset_counters = String.concat " & " (raz_c@raz_d)
		in
		let apply = Str.global_replace (Str.regexp_string "%%") reset_counters
		in
		"module "^(modname a)^"\n"^
		"\t"^(String.concat "\n\t" decls)^"\n\n"^
		"\t"^apply (String.concat "\n\t" actions)^"\n\n"^
		"endmodule"
	in

	let prism_is_state a i = 
		statemod a^"="^string_of_int i
	and prism_set_state a i' =
		"("^statemod a^"'="^string_of_int i'^")"
	in

	let register_hit (b,j) (((a,i),stoch),k) (modules, (r_consts, sa_consts), hitid) =
		let rsa = rsa_of_stochatime stoch
		in
		let modules =
			let sa = match rsa with None -> 1 | Some (_,sa) -> sa
			in
			let str_r = r_const hitid ^ (if sa > 1 then "*"^sa_const hitid else "")^ ": "
			in
			if (a,i) = (b,j) then (
				let mod_a = 
					if sa = 1 then (
						[],
						["[] "^prism_is_state a i^" -> "^str_r^prism_set_state a k^";"],
						[],[]
					) else (
						let hc = hitcounter hitid
						in
						[hc ^": [1.."^sa_const hitid^"] init 1;"],
						["[] "^prism_is_state a i^" & "^hc^"<"^sa_const hitid^
								" -> "^str_r^"("^hc^"'="^hc^"+1);"
						;"[] "^prism_is_state a i^" & "^hc^"="^sa_const hitid^
								" -> "^str_r^prism_set_state a k^" & %%;"],
						[hc],[]
					)
				in
				modules_update modules [a,mod_a]
			) else (
				let sync = "[h_"^string_of_int hitid^"] "
				in
				let mod_a =
					if sa = 1 then (
						[],[sync^prism_is_state a i^" -> true;"],[],[]
					) else (
						[raz hitid^": bool init false;"],
						[sync^prism_is_state a i^" -> ("^raz hitid^"'=false);"],
						[],[hitid]
					)
				and mod_b =
					if sa = 1 then (
						[],
						[sync^prism_is_state b j^" -> "^str_r^prism_set_state b k^";"],
						[],[]
					) else (
						let hc = hitcounter hitid
						in
						[hc ^": [1.."^sa_const hitid^"] init 1;"],
						[
						sync^prism_is_state b j^" & "^raz hitid^" -> "^str_r^"("^hc^"'=2);";
						sync^prism_is_state b j^" & !"^raz hitid^" & "^hc^"<"^sa_const hitid^
								" -> "^str_r^"("^hc^"'="^hc^"+1);"
						;sync^prism_is_state b j^" & !"^raz hitid^" & "^hc^"="^sa_const hitid^
								" -> "^str_r^prism_set_state b k^" & %%;"],
						[hc],[]
					)
				in
				modules_update modules [a,mod_a;b,mod_b]
			)
		in
		let consts = match rsa with
		   	  None -> (hitid, -1.)::r_consts, sa_consts
			| Some (r, 1) -> (hitid, r)::r_consts, sa_consts
			| Some (r, sa) -> (hitid, r)::r_consts, (hitid, sa)::sa_consts
		in
		modules, consts, hitid + 1
	in
	let modules, (r_consts, sa_consts), _ = Hashtbl.fold register_hit hits (modules, ([],[]), 0)
	in

	let string_of_r_const (hitid, r) =
		"const double " ^ r_const hitid ^ " = "
			^ (if r < 0. then "infty" else Util.string_of_float0 r) ^ ";\n"
	and string_of_sa_const (hitid, sa) =
		"const int " ^ sa_const hitid ^ " = " 
			^ string_of_int sa ^ ";\n"
	in

	let header = "ctmc"
	in
	header ^ "\n\n" 
	^ (String.concat "" (List.map string_of_r_const r_consts)) ^ "\n"
	^ (String.concat "" (List.map string_of_sa_const sa_consts)) ^ "\n"
	^ (String.concat "\n\n" (List.map string_of_module modules))
	^ "\n\n"

let dump_of_ph (ps,hits) ctx =
(*	(String.concat "\n" (List.map (fun (pname, pvalue) -> 
	"directive "^pname^" "^pvalue) properties))
	^"\n\n"^ *)
	(String.concat "\n" (List.map (fun (a,i) ->
	"process "^a^" "^string_of_int i) ps))
	^"\n\n"^
	let string_of_rsa = function
		  Instantaneous -> " @ Inf"
		| RateSA (r,sa) -> " @ "^string_of_float r^" ~ "^string_of_int sa
		| FiringInterval (d1, d2, cc) -> " @ ["^string_of_float d1^";"^string_of_float d2^"]"
		(*| FiringInterval (d1, d2, cc) -> " @ ["^string_of_float d1^";"^string_of_float d2^"]#"^string_of_float cc*)
	in
	let string_of_hits (b,j) (((a,i),rsa),k) str =
		str^
		a^" "^string_of_int i^" -> "^b^" "^string_of_int j^" "^string_of_int k
		^string_of_rsa rsa
		^"\n"
	in
	Hashtbl.fold string_of_hits hits ""
	^"\n"
	^
	let procs = procs_of_ctx ctx
	in
	let is_state = SMap.for_all (fun a is -> ISet.cardinal is = 1) ctx
	in
	let procs = if is_state then PSet.filter (fun (a,i) -> i <> 0) procs else procs
	in
	if PSet.is_empty procs then "" else
		(if is_state then "initial_state " else "initial_context ")
		^ String.concat ", " (List.map pintstring_of_proc (PSet.elements procs))
	^"\n\n"

let romeo_pid (ps,_) (a,i) =
	let sorts = List.map fst ps
	in
	let sort_id a = Util.index_of a sorts
	in
	let base_id = let folder acc (a,i) =
			(i+1+List.hd acc)::acc
		in List.rev (List.fold_left folder [1] ps)
	in
	string_of_int (List.nth base_id (sort_id a) + i)

let romeo_of_ph opts (ps,hits) ctx =
	let init_state = state_of_ctx ctx
	in
	let sorts = List.map fst ps
	in
	let sort_id a = Util.index_of a sorts
	in
	let proc_id = romeo_pid (ps,hits)
	in
	let string_of_proc (a,i) =
		"<place id=\""^proc_id (a,i)^"\" label=\""^string_of_proc (a,i)^"\" "^
			" initialMarking=\""^(if state_value init_state a = i then "1" else "0")^"\">\n"^
			"\t<graphics><position x=\""^string_of_int (i*100+100)^"\" y=\""^string_of_int (100*sort_id a+100)^"\"/></graphics>\n"^
			"\t<scheduling gamma=\"0\" omega=\"0\"/>\n"^
		"</place>"

	and string_of_hit (b,j) (((a,i),stoch),k) ((hid, pids), str) =
		let dmin, dmax = match stoch with
			  Instantaneous -> "0", "0"
			| RateSA (r,sa) -> 
				let fi = Param.firing_interval opts.alpha r sa
				in
				let dmin, dmax = opts.round_fi fi
				in
				string_of_int dmin, string_of_int dmax
			| FiringInterval (d1, d2, _) ->
				let dmin, dmax = opts.round_fi (d1,d2)
				in
				string_of_int dmin, string_of_int dmax
		in
		let pid = 1 + try List.assoc (b,j) pids with Not_found -> 0
		and hlabel = string_of_proc (a,i)^"->"^string_of_proc (b,j)^" "^string_of_int k
		in
		(hid+1,	((b,j),pid)::List.remove_assoc (a,i) pids),
		str ^
		"<transition id=\""^string_of_int hid^"\" label=\""^hlabel^"\"  "^
			"eft=\""^dmin^"\" lft=\""^dmax^"\" "^
			"eft_param=\"a"^string_of_int hid^"\" lft_param=\"b"^string_of_int hid^"\" >\n"^
			"\t<graphics><position x=\""^string_of_int (j*100+50+100)^"\" "^
				"y=\""^string_of_int (100*sort_id b+pid*15-40+100)^"\"/>"^
				"<deltaLabel deltax=\"5\" deltay=\"5\"/>"^
			"</graphics>\n"^
		"</transition>\n"^
		"<arc place=\""^proc_id (b,j)^"\" transition=\""^string_of_int hid^"\" type=\"PlaceTransition\" weight=\"1\"/>\n"^
		(if a <> b then "<arc place=\""^proc_id (a,i)^"\" transition=\""^string_of_int hid^"\" type=\"read\" weight=\"1\"/>\n" else "")^
		"<arc place=\""^proc_id (b,k)^"\" transition=\""^string_of_int hid^"\" type=\"TransitionPlace\" weight=\"1\"/>"^
		"\n\n"
	in

	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<TPN>\n"
	(* places definitions *)
	^ (String.concat "\n\n" (List.flatten (List.map (fun (a,i) -> List.map (fun j -> string_of_proc (a,j)) (Util.range 0 i)) ps)))
	^"\n\n"
	(* hits *)
	^ snd (Hashtbl.fold string_of_hit hits ((1,[]), ""))
	^ "\n</TPN>\n"


let asp_of_ph (ps, hits) _ =
	(*** Écriture dans le fichier de sortie ***)
	"% Translation of the Process Hitting to ASP\n"
	(* Sortes *)
	^ "\n% Sorts\n"
	^ (String.concat "" (List.map (fun sort ->
				"sort(\"" ^ (fst sort) ^ "\"," ^ (string_of_int (snd sort)) ^ ").\n") ps))
	(* Frappes du PH *)
	^ "\n% Actions\n"
	^ Hashtbl.fold (fun target value buf -> let ((hit, _), bounce) = value in
		  buf ^ ("action(\"" ^ (fst hit) ^ "\"," ^ (string_of_int (snd hit)) ^ "," ^
		  "\"" ^ (fst target) ^ "\"," ^ (string_of_int (snd target)) ^ "," ^ (string_of_int bounce) ^
		  ").\n")) hits ""

let an_of_ph opts (ps, hits) ctx =
	let sd = List.fold_left (fun sd (a,l) -> SMap.add a (Util.range 0 l) sd)
				SMap.empty ps
	in
	let is_sort_cooperative a = 
		(try String.sub a 0 1 = "_" with Invalid_argument _ -> false) 
			|| SMap.mem a !Ph_instance.cooperativities
	in
	let an_of_proc (a,i) =
		"\""^a^"\"="^string_of_int i
	in
	let an_of_transition (b,j) (b,k) cond =
		let strans = "\""^b^"\" "^string_of_int j^" -> "^string_of_int k
		and procs = list_of_state cond
		in
		match procs with
		[] -> strans
		| _ -> 
			let scond = String.concat " and " (List.map an_of_proc procs)
			in
			strans ^ " when " ^ scond
	in
	let trs = Hashtbl.create (Hashtbl.length hits / 2)
	in
	let fold_action (b,j) (((a,i),_),k) an_transitions =
		if opts.coop_priority && is_sort_cooperative b then
			an_transitions
		else
		let ai, bj, bk = (a,i), (b,j), (b,k)
		in
		let conds =
			if opts.coop_priority && is_sort_cooperative a then
				let vs = Ph_cooperativity.local_fixed_points 
							!Ph_instance.cooperativities (ps, hits) ai
				in
				let filter_v v =
					SMap.filter (fun b _ -> not (is_sort_cooperative b)) v
				in
				List.map filter_v vs
			else if ai = bj then
				[SMap.empty]
			else
				[SMap.singleton a i]
		in
		Hashtbl.add trs (b,j,k) conds;
		ObjSet.add (b,j,k) an_transitions
	in
	let an_transitions = Hashtbl.fold fold_action hits ObjSet.empty
	in
	let folder (b,j,k) data = data @
		let vs = List.flatten (Hashtbl.find_all trs (b,j,k))
		in
		let vs = if opts.coop_priority then 
					ValSet.simplify sd vs else vs
		in
		List.map (an_of_transition (b,j) (b,k)) vs
	in
	let an_transitions = ObjSet.fold folder an_transitions []
	in
	let an_of_def (a,l) = 
		if opts.coop_priority && is_sort_cooperative a then "" else 
		("\""^a^"\" ["^
			(String.concat "," (List.map string_of_int (Util.range 0 l)))^"]\n")
	in
	let defs = List.map an_of_def ps
	in
	let procs = procs_of_ctx ctx
	in
	let procs = if opts.coop_priority then
				PSet.filter (fun (a,_) -> not (is_sort_cooperative a)) procs
				else procs
	in
	let procs = PSet.filter (fun (_,i) -> i > 0) procs
	in
	let procs = List.map an_of_proc (PSet.elements procs)
	in
	(String.concat "" defs) ^ "\n\n"
	^ (String.concat "\n" an_transitions) ^ "\n\n"
	^ if procs <> [] then ("initial_context " ^ (String.concat ", " procs) ^ "\n") else ""

