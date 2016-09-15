
open Debug

open PintTypes
open Ph_types

open LocalCausalityGraph
open AutomataNetwork

open An_reach
open An_reach_asp

open ASP_solver

let asp_ucont_lcg asp lcg =
	let edge_asp ?(instance=any_instance) n1 n2 =
		"oa_lcg("^instance^","^n1^","^n2^")"
	in
	let nodesol_asp obj asp (i, n) =
		let orig = sol_asp obj i
		in
		let children = lcg#childs n
		in
		match children with [] ->
			decl asp (edge_asp orig "top"^" :- "^edge_asp "_" orig)
		| _ -> 
		let asp = List.fold_left (fun asp ->
			(function NodeProc ai ->
				decl asp (edge_asp orig (ls_asp ai)^" :- "^edge_asp "_" orig)
			| _ -> asp)) asp children
		in
		decl asp ("oa_valid(G,"^orig^") :- "^
			String.concat ","
				(List.map (function NodeProc ai -> "oa_valid(G,"^ls_asp ai^")"
				| _ -> "") children))
	in
	let nodeobj_asp n asp = match n with
		  NodeObj obj ->
			let orig = obj_asp obj
			in
			let children = lcg#childs n
			in
			let sols = List.filter (function NodeSol _ | NodeSyncSol _ -> true | _ -> false) children
			in
			let isols = List.mapi (fun i s -> (i,s)) sols
			in
			(*
			let asp = List.fold_left (fun asp (i,s) ->
				decl asp (sol_asp obj i)) asp isols
			in*)
			let buf =
				match sols with
				  [] -> [edge_asp orig "bottom"]
				| _ ->
					List.mapi (fun i _ ->
						edge_asp orig (sol_asp obj i)) sols
			in
			let asp = List.fold_left (fun asp left ->
					decl asp (left^":- "^edge_asp "_" orig)) asp buf
			in
			List.fold_left (nodesol_asp obj) asp isols

		| _ -> asp
	in
	NodeSet.fold nodeobj_asp lcg#nodes asp


let asp_bifurcation_lcg asp (an, ctx) goal =
	(* build full lcg *)
	let cache = An_localpaths.create_cache ()
	in
	(*let usols = An_localpaths.MinUnordUnsyncSol.solutions cache an
	in*)
	let fctx = full_ctx an
	in
	let all_ls = PSet.elements (procs_of_ctx fctx)
	in
	let sols = An_localpaths.MinUnordSol.solutions cache an
	in
	let full_lcg =
		let lcg = new glc oa_glc_setup fctx all_ls sols make_unord_sol
		in
		lcg#set_auto_conts false;
		lcg#build;
		lcg
	in

	(* push local states definition *)
	let asp = List.fold_left (fun asp ai -> decl asp (ls_asp ai)) asp all_ls
	in
	(* reference boolean automata *)
	let asp = SSet.fold (fun a asp -> decl asp (bool_asp a)) (boolean_automata an) asp
	and trmap = Hashtbl.create (count_transitions an)
	in
	(* push transitions definition *)
	let register_transition (a,i,j) pstate (asp, trid) =
		if (a,i) = goal then (asp, trid) else
		let asp = decl asp ("tr("^string_of_int trid^","
			^automaton_asp a^","^string_of_int i^","^string_of_int j^")")
		in
		Hashtbl.add trmap trid ((a,i,j),pstate);
		SMap.fold (fun b k asp ->
			decl asp ("trcond("^string_of_int trid^","^
			automaton_asp b^","^string_of_int k^")")) pstate asp,
			(trid+1)
	in
	let asp = fst(Hashtbl.fold register_transition an.conditions (asp, 0))
	in
	(* push LCG for over-approximation *)
	let asp = asp_ucont_lcg asp full_lcg
	in
	(* push LCG for under-approximation *)
	let asp = asp_lcg asp full_lcg
	in
	(* push generic rules for under-approximation *)
	let asp = asp_unordua asp
	in
	(* push goal *)
	let asp = decl asp ("goal("^automaton_asp (fst goal)^","^string_of_int (snd goal)^")")
	in
	(* push initial state *)
	let asp = asp_ctx ~instance:"s0" asp ctx
	in
	asp, trmap

let bifurcations_solver inputs =
	ASP_solver.solver ~opts:"0 --project --conf=trendy" ~inputs ()

let regexp_trid = Str.regexp "^btr(\\([0-9]+\\)\\b"

let parse_solution trmap sol =
	assert(Str.string_match regexp_trid sol 0);
	let trid = int_of_string (Str.matched_group 1 sol)
	in
	Hashtbl.find trmap trid

let solve_bifurcations solver handler (an,ctx) goal =
	let solver, trmap = asp_bifurcation_lcg solver (an,ctx) goal
	in
	let handler sol =
		let aij,cond = parse_solution trmap sol
		in
		handler aij cond
	in
	ASP_solver.solutions solver handler

let aspf = ASP_solver.pint_asp_abspath

let ua_bifurcations_ua handler =
	let solver = bifurcations_solver [
			"-";
			aspf "bifurcations/ua_bifurcation.asp";
			aspf "bifurcations/sb_ua_reachability.asp"]
	in
	solve_bifurcations solver handler





