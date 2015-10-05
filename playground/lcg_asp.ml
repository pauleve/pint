
open PintTypes

open LocalCausalityGraph
open AutomataNetwork

open Ph_types

open An_cli
open An_reach

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts

let args, abort = An_cli.parse cmdopts ""

let an, ctx = An_cli.process_input ()

let goal = match args with
	  [str_ls] -> [An_cli.parse_local_state an str_ls]
	| _ -> abort ()


let env = init_env an ctx goal

let cache = env.sol_cache

let sols = An_localpaths.MinUnordUnsyncSol.solutions cache an

let uoa, oadom = unordered_oa' env sols

let sols = An_localpaths.MinUnordSol.filtered_solutions cache oadom an

let lcg = new glc (ua_lcg_setup env.an) ctx goal sols make_unord_sol

let automaton_asp a =
	"\""^a^"\""

let obj_asp (a,i,j) =
	"obj("^automaton_asp a^","^string_of_int i^","^string_of_int j^")"

let sol_asp obj n =
	"sol("^obj_asp obj^","^string_of_int n^")"

let ls_asp (a,i) =
	"ls("^automaton_asp a^","^string_of_int i^")"

let edge_asp n1 n2 =
	"edge("^n1^","^n2^")"

let init_asp a i =
	"init("^automaton_asp a^","^string_of_int i^")"

let indep_asp a i ls2 =
	"indep("^automaton_asp a^","^string_of_int i^","^ls_asp ls2^")"

let bool_asp a =
	"boolean("^automaton_asp a^")"

let asp_decl decl =
	print_endline (decl^".")

let nodesol_asp obj i n =
	let orig = sol_asp obj i
	in
	let children = lcg#childs n
	in
	List.iter (function NodeProc ai ->
		asp_decl (edge_asp orig (ls_asp ai)^" :- "^edge_asp "_" orig)
		| _ -> ()) children;
	match n with
	  NodeSyncSol (_, states, _) ->
	  	let asp_indep state =
			if SMap.cardinal state > 1 then
			let asp_indep_ls a i =
				SMap.iter (fun b j ->
					if b <> a then
						asp_decl (indep_asp a i (b,j))) state
			in
			SMap.iter asp_indep_ls state
		in
	  	StateSet.iter asp_indep states
	| _ -> ()


let nodeobj_asp n = match n with
	  NodeObj obj ->
	  	let orig = obj_asp obj
		in
	  	let children = lcg#childs n
		in
		let sols = List.filter (function NodeSol _ | NodeSyncSol _ -> true | _ -> false) children
		in
		let buf =
			match sols with [] -> "" | _ ->
				let asp_sols = List.mapi (fun i _ ->
					edge_asp orig (sol_asp obj i)) sols
				in
				"1 {"^(String.concat "; " asp_sols)^"} 1 "
		in
		(asp_decl (buf^":- "^edge_asp "_" orig);
		List.iteri (nodesol_asp obj) sols)

	| _ -> ()

let _ =
	SSet.iter (fun a -> asp_decl (bool_asp a)) (boolean_automata env.an);
	lcg#set_auto_conts false;
	lcg#build;
	lcg#saturate_ctx;
	NodeSet.iter nodeobj_asp lcg#nodes;
	SMap.iter (fun a is ->
		ISet.iter (fun i ->
			asp_decl (init_asp a i)) is) ctx;
	List.iter (fun ai ->
		asp_decl (edge_asp "root" (ls_asp ai))) env.goal


