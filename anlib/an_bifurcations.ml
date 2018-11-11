
open Debug

open PintTypes

open LocalCausalityGraph
open AutomataNetwork

open An_reach
open An_reach_asp

open ASP_solver
open Pn_mci

let asp_transitions asp trs =
    let asp_of_trstate pred trid state =
        String.concat ". " (List.map (fun (a,i) ->
            pred^"("^string_of_int trid^","
                ^string_of_int a^","^string_of_int i^")")
                (IMap.bindings state))
    in
    let asp_of_tr trid tr asp =
        let asp = decl asp ("tr("^string_of_int trid^")")
        in
        let asp = decl asp (asp_of_trstate "pre" trid tr.pre)
        in
        let asp = decl asp (asp_of_trstate "post" trid tr.dest)
        in
        decl asp (asp_of_trstate "post" trid tr.cond)
    in
    Hashtbl.fold asp_of_tr trs asp

let asp_ucont_lcg asp lcg =
	let edge_asp ?(instance=any_instance) n1 n2 =
		"oa_lcg("^instance^","^n1^","^n2^")"
	in
	let nodesol_asp obj asp (i, n) =
		let orig = sol_asp obj i
		in
		let children = lcg#children n
		in
		match children with [] ->
			decl asp (edge_asp orig "top"^" :- "^edge_asp "_" orig)
		| _ ->
		let asp = List.fold_left (fun asp ->
			(function NodeLS ai ->
				decl asp (edge_asp orig (ls_asp ai)^" :- "^edge_asp "_" orig)
			| _ -> asp)) asp children
		in
		decl asp ("oa_valid(G,"^orig^") :- "^
			String.concat ","
				(List.map (function NodeLS ai -> "oa_valid(G,"^ls_asp ai^")"
				| _ -> "") children))
	in
	let nodeobj_asp n asp = match n with
		  NodeObj obj ->
			let orig = obj_asp obj
			in
			let children = lcg#children n
			in
			let sols = List.filter (function NodeSol _ -> true | _ -> false) children
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

let asp_unfolding asp (an, ctx) =
	let ll = An_export.pep_of_an {An_export.contextual_ptnet = false} an ctx
	and mcif = Filename.temp_file "pint" ".mci"
	and netfile, netfile_out = Filename.open_temp_file "pint" ".ll"
	in
	at_exit (fun () -> Unix.unlink mcif; Unix.unlink netfile);
	output_string netfile_out ll;
	close_out netfile_out;
	let cmdline = "mole "^netfile^" -m "^mcif
	in
	(match Unix.system cmdline with
	| Unix.WEXITED 0 -> () | _ -> failwith ("mole exited badly."));
	let unf = load_mci mcif
	in
	let asp = Hashtbl.fold (fun e c asp ->
		decl asp ("edge(e"^string_of_int e^",c"^string_of_int c^")"))
			unf.preco asp
	in
	let asp = Hashtbl.fold (fun c e asp ->
		decl asp ("edge(c"^string_of_int c^",e"^string_of_int e^")"))
			unf.postco asp
	in
	let asp = Hashtbl.fold (fun e _ asp ->
		decl asp (":- e(e"^string_of_int e^")"))
			unf.cutoffs asp
	in
	let asp = Hashtbl.fold (fun c pl asp ->
		decl asp ("h(c"^string_of_int c^","^string_of_int pl^")"))
			unf.co2pl asp
	in
	fst (Array.fold_left (fun (asp, i) name ->
		(try
			let eqi = String.rindex name '='
			and l = String.length name
			in
			let a = String.sub name 0 eqi
			and j = String.sub name (eqi+1) (l-eqi-1)
			in
			decl asp ("name("^string_of_int i^",\""^a^"\","^j^")")
		with Not_found ->
			decl asp ("name(\""^name^"\")")), i+1)
		(asp, 1) unf.plname)

let asp_bifurcation_lcg asp ac (an, ctx) goal =
	(* build full lcg *)
	let full_lcg = full_lcg ac an
	in
	let fctx = full_ctx an
	in
	let all_ls = lsset_of_ctx fctx
	in
	(* push local states definition *)
	let asp = LSSet.fold (fun ai asp -> decl asp (ls_asp ai)) all_ls asp
	in
	(* reference boolean automata *)
	let asp = ISet.fold (fun a asp -> decl asp (bool_asp a)) (boolean_automata an) asp
	in
	(* push transitions definition *)
	let asp = asp_transitions asp an.trs
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
	asp

let bifurcations_solver inputs =
	ASP_solver.solver ~opts:"0 --project --conf=trendy" ~inputs ()

let regexp_trid = Str.regexp "^btr(\\([0-9]+\\)\\b"

let parse_solution sol =
	assert(Str.string_match regexp_trid sol 0);
	int_of_string (Str.matched_group 1 sol)

let solve_bifurcations solver handler ac (an,ctx) goal =
	let solver = asp_bifurcation_lcg solver ac (an,ctx) goal
	in
	ASP_solver.solutions solver (fun sol -> handler (parse_solution sol))

let aspf f = ASP_solver.pint_asp_abspath (Filename.concat "bifurcations" f)

let ua_bifurcations_ua handler =
	let solver = bifurcations_solver [
			"-";
			aspf "ua_bifurcation.asp";
			aspf "sb_ua_reachability.asp"]
	in
	solve_bifurcations solver handler

let ua_bifurcations_mole handler ac (an, ctx) =
	let solver = bifurcations_solver [
			"-";
			aspf "reachability.asp";
			aspf "ua_bifurcation.asp";
			aspf "sb_reachability.asp"]
	in
	let solver = asp_unfolding solver (an, ctx)
	in
	solve_bifurcations solver handler ac (an, ctx)


