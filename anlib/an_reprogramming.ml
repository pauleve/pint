open PintTypes
open Ph_types

open AutomataNetwork
open LocalCausalityGraph
open An_reach_asp

open ASP_solver

let aspf f = ASP_solver.pint_asp_abspath (Filename.concat "reprogramming" f)

(***
    ONESHOT MUTATIONS FOR GOAL CUT
***)

let ua_oneshot_mutations_for_cut (an, ctx) goal maxcard =
	let asp = ASP_solver.solver
        ~opts:"0 --project --conf=trendy --heuristic=domain --enum-mode=domRec --dom-mod=5,16"
        ~inputs:["-"; aspf "ua_oneshot_mutations_for_cut.lp"] ()
    in
	(* push local states definition *)
	let fctx = full_ctx an
	in
	let all_ls = PSet.elements (procs_of_ctx fctx)
	in
	let asp = List.fold_left (fun asp ai -> decl asp (ls_asp ai)) asp all_ls
	in
    let full_lcg = full_lcg an
    in
    let asp = An_bifurcations.asp_ucont_lcg asp full_lcg
    in
    let asp = asp_ctx ~instance:"s0" asp ctx
    in
	(* push goal *)
	let asp = decl asp ("goal("^automaton_asp (fst goal)^","^string_of_int (snd goal)^")")
	in
    let asp = decl asp ("#const maxcard = "^string_of_int maxcard)
    in
    let solutions = ASP_solver.all_solutions asp
    in
    let lss_of_solution sol =
        let solution = parse_answerset sol
        in
        let fold lss (f,args) =
            if f = "lock" then
                match args with
                  a::i::[] -> (a,int_of_string i)::lss
                | _ -> failwith "bad lock predicate"
            else lss
        in
        List.fold_left fold [] solution
    in
    List.map lss_of_solution solutions


