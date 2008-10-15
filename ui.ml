
let build_brg brg_spec =
	let brg = Brg.create 0 
	in
	List.iter (fun (a, t, e, b) -> Brg.add brg a ((t,e), b)) brg_spec;
	brg
;;

let show_deleted_channels chans =
	let chans = Util.list_uniq chans
	in
	print_endline ("deleted [\""^(String.concat "\";\"" (List.map Spig.string_of_transition chans))^"\"]");
	chans
;;

let spi_stable_state spi state =
	print_endline ("<<< stable "^Spig.string_of_state state);
	let chans = Spig.stable_state spi state
	in
	show_deleted_channels chans
;;

let spi_stable spi state substs =
	print_endline ("<<< stable "^Dynamic.string_of_substs Spig.string_of_state state substs);
	let chans = Spig.stable spi state substs
	in
	show_deleted_channels chans
;;

let spi_reach_only spi states start =
	print_endline ("<<< "^(Spig.string_of_state start)^" reaches only "^String.concat " or " (List.map Spig.string_of_state states));
	let chans_l = Spig.reach_only spi states start
	in
	let rec show_channels_list id = function
		  [] -> ()
		| chans::q ->
			print_endline (":: solution "^(string_of_int id));
			ignore(show_deleted_channels chans);
			show_channels_list (id+1) q
	in
	show_channels_list 1 chans_l;
	chans_l
;;

let test_reachability stateg dest start =
	Graph.reachability stateg dest start
;;
let show_test_reachability stateg dest start =
	print_endline (">>> reachability "^(Spig.string_of_state dest)^" from "^Spig.string_of_state start);
	print_endline (string_of_bool (test_reachability stateg dest start))
;;

let test_reach_only dyn states start =
	let colors = Util.list_uniq (Graph.color_reachability dyn states start)
	and all = Util.list_uniq (Graph.vertices dyn)
	in
	List.length colors = List.length all
;;
let show_test_reach_only dyn states start =
	print_endline (">>> "^(String.concat " or " (List.map Spig.string_of_state states))^" reached only by "^Spig.string_of_state start);
	print_endline (string_of_bool (test_reach_only dyn states start))
;;

let exists spi trace =
	print_endline (">>> exists "^(Inference.string_of_trace Spig.string_of_state trace));
	Inference.exists spi trace
;;

let proportion spi p trace =
	print_endline (">>> proportion "^(string_of_float p)^" "^(Inference.string_of_trace Spig.string_of_state trace));
	Inference.proportion spi p trace
;;

let make_spi spig constraints default_rate init_state =
	let valuation, equations = Valuation.valuation_of_constraints constraints
	in 
	assert (equations = []);
	Spig.spi_of_spig spig valuation default_rate init_state
;;

let print_constraints constraints =
	print_endline (Constraint.string_of_constraints Spig.string_of_rname constraints);
;;

let show_results constraints =
	print_constraints constraints;
	(try
		let valuation, equations = Valuation.valuation_of_constraints constraints
		in
			print_endline (Valuation.string_of_valuation Spig.string_of_rname valuation);
			if equations <> [] then (
				print_endline "TO SOLVE:";
				print_endline (String.concat "\n" (List.map (Valuation.string_of_equation Spig.string_of_rname) equations))
			)
	with Valuation.No_solution -> print_endline "NO SOLUTIONS!");
	print_endline ""
;;

let spig_of_k brg spig (d, r, k) =
    let ck = Brg.constraints_of_k brg spig d r k
	in
	print_string ("K"^d^"{"^(String.concat ";" r)^"} = "^(string_of_int k)^" => ");
	print_constraints ck;
	let spig2, cs = Spig.apply_constraints spig ck
	in
	spig2
;;

