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
let build_brg brg_spec =
	let brg = Brg.create 0 
	in
	List.iter (fun (a, t, e, b) -> Brg.add brg a ((t,e), b)) brg_spec;
	brg
;;

let show_deleted_channels = function [] -> print_endline "nothing to do."; []
	| chans -> 
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
	let chans = Spig.reach_only spi states start
	in
	show_deleted_channels chans
;;

let test_reachability stateg dest start =
	Graph.reachability stateg dest start
;;
let show_test_reachability stateg dest start =
	print_endline (">>> reachability "^(Spig.string_of_state dest)^" from "^Spig.string_of_state start);
	print_endline (string_of_bool (test_reachability stateg dest start))
;;

let test_reach_only dyn states start =
	let colors = Util.list_uniq (Graph.color_reachability dyn states)
	and all = Util.list_uniq (Graph.vertices dyn)
	in
	Util.list_sub all colors = []
;;
let show_test_reach_only dyn states start =
	print_endline (">>> "^(String.concat " or " (List.map Spig.string_of_state states))^" reached only by "^Spig.string_of_state start);
	print_endline (string_of_bool (test_reach_only dyn states start))
;;

let test_contains_path = Dynamic.contains_path 
;;
let show_test_contains_path dyn start substs name =
	print_endline (">>> contains "^name);
	print_endline (string_of_bool (test_contains_path dyn start substs))
;;
let show_test_paths dyn paths =
	print_endline ">>> registered paths";
	let paths = List.filter 
		(fun (name, state, substs) -> test_contains_path dyn state substs)
		paths
	in
	print_endline (String.concat " " (List.map (fun (a,_,_) -> a) paths));
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

let dump_dynamic dyn filename = 
	Util.dump_to_file filename (Spig.stateg_to_dot dyn)
;;

let dump_state_graph_for_decisions dec states filename =
	let dyn = Decision.dynamic_of_decisions dec states
	in
	let sg = Decision.state_graph_of_dynamic dyn
	in
	Util.dump_to_file filename (Graph.state_graph_to_dot sg)
;;

let show_decisions decisions = 
	print_endline "****** decisions *******";
	print_endline (String.concat "\n" 
		(List.map Decision.string_of_decision decisions));
	print_endline "**"
;;
let show_specification (constraints, properties) =
	print_endline "### SPEC ####";
	print_endline "-- constraints";
	print_endline (String.concat "\n"
		(List.map Decision.string_of_transition constraints));
	print_endline "-- properties";
	print_endline (String.concat "\n"
		(List.map Decision.string_of_transition properties));
	print_endline "##"
;;

let state_api_1_to_2 (s:Spig.pi_pname list) =
	List.map (fun (m,v) -> m,string_of_int v) s
;;
let subst_api_1_to_2 ((m,l),(m',l')) =
	assert (m = m');
	(m,string_of_int l), if l > l' then Decision.Dec else Decision.Inc
;;
let path_api_1_to_2 (init,substs) =
	(state_api_1_to_2 init), (List.map subst_api_1_to_2 substs)
;;

let show_path_rate r =
	print_endline (Decision.Polynome.to_string r)
;;

let show_path_rates_mapped decisions rs =
	let map = Decision.map_decisions decisions
	in
	List.iter (fun (d,x) -> print_endline 
					("# "^x^" <- "^Decision.string_of_decision d))
			map;
	List.iter (fun r -> print_endline (Decision.Polynome.to_mapped_string map r))
			rs
;;


