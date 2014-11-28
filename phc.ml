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
(***
	Process Hitting Compiler
***)

open Ph_translator;;
open Ph_types;;

let languages = ["dump"; "spim"; "prism"; "prism_mdp"; "romeo"; "tina"; "pep"; 
					"biocham"; "kappa";"bn";"an"];;

let opt_language = ref "dump"
and opt_output = ref ""
and opt_romeo_ctl = ref ""
and opt_romeo_ctl_file = ref ""
and opt_coop_priority = ref false
and opt_ptnet_context = ref false
and opt_mapfile = ref ""
and opt_goal = ref ([] : process list)
in
let setup_goal opt =
	try
		let goal = [Ph_parser.process Ph_lexer.lexer (Lexing.from_string opt)]
		in
		opt_goal := goal
	with Parsing.Parse_error ->
		failwith ("Error when parsing process '"^opt^"'")
in
let cmdopts = Ui.common_cmdopts @ Ui.input_cmdopts @ [
		("-l", Arg.Symbol (languages, (fun l -> opt_language := l)), "\tOutput language");
		("-o", Arg.Set_string opt_output, "<filename>\tOutput filename");
		("--romeo-ctl", Arg.Set_string opt_romeo_ctl,
				"<proc>\tExport CTL formula for reachability of proc (romeo)");
		("--romeo-ctl-file", Arg.Set_string opt_romeo_ctl_file, 
				"<filename>\tfilename for CTL export (romeo)");
		("--coop-priority", Arg.Set opt_coop_priority, 
									"\tAssume hits on cooperative sorts of higher priority");
		("--contextual-ptnet", Arg.Set opt_ptnet_context, 
									"\tContextual petri net");
		("--mapfile", Arg.Set_string opt_mapfile, 
									"\tOutput mapping of identifiers");
		("--reduce-for-goal", Arg.String setup_goal, 
			"<a i>\tReduce the model to include only transitions that may "
			^ "be involved in the reachability of the given process");
	]
and usage_msg = "phc"
in
let anon_fun _ = (Arg.usage cmdopts usage_msg; raise Exit)
in
Arg.parse cmdopts anon_fun usage_msg;
let opts = {
	alpha = 0.05;
	round_fi = Param.round_fi_ex;
	coop_priority = !opt_coop_priority;
	contextual_ptnet = !opt_ptnet_context;
}
in
let languages = [
	("dump", dump_of_ph);
	("spim", spim_of_ph);
	("prism", prism_of_ph);
	("prism_mdp", prism_mdp_of_ph);
	("romeo", romeo_of_ph opts);
	("tina", tina_of_ph);
	("pep", pep_of_ph opts ~mapfile:!opt_mapfile);
	("biocham", biocham_of_ph);
	("kappa", kappa_of_ph);
	("bn", bn_of_ph);
	("an", an_of_ph opts);
]
in
let translator = List.assoc !opt_language languages
in

let ph, ctx = Ph_util.parse !Ui.opt_channel_in
in
let ph = if !opt_goal = [] then ph else
	Ph_reach.reduce_ph_for_goal ph ctx !opt_goal
in
let data = translator ph ctx
in
let channel_out = if !opt_output = "" then stdout else open_out !opt_output
in
output_string channel_out data;
close_out channel_out;

if !opt_language = "romeo" && !opt_romeo_ctl <> "" then
	let ai = 
		match Ph_parser.processlist Ph_lexer.lexer (Lexing.from_string !opt_romeo_ctl) with
		  [ai] -> ai
		| _ -> failwith "romeo/CTL: invalid proc specification"
	in
	let ctl = "EF ( P_"^romeo_pid ph ai^Ph_types.string_of_proc ai^" = 1 );"
	in
	Util.dump_to_file (!opt_romeo_ctl_file) ctl

