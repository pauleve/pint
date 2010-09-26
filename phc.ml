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

let languages = ["dump"; "spim"; "prism"; "prism_mdp"; "romeo"; "tina"; "biocham"; "kappa"];;

let opt_language = ref "dump"
and opt_input = ref ""
and opt_output = ref ""
in
let cmdopts = Ui.common_cmdopts @ [
		("-l", Arg.Symbol (languages, (fun l -> opt_language := l)), "\tOutput language");
		("-i", Arg.Set_string opt_input, "\tInput filename");
		("-o", Arg.Set_string opt_output, "\tOutput filename");
	]
and usage_msg = "phc"
in
let anon_fun _ = (Arg.usage cmdopts usage_msg; raise Exit)
in
Arg.parse cmdopts anon_fun usage_msg;
let opts = {
	alpha = 0.05;
	round_fi = Param.round_fi_ex
}
in
let languages = [
	("dump", dump_of_ph);
	("spim", spim_of_ph);
	("prism", prism_of_ph);
	("prism_mdp", prism_mdp_of_ph);
	("romeo", romeo_of_ph opts);
	("tina", tina_of_ph);
	("biocham", biocham_of_ph);
	("kappa", kappa_of_ph);
]
in
let translator = List.assoc !opt_language languages
in

let channel_in = if !opt_input = "" then stdin else open_in !opt_input
in
let ph, init_state = Ph_util.parse channel_in
in
close_in channel_in;
let data = translator ph init_state
in
let channel_out = if !opt_output = "" then stdout else open_out !opt_output
in
output_string channel_out data;
close_out channel_out

