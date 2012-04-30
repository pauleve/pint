(*
Copyright or © or Copr. Maxime Folschette, Loïc Paulevé (2012)

maxime.folschette@irccyn.ec-nantes.fr
loic.pauleve@ens-cachan.org

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

open Debug;;
open Ph_types;;

let opt_dotfile = ref ""
and opt_asp = ref ""
and opt_format = ref "active"
and opt_enum = ref false
in
let cmdopts = Ui.common_cmdopts @ Ui.input_cmdopts @ [
	("--dot", Arg.Set_string  opt_dotfile, 
		"<graph.dot>\tExport the interaction graph to the given file.");
	("--debug-asp", Arg.Set_string  opt_asp, 
		"<file.lp>\tExport the generated ASP program.");
	("--format", Arg.Symbol (["active"; "AB"], (fun x -> opt_format := x)),
		("\tParameter format (default: "^ (!opt_format) ^")."));
	("--enumerate", Arg.Set opt_enum, "\tPerform parameterization enumeration.");
	]
and usage_msg = "ph2thomas [opts]"
in
let anon_fun _ = (Arg.usage cmdopts usage_msg; raise Exit)
in
Arg.parse cmdopts anon_fun usage_msg;;
let cmdopts = Ui.common_cmdopts @ Ui.input_cmdopts
and usage_msg = "ph-stable"
in
let anon_fun _ = (Arg.usage cmdopts usage_msg; raise Exit)
in
Arg.parse cmdopts anon_fun usage_msg;

let check_clingo () =
	if Sys.command "clingo -v" <> 0 then
		failwith ("The program 'clingo' is not in your path. Please read "
					^"http://process.wordpress.com/doc/install/#clingo")
in

check_clingo ();;


let ph, ctx = Ph_util.parse !Ui.opt_channel_in
in

let asp_data = Ph_translator.asp_of_ph ph ctx
in
print_string asp_data

