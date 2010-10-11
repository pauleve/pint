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

open Debug;;
open Ph_types;;

let opt_method = ref "static"
and opt_args = ref []
in
let cmdopts = Ui.common_cmdopts @ Ui.input_cmdopts @ [
		("--method", Arg.Symbol (["static"; "static-old"],
				(fun x -> opt_method := x)),
			"Method");
	]
and usage_msg = "ph-reach [opts] <z> <l>"
and anon_fun arg = opt_args := !opt_args@[arg]
in
Arg.parse cmdopts anon_fun usage_msg;
let zl = match !opt_args with
	   [z;l] -> (z, int_of_string l)
	 | _ -> (Arg.usage cmdopts usage_msg; raise Exit)
in

let ph, state = Ph_util.parse !Ui.opt_channel_in
in
let nb_actions = Ph_op.ph_count_actions ph
in

let phname = !Ui.opt_filename_in
in
dbg ("# "^phname^": "^(string_of_int nb_actions)^" actions");
dbg ("# testing reachability of "^string_of_process zl^" from state "^string_of_state state);

(*
let hdepend, _ = Ph_verif.process_reachability_prepare ph zl state
in
Util.dump_to_file (phname^"-hdepend.dot") (Ph_verif.dot_from_hdepend hdepend)
*)

let decision = 
match !opt_method with
	| "static" -> Ph_reach.process_reachability ph zl state
	| "static-old" -> Ph_reach.process_reachability_old ph zl state
	| _ -> failwith "Unknown method."
in
print_endline (string_of_ternary decision)

