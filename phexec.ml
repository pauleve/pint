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

open Ui;;

open Ph_types;;

Random.self_init ();;
R.set_seed (Random.bits ()) (Random.bits ());;

let opt_args = ref []
in
let cmdopts = Ui.common_cmdopts
and usage_msg = "ph-exec [opts] <model.ph> <duration> <outputdir>"
and anon_fun arg = opt_args := !opt_args@[arg]
in
Arg.parse cmdopts anon_fun usage_msg;
let phname, duration, outputdir = match !opt_args with
	  [phname; duration; outputdir] -> phname, float_of_string duration, outputdir
	| _ -> (Arg.usage cmdopts usage_msg; raise Exit)
in


let ph, state = Ui.ph_load2 phname
in

(** plot **)
if not (Sys.file_exists outputdir) then
	Unix.mkdir outputdir 0o750
else if not (Sys.is_directory outputdir) then
	failwith ("Cannot create directory '"^outputdir^"'");

let create_pts a i apts = 
	let fd = open_out (outputdir^"/"^a^".pts")
	in
	SMap.add a fd apts
in
let apts = SMap.fold create_pts state SMap.empty
in

let plotter t (a,i) =
	let fd = SMap.find a apts
	in
	output_string fd (string_of_float t^" "^string_of_int i^"\n")
in

(* start plots *)
SMap.iter (fun a i -> plotter 0. (a,i)) state;

(* execute *)
let state = Ph_machine.execute (Ph_machine.create_env ph) state duration plotter;
in
(** stop plot **)
SMap.iter (fun a i -> plotter duration (a,i)) state

