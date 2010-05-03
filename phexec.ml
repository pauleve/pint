
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

