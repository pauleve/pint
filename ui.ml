(*
Copyright or © or Copr. Loïc Paulevé, Morgan Magnin, Olivier Roux (2012)

loic.pauleve@ens-cachan.org
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

open Ph_types;;

let setup_opt_initial_procs opt =
	try 
		let ps = Ph_parser.processlist Ph_lexer.lexer (Lexing.from_string opt)
		in
		Ph_util.opt_initial_procs := procs_of_ps ps
	with Parsing.Parse_error -> 
		failwith ("Parsing error for initial_state '"^opt^"'")
;;

let common_cmdopts = [
	("--no-debug", Arg.Clear Debug.dodebug, "Disable debugging");
	("--debug", Arg.Set Debug.dodebug, "Enable debugging");
	("--debug-level", Arg.Set_int Debug.debuglevel, "Maximum debug level");
	("--version", Arg.Unit (fun () ->
			print_endline ("Pint version "^Distenv.version);
			ignore(exit 0)), "Print Pint version and quit");
];;

let opt_channel_in = ref stdin;;
let opt_filename_in = ref "<stdin>";;
let setup_opt_channel_in filename =
	opt_filename_in := filename;
	opt_channel_in := open_in filename
;;
let input_cmdopts = [
	("-i", Arg.String setup_opt_channel_in, "<model.ph>\tInput filename");
	("--autoinit", Arg.Bool (fun b -> Ph_useropts.autoinit := Some b),
			"<true|false>\tAutomatically initialize cooperativities");
	("--initial-state", Arg.String setup_opt_initial_procs,
		"<process list>\tInitial state");
	("--initial-context", Arg.String setup_opt_initial_procs,
		"<process list>\tInitial context (equivalent to --initial-state)");
];;

let simple_input () =
	let opt_args = ref []
	in
	Arg.parse input_cmdopts (fun x -> opt_args := !opt_args@[x]) "usage";
	let ph, ctx = Ph_util.parse !opt_channel_in
	in
	(ph, ctx), !opt_args
;;

let rec proclist_from_stringlist = function [] -> []
	| a::i::tail -> (a, int_of_string i)::proclist_from_stringlist tail
	| _ -> raise (Invalid_argument "proclist_from_stringlist: input list length should be even")
;;

