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

open Big_int;;

open Ph_types;;

let count_states (ps,hits) = 
	let counter c (_,la) = mult_int_big_int (la+1) c
	in
	List.fold_left counter unit_big_int ps
;;
let count_sorts (ps, _) = List.length ps
;;
let count_processes (ps, _) =
	let c acc (_,l) = acc + l + 1
	in
	List.fold_left c 0 ps
;;
let count_actions (_, hits) = Hashtbl.length hits;;

let opt_initial_procs = Ph_useropts.initial_procs;;
let parse channel_in =
	let lexbuf = Lexing.from_channel channel_in
	in
	let show_position () =
		let pos = Lexing.lexeme_start_p lexbuf
		in
		"Line " ^ string_of_int pos.Lexing.pos_lnum ^
		  " char " ^ string_of_int 
				(pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ^ ": "
	in
	try 
		let ph, ctx = Ph_parser.main Ph_lexer.lexer lexbuf
		in
		close_in channel_in;
		ph, ctx
	with Parsing.Parse_error ->
		failwith (show_position () ^ "Syntax error")
	| Failure msg ->
		failwith (show_position () ^ msg)
	| e -> (
		failwith (show_position () ^ Printexc.to_string e)
	)
;;


