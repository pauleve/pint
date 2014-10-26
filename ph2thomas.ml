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
open PintTypes;;
open Ph_types;;

open InteractionGraph;;

let opt_dotfile = ref ""
and opt_igfile = ref ""
and opt_asp = ref ""
and opt_format = ref "active"
and opt_enum = ref false
and opt_fullenum = ref false
and opt_test = ref false
in
let cmdopts = Ui.common_cmdopts @ Ui.input_cmdopts @ [
	("--dot", Arg.Set_string  opt_dotfile, 
		"<graph.dot>\tExport the interaction graph to the given file.");
	("--debug-asp", Arg.Set_string  opt_asp, 
		"<file.lp>\tExport the generated ASP program.");
	("--ig", Arg.Set_string opt_igfile,
		"<graph.ig>\tUse the interaction graph from given file instead of inference");
	("--format", Arg.Symbol (["active"; "AB"; "iter"], (fun x -> opt_format := x)),
		("\tParameter format (default: "^ (!opt_format) ^")."));
	("--enumerate", Arg.Set opt_enum, "\tPerform parameterization enumeration.");
	("--full-enumerate", Arg.Set opt_fullenum, "\tPerform parameterization enumeration, including intervals.");
	("--test", Arg.Set opt_test, "\tTest new implementation");
	]
and usage_msg = "ph2thomas [opts]"
in
let anon_fun _ = (Arg.usage cmdopts usage_msg; raise Exit)
in
Arg.parse cmdopts anon_fun usage_msg;

let cmdopts = Ui.common_cmdopts @ Ui.input_cmdopts
and usage_msg = "ph-stable"
in
let anon_fun _ = (Arg.usage cmdopts usage_msg; raise Exit)
in
Arg.parse cmdopts anon_fun usage_msg;

let check_clingo () =
	if Sys.command "clingo -v" <> 0 then
		failwith ("The program 'clingo' is not in your path. Please download it at "
					^"http://sourceforge.net/projects/potassco/files/clingo/")
in

check_clingo ();
let asp_path = Filename.concat Distenv.pint_share_path "contrib/ph2thomas"
in


let input_ig filename =
	let cin = open_in filename
	in
	let lexbuf = Lexing.from_channel cin
	in
	let show_position () =
		let pos = Lexing.lexeme_start_p lexbuf
		in
		"Line " ^ string_of_int pos.Lexing.pos_lnum ^
		  " char " ^ string_of_int 
				(pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ^ ": "
	in
	try
		let regulations = Ph_parser.interaction_graph Ph_lexer.lexer lexbuf
		in
		let edges = Hashtbl.create (List.length regulations)
		in
		let fold_regulation nodes = function
			Regulation (a, t, s, b, _) -> (
				let s_s = match s with Positive -> "+" | Negative -> "-"
				in
				Hashtbl.add edges (a, b) (s_s, t);
				SSet.add a (SSet.add b nodes)
			)
		in
		let nodes = List.fold_left fold_regulation SSet.empty regulations
		in
		close_in cin;
		(SSet.elements nodes, edges)
	with Parsing.Parse_error ->
		failwith (show_position () ^ "Syntax error")
	| Failure msg ->
		failwith (show_position () ^ msg)
	| e -> 
		failwith (show_position () ^ Printexc.to_string e)
in


let debug_asp data =
	if !opt_asp <> "" then
		let cout = open_out !opt_asp
		in
		(output_string cout data;
		close_out cout)

(*
and readall cin =
	let bs = 2048
	in
	let buffer = Buffer.create bs
	in
	let string = String.create bs
	and chars_read = ref 1
	in
	while !chars_read <> 0 do
		chars_read := input cin string 0 bs;
		Buffer.add_substring buffer string 0 !chars_read
	done;
	close_in cin;
	Buffer.contents buffer;
*)
in

let run_process_io cmdline input_data =
	dbg cmdline;
	let pin, pout = Unix.open_process cmdline
	in
	output_string pout input_data;
	close_out pout;
	pin, pout
in

let ph, ctx = Ph_util.parse !Ui.opt_channel_in
in

let parents a =
	let is = Util.range 0 (List.assoc a (fst ph))
	in
	let fold p i =
		let fold_hit p (((b,_),_),_) =
			SSet.add b p
		in
		let hits = Hashtbl.find_all (snd ph) (a,i)
		in
		List.fold_left fold_hit p hits
	in
	List.fold_left fold SSet.empty is
in
let rec predecessors known a =
	let pa = parents a
	in
	let todo = SSet.diff pa known
	in
	let known = SSet.union known todo
	in
	let fold b pa =
		let pb = predecessors known b
		in
		SSet.union pa pb
	in
	SSet.fold fold todo pa
in
let list_auto_fixed_points coops procs =
	let sorts = fst (List.split (fst ph))
	in
	let components = List.filter (fun a -> not (List.mem a coops)) sorts
	in
	let predecessors = 
		predecessors 
			(List.fold_left (fun s a -> SSet.add a s) SSet.empty components)
	in
	let fpa (a,i) =
		let ls = SSet.add a (predecessors a)
		in
		let register_action bj ((ai,_),_) hits =
			if SSet.mem (fst bj) ls && SSet.mem (fst ai) ls 
				&& ((fst bj) = a && (snd bj) == i || (fst bj <> a))
				&& ((fst ai) = a && (snd ai) == i || (fst ai <> a))
				&& ((fst bj) <> (fst ai) || (fst bj) = a)
			then
				(ai,bj)::hits
			else hits
		in
		let hits = Hashtbl.fold register_action (snd ph) []
		and defs = List.filter (fun (a,l) -> SSet.mem a ls) (fst ph)
		in
		let fps = Ph_fixpoint.fixpoints ~restrict:[(a,i)] (defs, hits)
		in
		let get_a ps =
			let ps = PSet.filter (fun (b,_) -> a = b) ps
			in
			PSet.choose ps
		in
		List.map (fun fp -> (get_a fp, fp)) fps
	in
	List.flatten (List.map fpa procs)
in
let asp_of_fixed_points fps =
	let asp_of_fp (buf, n) ((a,i), ps) =
		let atom (b,j) =
			"phi(\""^a^"\","^string_of_int i^",\""^string_of_int n
				^"\",\""^b^"\","^string_of_int j^")."
		in
		let facts = List.map atom (PSet.elements ps)
		in
		(buf^(String.concat "\n" facts)^"\n"), (n+1)
	in
	fst (List.fold_left asp_of_fp ("\n% Local fixed points\n", 0) fps)
in

let asp_data = Ph_translator.asp_of_ph ph ctx
in
debug_asp asp_data;

let t0 = tic ()
in
let cooperative_sorts =
	let fold (a,i) ((_,_),j) cs =
		if abs(j - i) > 1 then
			SSet.add a cs
		else cs
	in
	Hashtbl.fold fold (snd ph) SSet.empty
in
let sorts = fst (List.split (fst ph))
in
let components = List.filter (fun a -> not (SSet.mem a cooperative_sorts)) sorts
in
let cooperative_sorts = SSet.elements cooperative_sorts
and components = List.fold_left (fun s a -> SSet.add a s) SSet.empty components
in
let asp_coop = 
if !opt_test then (
let coop_fixed_points =
	let predecessors = parents (*predecessors components*)
	in
	let fpa a =
		let ls = SSet.add a (predecessors a)
		in
		let register_action bj ((ai,_),_) hits =
			if SSet.mem (fst bj) ls && SSet.mem (fst ai) ls 
				&& not (SSet.mem (fst bj) components)
			then 
				(ai,bj)::hits
			else hits
		in
		let hits = Hashtbl.fold register_action (snd ph) []
		and defs = List.filter (fun (a,l) -> SSet.mem a ls) (fst ph)
		in
		let fps = Ph_fixpoint.fixpoints (defs, hits)
		in
		(a, fps)
	in
	List.map fpa cooperative_sorts
in
let fold_coop_asp asp (a, fps) =
	let asp = asp ^ "cooperative_sort(\""^a^"\").\n" 
	in
	let asp_of_ps ps =
		let ps = PSet.elements ps
		in
		let i = List.assoc a ps
		in
		let ps = List.filter (fun (b,_) -> b <> a) ps
		in
		let asp_of_p (b,j) =
			"cooperation(\""^a^"\",\""^b^"\","^string_of_int j^","
					^string_of_int i^").\n"
		in
		String.concat "" (List.map asp_of_p ps)
	in
	asp ^ String.concat "" (List.map asp_of_ps fps)
in
List.fold_left fold_coop_asp "" coop_fixed_points
) else (
let clauses = Ph2thomas_asp.create_clauses ()
in
let p = run_process_io 
		("clingo 0 --verbose=0 "^(Filename.concat asp_path "phinfercoop.lp")^" -")
		asp_data
in
Ph2thomas_coop.input_clauses clauses (fst p);
ignore(Unix.close_process p);

let coops = Ph2thomas_coop.cooperative_sorts clauses
in
Ph2thomas_coop.asp_of_clauses clauses coops
)
in
let asp_data = asp_data ^ asp_coop
in
debug_asp asp_data;
toc ~label:"sorts split" t0;

let ig = 
	if !opt_igfile = "" then (
		dbg ~level:1 "Inferring Interaction Graph...";

		let t0 = tic ()
		in
		let clauses = Ph2thomas_asp.create_clauses ()
		in
		let p = run_process_io
			("clingo 0 --verbose=0 "^(Filename.concat asp_path "need_phi.lp")^" -")
				asp_data
		in
		Ph2thomas_asp.input_clauses (fst p) clauses ["need_phi"];
		ignore(Unix.close_process p);
		let need_phis = Hashtbl.find_all clauses "need_phi"
		in
		let parse_need_phi s =
			let a = Ph2thomas_asp.parse_for_string s
			in
			let i = Ph2thomas_asp.parse_for_word_at s (Ph2thomas_asp.after_s a)
			in
			(fst a, int_of_string (fst i))
		in
		let procs = List.map parse_need_phi need_phis
		in
		dbg ("need_phi for "^string_of_procs (procs_of_ps procs));
		let fps = list_auto_fixed_points cooperative_sorts procs
		in
		let asp_fps = asp_of_fixed_points fps
		in
		let asp_data = asp_data ^ asp_fps
		in
		debug_asp asp_data;
		toc ~label:"local fixed points" t0;
		let t0 = tic ()
		in
		let p = run_process_io
			("clingo 0 --verbose=0 "^(Filename.concat asp_path "phinferIG.lp")^" -")
			asp_data
		in
		let ig = Ph2thomas_ig.input_graph (fst p)
		in
		ignore(Unix.close_process p);
		toc ~label:"IG inference" t0;
		ig
	) else 
		input_ig !opt_igfile
in
(if !opt_dotfile <> "" then
	let cout = open_out !opt_dotfile
	in
	(output_string cout (Ph2thomas_ig.dot_of_graph ig);
	close_out cout)
);
let asp_data = asp_data ^ (Ph2thomas_ig.asp_of_graph ig)
in
debug_asp asp_data;

dbg ~level:1 "Inferring Parameters..";
let t0 = tic ()
in

let p = run_process_io
	("clingo 0 --verbose=0 "^(Filename.concat asp_path "phinferK.lp")^" -")
		asp_data
in
let params = Ph2thomas_param.input_params (fst p) ig
in
ignore(Unix.close_process p);
toc ~label:"K inference" t0;
let string_of_params = 
	match !opt_format with
		  "AB" -> Ph2thomas_param.string_AB_of_params
		| "active" -> Ph2thomas_param.string_active_of_params
		| "iter" -> Ph2thomas_param.string_iter_of_params
		| f -> failwith ("Invalid format '" ^ f ^ "'")
in
print_string (string_of_params ig params);
flush_all ();

if !opt_enum || !opt_fullenum then (
	let asp_data = asp_data ^ (Ph2thomas_param.asp_for_enum ig params)
	in
	debug_asp asp_data;
	let t0 = tic ()
	in
	let cmdline = "clingo 0 --verbose=1 "^(Filename.concat asp_path "phenumK.lp")^" -"
	in
	dbg cmdline;
	let pout = Unix.open_process_out cmdline
	in
	(if not !opt_fullenum then
		output_string pout 
			":- enum_param(A,P,I), enum_param(A,P,J), I != J, not infered_param(A,P).\n"
	);
	output_string pout asp_data;
	ignore(Unix.close_process_out pout);
	toc ~label:"K enumeration" t0
)

