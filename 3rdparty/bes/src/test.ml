(* $Id: promela.mli 2996 2012-03-14 09:58:12Z weissmam $

Copyright (c) 2011 - 2012 Technische Universitaet Muenchen
Copyright (c) 2011 Alexander Ostrovsky <ostrovsk@in.tum.de
Copyright (c) 2011 - 2012 Markus W. Weissmann <markus.weissmann@in.tum.de>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Apple Inc. nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)

open Bes

let (|>) a b = b a

(** Flattens a list of lists *)
let fast_flatten (l : 'a list list) = 
	List.fold_left (fun a x ->  List.rev_append x a) [] l

(** Maps a list *)
let fast_map f l = 
	Array.to_list (Array.map f (Array.of_list l))

(** Remove duplicates from a list. It is equavalien to creating a 
    set of list and converting it back to the list *)
let remove_duplicates l = 	
	if l <> [] then
		let l_sorted = List.fast_sort compare l in	
		let rec doit s l = function
			| [] -> []
			| xs::xr when l <> xs || s -> xs::(doit false xs xr)
			| xs::xr -> doit false xs xr		
		in 
		doit true (List.hd l) l_sorted
	else
		[]
		
(** Returns whether l1 is subset of l2 *)
let is_subset l1 l2 = 
	let rec doit = function
		| xs::xr -> if List.mem xs l2 then doit xr else false
		| [] -> true
	in 
	doit l1

(** Generates a random expression 
	with (up to) cnt1 minterms and cnt2 variables *)
let generate_expression_to_optimize part_def cnt1 cnt2 =  		
	Random.self_init ();
	let gen_ran () = 
		let c = if part_def then 3 else 2 in
		let t = Random.int c
	in 
	if t = 0 then 
		`False
	else if t = 1 then
		`True
	else
		`Dontcare
	in
	let rec gena cnt = if cnt = 0 then [] else (gen_ran ())::(gena (cnt - 1)) in
	let rec genb cnt = if cnt = 0 then [] else (gena cnt2)::(genb (cnt - 1)) in
	remove_duplicates (genb cnt1)

exception Not_possible

(** Expands the minterms with don't care terms. 
	E.g. 10x will become 100 + 101 *)
let rec expand_minterm = function
	| [] -> [[]]
	| h::t when h <> `Dontcare -> List.fold_left (fun xs t -> (h::t)::xs) [] (expand_minterm t)
	| h::t when h = `Dontcare -> List.fold_left (fun xs t -> (`True::t)::(`False::t)::xs) [] (expand_minterm t)
	| _ -> raise Not_possible

(** Calls expand_minterm for each minterm in the expression. *)
let expand_expression (expr : Bes.dnf_expression) = 
		expr
		|> List.map expand_minterm
		|> fast_flatten
		|> remove_duplicates	

(** Returns whether expr_from implies expr *)
let is_implicant (expr_from : Bes.dnf_minterm) (expr : Bes.dnf_expression) =	
	let rec expand_minterm = function
		| [] -> [[]]
		| h::t when h <> `Dontcare -> List.fold_left (fun xs t -> (h::t)::xs) [] (expand_minterm t)
		| h::t when h = `Dontcare -> List.fold_left (fun xs t -> (`True::t)::(`False::t)::xs) [] (expand_minterm t)
		| _ -> raise Not_possible
	in 
	let expanded_minterm = expand_minterm expr_from in	
	let expanded_expression = 
		expr 
		|> List.map expand_minterm 
		|> fast_flatten
	in
	is_subset expanded_minterm expanded_expression

(** Returns whether expr is equivalent min_expr. 
	Two logical expressions f and g are equivalent if and only if
	for all variable assignements v: f(v) = g(v) *)
let test_minimization_equivalence (expr : Bes.dnf_expression) (min_expr : Bes.dnf_expression) =	
	let e1 = 
		expr 
		|> expand_expression
		|> remove_duplicates
		|> List.sort Pervasives.compare		
	in
	let e2 = 
		min_expr 
		|> expand_expression
		|> remove_duplicates
		|> List.sort Pervasives.compare		
	in		
	e1 = e2

let _ =		
	Bes.set_verbose_mode true;		
	(* Test conversion functions *)	
	let test_conversions () =
		let sample = "100;001;x00;" in
		let sample2 = ["100"; "001"; "x00"] in
		let converted = Bes.dnf_expression_of_string ';' sample in
		let converted2 = Bes.dnf_expression_of_string_list sample2 in
		if converted <> [[`True; `False; `False]; [`False; `False; `True]; [`Dontcare; `False; `False]]
			|| converted2 <> [[`True; `False; `False]; [`False; `False; `True]; [`Dontcare; `False; `False]] then
		begin
			Bes.print_dnf_expression converted;
			failwith "String to expression conversion does not work"
		end
	in
	(* Test auto optimization *)
	let test_auto_optimization () =
		let expr = Bes.dnf_expression_of_string ';' "0000001;0000011;0000100;0000101;0000110;0001000;0001001;0001010;0001011;0001100;0001101;0001110;0010000;0010010;0010100;0010101;0010110;0010111;0011000;0011001;0011010;0011100;0011101;0011110;0011111;0100000;0100110;0100111;0101011;0101100;0101101;0101110;0101111;0110010;0110101;0110110;0111001;0111101;0111110;0111111;1000000;1000010;1000011;1000101;1000110;1000111;1001001;1001010;1001100;1001101;1001110;1010000;1010001;1010010;1010011;1010100;1011000;1011010;1100000;1100001;1100010;1100100;1100110;1100111;1101001;1101010;1101011;1101110;1101111;1110001;1110010;1110011;1110100;1110110;1110111;1111000;1111001;1111100;1111101;1111110;1111111" in	
		let (_, exact) = Bes.auto_optimize expr in
		if exact then failwith "Auto optimization optimized with a wrong algorithm"
	in
	(* Test equivalence *)
	let test_equivalence is_random_size iterations =
		let r1 = (Random.int 112) + 16 in 
		let r2 = (Random.int 8) + 16 in		
		let cnt1 = if is_random_size then r1 else 64 in
		let cnt2 = if is_random_size then r2 else 8 in
		for i = 0 to iterations do			
			let expr = generate_expression_to_optimize false cnt1 cnt2 in		
			let min_expr_pi = Bes.optimize Bes.Qmc expr in
			let min_expr1 = Bes.optimize Bes.Qmc_SimpleHeuristic_SortEachTime expr in		
			let min_expr1_1 = Bes.optimize Bes.Qmc_SimpleHeuristic_AdvancedHeuristic expr in		
			let min_expr2 = Bes.optimize Bes.Qmc_PetricksMethod expr in								
			
			let t1 = test_minimization_equivalence expr min_expr1 in
			let t1_1 = test_minimization_equivalence expr min_expr1_1 in
			let t2 = test_minimization_equivalence expr min_expr2 in					
			
			if not t1 then
			begin
				print_endline "Test failed (Simple Heuristic (length))";
				Bes.print_dnf_expression (expand_expression expr);
				print_newline ();
				print_endline "is not equivalent to";
				Bes.print_dnf_expression (expand_expression min_expr1);
				print_endline "or";			
				Bes.print_dnf_expression min_expr1;
				failwith "equivalence test failed"		
			end;
			
			if not t1_1 then
			begin
				print_endline "Test failed (Advanced heuristic)";
				Bes.print_dnf_expression (expand_expression expr);
				print_newline ();
				print_endline "is not equivalent to";
				Bes.print_dnf_expression (expand_expression min_expr1_1);
				print_endline "or";			
				Bes.print_dnf_expression min_expr1_1;
				failwith "equivalence test failed"		
			end;
			
			if not t2 then
			begin
				print_endline "Test failed (Petrick's Method)";
				Bes.print_dnf_expression (expand_expression expr);
				print_newline ();
				print_endline "is not equivalent to";
				Bes.print_dnf_expression (expand_expression min_expr2);
				print_endline "or";			
				Bes.print_dnf_expression min_expr2;
				failwith "equivalence test failed"		
			end;				
			
			print_string "Test run #";
			print_int i;
			print_string " passed (";
			print_string "Minterm Size: ";
			print_int cnt2;
			print_string ", Expression Size: ";
			print_int cnt1;
			print_endline ")";
			print_string "Expression with size: ";
			print_int (List.length expr);
			print_string " minimized to (simple heuristic, length) ";
			print_int (List.length min_expr1);
			print_string " and (advanced heuristic) ";
			print_int (List.length min_expr1_1);	
			print_string " and (exact) ";
			print_int (List.length min_expr2);
			if (List.length min_expr1) <> (List.length min_expr1_1) then
			begin
				print_string ("!!!!" ^ (string_of_int ((List.length min_expr1) - (List.length min_expr1_1))));				
			end;
			if (List.length min_expr1) <> (List.length min_expr2) then
			begin
				print_string ("!!!!" ^ (string_of_int ((List.length min_expr1) - (List.length min_expr2))));				
			end;
			print_newline ();
			print_string "Prime implicants found: ";
			print_int (List.length min_expr_pi);
			if cnt1 < (List.length min_expr_pi) then
				print_string "!!!!";
			print_newline ();			
			print_newline ()
		done
	in 
	Random.self_init ();	
	test_conversions ();
	test_auto_optimization ();	
	test_equivalence true 20;
	test_equivalence false 100;
	print_endline "All tests passed!"
	
let _ = print_newline ()
let _ = print_endline "All tests passed"
