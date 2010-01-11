
type 'a expr = 
	L of 'a | F | T | Not of 'a expr | And of ('a expr * 'a expr) | Or of ('a expr * 'a expr)
;;

module type LitType = sig 
	type t
	val to_string : t -> string
	val tautology : (bool * t) -> (bool * t) -> bool (* returns true if both arguments 
															are contrary *)
end;;

module type Literal_t = 
sig
	type t
	val to_string : t -> string
end;;

type connector_t = Conjonction | Disjonction

module type Connector_t =
sig
	val connector : connector_t
	val repr : string
end;;

module NormalForm ( Literal : Literal_t (*, Connector : Connector_t*) ) = 
struct
	module Literal = Literal
	module ClauseConnector = struct let connector = Conjonction let repr = " & " end
	module Clause = Set.Make (struct type t = Literal.t let compare = compare end)
	module Connector = struct let connector = Disjonction let repr = " | " end

	type t = True | Clauses of Clause.t list

	let clause_to_string clause =
		if Clause.is_empty clause then "False"
		else (
			let literals = Clause.elements clause
			in
			let literals_str = List.map Literal.to_string literals
			in
			String.concat ClauseConnector.repr literals_str
		)
	
	let to_string = function
		  True -> "True"
		| Clauses [] -> "False"
		| Clauses clauses -> String.concat Connector.repr (List.map clause_to_string clauses)
	
	let val_true = True
	let val_false = Clauses []
	
	let append clause = function (* assumes connector = disjonction *)
		  True -> True
		| Clauses clauses -> Clauses (
			(* case 1: clause is already in clauses *)
			if List.exists (fun clause' -> Clause.subset clause' clause) clauses then
				clauses
			(* case 2: remove clauses containing 'clause' *)
			else
				(clause::List.filter (fun clause' -> not(Clause.subset clause clause')) clauses)
		)
	
	let iter iterator = function
		  True -> ()
		| Clauses clauses -> List.iter iterator clauses 
	
	let rec union nf = function
		  True -> True
		| Clauses [] -> nf
		| Clauses (h::t) -> append h (union nf (Clauses t))
	
	let cross_literal literal = function
		  True -> Clauses [Clause.singleton literal]
		| Clauses clauses -> Clauses (List.map (fun clause -> Clause.add literal clause) clauses)

end;;

