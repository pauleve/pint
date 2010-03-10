
open Ph_types;;
open Ph_sortgraph;;

let string_of_ternary = function True -> "true" | False -> "false" | Inconc -> "inconc";;


let check_solution actions =
	let check_sortgraph a sortgraph =
		let str_eulerian =
			try
				let first,last = sortgraph_eulerian_extrem sortgraph
				and str_of_extrem = function None -> "Any" |
					Some ai -> string_of_process ai
				in
				"Eulerian ("^str_of_extrem first^","^str_of_extrem last^")"
			with Non_eulerian -> "Non-Eulerian"
		and str_reeulerisable =
			"Re-Eulerisable: "^string_of_ternary (sortgraph_reeulerisable sortgraph)
		in
		print_endline ("sort "^a^": "^str_eulerian^"; "^str_reeulerisable)
	in
	print_endline ("************************");
	let sortgraphs = sortgraphs_of_actions actions
	in
	SMap.iter check_sortgraph sortgraphs
;;


let solutions = [
	[
		Hit (("b",1),("z",2),1);
		Hit (("a",1),("z",1),0);
		Hit (("z",0),("b",1),0);
		Hit (("z",0),("a",1),0);
		Hit (("a",0),("z",0),1);
		Hit (("b",0),("z",1),2);
(*		Hit (("b",0),("b",0),1); *)
	];
] in
List.iter check_solution solutions;;





