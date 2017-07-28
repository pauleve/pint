
open Debug

open PintTypes

open LocalCausalityGraph
open AutomataNetwork

open ASP_solver

let any_instance = "G"

let has_indep = ref false

let automaton_asp a = string_of_int a

let obj_asp (a,i,j) =
	"obj("^automaton_asp a^","^string_of_int i^","^string_of_int j^")"

let sol_asp obj n =
	"lpath("^obj_asp obj^","^string_of_int n^")"

let ls_asp (a,i) =
	"ls("^automaton_asp a^","^string_of_int i^")"

let edge_asp ?(instance=any_instance) n1 n2 =
	"ua_lcg("^instance^","^n1^","^n2^")"

let init_asp ?(instance=any_instance) a i =
	"init("^instance^","^automaton_asp a^","^string_of_int i^")"

let indep_asp ?(instance=any_instance) scope a i ls2 =
	has_indep := true;
	"indep("^instance^","^scope^","^automaton_asp a^","^string_of_int i^","^ls_asp ls2^")"

let bool_asp a =
	"boolean("^automaton_asp a^")"


let asp_unordua asp =
	let asp = decls asp [
	(* objective per initial state *)
	"ua_lcg(G,ls(A,I),obj(A,J,I)) :- ua_lcg(G,_,ls(A,I)), init(G,A,J)";

	"conn(G,X,Y) :- ua_lcg(G,X,Y)";
	"conn(G,X,Y) :- ua_lcg(G,X,Z), conn(G,Z,Y)";
	":- conn(G,X,X)"; (* no cycle *)
	(* context saturation *)
	"init(G,A,I) :- ua_lcg(G,N,ls(A,I)), N != goal";
	(* sufficient continuity *)
	"ua_lcg(G,obj(A,I,J),obj(A,K,J)) :- not boolean(A),conn(G,obj(A,I,J),ls(A,K)),quick(obj(A,I,J)),I != K, J != K";
	"ua_lcg(G,obj(A,I,J),obj(A,K,J)) :- not boolean(A),conn(G,obj(A,I,J),ls(A,K)),not quick(obj(A,I,J)),J != K";
	]
	in
	if !has_indep then
	(* synchronisation independence *)
	decls asp [
		"indepfailure(Y,ls(A,I)) :- indep(G,Y,A,I,N),conn(G,N,ls(A,J)),J!=I";
		":- indepfailure(Y,N),indepfailure(Y,M),M!=N";
	]
	else asp


let asp_lcg asp lcg =
	let nodesol_asp obj asp (i, n) =
		let orig = sol_asp obj i
		in
		let children = lcg#children n
		in
		let asp = List.fold_left (fun asp ->
			(function NodeLS ai ->
				decl asp (edge_asp orig (ls_asp ai)^" :- "^edge_asp "_" orig)
			| _ -> asp)) asp children
		in
		match n with
		  NodeSol (_, alp) ->
			let asp_indep state asp =
				if IMap.cardinal state > 1 then
				let asp_indep_ls a i asp =
					IMap.fold (fun b j asp ->
						if b <> a then
							decl asp (indep_asp orig a i (b,j)^" :- "^edge_asp "_" orig)
						else asp) state asp
				in
				IMap.fold asp_indep_ls state asp else asp
			in
			StateSet.fold asp_indep alp.An_localpaths.conds asp
		| _ -> asp
	in
	let nodeobj_asp n asp = match n with
		  NodeObj obj ->
			let orig = obj_asp obj
			in
			let children = lcg#children n
			in
			let sols = List.filter (function NodeSol _ -> true | _ -> false) children
			in
			let isols = List.mapi (fun i s -> (i,s)) sols
			in
			let only_quick = List.for_all (function
				NodeSol (_, alp) -> ISet.is_empty (alp.An_localpaths.interm)
				| _ -> true) sols
			in
			let asp = if only_quick then
						decl asp ("quick("^orig^")")
				else asp
			in
			let buf =
				match sols with [] -> "" | _ ->
					let asp_sols = List.mapi (fun i _ ->
						edge_asp orig (sol_asp obj i)) sols
					in
					"1 {"^(String.concat "; " asp_sols)^"} 1 "
			in
			let asp = decl asp (buf^":- "^edge_asp "_" orig)
			in
			List.fold_left (nodesol_asp obj) asp isols

		| _ -> asp
	in
	has_indep := false;
	NodeSet.fold nodeobj_asp lcg#nodes asp

let asp_ctx ?(instance=any_instance) asp ctx =
	IMap.fold (fun a is asp ->
		ISet.fold (fun i asp ->
			decl asp (init_asp ~instance a i)) is asp) ctx asp

let unordered_ua an ctx goal sols =
	dbg ~level:1 ". unordered under-approximation (ASP implementation)";
	let asp = solver ()
	in
	let asp = ISet.fold (fun a asp -> decl asp (bool_asp a))
				(boolean_automata an) asp
	and instance = "uua"
	in
	let ua = new lcg default_lcg_setup an ctx goal sols
	in
	ua#set_auto_conts false;
	ua#build;
	ua#saturate_ctx;
	let asp = asp_lcg asp ua
	in
	let asp = asp_ctx ~instance asp ctx
	in
	let goal = List.rev goal
	in
	let gl, pre_goals = List.hd goal, List.tl goal
	in
	let asp = List.fold_left (fun asp ai ->
		decl asp (edge_asp ~instance "root" (ls_asp ai))) asp pre_goals
	in
	let asp = decl asp (edge_asp ~instance "goal" (ls_asp gl))
	in
	let asp = asp_unordua asp
	in
	let asp = decl asp "#show init/3"
	in
	sat asp


