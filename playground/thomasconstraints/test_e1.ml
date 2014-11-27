
open PintTypes;;

let dump_ph outf ph ctx =
	let data = Ph_translator.dump_of_ph ph ctx
	in
	let cout = open_out outf
	in
	output_string cout data;
	close_out cout

in
let model = "e1.ph"
in
let ph, ctx = Ph_util.parse (open_in model)
in
let ig = !Ph_instance.interaction_graph
in
let cctx = InteractionGraph.IG.ctx_for_resources ig (fst ph) 
			"c"(SSet.singleton "a")
in
let ph = Ph_thomas.constrained_ph ph ig "c" cctx [0]
in
dump_ph "gen/e1-1.ph" ph ctx;

let cctx = InteractionGraph.IG.ctx_for_resources ig (fst ph) 
			"c"(SSet.singleton "b")
in
let ph = Ph_thomas.constrained_ph ph ig "c" cctx [0]
in
dump_ph "gen/e1-2.ph" ph ctx;

