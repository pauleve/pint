
open PintTypes
open AutomataNetwork
open InteractionGraph

let dump outf an ctx =
	let data = An_export.dump_of_an an ctx
	in
	let cout = open_out outf
	in
	output_string cout data;
	close_out cout

let phmodel = "e1.ph"
let model = "e1.an"

let ig =
	let phmodel = "e1.ph"
	in
	ignore(Ph_util.parse (open_in phmodel));
	!Ph_instance.interaction_graph

let an, ctx = An_input.parse (open_in model)

let ps = automata_limits an

let _ =
	let cctx = IG.extended_local_ctx ig ps "c" (SMap.add "a" (ISet.singleton 1) ctx)
	in
	An_focal.restrict_focal an "c" cctx (ISet.singleton 0);
	dump "gen/e1-1.an" an ctx;

	let cctx = IG.extended_local_ctx ig ps "c" (SMap.add "b" (ISet.singleton 1) ctx)
	in
	An_focal.restrict_focal an "c" cctx (ISet.singleton 0);
	dump "gen/e1-2.an" an ctx

