
(* TODO: move to a dedicated structure returned by the parser *)
open PintTypes
open Ph_types

type cooperativities_t = (Ph_types.sort list * (Ph_types.sortidx list -> int)) PintTypes.SMap.t 
type instance_t = (cooperativities_t * InteractionGraph.IG.t)

let interaction_graph = ref SMap.empty

let cooperativities = ref SMap.empty

let reset () =
	cooperativities := SMap.empty;
	interaction_graph := SMap.empty

let copy () =
	(!cooperativities, !interaction_graph)

let restore (c, ig) =
	cooperativities := c;
	interaction_graph := ig

