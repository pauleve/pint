
(* TODO: move to a dedicated structure returned by the parser *)
open PintTypes;;
open Ph_types;;

let interaction_graph = ref PintTypes.SMap.empty;;

let cooperativities = ref SMap.empty;;

