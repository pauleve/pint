

type cooperativities_t = (Ph_types.sort list * (Ph_types.sortidx list -> int)) PintTypes.SMap.t 

val interaction_graph : InteractionGraph.IG.t ref

val cooperativities : cooperativities_t ref

val reset : unit -> unit

type instance_t = (cooperativities_t * InteractionGraph.IG.t)

val copy : unit -> instance_t

val restore: instance_t -> unit

