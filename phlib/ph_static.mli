(** Static properties of Process Hitting *)

(** returns the set of dynamics fix points of the given Process Hitting *)
val stable_states : Ph_types.ph -> Ph_types.process list list

val resolve_cooperativities : Ph_types.ph -> Ph_types.process list -> 
			(Ph_types.process * Ph_types.PSet.t list) list

