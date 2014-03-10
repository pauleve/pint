(**
	Local reachability by static analysis.
*)

type env = {
	ph : Ph_types.ph;
	ctx : Ph_types.ctx;
	pl : Ph_types.process list;
	bs_cache : Ph_bounce_seq.bs_cache;
}


val init_env : Ph_types.ph -> Ph_types.ctx -> Ph_types.process list -> env
val update_env : env -> Ph_types.ctx -> Ph_types.process list -> env

type refGLC = NullGLC | GLC of Ph_glc.glc;;

(** [local_reachability env]
returns the semi-decision (ternary) of the concretizability of sequence of local reachability as configured in [env].
*)
val local_reachability : ?saveGLC:refGLC ref -> env -> Ph_types.ternary


val coop_priority_ua_glc_setup : Ph_glc.glc_setup

(** (WiP) [coop_priority_reachability ph s w]
returns the semi-decision (ternary) of the concretizability of objective sequence [w] in the state [s] in the process hitting [ph].
*)
val coop_priority_reachability : env -> Ph_types.ternary

val color_nodes_connected_to_trivial_sols :
  #Ph_glc.glc -> Ph_glc.NodeSet.t

val get_Sols : env -> Ph_types.objective -> Ph_types.PSet.t list

val bot_trimmed_cwA : env -> #Ph_glc.glc -> Ph_glc.glc
val top_trimmed_cwA : env -> #Ph_glc.graph -> unit

