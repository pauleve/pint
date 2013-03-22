(**
Process reachability by static analysis.
*)

type abstr_struct = {
	mutable procs : Ph_types.PSet.t;
	mutable objs : Ph_types.ObjSet.t;
	mutable _Sol : (Ph_types.PSet.t list) Ph_types.ObjMap.t;
	mutable _Req : (Ph_types.objective list) Ph_types.PMap.t;
	mutable _Cont : Ph_types.ObjSet.t Ph_types.ObjMap.t;
}

type objective_seq = Ph_types.objective list

type env = {
	ph : Ph_types.ph;
	ctx : Ph_types.ctx;
	pl : Ph_types.process list;
	bs_cache : Ph_bounce_seq.bs_cache;
	a : abstr_struct;
	s : Ph_types.state;
	w : objective_seq;
}


val init_oldenv : Ph_types.ph -> Ph_types.ctx -> Ph_types.process list -> env
val init_env : Ph_types.ph -> Ph_types.ctx -> Ph_types.process list -> env
val update_env : env -> Ph_types.ctx -> Ph_types.process list -> env

(** returns a string representation of the given objective sequence. *)
val string_of_objseq : objective_seq -> string

(** returns the objective sequences corresponding to the successive rechability of given processes in the given state. *)
val objseq_from_procseq : Ph_types.state -> Ph_types.process list -> objective_seq

(** (WiP) [process_reachability ph s w]
returns the semi-decision (ternary) of the concretizability of objective sequence [w] in the state [s] in the process hitting [ph].
*)
val coop_priority_reachability : env -> Ph_types.ternary

(** [process_reachability ph s w]
returns the semi-decision (ternary) of the concretizability of objective sequence [w] in the state [s] in the process hitting [ph].
*)
val process_reachability : env -> Ph_types.ternary


(** EXPERIMENTAL API - TO BE DOCUMENTED WHEN STABLE *)

val test : env -> Ph_types.ternary

val color_nodes_connected_to_trivial_sols :
  #Ph_glc.glc -> Ph_glc.NodeSet.t

val get_Sols : env -> Ph_types.objective -> Ph_types.PSet.t list

val bot_trimmed_cwA : env -> #Ph_glc.glc -> Ph_glc.glc
val top_trimmed_cwA : env -> #Ph_glc.graph -> unit

