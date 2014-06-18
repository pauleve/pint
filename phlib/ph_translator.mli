(** Translation of Process Hitting into different formats. *)

(** Returns the Biocham string representation of given process. *)
val biocham_of_process : Ph_types.process -> string

(** Returns the Biocham translation of the given Process Hitting. *)
val biocham_of_ph : Ph_types.ph -> Ph_types.ctx -> string

(** Returns a Process Hitting source as a flat action list from the given Process Hitting. *)
val dump_of_ph : Ph_types.ph -> Ph_types.ctx -> string

(** Returns the Kappa translation of the given Process Hitting. *)
val kappa_of_ph : Ph_types.ph -> Ph_types.ctx -> string

(** Returns the PRISM (ctmc) translation of the given Process Hitting. *)
val prism_of_ph : Ph_types.ph -> Ph_types.ctx -> string

(** Returns the PRISM (mdp) translation of the given Process Hitting. *)
val prism_mdp_of_ph : Ph_types.ph -> Ph_types.ctx -> string

(** Returns the SPiM translation of the given Process Hitting. *)
val spim_of_ph : Ph_types.ph -> Ph_types.ctx -> string

(** Returns the Tina translation of the given Process Hitting. *)
val tina_of_ph : Ph_types.ph -> Ph_types.ctx -> string


(** Stochastic parameters to strict timed interval conversion options:
	[alpha] is the confidence coefficient;
	[round_fi (d,D)] converts a float interval into an integer interva;
*)
type opts = { alpha : float; round_fi : float * float -> int * int;
				coop_priority: bool;
				contextual_ptnet: bool}

(** Returns the Romeo string representation of given process. *)
val romeo_pid : Ph_types.ph -> Ph_types.process -> string

(** Returns the Romeo translation of the given Process Hitting. *)
val romeo_of_ph : opts -> Ph_types.ph -> Ph_types.ctx -> string

(** Returns the ASP translation of the given Process Hitting - WiP *)
val asp_of_ph : Ph_types.ph -> Ph_types.ctx -> string

(** Returns the PEP translation of the given Process Hitting. *)
val pep_of_ph : opts -> Ph_types.ph -> Ph_types.ctx -> string

(** Returns the BoolNet translation of the given Process Hitting.
	Warning: no sanity check is done. Actually returns the fixed-point condition 
	for each non-cooperative sort to be 1 *)
val bn_of_ph : Ph_types.ph -> Ph_types.ctx -> string


