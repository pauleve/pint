
module P2Map : Map.S with type key = Ph_types.process * Ph_types.process

type env = (Ph_types.PSet.t * Ph_types.rate * Ph_types.PSet.t) list P2Map.t

(** Create a new execution environment from the given Process Hitting. *)
val create_env : Ph_types.ph -> env

(** Function to plot at the given time the presence of the given process. *)
type plotter = (float -> Ph_types.process -> unit)

(** [execute env init duration plotter] simulates the environment [env] from the initial state [init]
		and calls [plotter t p] each time the process [p] appears at time [t].
		The simulation stops either when a fix point has been reached, or after [duration] units of time.
		Returns the resulting state. *)
val execute : env -> Ph_types.state -> float -> plotter -> Ph_types.state

