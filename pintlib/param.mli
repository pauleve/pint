(** Stochastic and temporal parameters related functions. *)

(** [firing_interval alpha r sa] returns the (float) confidence interval at confidence coefficient [alpha] for the sum of [sa] exponential variables of [rate r*sa]. *)
val firing_interval : float -> float -> int -> float * float

(** [random_firing_time r sa] returns a random value following an Erlang [(r*sa, sa)]. *)
val random_firing_time : float -> int -> float

(** Rounds the given float to the nearest ceil or floor. *)
val round_float : float -> float

(** Truncates given couple of floats into a couple of integers. *)
val int_of_fi : float * float -> int * int

(** Rounds given firing interval to the closest integers. *)
val round_fi_closest : float * float -> int * int

(** Rounds given firing interval to the bounding integer interval. *)
val round_fi_ex : float * float -> int * int

(** Rounds given firing interval to the bounded integer interval. *)
val round_fi_in : float * float -> int * int

(** [zoom_fi factor fi] multiplies the bounds of [fi] by [factor]. *)
val zoom_fi : float -> float * float -> float * float

(** Estimes the rate and stochasticity absorption factor corresponding to the given firing interval at the given confidence coefficient *)
val rsa_of_firinginterval : float * float -> float -> float * int


