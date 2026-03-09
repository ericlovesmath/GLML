open Core

(** Converts [Anf.t] to [Glsl.t] format, currently failiable *)
val translate : Tail_call.t -> Glsl.t Or_error.t
