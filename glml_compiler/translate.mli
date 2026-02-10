open Core

(** Converts [Anf.t] to [Glsl.t] format, currently still failiable despite [Or_error.t] *)
val translate : Stlc.ty String.Map.t -> Anf.t -> Glsl.t Or_error.t
