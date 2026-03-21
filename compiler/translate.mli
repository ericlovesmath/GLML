open Core

(** Converts [Anf.t] to [Glsl.t] format, currently failiable *)
val translate : Lower_variants.t -> Glsl.t Or_error.t
