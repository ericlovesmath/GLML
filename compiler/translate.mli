(** Converts [Anf.t] to [Glsl.t] format, currently failiable *)
val translate : Lower_variants.t -> Glsl.t Compiler_error.t
