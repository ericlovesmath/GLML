(** Converts [Anf.t] to [Glsl.t] format, currently failiable *)
val translate : Remove_placeholder.t -> Glsl.t Compiler_error.t
