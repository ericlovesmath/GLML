(** Replaces toplevel constants that can't be translated directly
    to GLSL constant terms into zero-argument functions, along with
    their callsites *)
val lift : Anf.t -> Anf.t Compiler_error.t
