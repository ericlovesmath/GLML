(** Removes duplicate names, preventing cases of shadowing *)
val uniquify : Desugar.t -> Desugar.t Compiler_error.t
