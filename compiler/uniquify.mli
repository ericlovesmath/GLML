(** Removes duplicate names, preventing cases of shadowing *)
val uniquify : Stlc.t -> Stlc.t Compiler_error.t
