(** Substitutes each stamp in [elab.program] for proper concrete repr *)
val erase : Typecheck.elaborated -> Typecheck.t Compiler_error.t
