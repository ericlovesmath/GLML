(** Eliminates all first-class function values by converting them to variant
    constructors and routing calls through generated dispatch functions. *)
val defunctionalize : Uncurry.t -> Uncurry.t Compiler_error.t
