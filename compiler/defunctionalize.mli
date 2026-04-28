(** Eliminates all first-class function values by converting them to variant
    constructors and routing calls through generated dispatch functions. *)
val defunctionalize : Lambda_lift.t -> Lambda_lift.t Compiler_error.t
