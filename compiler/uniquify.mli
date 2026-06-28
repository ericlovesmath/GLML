(** Alpha conversion such that identical symbols only refer to identical varibles,
    removing the need for a local namespace, and removes module name resolutions
    and qualfiers.

    Flattens modules to top-level definitions. *)
val uniquify : Desugar.t -> Desugar.t Compiler_error.t
