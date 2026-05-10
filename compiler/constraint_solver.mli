open Type_system

(** Solve a constraint set, returning a substitution and deferred constraints.
    Raises {!Compiler_error.Compile_error} on failure. *)
val solve : constr list -> substitution * constr list

(** Specialize a polymorphic scheme.
    Raises {!Compiler_error.Compile_error} on failure. *)
val solve_scheme : constr list -> substitution -> substitution
