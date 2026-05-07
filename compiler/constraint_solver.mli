open Type_system

(** Solve a constraint set, returning a substitution and deferred constraints. *)
val solve : constr list -> (substitution * constr list) Compiler_error.t

(** Specialize a polymorphic scheme *)
val solve_scheme : constr list -> substitution -> substitution Compiler_error.t
