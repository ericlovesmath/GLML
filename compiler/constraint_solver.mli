open Core
open Type_system

(** Apply [int <: float] subtyping pointwise *)
val widen_numeric : ty -> ty

(** Decide whether a concrete type belongs to a typeclass, caller has to check that
    there are no free variables themselves, as goals on [TyVars] are deferred by
    [resolve_constraints] *)
val check_class : type_class -> ty -> bool

(** Robinson unification with occurs check *)
val unify : (Lexer.loc * ty * ty) list -> substitution Compiler_error.t

(** Solve a constraint set against a struct environment.

    (name => [(type-params, fields)]) -> constraints -> substition + rem *)
val solve
  :  (string list * (string * ty) list) String.Map.t
  -> constr list
  -> (substitution * constr list) Compiler_error.t

(** Specialize a polymorphic scheme *)
val solve_scheme
  :  ?structs:(string list * (string * ty) list) String.Map.t
  -> constr list
  -> substitution
  -> substitution Compiler_error.t
