open Core
open Type_system

(* TODO: Exposing this for typechecker feels a bit weird *)
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
