open Core

type t = Program of Stlc.ty String.Map.t * Stlc.term list [@@deriving sexp_of]

(** Creates type map for all variables in [Stlc.t],
    returns [Error _] if types are not sound *)
val typecheck : Stlc.t -> t Or_error.t
