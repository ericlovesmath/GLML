open Core

(** Creates type map for all variables in [Stlc.t],
    returns [Error _] if types are not sound *)
val typecheck : Stlc.t -> Stlc.ty String.Map.t Or_error.t
