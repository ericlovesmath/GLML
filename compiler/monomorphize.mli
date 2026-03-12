open Core

(* TODO: Generate a new term with a new type that does not contain tyvars *)

(** Specializes polymorphic functions, generating a new instance for every concrete
    monomorphic type that lives in the general polymorphic type *)
val monomorphize : Typecheck.t -> Typecheck.t Or_error.t
