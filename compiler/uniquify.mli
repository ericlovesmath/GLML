open Core

(** Removes duplicate names, preventing cases of shadowing *)
val uniquify : Stlc.t -> Stlc.t Or_error.t
