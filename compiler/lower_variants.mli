open Core

(** Removes variants and replaces them with tagged structs *)
val lower : Tail_call.t -> Tail_call.t Or_error.t
