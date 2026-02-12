open Core

module Passes : sig
  type t =
    | Stlc
    | Uniquify
    | Typecheck
    | Anf
    | Translate
    | Glsl
  [@@deriving compare, sexp_of]

  include Comparable.S with type t := t
end

(** Run full compiler pipeline, transpiling GLML to GLSL strings *)
val compile_source : string -> string

(** Compile from [Stlc.t] string repr to GLSL, pass handlers to dump the sexp
    output of each [Passes.t] if desired (defaults to none) *)
val compile_stlc : ?dump:(Sexp.t -> unit) Passes.Map.t -> string -> string Or_error.t
