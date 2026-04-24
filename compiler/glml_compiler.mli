open Core
module Compiler_error = Compiler_error

(** Passes in compiler available to be dumped *)
module Passes : sig
  type t =
    | Frontend
    | Desugar
    | Uniquify
    | Typecheck
    | Monomorphize
    | Uncurry
    | Defunctionalize
    | Lambda_lift
    | Anf
    | Tail_call
    | Lower_variants
    | Promote_ints
    | Remove_placeholder
    | Lift_consts
    | Translate
    | Patch_main
  [@@deriving sexp_of, enumerate, string]

  include Comparable.S with type t := t
end

(** Compile from [Stlc.t] string repr to GLSL, pass handlers to dump the sexp
    output of each [Passes.t] if desired (defaults to none) *)
val compile : ?dump:(Sexp.t -> unit) Passes.Map.t -> string -> string Compiler_error.t
