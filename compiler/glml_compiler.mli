open Core
module Compiler_error = Compiler_error

(** Passes in compiler available to be dumped *)
module Passes : sig
  type t =
    | Frontend
    | Desugar
    | Uniquify
    | Typecheck
    | Promote_ints
    | Monomorphize
    | Uncurry
    | Lambda_lift
    | Defunctionalize
    | Anf
    | Tail_call
    | Lower_variants
    | Remove_placeholder
    | Const_fold
    | Dce
    | Lift_consts
    | Translate
    | Patch_main
  [@@deriving sexp_of, enumerate, string]

  include Comparable.S with type t := t
end

(** Compile from [Stlc.t] string repr to GLSL, pass handlers to dump the sexp
    output of each [Passes.t] if desired (defaults to none) *)
val compile : ?dump:(Sexp.t -> unit) Passes.Map.t -> string -> string Compiler_error.t
