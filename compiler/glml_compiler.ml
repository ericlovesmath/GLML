open Core
module Compiler_error = Compiler_error

(** Passes in compiler available to be dumped *)
module Passes = struct
  module T = struct
    type t =
      | Stlc
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
      | Translate
      | Patch_main
    [@@deriving compare, sexp, enumerate, string ~capitalize:"lower sentence case"]
  end

  include T
  include Comparable.Make (T)
end

let compile ?(dump : (Sexp.t -> unit) Passes.Map.t = Passes.Map.empty) (s : string)
  : string Compiler_error.t
  =
  let trace pass sexp = Map.find dump pass |> Option.iter ~f:(fun f -> f sexp) in
  let open Compiler_error.Let_syntax in
  Utils.reset ();
  let%bind tokens = Lexer.lex (Lexer.init s) in
  let%bind t = Chomp.run Parser.glml_p tokens in
  trace Stlc (Stlc.sexp_of_t t);
  let%bind t = Uniquify.uniquify t in
  trace Uniquify (Stlc.sexp_of_t t);
  let%bind t = Typecheck.typecheck t in
  trace Typecheck (Typecheck.sexp_of_t t);
  let%bind t = Monomorphize.monomorphize t in
  trace Monomorphize (Monomorphize.sexp_of_t t);
  let t = Uncurry.uncurry t in
  trace Uncurry (Uncurry.sexp_of_t t);
  let%bind t = Defunctionalize.defunctionalize t in
  trace Defunctionalize (Uncurry.sexp_of_t t);
  let%bind t = Lambda_lift.lift t in
  trace Lambda_lift (Lambda_lift.sexp_of_t t);
  let t = Anf.to_anf t in
  trace Anf (Anf.sexp_of_t t);
  let%bind t = Tail_call.remove_rec t in
  trace Tail_call (Tail_call.sexp_of_t t);
  let%bind t = Lower_variants.lower t in
  trace Lower_variants (Lower_variants.sexp_of_t t);
  let t = Promote_ints.promote t in
  trace Promote_ints (Lower_variants.sexp_of_t t);
  let t = Remove_placeholder.remove t in
  trace Remove_placeholder (Remove_placeholder.sexp_of_t t);
  let%bind glsl = Translate.translate t in
  trace Translate (Glsl.sexp_of_t glsl);
  let%bind glsl = Patch_main.patch glsl in
  trace Patch_main (Glsl.sexp_of_t glsl);
  return (Glsl.to_string glsl)
;;
