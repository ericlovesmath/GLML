open Core
module Compiler_error = Compiler_error

(** Passes in compiler available to be dumped *)
module Passes = struct
  module T = struct
    type t =
      | Frontend
      | Desugar
      | Uniquify
      | Typecheck
      | Promote_ints
      | Monomorphize
      | Lower_tuples
      | Uncurry
      | Lambda_lift
      | Defunctionalize
      | Lower_variants
      | Anf
      | Optimize
      | Lift_consts
      | Tail_call
      | Translate
      | Patch_main
    [@@deriving compare, sexp, enumerate, string ~capitalize:"lower sentence case"]
  end

  include T
  include Comparable.Make (T)
end

let compile
      ?(dump : (Sexp.t -> unit) Passes.Map.t = Passes.Map.empty)
      ?(optimize : bool = true)
      (s : string)
  : string Compiler_error.t
  =
  let trace pass sexp = Map.find dump pass |> Option.iter ~f:(fun f -> f sexp) in
  let open Compiler_error.Let_syntax in
  Utils.reset ();
  let%bind tokens = Lexer.lex (Lexer.init s) in
  let%bind t = Chomp.run Parser.glml_p tokens in
  trace Frontend (Frontend.sexp_of_t t);
  let%bind t = Desugar.desugar t in
  trace Desugar (Desugar.sexp_of_t t);
  let%bind t = Uniquify.uniquify t in
  trace Uniquify (Desugar.sexp_of_t t);
  let%bind t = Typecheck.typecheck t in
  trace Typecheck (Typecheck.sexp_of_t t);
  let t = Promote_ints.materialize t in
  trace Promote_ints (Typecheck.sexp_of_t t);
  let%bind t = Monomorphize.monomorphize t in
  trace Monomorphize (Monomorphize.sexp_of_t t);
  let%bind t = Lower_tuples.lower t in
  trace Lower_tuples (Lower_tuples.sexp_of_t t);
  let%bind t = Uncurry.uncurry t in
  trace Uncurry (Uncurry.sexp_of_t t);
  let%bind t = Lambda_lift.lift t in
  trace Lambda_lift (Lambda_lift.sexp_of_t t);
  let%bind t = Defunctionalize.defunctionalize t in
  trace Defunctionalize (Lambda_lift.sexp_of_t t);
  let%bind t = Lower_variants.lower t in
  trace Lower_variants (Lower_variants.sexp_of_t t);
  let%bind t = Anf.to_anf t in
  trace Anf (Anf.sexp_of_t t);
  let%bind t =
    if not optimize
    then Ok t
    else (
      let rec go n t =
        if n <= 0
        then Ok t
        else (
          let t = t |> Case_of_case.rewrite |> Inline.rewrite in
          let%bind t = Const_fold.rewrite t in
          let t = t |> Cse.rewrite |> Dce.rewrite in
          go (n - 1) t)
      in
      let%map t = go 3 t in
      trace Optimize (Anf.sexp_of_t t);
      t)
  in
  let%bind t = Lift_consts.lift t in
  trace Lift_consts (Anf.sexp_of_t t);
  let%bind t = Tail_call.remove_rec t in
  trace Tail_call (Tail_call.sexp_of_t t);
  let%bind glsl = Translate.translate t in
  trace Translate (Glsl.sexp_of_t glsl);
  let%bind glsl = Patch_main.patch glsl in
  trace Patch_main (Glsl.sexp_of_t glsl);
  Ok (Glsl.to_string glsl)
;;
