open Core

(** Passes in compiler available to be dumped, using GADT witnesses to have
    generic [sexp_of_pass] function with [trace] *)
module Passes = struct
  type _ pass =
    | Stlc : Stlc.t pass
    | Uniquify : Stlc.t pass
    | Typecheck : Typecheck.t pass
    | Uncurry : Uncurry.t pass
    | Lambda_lift : Lambda_lift.t pass
    | Anf : Anf.t pass
    | Translate : Glsl.t pass
    | Patch_main : Glsl.t pass

  let sexp_of_pass : type a. a pass -> a -> Sexp.t = function
    | Stlc -> Stlc.sexp_of_t
    | Uniquify -> Stlc.sexp_of_t
    | Typecheck -> Typecheck.sexp_of_t
    | Uncurry -> Uncurry.sexp_of_t
    | Lambda_lift -> Lambda_lift.sexp_of_t
    | Anf -> Anf.sexp_of_t
    | Translate -> Glsl.sexp_of_t
    | Patch_main -> Glsl.sexp_of_t
  ;;

  (* TODO: Maybe something like [typed_variants] in [ppx_typed_fields] can
   cut down a lot of the repeated code here? This is basically a [Packed.t] *)
  module T = struct
    type t =
      | Stlc
      | Uniquify
      | Typecheck
      | Uncurry
      | Lambda_lift
      | Anf
      | Translate
      | Patch_main
    [@@deriving compare, sexp, enumerate, string ~capitalize:"lower sentence case"]
  end

  include T
  include Comparable.Make (T)

  let of_pass : type a. a pass -> t = function
    | Stlc -> Stlc
    | Uniquify -> Uniquify
    | Typecheck -> Typecheck
    | Uncurry -> Uncurry
    | Lambda_lift -> Lambda_lift
    | Anf -> Anf
    | Translate -> Translate
    | Patch_main -> Patch_main
  ;;
end

let compile ?(dump : (Sexp.t -> unit) Passes.Map.t = Passes.Map.empty) (s : string)
  : string Or_error.t
  =
  let trace : type a. a Passes.pass -> a -> unit =
    fun pass value ->
    Passes.of_pass pass
    |> Map.find dump
    |> Option.iter ~f:(fun f -> f (Passes.sexp_of_pass pass value))
  in
  let open Or_error.Let_syntax in
  Utils.reset ();
  let%bind tokens = Lexer.lex (Lexer.init s) in
  let%bind t = Chomp.run Parser.glml_p tokens in
  trace Stlc t;
  let t = Uniquify.uniquify t in
  trace Uniquify t;
  let%bind t = Typecheck.typecheck t in
  trace Typecheck t;
  let t = Uncurry.uncurry t in
  trace Uncurry t;
  let%bind t = Lambda_lift.lift t in
  trace Lambda_lift t;
  let t = Anf.to_anf t in
  trace Anf t;
  let glsl = Translate.translate t in
  trace Translate glsl;
  let glsl = Patch_main.patch glsl in
  trace Patch_main glsl;
  return (Glsl.to_string glsl)
;;
