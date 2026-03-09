open Core

(** Passes in compiler available to be dumped, using GADT witnesses to have
    generic [sexp_of_pass] function with [trace] *)
module Passes = struct
  type dump_asts =
    | Stlc of Stlc.t
    | Uniquify of Stlc.t
    | Typecheck of Typecheck.t
    | Uncurry of Uncurry.t
    | Lambda_lift of Lambda_lift.t
    | Anf of Anf.t
    | Tail_call of Tail_call.t
    | Translate of Glsl.t
    | Patch_main of Glsl.t
  [@@deriving typed_variants]

  type 'a pass = 'a Typed_variant_of_dump_asts.t

  let sexp_of_pass : type a. a pass -> a -> Sexp.t = function
    | Stlc -> Stlc.sexp_of_t
    | Uniquify -> Stlc.sexp_of_t
    | Typecheck -> Typecheck.sexp_of_t
    | Uncurry -> Uncurry.sexp_of_t
    | Lambda_lift -> Lambda_lift.sexp_of_t
    | Anf -> Anf.sexp_of_t
    | Tail_call -> Tail_call.sexp_of_t
    | Translate -> Glsl.sexp_of_t
    | Patch_main -> Glsl.sexp_of_t
  ;;

  module T = struct
    include Typed_variant_of_dump_asts.Packed

    let to_string ({ f = T p } : t) = Typed_variant_of_dump_asts.name p
    let of_string s = List.find_exn all ~f:(fun t -> String.equal s (to_string t))
  end

  include T
  include Comparable.Make (T)

  let of_pass : type a. a pass -> t = Typed_variant_of_dump_asts.Packed.pack
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
  let%bind t = Uniquify.uniquify t in
  trace Uniquify t;
  let%bind t = Typecheck.typecheck t in
  trace Typecheck t;
  let t = Uncurry.uncurry t in
  trace Uncurry t;
  let%bind t = Lambda_lift.lift t in
  trace Lambda_lift t;
  let t = Anf.to_anf t in
  trace Anf t;
  let%bind t = Tail_call.remove_rec t in
  trace Tail_call t;
  let%bind glsl = Translate.translate t in
  trace Translate glsl;
  let%bind glsl = Patch_main.patch glsl in
  trace Patch_main glsl;
  return (Glsl.to_string glsl)
;;
