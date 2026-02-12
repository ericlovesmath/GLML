open Core

(** Passes in compiler available to be dumped, using GADT witnesses to have
    generic [sexp_of_pass] function with [trace] *)
module Passes = struct
  type _ pass =
    | Stlc : Stlc.t pass
    | Uniquify : Stlc.t pass
    | Typecheck : Stlc.ty String.Map.t pass
    | Anf : (Stlc.ty String.Map.t * Anf.t) pass
    | Translate : Glsl.t pass

  let sexp_of_pass : type a. a pass -> a -> Sexp.t = function
    | Stlc -> Stlc.sexp_of_t
    | Uniquify -> Stlc.sexp_of_t
    | Typecheck -> [%sexp_of: Stlc.ty String.Map.t]
    | Anf -> [%sexp_of: Stlc.ty String.Map.t * Anf.t]
    | Translate -> Glsl.sexp_of_t
  ;;

  module T = struct
    type t =
      | Stlc
      | Uniquify
      | Typecheck
      | Anf
      | Translate
    [@@deriving compare, sexp, enumerate, string ~capitalize:"lower sentence case"]
  end

  include T
  include Comparable.Make (T)

  let of_pass : type a. a pass -> t = function
    | Stlc -> Stlc
    | Uniquify -> Uniquify
    | Typecheck -> Typecheck
    | Anf -> Anf
    | Translate -> Translate
  ;;
end

let compile_source src = Glsl.to_shader (Glsl.of_string src)

let compile_stlc ?(dump : (Sexp.t -> unit) Passes.Map.t = Passes.Map.empty) (s : string)
  : string Or_error.t
  =
  let trace : type a. a Passes.pass -> a -> unit =
    fun pass value ->
    Passes.of_pass pass
    |> Map.find dump
    |> Option.iter ~f:(fun f -> f (Passes.sexp_of_pass pass value))
  in
  let open Or_error.Let_syntax in
  let%bind t = Stlc.of_string s in
  trace Stlc t;
  let t = Uniquify.uniquify t in
  trace Uniquify t;
  let%bind ctx = Typecheck.typecheck t in
  trace Typecheck ctx;
  let ctx, t = Anf.normalize ctx t in
  trace Anf (ctx, t);
  let%bind glsl = Translate.translate ctx t in
  trace Translate glsl;
  return (Glsl.to_shader glsl)
;;

let%expect_test "simple tests for compile_stlc" =
  let test s =
    match compile_stlc s with
    | Error err -> print_s (Error.sexp_of_t err)
    | Ok glsl -> print_endline glsl
  in
  test "(let x = 2.0 in (+ (* 12.0 x) 10.0))";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        float x_0 = 2.;
        float anf_1 = (12. * x_0);
        return (anf_1 + 10.);
    }
    |}];
  test "(if (&& #t #f) (let x = 2.0 in (* x 1.0)) 2.0)";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        bool anf_1 = (true && false);
        if (anf_1) {
            float x_0 = 2.;
            return (x_0 * 1.);
        } else {
            return 2.;
        }
    }
    |}];
  test "(let x = (if #f 0 1) in (* x 2))";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        int x_0 = 0;
        if (false) {
            x_0 = 0;
        } else {
            x_0 = 1;
        }
        return (x_0 * 2);
    }
    |}]
;;
