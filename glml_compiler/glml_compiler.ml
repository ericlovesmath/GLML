open Core
module Glsl = Glsl
module Stlc = Stlc
module Uniquify = Uniquify
module Typecheck = Typecheck
module Translate = Translate

let compile_source src = Glsl.to_shader (Glsl.of_string src)

(* TODO: Move this to compile_source *)
let compile_stlc (t : Stlc.t) : string Or_error.t =
  let open Or_error.Let_syntax in
  let t = Uniquify.uniquify t in
  let%bind ctx = Typecheck.typecheck t in
  let ctx, t = Anf.normalize ctx t in
  let%bind glsl = Translate.translate ctx t in
  return (Glsl.to_shader glsl)
;;

let%expect_test "simple tests for compile_stlc" =
  let test s =
    match compile_stlc (Stlc.of_string s) with
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
