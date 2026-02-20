open Core
open Glml_compiler

let test s =
  match compile_stlc s with
  | Error err -> print_s (Error.sexp_of_t err)
  | Ok glsl -> print_endline glsl
;;

let wrap_main s = [%string "((let main = (fun u : unit -> %{s})))"]

let%expect_test "simple tests for compile_stlc" =
  test (wrap_main "(let x = 2.0 in (vec3 (+ (* 12.0 x) 10.0) 0.0 0.0))");
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    void main() {
        float x_1 = 2.;
        float anf_2 = (12. * x_1);
        float anf_3 = (anf_2 + 10.);
        fragColor = vec3(anf_3, 0., 0.);
        return;
    }
    |}];
  test (wrap_main "(if (&& #t #f) (vec3 1.0 0.0 0.0) (vec3 0.0 0.0 0.0))");
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    void main() {
        bool anf_1 = (true && false);
        if (anf_1) {
            fragColor = vec3(1., 0., 0.);
            return;
        } else {
            fragColor = vec3(0., 0., 0.);
            return;
        }
    }
    |}];
  test
    {|
    ((let f = (fun x : float -> (+ x 1.0)))
     (let main = (fun u : unit -> (vec3 (f 10.0) 0.0 0.0))))
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    float f_1(float x_0) {
        return (x_0 + 1.);
    }
    void main() {
        float anf_3 = f_1(10.);
        fragColor = vec3(anf_3, 0., 0.);
        return;
    }
    |}];
  test
    {|
    ((extern float n)
     (let f = (fun x : float -> (+ x n)))
     (let main = (fun u : unit -> (vec3 (f 10.0) 0.0 0.0))))
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    uniform float n;
    float f_1(float x_0) {
        return (x_0 + n);
    }
    void main() {
        float anf_3 = f_1(10.);
        fragColor = vec3(anf_3, 0., 0.);
        return;
    }
    |}];
  test
    {|
    ((extern float n)
     (let f (x : float) = (+ x n))
     (let main (u : unit) = (vec3 (f 10.0) 0.0 0.0)))
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    uniform float n;
    float f_1(float x_0) {
        return (x_0 + n);
    }
    void main() {
        float anf_3 = f_1(10.);
        fragColor = vec3(anf_3, 0., 0.);
        return;
    }
    |}]
;;

let%expect_test "generic vectors and matrices" =
  test
    {|
    ((let main (u : unit) =
     let m = (mat3 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0) in
     let m2 = (mat3x2 1.0 2.0 3.0 4.0 5.0 6.0) in
     let v = (vec2 1.0 2.0) in
     vec3 1.0 0.0 0.0))
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    void main() {
        mat3 m_1 = mat3(1., 0., 0., 0., 1., 0., 0., 0., 1.);
        mat3x2 m2_2 = mat3x2(1., 2., 3., 4., 5., 6.);
        vec2 v_3 = vec2(1., 2.);
        fragColor = vec3(1., 0., 0.);
        return;
    }
    |}]
;;
