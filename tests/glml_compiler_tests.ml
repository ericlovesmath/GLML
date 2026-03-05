open Core
open Glml_compiler

let test s =
  match compile s with
  | Error err -> print_s (Error.sexp_of_t err)
  | Ok glsl -> print_endline glsl
;;

let test_term s = test ("let main (coord : vec2) = " ^ s)

let%expect_test "simple tests for compile_stlc" =
  test_term "let x = 2.0 in < 12.0 * x + 10.0, 0.0, 0.0>";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        float x_1 = 2.;
        float anf_2 = (12. * x_1);
        float anf_3 = (anf_2 + 10.);
        return vec3(anf_3, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "if true && false then < 1.0, 0.0, 0.0 > else < 0.0, 0.0, 0.0 >";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        bool anf_1 = (true && false);
        if (anf_1) {
            return vec3(1., 0., 0.);
        } else {
            return vec3(0., 0., 0.);
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    #extern float n
    let f = fun (x : float) -> x + n
    let main = fun (u : vec2) -> <f 10.0, 0.0, 0.0>
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float n;
    float f_1(float x_0) {
        return (x_0 + n);
    }
    vec3 main_pure(vec2 u_2) {
        float anf_3 = f_1(10.);
        return vec3(anf_3, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    #extern float n
    let f (x : float) = x + n
    let main (u : vec2) = < f 10.0, 0.0, 0.0 >
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float n;
    float f_1(float x_0) {
        return (x_0 + n);
    }
    vec3 main_pure(vec2 u_2) {
        float anf_3 = f_1(10.);
        return vec3(anf_3, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "generic vectors and matrices" =
  test
    {|
    let main (u : vec2) =
      let m = < <1.0, 0.0, 0.0>, < 0.0, 1.0, 0.0 >, < 0.0, 0.0, 1.0> > in
      let m = <<1.0, 2.0>, <3.0, 4.0>, <5.0, 6.0>> in
      let v = < 1.0, 2.0 > in
      < 1.0, 0.0, 0.0 >
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 u_0) {
        mat3 m_1 = mat3(1., 0., 0., 0., 1., 0., 0., 0., 1.);
        mat3x2 m_2 = mat3x2(1., 2., 3., 4., 5., 6.);
        vec2 v_3 = vec2(1., 2.);
        return vec3(1., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "indexing" =
  test_term "let v = < 1.0, 2.0, 3.0 > in < v[0], 0.0, 0.0>";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        vec3 v_1 = vec3(1., 2., 3.);
        float anf_2 = v_1[0];
        return vec3(anf_2, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term
    {|
    let m = <<1.0, 0.0, 0.0>, <0.0, 1.0, 0.0>, <0.0, 0.0, 1.0>> in
    let c = m[0] in
    <c[0], c[1], c[2]>
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        mat3 m_1 = mat3(1., 0., 0., 0., 1., 0., 0., 0., 1.);
        vec3 c_2 = m_1[0];
        float anf_3 = c_2[0];
        float anf_4 = c_2[1];
        float anf_5 = c_2[2];
        return vec3(anf_3, anf_4, anf_5);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "<0.0, 0.0, 0.0>[4]";
  [%expect {| (typecheck: "vec index out of bounds" (n 3) (i 4) (loc (1:27 - 1:45))) |}]
;;

let%expect_test "builtins" =
  test_term "let v = < 1.0, 2.0, 3.0 > in < #sin(1.0), #dot(v, v), #length(v) >";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        vec3 v_1 = vec3(1., 2., 3.);
        float anf_2 = sin(1.);
        float anf_3 = dot(v_1, v_1);
        float anf_4 = length(v_1);
        return vec3(anf_2, anf_3, anf_4);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "#cross(<1.0, 2.0, 3.0>, <0.0, 2.0, 5.0>)";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        vec3 anf_1 = vec3(1., 2., 3.);
        vec3 anf_2 = vec3(0., 2., 5.);
        return cross(anf_1, anf_2);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "#cross(< 1.0, 1.0 >, < 0.0, 0.0 >)";
  [%expect
    {|
    (typecheck: "invalid geometric call" (name Cross) (tys ((vec 2) (vec 2)))
     (loc (1:27 - 1:61)))
    |}]
;;

let%expect_test "multi argument functions / lambdas" =
  test
    {|
    let f (x : float) (y : float) = x + y
    let g = fun (x : float) (y : float) -> x - y
    let main (u : vec2) = < f 10.0 5.0, g 0.0 0.0, 0.0 >
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float f_2(float x_0, float y_1) {
        return (x_0 + y_1);
    }
    float g_5(float x_3, float y_4) {
        return (x_3 - y_4);
    }
    vec3 main_pure(vec2 u_6) {
        float anf_7 = f_2(10., 5.);
        float anf_8 = g_5(0., 0.);
        return vec3(anf_7, anf_8, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "lambda lifting" =
  test
    {|
    let main (u : vec2) =
      let x = 10.0 in
      let y = 5.0 in
      let add (z : float) = x + y + z in
      < add 1.0, 0.0, 0.0 >
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float add_4_5(float x_1, float y_2, float z_3) {
        float anf_6 = (x_1 + y_2);
        return (anf_6 + z_3);
    }
    vec3 main_pure(vec2 u_0) {
        float x_1 = 10.;
        float y_2 = 5.;
        float anf_7 = add_4_5(x_1, y_2, 1.);
        return vec3(anf_7, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let main (u : vec2) =
      let f (x : float) =
        let g (y : float) = x + y in
        (< g 1.0, 0.0, 0.0 >)
      in
      f 10.0
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float g_3_6(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    vec3 f_4_5(float x_1) {
        float anf_7 = g_3_6(x_1, 1.);
        return vec3(anf_7, 0., 0.);
    }
    vec3 main_pure(vec2 u_0) {
        return f_4_5(10.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let main (u : vec2) =
      let f = fun (x : float) -> x + 1.0 in
      f
    |};
  [%expect
    {| ("first-class functions are not supported" (t.loc (4:7 - 4:8))) |}];
  test
    {|
    let apply_f (f : float -> float) (x : float) = f x
    let main (u : vec2) =
      < apply_f (fun (x : float) -> x + 1.0) 10.0, 0.0, 0.0 >
    |};
  [%expect
    {| ("first-class anon functions are unsupported" (t.loc (4:18 - 4:44))) |}]
;;
