open Core
open Glml_compiler

let test source =
  match compile source with
  | Error err -> print_endline (Compiler_error.to_string_hum ~source err)
  | Ok glsl -> print_endline glsl
;;

let test_term s = test ("let main (coord : vec2) = " ^ s)

let%expect_test "simple tests for compile_stlc" =
  test_term "let x = 2.0 in [ 12.0 * x + 10.0, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        float x_1 = 2.;
        float anf_4 = (12. * x_1);
        float anf_5 = (anf_4 + 10.);
        return vec3(anf_5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "if true && false then [ 1.0, 0.0, 0.0 ] else [ 0.0, 0.0, 0.0 ]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        bool anf_2 = (true && false);
        if (anf_2) {
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
    let main = fun (u : vec2) -> [f 10.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float n;
    float f_0(float x_1) {
        return (x_1 + n);
    }
    vec3 main_pure(vec2 u_2) {
        float anf_5 = f_0(10.);
        return vec3(anf_5, 0., 0.);
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
    let main (u : vec2) = [ f 10.0, 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float n;
    float f_0(float x_1) {
        return (x_1 + n);
    }
    vec3 main_pure(vec2 u_2) {
        float anf_5 = f_0(10.);
        return vec3(anf_5, 0., 0.);
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
      let m = [ [1.0, 0.0, 0.0], [ 0.0, 1.0, 0.0 ], [ 0.0, 0.0, 1.0] ] in
      let m = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]] in
      let v = [ 1.0, 2.0 ] in
      [ 1.0, 0.0, 0.0 ]
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
  test_term "let v = [ 1.0, 2.0, 3.0 ] in [ v.0, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        vec3 v_1 = vec3(1., 2., 3.);
        float anf_3 = v_1[0];
        return vec3(anf_3, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term
    {|
    let m = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]] in
    let c = m.0 in
    [c.0, c.1, c.2]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        mat3 m_1 = mat3(1., 0., 0., 0., 1., 0., 0., 0., 1.);
        vec3 c_2 = m_1[0];
        float anf_7 = c_2[0];
        float anf_8 = c_2[1];
        float anf_9 = c_2[2];
        return vec3(anf_7, anf_8, anf_9);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "[0.0, 0.0, 0.0].4";
  [%expect
    {|
    [typecheck] at 1:27-1:44: vec index out of bounds
      n: 3
      i: 4
      |
    1 | let main (coord : vec2) = [0.0, 0.0, 0.0].4
      |                           ^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "builtins" =
  test_term "let v = [ 1.0, 2.0, 3.0 ] in [ #sin(1.0), #dot(v, v), #length(v) ]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        vec3 v_1 = vec3(1., 2., 3.);
        float anf_5 = sin(1.);
        float anf_6 = dot(v_1, v_1);
        float anf_7 = length(v_1);
        return vec3(anf_5, anf_6, anf_7);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "#cross([1.0, 2.0, 3.0], [0.0, 2.0, 5.0])";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        vec3 anf_2 = vec3(1., 2., 3.);
        vec3 anf_3 = vec3(0., 2., 5.);
        return cross(anf_2, anf_3);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "#cross([ 1.0, 1.0 ], [ 0.0, 0.0 ])";
  [%expect
    {|
    [typecheck] at 1:27-1:61: type mismatch
      ty: (vec 2)
      ty': (vec 3)
      |
    1 | let main (coord : vec2) = #cross([ 1.0, 1.0 ], [ 0.0, 0.0 ])
      |                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "multi argument functions / lambdas" =
  test
    {|
    let f (x : float) (y : float) = x + y
    let g = fun (x : float) (y : float) -> x - y
    let main (u : vec2) = [ f 10.0 5.0, g 0.0 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float f_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float g_3(float x_4, float y_5) {
        return (x_4 - y_5);
    }
    vec3 main_pure(vec2 u_6) {
        float anf_13 = f_0(10., 5.);
        float anf_14 = g_3(0., 0.);
        return vec3(anf_13, anf_14, 0.);
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
      [ add 1.0, 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float add_3_8(float x_1, float y_2, float z_4) {
        float anf_9 = (x_1 + y_2);
        return (anf_9 + z_4);
    }
    vec3 main_pure(vec2 u_0) {
        float x_1 = 10.;
        float y_2 = 5.;
        float anf_10 = add_3_8(x_1, y_2, 1.);
        return vec3(anf_10, 0., 0.);
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
        ([ g 1.0, 0.0, 0.0 ])
      in
      f 10.0
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float g_3_9(float x_2, float y_4) {
        return (x_2 + y_4);
    }
    vec3 f_1_8(float x_2) {
        float anf_10 = g_3_9(x_2, 1.);
        return vec3(anf_10, 0., 0.);
    }
    vec3 main_pure(vec2 u_0) {
        return f_1_8(10.);
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
    {|
    [lambda_lift] at 4:7-4:8: first-class functions are not supported
      |
    4 |       f
      |       ^
    |}];
  test
    {|
    let apply_f (f : float -> float) (x : float) = f x
    let main (u : vec2) =
      [ apply_f (fun (x : float) -> x + 1.0) 10.0, 0.0, 0.0 ]
    |};
  [%expect
    {|
    [lambda_lift] at 4:18-4:44: first-class anon functions are unsupported
      |
    4 |       [ apply_f (fun (x : float) -> x + 1.0) 10.0, 0.0, 0.0 ]
      |                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "recursive functions" =
  test
    {|
    let rec fact (n : int) (acc : int) : int =
      if n = 0 then acc else fact (n - 1) (acc * n)

    let main (u : vec2) =
      let num = fact 5 1 in
      [ 0., 0., 0. ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    int fact_0(int n_1, int acc_2) {
        int _iter_15 = 0;
        while ((_iter_15 < 1000)) {
            bool anf_12 = (n_1 == 0);
            if (anf_12) {
                return acc_2;
            } else {
                int anf_13 = (n_1 - 1);
                int anf_14 = (acc_2 * n_1);
                n_1 = anf_13;
                acc_2 = anf_14;
                int _iter_inc_16 = (_iter_15 + 1);
                _iter_15 = _iter_inc_16;
                continue;
            }
        }
        return 0;
    }
    vec3 main_pure(vec2 u_3) {
        int num_4 = fact_0(5, 1);
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "structs" =
  test
    {|
    type point = { x: float, y: float }
    type color = { r: float, g: float, b: float }

    let make_red (p: point) =
      let p_y = p.y in
      { r = p_y, g = 0.0, b = 0.0 }

    let main (u: vec2) =
      let p = { x = 1.0, y = 2.0 } in
      let c = make_red p in
      [c.r, c.g, c.b]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct point {
        float x;
        float y;
    };
    struct color {
        float r;
        float g;
        float b;
    };
    color make_red_0(point p_1) {
        float p_y_2 = p_1.y;
        return color(p_y_2, 0., 0.);
    }
    vec3 main_pure(vec2 u_3) {
        point p_4 = point(1., 2.);
        color c_5 = make_red_0(p_4);
        float anf_11 = c_5.r;
        float anf_12 = c_5.g;
        float anf_13 = c_5.b;
        return vec3(anf_11, anf_12, anf_13);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type point = { x: float, y: float }
    type color = { r: float, g: float, b: float }

    let make_red (p: point) =
      let col =
        if true then
          { r = 1.0, g = 0.0, b = 0.0 }
        else
          { r = 0.0, g = 0.0, b = 1.0 }
      in
      col

    let main (u: vec2) =
      let p = { x = 1.0, y = 2.0 } in
      let c = make_red p in
      [c.r, c.g, c.b]
  |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct point {
        float x;
        float y;
    };
    struct color {
        float r;
        float g;
        float b;
    };
    color make_red_0(point p_1) {
        color col_2 = color(0., 0., 0.);
        if (true) {
            col_2 = color(1., 0., 0.);
        } else {
            col_2 = color(0., 0., 1.);
        }
        return col_2;
    }
    vec3 main_pure(vec2 u_3) {
        point p_4 = point(1., 2.);
        color c_5 = make_red_0(p_4);
        float anf_10 = c_5.r;
        float anf_11 = c_5.g;
        float anf_12 = c_5.b;
        return vec3(anf_10, anf_11, anf_12);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type point = { x: float, y: float }

    let main (u: vec2) =
      let p = { x = 1.0, z = 2.0 } in
      [p.x, p.x, p.x]
    |};
  [%expect
    {|
    [typecheck] at 5:15-5:35: record does not match any known struct
      provided_fields: (x z)
      |
    5 |       let p = { x = 1.0, z = 2.0 } in
      |               ^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "nested structs" =
  let test_program =
    {|
    type point = { x: float, y: float }
    type segment = { start: point, end: point }

    let make_seg (u: float) =
      let s =
        if true then
          { start = { x = 0.0, y = 0.0 }, end = { x = 1.0, y = 1.0 } }
        else
          { start = { x = 1.0, y = 1.0 }, end = { x = 0.0, y = 0.0 } }
      in
      s

    let main (u: vec2) =
      let seg = make_seg 1.0 in
      let c = seg.end.x in
      [c, c, c]
    |}
  in
  test test_program;
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct point {
        float x;
        float y;
    };
    struct segment {
        point start;
        point end;
    };
    segment make_seg_0(float u_1) {
        segment s_2 = segment(point(0., 0.), point(0., 0.));
        if (true) {
            point anf_9 = point(0., 0.);
            point anf_10 = point(1., 1.);
            s_2 = segment(anf_9, anf_10);
        } else {
            point anf_11 = point(1., 1.);
            point anf_12 = point(0., 0.);
            s_2 = segment(anf_11, anf_12);
        }
        return s_2;
    }
    vec3 main_pure(vec2 u_3) {
        segment seg_4 = make_seg_0(1.);
        point anf_13 = seg_4.end;
        float c_5 = anf_13.x;
        return vec3(c_5, c_5, c_5);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "monomorphization tests" =
  test
    {|
    let id x = x
    let main (coord : vec2) =
      let a = id 1.0 in
      let b = id true in
      if b then [a, 0.0, 0.0] else [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    bool id_0_bool_to_bool_10(bool x_1) {
        return x_1;
    }
    float id_0_float_to_float_11(float x_1) {
        return x_1;
    }
    vec3 main_pure(vec2 coord_2) {
        float a_3 = id_0_float_to_float_11(1.);
        bool b_4 = id_0_bool_to_bool_10(true);
        if (b_4) {
            return vec3(a_3, 0., 0.);
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
    let main (coord : vec2) =
      let id x = x in
      let a = id 1.0 in
      let b = id true in
      if b then [a, 0.0, 0.0] else [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    bool id_1_bool_to_bool_10_12(bool x_2) {
        return x_2;
    }
    float id_1_float_to_float_11_13(float x_2) {
        return x_2;
    }
    vec3 main_pure(vec2 coord_0) {
        float a_3 = id_1_float_to_float_11_13(1.);
        bool b_4 = id_1_bool_to_bool_10_12(true);
        if (b_4) {
            return vec3(a_3, 0., 0.);
        } else {
            return vec3(0., 0., 0.);
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
  |}];
  (* Unused polymorphic function *)
  test
    {|
    let id x = x
    let main (coord : vec2) = [1.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_2) {
        return vec3(1., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
  |}];
  (* Validate no duplication of polymorphic function *)
  test
    {|
    let main (coord : vec2) =
      let id x = x in
      let a = id 1.0 in
      let b = id 2.0 in
      [a, b, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float id_1_float_to_float_10_11(float x_2) {
        return x_2;
    }
    vec3 main_pure(vec2 coord_0) {
        float a_3 = id_1_float_to_float_10_11(1.);
        float b_4 = id_1_float_to_float_10_11(2.);
        return vec3(a_3, b_4, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "advanced monomorphization example" =
  test
    {|
    let id x = x
    let const x y = x
    let main (coord : vec2) =
      let a = id 1.0 in
      let b = const 2.0 true in
      [a, b, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float const_2_float_to_bool_to_float_17(float x_3, bool y_4) {
        return x_3;
    }
    float id_0_float_to_float_18(float x_1) {
        return x_1;
    }
    vec3 main_pure(vec2 coord_5) {
        float a_6 = id_0_float_to_float_18(1.);
        float b_7 = const_2_float_to_bool_to_float_17(2., true);
        return vec3(a_6, b_7, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "constrained polymorphism tests" =
  test
    {|
    let f x y = x * y
    let main (coord : vec2) = [f 1.0 2.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float f_0_float_to_float_to_float_12(float x_1, float y_2) {
        return (x_1 * y_2);
    }
    vec3 main_pure(vec2 coord_3) {
        float anf_13 = f_0_float_to_float_to_float_12(1., 2.);
        return vec3(anf_13, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
  |}];
  test
    {|
    let scale x = x * 2.0 - 1.0

    let main (coord : vec2) =
      let v = scale [scale 1.0, 2.0] in
      [v.0, v.1, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float scale_0_float_to_float_13(float x_1) {
        float anf_15 = (x_1 * 2.);
        return (anf_15 - 1.);
    }
    vec2 scale_0_vec2_to_vec2_14(vec2 x_1) {
        vec2 anf_16 = (x_1 * 2.);
        return (anf_16 - 1.);
    }
    vec3 main_pure(vec2 coord_2) {
        float anf_17 = scale_0_float_to_float_13(1.);
        vec2 anf_18 = vec2(anf_17, 2.);
        vec2 v_3 = scale_0_vec2_to_vec2_14(anf_18);
        float anf_19 = v_3[0];
        float anf_20 = v_3[1];
        return vec3(anf_19, anf_20, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
  |}];
  test
    {|
    let f v = (v - #floor(v)).0

    let main (coord : vec2) =
      let a = f [0.5, 1.5] in
      let b = f [0.5, 1.5, 2.5] in
      [a, b, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float f_0_vec3_to_float_15(vec3 v_1) {
        vec3 anf_17 = floor(v_1);
        vec3 anf_18 = (v_1 - anf_17);
        return anf_18[0];
    }
    float f_0_vec2_to_float_16(vec2 v_1) {
        vec2 anf_19 = floor(v_1);
        vec2 anf_20 = (v_1 - anf_19);
        return anf_20[0];
    }
    vec3 main_pure(vec2 coord_2) {
        vec2 anf_21 = vec2(0.5, 1.5);
        float a_3 = f_0_vec2_to_float_16(anf_21);
        vec3 anf_22 = vec3(0.5, 1.5, 2.5);
        float b_4 = f_0_vec3_to_float_15(anf_22);
        return vec3(a_3, b_4, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "variants and matching" =
  test
    {|
    type shape =
      | Circle of float
      | Rect of float * float
      | Empty

    let area (s : shape) =
      match s with
      | Circle r -> 3.14159 * r * r
      | Rect (w, h) -> w * h
      | Empty -> 0.0

    let main (coord : vec2) =
      let a = area (Circle 2.0) in
      let b = area (Rect (3.0, 4.0)) in
      let c = area Empty in
      [a, b, c]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct shape {
        int tag;
        float Circle_0;
        float Rect_0;
        float Rect_1;
    };
    float area_0(shape s_1) {
        int _lv_tag_20 = s_1.tag;
        switch (_lv_tag_20) {
            case 0: {
                float r_2 = s_1.Circle_0;
                float anf_16 = (3.14159 * r_2);
                return (anf_16 * r_2);
                break;
            }
            case 1: {
                float w_3 = s_1.Rect_0;
                float h_4 = s_1.Rect_1;
                return (w_3 * h_4);
                break;
            }
            default: {
                return 0.;
                break;
            }
        }
    }
    vec3 main_pure(vec2 coord_5) {
        shape anf_17 = shape(0, 2., 0., 0.);
        float a_6 = area_0(anf_17);
        shape anf_18 = shape(1, 0., 3., 4.);
        float b_7 = area_0(anf_18);
        shape anf_19 = shape(2, 0., 0., 0.);
        float c_8 = area_0(anf_19);
        return vec3(a_6, b_7, c_8);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "variant match in let binding" =
  test
    {|
    type opt =
      | Some of float
      | None

    let main (coord : vec2) =
      let x = Some 5.0 in
      let v = match x with
        | Some f -> f
        | None -> 0.0
      in
      [v, v, v]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct opt {
        int tag;
        float Some_0;
    };
    vec3 main_pure(vec2 coord_0) {
        opt x_1 = opt(0, 5.);
        int _lv_tag_5 = x_1.tag;
        float v_2 = 0.;
        switch (_lv_tag_5) {
            case 0: {
                float f_3 = x_1.Some_0;
                v_2 = f_3;
                break;
            }
            default: {
                v_2 = 0.;
                break;
            }
        }
        return vec3(v_2, v_2, v_2);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "variant exhaustive checking and incorrect maching" =
  test
    {|
    type color = | Red | Green | Blue

    let main (coord : vec2) =
      let v = match Red with
        | Red -> 1.0
        | Blue -> 2.0
      in
      [v, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 5:15-7:22: non-exhaustive match
      missing: (Green)
      |
    5 |       let v = match Red with
    6 |         | Red -> 1.0
    7 |         | Blue -> 2.0
      |
    |}];
  test
    {|
    type shape =
      | Circle of float
      | Empty

    let main (coord : vec2) =
      let s = Circle (1.0, 2.0) in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 7:15-7:32: wrong number of args to constructor
      ctor: Circle
      |
    7 |       let s = Circle (1.0, 2.0) in
      |               ^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "toplevel constant (atomic only)" =
  test
    {|
    let pi = 3.14159

    let main (u : vec2) = [pi, pi, pi]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const float pi_0 = 3.14159;
    vec3 main_pure(vec2 u_1) {
        return vec3(pi_0, pi_0, pi_0);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let x = #sin(1.0) + #cos(2.0)

    let main (u : vec2) = [x, x, x]
    |};
  [%expect
    {|
    [translate] at 2:5-2:34: top-level constant must be atomic
      name: x_0
      |
    2 |     let x = #sin(1.0) + #cos(2.0)
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "promotion of ints to floats" =
  test
    {|
    let main (u : vec2) =
      let b = 1 + 2 in
      let a = b + 2. in
      [b, a, 3]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 u_0) {
        int b_1 = (1 + 2);
        float pf_5 = float(b_1);
        float a_2 = (pf_5 + 2.);
        float pf_6 = float(b_1);
        return vec3(pf_6, a_2, 3.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "bool match" =
  test
    {|
    #extern bool b
    let main (coord : vec2) =
      match b with
      | true -> [1.0, 0.0, 0.0]
      | false -> [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform bool b;
    vec3 main_pure(vec2 coord_0) {
        if (b) {
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
    #extern bool b
    let main (coord : vec2) =
      let x = match b with
        | true -> 1.0
      in [x, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-5:22: non-exhaustive bool match (missing true or false)
      |
    4 |       let x = match b with
    5 |         | true -> 1.0
      |
    |}];
  test
    {|
    #extern bool b
    let main (coord : vec2) =
      let x = match b with
        | true -> 1.0
        | true -> 1.0
        | false -> 1.0
      in [x, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-7:23: duplicate bool pattern
      b: true
      |
    4 |       let x = match b with
    5 |         | true -> 1.0
    6 |         | true -> 1.0
    7 |         | false -> 1.0
      |
    |}];
  test
    {|
    #extern bool b
    let main (coord : vec2) =
      let x = match b with
        | false -> 1.0
        | _ -> 0.0
      in [x, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform bool b;
    vec3 main_pure(vec2 coord_0) {
        float x_1 = 0.;
        if (b) {
            bool __2 = b;
            x_1 = 0.;
        } else {
            x_1 = 1.;
        }
        return vec3(x_1, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "int match" =
  test
    {|
    #extern int n
    let main (coord : vec2) =
      let x = match n with
        | 0 -> 0.0
        | 1 -> 1.0
        | _ -> 2.0
      in [x, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform int n;
    vec3 main_pure(vec2 coord_0) {
        float x_1 = 0.;
        switch (n) {
            case 0: {
                x_1 = 0.;
                break;
            }
            case 1: {
                x_1 = 1.;
                break;
            }
            default: {
                int __2 = n;
                x_1 = 2.;
                break;
            }
        }
        return vec3(x_1, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    #extern int n
    let main (coord : vec2) =
      let x = match n with
        | 0 -> 0.0
        | 4 -> 0.0
      in [x, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-6:19: int match must have a catch-all
      |
    4 |       let x = match n with
    5 |         | 0 -> 0.0
    6 |         | 4 -> 0.0
      |
    |}];
  test
    {|
    #extern int n
    let main (coord : vec2) =
      let x = match n with
        | 0 -> 0.0
        | 0 -> 1.0
        | k -> 0.0
      in [x, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-7:19: duplicate int pattern
      n: 0
      |
    4 |       let x = match n with
    5 |         | 0 -> 0.0
    6 |         | 0 -> 1.0
    7 |         | k -> 0.0
      |
    |}]
;;

let%expect_test "float match" =
  test
    {|
    #extern float x
    let main (coord : vec2) =
      let c = match x with
        | 1.0 -> 0.0
        | 2.5 -> 1.0
        | _ -> 2.0
      in [c, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float x;
    vec3 main_pure(vec2 coord_0) {
        bool _lv_cmp_5 = (x == 1.);
        float c_1 = 0.;
        if (_lv_cmp_5) {
            c_1 = 0.;
        } else {
            bool _lv_cmp_4 = (x == 2.5);
            if (_lv_cmp_4) {
                c_1 = 1.;
            } else {
                float __2 = x;
                c_1 = 2.;
            }
        }
        return vec3(c_1, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Float match in return position *)
  test
    {|
    #extern float x
    let main (coord : vec2) =
      match x with
        | 0.0 -> [1.0, 0.0, 0.0]
        | 1.0 -> [0.0, 1.0, 0.0]
        | _ -> [0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float x;
    vec3 main_pure(vec2 coord_0) {
        bool _lv_cmp_4 = (x == 0.);
        if (_lv_cmp_4) {
            return vec3(1., 0., 0.);
        } else {
            bool _lv_cmp_3 = (x == 1.);
            if (_lv_cmp_3) {
                return vec3(0., 1., 0.);
            } else {
                float __1 = x;
                return vec3(0., 0., 1.);
            }
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    #extern float x
    let main (coord : vec2) =
      match x with
        | 0.0 -> [1.0, 0.0, 0.0]
        | 1.0 -> [0.0, 1.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:7-6:33: float match must have a catch-all
      |
    4 |       match x with
    5 |         | 0.0 -> [1.0, 0.0, 0.0]
    6 |         | 1.0 -> [0.0, 1.0, 0.0]
      |
    |}];
  test
    {|
    #extern float x
    let main (coord : vec2) =
      match x with
        | 0.0 -> [1.0, 0.0, 0.0]
        | 0.0 -> [0.0, 1.0, 0.0]
        | _ -> [0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 4:7-7:31: duplicate float pattern
      f: 0
      |
    4 |       match x with
    5 |         | 0.0 -> [1.0, 0.0, 0.0]
    6 |         | 0.0 -> [0.0, 1.0, 0.0]
    7 |         | _ -> [0.0, 0.0, 1.0]
      |
    |}];
  test
    {|
    #extern float x
    let main (coord : vec2) =
      match x with
        | 0.0 -> [1.0, 0.0, 0.0]
        | -0.0 -> [0.0, 1.0, 0.0]
        | _ -> [0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 4:7-7:31: duplicate float pattern
      f: -0
      |
    4 |       match x with
    5 |         | 0.0 -> [1.0, 0.0, 0.0]
    6 |         | -0.0 -> [0.0, 1.0, 0.0]
    7 |         | _ -> [0.0, 0.0, 1.0]
      |
    |}]
;;
