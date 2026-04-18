open Runner

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
        float anf_7 = sin(1.);
        float anf_8 = dot(v_1, v_1);
        float anf_9 = length(v_1);
        return vec3(anf_7, anf_8, anf_9);
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
    [patch_main]: unexpected type of main
      t: (Function (name main) (desc ()) (params (((TyVec 2) u_0)))
     (ret_type (TyStruct DFn_5))
     (body ((set () DFn_5 f_1 ((DFn_5 0))) (return f_1))))
    |}];
  test
    {|
    let apply_f (f : float -> float) (x : float) = f x
    let main (u : vec2) =
      [ apply_f (fun x -> x + 1) 10.0, 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_11 {
        int tag;
    };
    float dapply_10(DFn_11 dfn_13, float da_14) {
        return (da_14 + 1.);
    }
    float apply_f_0(DFn_11 f_1, float x_2) {
        return dapply_10(f_1, x_2);
    }
    vec3 main_pure(vec2 u_3) {
        DFn_11 anf_15 = DFn_11(0);
        float anf_16 = apply_f_0(anf_15, 10.);
        return vec3(anf_16, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
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
        int _iter_16 = 0;
        while ((_iter_16 < 1000)) {
            bool anf_13 = (n_1 == 0);
            if (anf_13) {
                return acc_2;
            } else {
                int anf_14 = (n_1 - 1);
                int anf_15 = (acc_2 * n_1);
                n_1 = anf_14;
                acc_2 = anf_15;
                int _iter_inc_17 = (_iter_16 + 1);
                _iter_16 = _iter_inc_17;
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
    struct color {
        float r;
        float g;
        float b;
    };
    struct point {
        float x;
        float y;
    };
    color make_red_0(point p_1) {
        float p_y_2 = p_1.y;
        return color(p_y_2, 0., 0.);
    }
    vec3 main_pure(vec2 u_3) {
        point p_4 = point(1., 2.);
        color c_5 = make_red_0(p_4);
        float anf_16 = c_5.r;
        float anf_17 = c_5.g;
        float anf_18 = c_5.b;
        return vec3(anf_16, anf_17, anf_18);
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
    struct color {
        float r;
        float g;
        float b;
    };
    struct point {
        float x;
        float y;
    };
    color make_red_0(point p_1) {
        color col_2;
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
        float anf_18 = c_5.r;
        float anf_19 = c_5.g;
        float anf_20 = c_5.b;
        return vec3(anf_18, anf_19, anf_20);
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
        segment s_2;
        if (true) {
            point anf_17 = point(0., 0.);
            point anf_18 = point(1., 1.);
            s_2 = segment(anf_17, anf_18);
        } else {
            point anf_19 = point(1., 1.);
            point anf_20 = point(0., 0.);
            s_2 = segment(anf_19, anf_20);
        }
        return s_2;
    }
    vec3 main_pure(vec2 u_3) {
        segment seg_4 = make_seg_0(1.);
        point anf_21 = seg_4.end;
        float c_5 = anf_21.x;
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
    float scale_0_float_to_float_17(float x_1) {
        float anf_19 = (x_1 * 2.);
        return (anf_19 - 1.);
    }
    vec2 scale_0_vec2_to_vec2_18(vec2 x_1) {
        vec2 anf_20 = (x_1 * 2.);
        return (anf_20 - 1.);
    }
    vec3 main_pure(vec2 coord_2) {
        float anf_21 = scale_0_float_to_float_17(1.);
        vec2 anf_22 = vec2(anf_21, 2.);
        vec2 v_3 = scale_0_vec2_to_vec2_18(anf_22);
        float anf_23 = v_3[0];
        float anf_24 = v_3[1];
        return vec3(anf_23, anf_24, 0.);
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
    float f_0_vec2_to_float_21(vec2 v_1) {
        vec2 anf_22 = floor(v_1);
        vec2 anf_23 = (v_1 - anf_22);
        return anf_23[0];
    }
    float f_0_vec3_to_float_20(vec3 v_1) {
        vec3 anf_24 = floor(v_1);
        vec3 anf_25 = (v_1 - anf_24);
        return anf_25[0];
    }
    vec3 main_pure(vec2 coord_2) {
        vec2 anf_26 = vec2(0.5, 1.5);
        float a_3 = f_0_vec2_to_float_21(anf_26);
        vec3 anf_27 = vec3(0.5, 1.5, 2.5);
        float b_4 = f_0_vec3_to_float_20(anf_27);
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
        int _lv_tag_23 = s_1.tag;
        switch (_lv_tag_23) {
            case 0: {
                float r_2 = s_1.Circle_0;
                float anf_19 = (3.14159 * r_2);
                return (anf_19 * r_2);
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
        shape anf_20 = shape(0, 2., 0., 0.);
        float a_6 = area_0(anf_20);
        shape anf_21 = shape(1, 0., 3., 4.);
        float b_7 = area_0(anf_21);
        shape anf_22 = shape(2, 0., 0., 0.);
        float c_8 = area_0(anf_22);
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
        int _lv_tag_6 = x_1.tag;
        float v_2;
        switch (_lv_tag_6) {
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
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const float x_0 = (sin(1.) + cos(2.));
    vec3 main_pure(vec2 u_1) {
        return vec3(x_0, x_0, x_0);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
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

let%expect_test "int broadcasting with vecs and builtins" =
  (* int * vec3 literal *)
  test_term "let n = 2 in n * [0.5, 0.5, 0.5]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        int n_1 = 2;
        vec3 anf_3 = vec3(0.5, 0.5, 0.5);
        float pf_4 = float(n_1);
        return (pf_4 * anf_3);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* #extern int * vec3 *)
  test
    {|
    #extern int n
    let main (u : vec2) = n * [0.5, 0.5, 0.5]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform int n;
    vec3 main_pure(vec2 u_0) {
        vec3 anf_2 = vec3(0.5, 0.5, 0.5);
        float pf_3 = float(n);
        return (pf_3 * anf_2);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "let n = 2 in n + [0.1, 0.2, 0.3]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        int n_1 = 2;
        vec3 anf_3 = vec3(0.1, 0.2, 0.3);
        float pf_4 = float(n_1);
        return (pf_4 + anf_3);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* #extern int to unary GenType builtin *)
  test
    {|
    #extern int n
    let main (u : vec2) =
      let r = #sin(n) in
      [r, r, r]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform int n;
    vec3 main_pure(vec2 u_0) {
        float pf_4 = float(n);
        float r_1 = sin(pf_4);
        return vec3(r_1, r_1, r_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "let r = #abs(5) in [r, r, r]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        float r_1 = abs(5.);
        return vec3(r_1, r_1, r_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "let r = #min(1, 2) in [r, r, r]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        float r_1 = min(1., 2.);
        return vec3(r_1, r_1, r_1);
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
        float x_1;
        if (b) {
            bool _x_2 = b;
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
        float x_1;
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
                int _x_2 = n;
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
        float c_1;
        if (_lv_cmp_5) {
            c_1 = 0.;
        } else {
            bool _lv_cmp_4 = (x == 2.5);
            if (_lv_cmp_4) {
                c_1 = 1.;
            } else {
                float _x_2 = x;
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
                float _x_1 = x;
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

let%expect_test "parametrized structs" =
  (* Simple box: box[float] generates box_float struct *)
  test
    {|
    type box['a] = { value: 'a }
    let f (b: box[float]) : float = b.value
    let main (coord: vec2) : vec3 = [f { value = 1.0 }, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_float {
        float value;
    };
    float f_0(r_box_float b_1) {
        return b_1.value;
    }
    vec3 main_pure(vec2 coord_2) {
        r_box_float anf_6 = r_box_float(1.);
        float anf_7 = f_0(anf_6);
        return vec3(anf_7, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Pair type: two type params *)
  test
    {|
    type pair['a, 'b] = { fst: 'a, snd: 'b }
    let get_fst (p: pair[float, int]) : float = p.fst
    let main (coord: vec2) : vec3 =
      let p = { fst = 1.0, snd = 0 } in
      [get_fst p, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_pair_float_int {
        float fst;
        int snd;
    };
    float get_fst_0(r_pair_float_int p_1) {
        return p_1.fst;
    }
    vec3 main_pure(vec2 coord_2) {
        r_pair_float_int p_3 = r_pair_float_int(1., 0);
        float anf_8 = get_fst_0(p_3);
        return vec3(anf_8, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Inferred type args: no explicit annotation on record literal *)
  test
    {|
    type box['a] = { value: 'a }
    let main (coord: vec2) : vec3 =
      let b = { value = 1.0 } in
      [b.value, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_float {
        float value;
    };
    vec3 main_pure(vec2 coord_0) {
        r_box_float b_1 = r_box_float(1.);
        float anf_4 = b_1.value;
        return vec3(anf_4, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Deduplication: two functions using box[float] produce only one struct *)
  test
    {|
    type box['a] = { value: 'a }
    let get1 (b: box[float]) : float = b.value
    let get2 (b: box[float]) : float = b.value
    let main (coord: vec2) : vec3 =
      [get1 { value = 1.0 } + get2 { value = 2.0 }, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_float {
        float value;
    };
    float get1_0(r_box_float b_1) {
        return b_1.value;
    }
    float get2_2(r_box_float b_3) {
        return b_3.value;
    }
    vec3 main_pure(vec2 coord_4) {
        r_box_float anf_12 = r_box_float(1.);
        float anf_13 = get1_0(anf_12);
        r_box_float anf_14 = r_box_float(2.);
        float anf_15 = get2_2(anf_14);
        float anf_16 = (anf_13 + anf_15);
        return vec3(anf_16, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Error: wrong arity for type application *)
  test
    {|
    type box['a] = { value: 'a }
    let f (b: box[float, int]) : float = b.value
    let main (coord: vec2) : vec3 = [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 3:42-3:49: wrong number of type args for struct
      struct_name: box
      |
    3 |     let f (b: box[float, int]) : float = b.value
      |                                          ^^^^^^^
  |}];
  test
    {|
    type box['a] = { value: 'a }
    type point['a, 'b] = { x : box['a], y: 'b }
    let main (coord: vec2) : vec3 =
    let b = { x = { value = { value = 1.0 } }, y = { value = 2.0 } } in
    [b.x.value.value, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_float {
        float value;
    };
    struct r_box_r_box_float {
        r_box_float value;
    };
    struct r_point_r_box_float_r_box_float {
        r_box_r_box_float x;
        r_box_float y;
    };
    vec3 main_pure(vec2 coord_0) {
        r_box_float anf_10 = r_box_float(1.);
        r_box_r_box_float anf_11 = r_box_r_box_float(anf_10);
        r_box_float anf_12 = r_box_float(2.);
        r_point_r_box_float_r_box_float b_1 = r_point_r_box_float_r_box_float(anf_11, anf_12);
        r_box_r_box_float anf_13 = b_1.x;
        r_box_float anf_14 = anf_13.value;
        float anf_15 = anf_14.value;
        return vec3(anf_15, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "parametrized variants" =
  test
    {|
    type option['a] = Some of 'a | None

    let unwrap (opt : option[int]) (default : int) =
      match opt with
      | Some x -> x
      | None -> default

    let main (uv : vec2) =
      let a = unwrap (Some 10) 10 in
      let b = unwrap None 5 in
      let c = unwrap None 5 in
      [a, b, c]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct v_option_int {
        int tag;
        int Some_0;
    };
    int unwrap_0(v_option_int opt_1, int default_2) {
        int _lv_tag_22 = opt_1.tag;
        switch (_lv_tag_22) {
            case 0: {
                int x_3 = opt_1.Some_0;
                return x_3;
                break;
            }
            default: {
                return default_2;
                break;
            }
        }
    }
    vec3 main_pure(vec2 uv_4) {
        v_option_int anf_19 = v_option_int(0, 10);
        int a_5 = unwrap_0(anf_19, 10);
        v_option_int anf_20 = v_option_int(1, 0);
        int b_6 = unwrap_0(anf_20, 5);
        v_option_int anf_21 = v_option_int(1, 0);
        int c_7 = unwrap_0(anf_21, 5);
        float pf_23 = float(a_5);
        float pf_24 = float(b_6);
        float pf_25 = float(c_7);
        return vec3(pf_23, pf_24, pf_25);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type result['a, 'b] = Ok of 'a | Err of 'b

    let unwrap (r : result[float, int]) (default : float) =
      match r with
      | Ok x -> x
      | Err _ -> default

    let main (uv : vec2) : vec3 =
      let a = unwrap (Ok 5.4) 5.0 in
      let b = unwrap (Err 2) 2.3 in
      [uv.0, a, b]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct v_result_float_int {
        int tag;
        float Ok_0;
        int Err_0;
    };
    float unwrap_0(v_result_float_int r_1, float default_2) {
        int _lv_tag_23 = r_1.tag;
        switch (_lv_tag_23) {
            case 0: {
                float x_3 = r_1.Ok_0;
                return x_3;
                break;
            }
            default: {
                int _x_4 = r_1.Err_0;
                return default_2;
                break;
            }
        }
    }
    vec3 main_pure(vec2 uv_5) {
        v_result_float_int anf_20 = v_result_float_int(0, 5.4, 0);
        float a_6 = unwrap_0(anf_20, 5.);
        v_result_float_int anf_21 = v_result_float_int(1, 0., 2);
        float b_7 = unwrap_0(anf_21, 2.3);
        float anf_22 = uv_5[0];
        return vec3(anf_22, a_6, b_7);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "parametrized variants in functions (explicitly annotated)" =
  test
    {|
    #extern vec2 u_resolution
    #extern float u_time

    type option['a] = Some of 'a | None

    let unwrap opt default =
      match opt with
      | Some x -> x
      | None -> default

    let main (coord : vec2) =
      let x = unwrap (Some true) false in
      let y = unwrap (Some 5) 5 in
      [ 0, 0, 0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct v_option_bool {
        int tag;
        bool Some_0;
    };
    bool unwrap_0_v_option_bool_to_bool_to_bool_20(v_option_bool opt_1, bool default_2) {
        int _lv_tag_23 = opt_1.tag;
        switch (_lv_tag_23) {
            case 0: {
                bool x_3 = opt_1.Some_0;
                return x_3;
                break;
            }
            default: {
                return default_2;
                break;
            }
        }
    }
    struct v_option_int {
        int tag;
        int Some_0;
    };
    int unwrap_0_v_option_int_to_int_to_int_19(v_option_int opt_1, int default_2) {
        int _lv_tag_24 = opt_1.tag;
        switch (_lv_tag_24) {
            case 0: {
                int x_3 = opt_1.Some_0;
                return x_3;
                break;
            }
            default: {
                return default_2;
                break;
            }
        }
    }
    vec3 main_pure(vec2 coord_4) {
        v_option_bool anf_21 = v_option_bool(0, true);
        bool x_5 = unwrap_0_v_option_bool_to_bool_to_bool_20(anf_21, false);
        v_option_int anf_22 = v_option_int(0, 5);
        int y_6 = unwrap_0_v_option_int_to_int_to_int_19(anf_22, 5);
        return vec3(0., 0., 0.);
    }
    uniform vec2 u_resolution;
    uniform float u_time;
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type box['a] = { value: 'a }

    let f (b : box['a]) = let a = b.value in a

    let main (coord: vec2) : vec3 =
      let a = f { value = 1.0 } in
      let b = if f { value = true } then 1 else 2 in
      [a, b, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_bool {
        bool value;
    };
    bool f_0_r_box_bool_to_bool_13(r_box_bool b_1) {
        bool a_2 = b_1.value;
        return a_2;
    }
    struct r_box_float {
        float value;
    };
    float f_0_r_box_float_to_float_14(r_box_float b_1) {
        float a_2 = b_1.value;
        return a_2;
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_15 = r_box_float(1.);
        float a_4 = f_0_r_box_float_to_float_14(anf_15);
        r_box_bool anf_16 = r_box_bool(true);
        bool anf_17 = f_0_r_box_bool_to_bool_13(anf_16);
        int b_5;
        if (anf_17) {
            b_5 = 1;
        } else {
            b_5 = 2;
        }
        float pf_18 = float(b_5);
        return vec3(a_4, pf_18, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "phantom parameters, sort of" =
  test
    {|
    type tagged['tag, 'value] = { data: 'value }

    let add (l : tagged['a, 'b]) (r : tagged['a, 'b]) = l

    let main (uv: vec2) : vec3 =
      let a : tagged[int, float] = { data = 1.0 } in
      let b : tagged[bool, float] = { data = 2.0 } in
      let c = add a b in
      [a.data, b.data, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 9:15-9:20: type mismatch
      ty: int
      ty': bool
      |
    9 |       let c = add a b in
      |               ^^^^^
    |}]
;;

let%expect_test "defunctionalization" =
  (* Named function reference as higher-order argument *)
  test
    {|
    let apply f x = f x
    let double n = n * 2.0
    let main (pos : vec2) =
      let r = apply double pos.0 in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_22 {
        int tag;
    };
    float double_3_float_to_float_19(float n_4) {
        return (n_4 * 2.);
    }
    float dapply_21(DFn_22 dfn_24, float da_25) {
        return double_3_float_to_float_19(da_25);
    }
    float apply_0_float_to_float_to_float_to_float_20(DFn_22 f_1, float x_2) {
        return dapply_21(f_1, x_2);
    }
    vec3 main_pure(vec2 pos_5) {
        DFn_22 anf_26 = DFn_22(0);
        float anf_27 = pos_5[0];
        float r_6 = apply_0_float_to_float_to_float_to_float_20(anf_26, anf_27);
        return vec3(r_6, r_6, r_6);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Lambda in argument position *)
  test
    {|
    let apply f x = f x
    let main (pos : vec2) =
      let r = apply (fun y -> y + 1.0) (pos.0) in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_18 {
        int tag;
    };
    float dapply_17(DFn_18 dfn_20, float da_21) {
        return (da_21 + 1.);
    }
    float apply_0_float_to_float_to_float_to_float_16(DFn_18 f_1, float x_2) {
        return dapply_17(f_1, x_2);
    }
    vec3 main_pure(vec2 pos_3) {
        DFn_18 anf_22 = DFn_18(0);
        float anf_23 = pos_3[0];
        float r_4 = apply_0_float_to_float_to_float_to_float_16(anf_22, anf_23);
        return vec3(r_4, r_4, r_4);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Lambda capturing a free variable (closure) *)
  test
    {|
    let apply f x = f x
    let main (pos : vec2) =
      let px = pos.0 in
      let r = apply (fun y -> px + y) pos.1 in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_20 {
        int tag;
        float lctor_21_0;
    };
    float dapply_19(DFn_20 dfn_22, float da_23) {
        float px_4 = dfn_22.lctor_21_0;
        return (px_4 + da_23);
    }
    float apply_0_float_to_float_to_float_to_float_18(DFn_20 f_1, float x_2) {
        return dapply_19(f_1, x_2);
    }
    vec3 main_pure(vec2 pos_3) {
        float px_4 = pos_3[0];
        DFn_20 anf_24 = DFn_20(0, px_4);
        float anf_25 = pos_3[1];
        float r_5 = apply_0_float_to_float_to_float_to_float_18(anf_24, anf_25);
        return vec3(r_5, r_5, r_5);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Lambda stored in let binding, used as value *)
  test
    {|
    let apply (f : float -> float) (x : float) = f x
    let main (pos : vec2) =
      let scale = fun (y : float) -> y * pos.0 in
      let r = apply scale (pos.1) in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_14 {
        int tag;
        vec2 lctor_15_0;
    };
    float dapply_13(DFn_14 dfn_16, float da_17) {
        vec2 pos_3 = dfn_16.lctor_15_0;
        float anf_18 = pos_3[0];
        return (da_17 * anf_18);
    }
    float apply_0(DFn_14 f_1, float x_2) {
        return dapply_13(f_1, x_2);
    }
    vec3 main_pure(vec2 pos_3) {
        DFn_14 scale_4 = DFn_14(0, pos_3);
        float anf_19 = pos_3[1];
        float r_6 = apply_0(scale_4, anf_19);
        return vec3(r_6, r_6, r_6);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Three named functions of the same type (3-case switch) *)
  test
    {|
    let apply (f : float -> float) (x : float) = f x
    let double n = n * 2.0
    let triple n = n * 3.0
    let quadruple n = n * 4.0
    let main (pos : vec2) =
      let a = apply double pos.0 in
      let b = apply triple pos.1 in
      let c = apply quadruple pos.0 in
      [ a, b, c ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_39 {
        int tag;
    };
    float double_3_float_to_float_37(float n_4) {
        return (n_4 * 2.);
    }
    float quadruple_7_float_to_float_35(float n_8) {
        return (n_8 * 4.);
    }
    float triple_5_float_to_float_36(float n_6) {
        return (n_6 * 3.);
    }
    float dapply_38(DFn_39 dfn_43, float da_44) {
        int _lv_tag_51 = dfn_43.tag;
        switch (_lv_tag_51) {
            case 0: {
                return double_3_float_to_float_37(da_44);
                break;
            }
            case 1: {
                return triple_5_float_to_float_36(da_44);
                break;
            }
            default: {
                return quadruple_7_float_to_float_35(da_44);
                break;
            }
        }
    }
    float apply_0(DFn_39 f_1, float x_2) {
        return dapply_38(f_1, x_2);
    }
    vec3 main_pure(vec2 pos_9) {
        DFn_39 anf_45 = DFn_39(0);
        float anf_46 = pos_9[0];
        float a_10 = apply_0(anf_45, anf_46);
        DFn_39 anf_47 = DFn_39(1);
        float anf_48 = pos_9[1];
        float b_11 = apply_0(anf_47, anf_48);
        DFn_39 anf_49 = DFn_39(2);
        float anf_50 = pos_9[0];
        float c_12 = apply_0(anf_49, anf_50);
        return vec3(a_10, b_11, c_12);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* HOF with binary function type *)
  test
    {|
    let apply2 f x y = f x y
    let add a b = a + b
    let main (pos : vec2) =
      let r = apply2 add pos.0 pos.1 in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_31 {
        int tag;
    };
    float add_4_float_to_float_to_float_28(float a_5, float b_6) {
        return (a_5 + b_6);
    }
    float dapply_30(DFn_31 dfn_33, float da_34, float da_35) {
        return add_4_float_to_float_to_float_28(da_34, da_35);
    }
    float apply2_0_float_to_float_to_float_to_float_to_float_to_float_29(DFn_31 f_1, float x_2, float y_3) {
        return dapply_30(f_1, x_2, y_3);
    }
    vec3 main_pure(vec2 pos_7) {
        DFn_31 anf_36 = DFn_31(0);
        float anf_37 = pos_7[0];
        float anf_38 = pos_7[1];
        float r_8 = apply2_0_float_to_float_to_float_to_float_to_float_to_float_29(anf_36, anf_37, anf_38);
        return vec3(r_8, r_8, r_8);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* map! *)
  test
    {|
    let map f v = [f v.0, f v.1, f v.2]
    let main (uv : vec2) =
      let color = map (fun x -> x * 2) [0, 1, 2] in
      [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_23 {
        int tag;
    };
    float dapply_22(DFn_23 dfn_25, float da_26) {
        return (da_26 * 2.);
    }
    vec3 map_0_float_to_float_to_vec3_to_vec3_21(DFn_23 f_1, vec3 v_2) {
        float anf_27 = v_2[0];
        float anf_28 = dapply_22(f_1, anf_27);
        float anf_29 = v_2[1];
        float anf_30 = dapply_22(f_1, anf_29);
        float anf_31 = v_2[2];
        float anf_32 = dapply_22(f_1, anf_31);
        return vec3(anf_28, anf_30, anf_32);
    }
    vec3 main_pure(vec2 uv_3) {
        DFn_23 anf_33 = DFn_23(0);
        vec3 anf_34 = vec3(0., 1., 2.);
        vec3 color_4 = map_0_float_to_float_to_vec3_to_vec3_21(anf_33, anf_34);
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "defunctionalization - returning closures" =
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) =
      let f = addn 0. in
      let r = f 1. in
      [r, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_17 {
        int tag;
        float lctor_18_0;
    };
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float addn_3(float n_4, float x_5) {
        return add_0(n_4, x_5);
    }
    float dapply_16(DFn_17 dfn_19, float da_20) {
        float ca_15 = dfn_19.lctor_18_0;
        return addn_3(ca_15, da_20);
    }
    vec3 main_pure(vec2 coord_6) {
        DFn_17 f_7 = DFn_17(0, 0.);
        float r_8 = dapply_16(f_7, 1.);
        return vec3(r_8, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let main (coord : vec2) =
      let addn (n : float) = fun (x : float) -> n + x in
      let f = addn 0. in
      let r = f 1. in
      [r, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_12 {
        int tag;
        float lctor_13_0;
    };
    float dapply_11(DFn_12 dfn_14, float da_15) {
        float ca_9 = dfn_14.lctor_13_0;
        return (ca_9 + da_15);
    }
    float addn_1_16(float n_2, float x_3) {
        return (n_2 + x_3);
    }
    vec3 main_pure(vec2 coord_0) {
        DFn_12 f_4 = DFn_12(0, 0.);
        float r_5 = dapply_11(f_4, 1.);
        return vec3(r_5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let addn (n : float) = fun (x : float) -> n + x
    let main (coord : vec2) =
      let f = addn 1. in
      let g = f in
      let r = g 2. in
      [r, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_13 {
        int tag;
        float lctor_14_0;
    };
    float addn_0(float n_1, float x_2) {
        return (n_1 + x_2);
    }
    float dapply_12(DFn_13 dfn_15, float da_16) {
        float ca_11 = dfn_15.lctor_14_0;
        return addn_0(ca_11, da_16);
    }
    vec3 main_pure(vec2 coord_3) {
        DFn_13 f_4 = DFn_13(0, 1.);
        DFn_13 g_5 = f_4;
        float r_6 = dapply_12(g_5, 2.);
        return vec3(r_6, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "defunctionalization - partial application of first-class functions" =
  (* Simple test *)
  test
    {|
      let main (pos : vec2) =
        let add = fun (a : float) (b : float) -> a + b in
        let f = add in
        let g = f pos.0 in
        let r = g pos.1 in
        [r, r, r]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_13 {
        int tag;
    };
    struct DFn_19 {
        int tag;
        DFn_13 lctor_20_0;
        float lctor_20_1;
    };
    float dapply_12(DFn_13 dfn_21, float da_22, float da_23) {
        return (da_22 + da_23);
    }
    float dapply_18(DFn_19 dfn_24, float da_25) {
        DFn_13 ca_15 = dfn_24.lctor_20_0;
        float ca_16 = dfn_24.lctor_20_1;
        return dapply_12(ca_15, ca_16, da_25);
    }
    vec3 main_pure(vec2 pos_0) {
        DFn_13 add_1 = DFn_13(0);
        DFn_13 f_4 = add_1;
        float anf_26 = pos_0[0];
        DFn_19 g_5 = DFn_19(0, f_4, anf_26);
        float anf_27 = pos_0[1];
        float r_6 = dapply_18(g_5, anf_27);
        return vec3(r_6, r_6, r_6);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Global function as value with multi-level partial application *)
  test
    {|
      let add3 (a : float) (b : float) (c : float) = a + b + c
      let main (pos : vec2) =
        let f : float -> float -> float -> float = add3 in
        let g = f 1.0 in
        let h = g 2.0 in
        let r = h pos.0 in
        [r, r, r]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_16 {
        int tag;
    };
    struct DFn_23 {
        int tag;
        DFn_16 lctor_24_0;
        float lctor_24_1;
    };
    struct DFn_29 {
        int tag;
        DFn_23 lctor_30_0;
        float lctor_30_1;
    };
    float add3_0(float a_1, float b_2, float c_3) {
        float anf_40 = (a_1 + b_2);
        return (anf_40 + c_3);
    }
    float dapply_15(DFn_16 dfn_31, float da_32, float da_33, float da_34) {
        return add3_0(da_32, da_33, da_34);
    }
    float dapply_22(DFn_23 dfn_35, float da_36, float da_37) {
        DFn_16 ca_18 = dfn_35.lctor_24_0;
        float ca_19 = dfn_35.lctor_24_1;
        return dapply_15(ca_18, ca_19, da_36, da_37);
    }
    float dapply_28(DFn_29 dfn_38, float da_39) {
        DFn_23 ca_25 = dfn_38.lctor_30_0;
        float ca_26 = dfn_38.lctor_30_1;
        return dapply_22(ca_25, ca_26, da_39);
    }
    vec3 main_pure(vec2 pos_4) {
        DFn_16 f_5 = DFn_16(0);
        DFn_23 g_6 = DFn_23(0, f_5, 1.);
        DFn_29 h_7 = DFn_29(0, g_6, 2.);
        float anf_41 = pos_4[0];
        float r_8 = dapply_28(h_7, anf_41);
        return vec3(r_8, r_8, r_8);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Partial application of first-class function passed to HOF *)
  test
    {|
      let apply f x = f x
      let add (a : float) (b : float) = a + b
      let main (pos : vec2) =
        let add_as_value : float -> float -> float = add in
        let r = apply (add_as_value pos.0) pos.1 in
        [r, r, r]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_24 {
        int tag;
    };
    struct DFn_22 {
        int tag;
        DFn_24 lctor_29_0;
        float lctor_29_1;
    };
    float add_3(float a_4, float b_5) {
        return (a_4 + b_5);
    }
    float dapply_23(DFn_24 dfn_32, float da_33, float da_34) {
        return add_3(da_33, da_34);
    }
    float dapply_21(DFn_22 dfn_30, float da_31) {
        DFn_24 ca_26 = dfn_30.lctor_29_0;
        float ca_27 = dfn_30.lctor_29_1;
        return dapply_23(ca_26, ca_27, da_31);
    }
    float apply_0_float_to_float_to_float_to_float_20(DFn_22 f_1, float x_2) {
        return dapply_21(f_1, x_2);
    }
    vec3 main_pure(vec2 pos_6) {
        DFn_24 add_as_value_7 = DFn_24(0);
        float anf_35 = pos_6[0];
        DFn_22 anf_36 = DFn_22(0, add_as_value_7, anf_35);
        float anf_37 = pos_6[1];
        float r_8 = apply_0_float_to_float_to_float_to_float_20(anf_36, anf_37);
        return vec3(r_8, r_8, r_8);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let mkinc n =
      let x = 1 in
      fun y -> x + y

    let main (uv : vec2) =
      let inc = mkinc 0 in
      inc 2 * [1, 1, 1]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_20 {
        int tag;
        int lctor_21_0;
    };
    int dapply_19(DFn_20 dfn_22, int da_23) {
        int x_2 = dfn_22.lctor_21_0;
        return (x_2 + da_23);
    }
    DFn_20 mkinc_0_int_to_int_to_int_18(int n_1) {
        int x_2 = 1;
        return DFn_20(0, x_2);
    }
    vec3 main_pure(vec2 uv_4) {
        DFn_20 inc_5_int_to_int_17 = mkinc_0_int_to_int_to_int_18(0);
        int anf_24 = dapply_19(inc_5_int_to_int_17, 2);
        vec3 anf_25 = vec3(1., 1., 1.);
        float pf_26 = float(anf_24);
        return (pf_26 * anf_25);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "non-parametrized type aliases" =
  test
    {|
    type a = int
    type b = a
    type c = b

    let f (n : b) : c = n
    let main (u : vec2) = [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    int f_0(int n_1) {
        return n_1;
    }
    vec3 main_pure(vec2 u_2) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
  |}];
  test
    {|
    type option['a] = Some of 'a | None
    type a = option[int]
    type b = a

    let f (n : a) : b = n
    let main (u : vec2) = [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct v_option_int {
        int tag;
        int Some_0;
    };
    v_option_int f_0(v_option_int n_1) {
        return n_1;
    }
    vec3 main_pure(vec2 u_2) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "toplevel complex consts / promotion to zero-arg functions" =
  test
    {|
    #extern float u_scale
    let scale = u_scale
    let pi = 3.14159
    let main (coord : vec2) = [pi, pi, pi]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const float pi_1 = 3.14159;
    vec3 main_pure(vec2 coord_2) {
        return vec3(pi_1, pi_1, pi_1);
    }
    uniform float u_scale;
    float scale_0() {
        return u_scale;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type v = { a : float }
    let base = 2 + 1.0
    let derived = { a = base * 2.0 }.a
    let main (coord : vec2) = [derived, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const float base_0 = (2. + 1.);
    struct v {
        float a;
    };
    const float derived_1 = v((base_0 * 2.)).a;
    vec3 main_pure(vec2 coord_2) {
        return vec3(derived_1, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    #extern bool u_flag
    let chosen = if u_flag then 1.0 else 0.0
    let main (coord : vec2) = [chosen, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform bool u_flag;
    float chosen_0() {
        if (u_flag) {
            return 1.;
        } else {
            return 0.;
        }
    }
    vec3 main_pure(vec2 coord_1) {
        float _lc_2 = chosen_0();
        return vec3(_lc_2, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
