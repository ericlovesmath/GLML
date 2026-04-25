open Runner

let%expect_test "int promotion edge cases" =
  (* int variable inferred *)
  test_term " let x = 5 in let y = x + 3.0 in [y, y, y]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        int x_1 = 5;
        float pf_4 = float(x_1);
        float y_2 = (pf_4 + 3.);
        return vec3(y_2, y_2, y_2);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int variable in vec *)
  test_term "let n = 2 in [n, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_0) {
        int n_1 = 2;
        float pf_2 = float(n_1);
        return vec3(pf_2, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int compared with float *)
  test
    {|
    #extern int n
    let main (u : vec2) =
      if n < 0.5 then [1.0, 0.0, 0.0] else [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform int n;
    vec3 main_pure(vec2 u_0) {
        float pf_4 = float(n);
        bool anf_3 = (pf_4 < 0.5);
        if (anf_3) {
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
  (* int literal in struct with float field *)
  test
    {|
    type point = { x: float, y: float }
    let main (u : vec2) =
      let p = { x = 1, y = 2 } in
      [p.x, p.y, 0.0]
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
    vec3 main_pure(vec2 u_0) {
        point p_1 = point(1., 2.);
        float anf_6 = p_1.x;
        float anf_7 = p_1.y;
        return vec3(anf_6, anf_7, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int variable in struct with float field *)
  test
    {|
    type point = { x: float, y: float }
    let main (u : vec2) =
      let a = 3 in
      let p = { x = a, y = 0.0 } in
      [p.x, p.y, 0.0]
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
    vec3 main_pure(vec2 u_0) {
        int a_1 = 3;
        float pf_9 = float(a_1);
        point p_2 = point(pf_9, 0.);
        float anf_7 = p_2.x;
        float anf_8 = p_2.y;
        return vec3(anf_7, anf_8, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* parametrized struct where non-param field is float, value is int *)
  test
    {|
    type pair['a] = { fst: 'a, snd: float }
    let main (u : vec2) =
      let p = { fst = true, snd = 2 } in
      [p.snd, p.snd, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_pair_bool {
        bool fst;
        float snd;
    };
    vec3 main_pure(vec2 u_0) {
        r_pair_bool p_1 = r_pair_bool(true, 2.);
        float anf_6 = p_1.snd;
        float anf_7 = p_1.snd;
        return vec3(anf_6, anf_7, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* variant constructor float/int *)
  test
    {|
    type color = Gray of float | Black
    let main (u : vec2) =
      match Gray 1 with
      | Gray v -> [v, v, v]
      | Black -> [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct color {
        int tag;
        float Gray_0;
    };
    vec3 main_pure(vec2 u_0) {
        color anf_4 = color(0, 1.);
        int _lv_tag_5 = anf_4.tag;
        switch (_lv_tag_5) {
            case 0: {
                float v_1 = anf_4.Gray_0;
                return vec3(v_1, v_1, v_1);
                break;
            }
            default: {
                return vec3(0., 0., 0.);
                break;
            }
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - polymorphic struct type in function" =
  test
    {|
    type box['a] = { value: 'a }
    let f (b: box['a]) : 'a = b.value
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
    float f_0_r_box_float_to_float_8(r_box_float b_1) {
        return b_1.value;
    }
    vec3 main_pure(vec2 coord_2) {
        r_box_float anf_9 = r_box_float(1.);
        float anf_10 = f_0_r_box_float_to_float_8(anf_9);
        return vec3(anf_10, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type box['a] = { value: 'a }
    let f b = b.value
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
    bool f_0_r_box_bool_to_bool_17(r_box_bool b_1) {
        return b_1.value;
    }
    struct r_box_float {
        float value;
    };
    float f_0_r_box_float_to_float_18(r_box_float b_1) {
        return b_1.value;
    }
    vec3 main_pure(vec2 coord_2) {
        r_box_float anf_19 = r_box_float(1.);
        float a_3 = f_0_r_box_float_to_float_18(anf_19);
        r_box_bool anf_20 = r_box_bool(true);
        bool anf_21 = f_0_r_box_bool_to_bool_17(anf_20);
        int b_4;
        if (anf_21) {
            b_4 = 1;
        } else {
            b_4 = 2;
        }
        float pf_22 = float(b_4);
        return vec3(a_3, pf_22, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - polymorphic variant type in function" =
  test
    {|
    type option['a] = Some of 'a | None

    let is_some o = match o with | Some _ -> true | None -> false

    let main (coord: vec2) : vec3 =
      let b = if is_some (Some 1.0) then 1.0 else 0.0 in
      [b, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct v_option_float {
        int tag;
        float Some_0;
    };
    bool is_some_0_v_option_float_to_bool_12(v_option_float o_1) {
        int _lv_tag_15 = o_1.tag;
        switch (_lv_tag_15) {
            case 0: {
                float _x_2 = o_1.Some_0;
                return true;
                break;
            }
            default: {
                return false;
                break;
            }
        }
    }
    vec3 main_pure(vec2 coord_3) {
        v_option_float anf_13 = v_option_float(0, 1.);
        bool anf_14 = is_some_0_v_option_float_to_bool_12(anf_13);
        float b_4;
        if (anf_14) {
            b_4 = 1.;
        } else {
            b_4 = 0.;
        }
        return vec3(b_4, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "removal of term that is does not resolve to a concrete type" =
  test
    {|
    type option['a] = Some of 'a | None

    let unwrap opt default =
      match opt with
      | Some x -> x
      | None -> default

    let main (coord : vec2) =
      let y = unwrap None None in
      [ 0, 0, 0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord_4) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "field access in let binding (unannotated)" =
  (* Regression tests for over-generalization of let-bound variables *)
  test
    {|
    type box['a] = { value: 'a }
    let f b = let a = b.value in a
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
    float f_0_r_box_float_to_float_11(r_box_float b_1) {
        float a_2 = b_1.value;
        return a_2;
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_12 = r_box_float(1.);
        float anf_13 = f_0_r_box_float_to_float_11(anf_12);
        return vec3(anf_13, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Polymorphic usage *)
  test
    {|
    type box['a] = { value: 'a }
    let f b = let a = b.value in a
    let main (coord: vec2) : vec3 =
      let x = f { value = 1.0 } in
      let y = if f { value = true } then 1 else 2 in
      [x, y, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_bool {
        bool value;
    };
    bool f_0_r_box_bool_to_bool_18(r_box_bool b_1) {
        bool a_2 = b_1.value;
        return a_2;
    }
    struct r_box_float {
        float value;
    };
    float f_0_r_box_float_to_float_19(r_box_float b_1) {
        float a_2 = b_1.value;
        return a_2;
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_20 = r_box_float(1.);
        float x_4 = f_0_r_box_float_to_float_19(anf_20);
        r_box_bool anf_21 = r_box_bool(true);
        bool anf_22 = f_0_r_box_bool_to_bool_18(anf_21);
        int y_5;
        if (anf_22) {
            y_5 = 1;
        } else {
            y_5 = 2;
        }
        float pf_23 = float(y_5);
        return vec3(x_4, pf_23, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* [let x = b.value in x * 2.0 — x]'s type constrained to float through Broadcast *)
  test
    {|
    type box['a] = { value: 'a }
    let scale b = let x = b.value in x * 2.0
    let main (coord: vec2) : vec3 = [scale { value = 1.0 }, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_float {
        float value;
    };
    float scale_0_r_box_float_to_float_13(r_box_float b_1) {
        float x_2 = b_1.value;
        return (x_2 * 2.);
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_14 = r_box_float(1.);
        float anf_15 = scale_0_r_box_float_to_float_13(anf_14);
        return vec3(anf_15, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Same test with IndexAccess *)
  test
    {|
    let get_x v = let x = v.0 in x
    let main (coord: vec2) : vec3 = [get_x coord, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float get_x_0_vec2_to_float_10(vec2 v_1) {
        float x_2 = v_1[0];
        return x_2;
    }
    vec3 main_pure(vec2 coord_3) {
        float anf_11 = get_x_0_vec2_to_float_10(coord_3);
        return vec3(anf_11, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - placeholder structs and variants in tail position" =
  test
    {|
    type box['a] = { v: 'a }
    let main (coord: vec2) : vec3 =
      let rec f x = if x then { v = [1, 1, 1] } else f true in
      (f false).v
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_vec3 {
        vec3 v;
    };
    r_box_vec3 f_1_11(bool x_2) {
        int _iter_14 = 0;
        while ((_iter_14 < 1000)) {
            if (x_2) {
                vec3 anf_12 = vec3(1., 1., 1.);
                return r_box_vec3(anf_12);
            } else {
                x_2 = true;
                int _iter_inc_15 = (_iter_14 + 1);
                _iter_14 = _iter_inc_15;
                continue;
            }
        }
        r_box_vec3 _tmp_16;
        return _tmp_16;
    }
    vec3 main_pure(vec2 coord_0) {
        r_box_vec3 anf_13 = f_1_11(false);
        return anf_13.v;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - no recursive DFn structs from partial application" =
  (* blend captures a DFn value, so the partial application `blend f`
     must produce a SEPARATE, higher-level DFn type, NOT a self-referential one.
     This occurs because it takes in a [float -> vec3] and ALSO returns it *)
  test
    {|
    let blend (f : float -> vec3) (w : float) = f w
    let main (coord : vec2) =
      let f = fun x -> [x, x, x] in
      let a = blend f in
      [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_15 {
        int tag;
    };
    struct DFn_20 {
        int tag;
        DFn_15 lctor_21_0;
    };
    vec3 dapply_14(DFn_15 dfn_22, float da_23) {
        return vec3(da_23, da_23, da_23);
    }
    vec3 blend_0(DFn_15 f_1, float w_2) {
        return dapply_14(f_1, w_2);
    }
    vec3 dapply_19(DFn_20 dfn_24, float da_25) {
        DFn_15 ca_18 = dfn_24.lctor_21_0;
        return blend_0(ca_18, da_25);
    }
    vec3 main_pure(vec2 coord_3) {
        DFn_15 f_4_float_to_vec3_13 = DFn_15(0);
        DFn_20 a_6 = DFn_20(0, f_4_float_to_vec3_13);
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "return type annotation for function-returning functions" =
  (* palette: 4 params, return type is (float -> vec3) *)
  test
    {|
    let palette (a : vec3) (b : vec3) (c : vec3) (d : vec3) : (float -> vec3) =
      fun t -> a + b * #cos(6.28318 * (c * t + d))
    let main (coord : vec2) : vec3 = palette [0.,0.,0.] [0.,0.,0.] [0.,0.,0.] [0.,0.,0.] 0.
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 palette_0(vec3 a_1, vec3 b_2, vec3 c_3, vec3 d_4, float t_5) {
        vec3 anf_25 = (c_3 * t_5);
        vec3 anf_26 = (anf_25 + d_4);
        vec3 anf_27 = (6.28318 * anf_26);
        vec3 anf_28 = cos(anf_27);
        vec3 anf_29 = (b_2 * anf_28);
        return (a_1 + anf_29);
    }
    vec3 main_pure(vec2 coord_6) {
        vec3 anf_30 = vec3(0., 0., 0.);
        vec3 anf_31 = vec3(0., 0., 0.);
        vec3 anf_32 = vec3(0., 0., 0.);
        vec3 anf_33 = vec3(0., 0., 0.);
        return palette_0(anf_30, anf_31, anf_32, anf_33, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* simple: one param, return type is (float -> float) *)
  test
    {|
    let add (x : float) : (float -> float) = fun y -> x + y
    let main (coord : vec2) : vec3 = let r = add 1. 2. in [r, r, r]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    vec3 main_pure(vec2 coord_3) {
        float r_4 = add_0(1., 2.);
        return vec3(r_4, r_4, r_4);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - defunctionalization closure globals use correct dapply" =
  (* closure global that captures a DFn *)
  test
    {|
    let adder (x : float) : float -> float =
      fun y -> x + y

    let scene : float -> float =
      adder 0.5

    let main (coord : vec2) =
      let d = scene coord.0 in
      [d, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_16 {
        int tag;
        float lctor_17_0;
    };
    float adder_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float dapply_15(DFn_16 dfn_18, float da_19) {
        float ca_14 = dfn_18.lctor_17_0;
        return adder_0(ca_14, da_19);
    }
    const DFn_16 scene_3 = DFn_16(0, 0.5);
    vec3 main_pure(vec2 coord_4) {
        float anf_20 = coord_4[0];
        float d_5 = dapply_15(scene_3, anf_20);
        return vec3(d_5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "toplevel vectors of ints are treated as consts with builtin #floats" =
  test
    {|
    let a = 10
    let x = [a, a, a]
    let main (coord : vec2) = x
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const int a_0 = 10;
    const vec3 x_1 = vec3(float(a_0), float(a_0), float(a_0));
    vec3 main_pure(vec2 coord_2) {
        return x_1;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "toplevel let-wrapped lambdas + partial application" =
  (* let-binding wrapping a lambda at the top level *)
  test
    {|
    let inc =
      let x = 1 in
      fun y -> x + y

    let main (uv : vec2) = [inc 1, 1, 1]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_12 {
        int tag;
        int lctor_13_0;
    };
    int dapply_11(DFn_12 dfn_14, int da_15) {
        int x_1 = dfn_14.lctor_13_0;
        return (x_1 + da_15);
    }
    const DFn_12 inc_0_int_to_int_10 = DFn_12(0, 1);
    vec3 main_pure(vec2 uv_3) {
        int anf_16 = dapply_11(inc_0_int_to_int_10, 1);
        float pf_17 = float(anf_16);
        return vec3(pf_17, 1., 1.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Called with float argument *)
  test
    {|
    let inc =
      let x = 1 in
      fun y -> x + y

    let main (uv : vec2) = [inc 1.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_12 {
        int tag;
        int lctor_13_0;
    };
    float dapply_11(DFn_12 dfn_14, float da_15) {
        int x_1 = dfn_14.lctor_13_0;
        float pf_17 = float(x_1);
        return (pf_17 + da_15);
    }
    const DFn_12 inc_0_float_to_float_10 = DFn_12(0, 1);
    vec3 main_pure(vec2 uv_3) {
        float anf_16 = dapply_11(inc_0_float_to_float_10, 1.);
        return vec3(anf_16, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let add_one =
      let k = 1 in
      fun x -> x + k

    let scale =
      let s = 2.0 in
      fun x -> x * s

    let main (uv : vec2) = [scale (add_one 3.0), 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_22 {
        int tag;
        int lctor_23_0;
        float lctor_24_0;
    };
    const DFn_22 add_one_0_float_to_float_19 = DFn_22(0, 1, 0.);
    float dapply_21(DFn_22 dfn_25, float da_26) {
        int _lv_tag_29 = dfn_25.tag;
        switch (_lv_tag_29) {
            case 0: {
                int k_1 = dfn_25.lctor_23_0;
                float pf_30 = float(k_1);
                return (da_26 + pf_30);
                break;
            }
            default: {
                float s_4 = dfn_25.lctor_24_0;
                return (da_26 * s_4);
                break;
            }
        }
    }
    const DFn_22 scale_3_float_to_float_20 = DFn_22(1, 0, 2.);
    vec3 main_pure(vec2 uv_6) {
        float anf_27 = dapply_21(add_one_0_float_to_float_19, 3.);
        float anf_28 = dapply_21(scale_3_float_to_float_20, anf_27);
        return vec3(anf_28, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - partial application stored as top level value" =
  test
    {|
    let palette (a : vec3) = fun t -> #cos(a * t)
    let warm = palette [0.5, 0.3, 0.1]
    let main (coord : vec2) =
      let a = (warm 2).0 in
      [a, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_26 {
        int tag;
        vec3 lctor_27_0;
    };
    vec3 palette_0_vec3_to_int_to_vec3_22(vec3 a_1, int t_2) {
        float pf_33 = float(t_2);
        vec3 anf_30 = (a_1 * pf_33);
        return cos(anf_30);
    }
    vec3 dapply_25(DFn_26 dfn_28, int da_29) {
        vec3 ca_24 = dfn_28.lctor_27_0;
        return palette_0_vec3_to_int_to_vec3_22(ca_24, da_29);
    }
    const DFn_26 warm_3_int_to_vec3_21 = DFn_26(0, vec3(0.5, 0.3, 0.1));
    vec3 main_pure(vec2 coord_4) {
        vec3 anf_32 = dapply_25(warm_3_int_to_vec3_21, 2);
        float a_5 = anf_32[0];
        return vec3(a_5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let add (x : float) (y : float) = x + y
    let add5 = add 5.0
    let main (coord : vec2) =
      let r = add5 3.0 in
      [r, r, r]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_14 {
        int tag;
        float lctor_15_0;
    };
    const DFn_14 add5_3 = DFn_14(0, 5.);
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float dapply_13(DFn_14 dfn_16, float da_17) {
        float ca_12 = dfn_16.lctor_15_0;
        return add_0(ca_12, da_17);
    }
    vec3 main_pure(vec2 coord_4) {
        float r_5 = dapply_13(add5_3, 3.);
        return vec3(r_5, r_5, r_5);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - int promotion through closures / partial application" =
  (* int literal passed to float param via partial application through closure *)
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) =
      let f = addn 0 in
      let r = f 1 in
      [r, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_21 {
        int tag;
        float lctor_22_0;
    };
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float addn_3(float n_4, float x_5) {
        return add_0(n_4, x_5);
    }
    float dapply_20(DFn_21 dfn_23, float da_24) {
        float ca_19 = dfn_23.lctor_22_0;
        return addn_3(ca_19, da_24);
    }
    vec3 main_pure(vec2 coord_6) {
        DFn_21 f_7 = DFn_21(0, 0.);
        float r_8 = dapply_20(f_7, 1.);
        return vec3(r_8, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* fully applied with int args — both promoted at call site *)
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) =
      let r = addn 0 1 in
      [r, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float addn_3(float n_4, float x_5) {
        return add_0(n_4, x_5);
    }
    vec3 main_pure(vec2 coord_6) {
        float r_7 = addn_3(0., 1.);
        return vec3(r_7, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int variable captured in closure then passed to float param *)
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) =
      let n = 5 in
      let f = addn n in
      let r = f 1 in
      [r, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_22 {
        int tag;
        float lctor_23_0;
    };
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float addn_3(float n_4, float x_5) {
        return add_0(n_4, x_5);
    }
    float dapply_21(DFn_22 dfn_24, float da_25) {
        float ca_20 = dfn_24.lctor_23_0;
        return addn_3(ca_20, da_25);
    }
    vec3 main_pure(vec2 coord_6) {
        int n_7 = 5;
        float pf_26 = float(n_7);
        DFn_22 f_8 = DFn_22(0, pf_26);
        float r_9 = dapply_21(f_8, 1.);
        return vec3(r_9, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - inferred type in higher-order local function" =
  (* implicit t parameter, int path via call site

     When func t is written without an explicit type annotation, the
     inner let binding let app_t f = f t gets a polymorphic scheme
     type with orphan constraint variables which caused issues *)
  test
    {|
    let func t =
      let app_t f = f t in
      let x = app_t (fun t -> t + 1) in
      [x, x]
    let main (uv : vec2) =
      let result = func 0 in
      [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_24 {
        int tag;
    };
    int dapply_23(DFn_24 dfn_26, int da_27) {
        return (da_27 + 1);
    }
    int app_t_2_28(int t_1, DFn_24 f_3) {
        return dapply_23(f_3, t_1);
    }
    vec2 func_0_int_to_vec2_22(int t_1) {
        DFn_24 anf_29 = DFn_24(0);
        int x_4 = app_t_2_28(t_1, anf_29);
        float pf_30 = float(x_4);
        float pf_31 = float(x_4);
        return vec2(pf_30, pf_31);
    }
    vec3 main_pure(vec2 uv_6) {
        vec2 result_7 = func_0_int_to_vec2_22(0);
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Float variant: t inferred as float *)
  test
    {|
    let func t =
      let app_t f = f t in
      let x = app_t (fun t -> t + 1.0) in
      [x, x, x]
    let main (uv : vec2) = func 0.0
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
        return (da_26 + 1.);
    }
    float app_t_2_27(float t_1, DFn_23 f_3) {
        return dapply_22(f_3, t_1);
    }
    vec3 func_0_float_to_vec3_21(float t_1) {
        DFn_23 anf_28 = DFn_23(0);
        float x_4 = app_t_2_27(t_1, anf_28);
        return vec3(x_4, x_4, x_4);
    }
    vec3 main_pure(vec2 uv_6) {
        return func_0_float_to_vec3_21(0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "closures in records / structs" =
  test
    {|
    type fn_box = { fn : float -> float }

    let add x y = x + y

    let box_add_n x = { fn = add x }

    let main (pos : vec2) : vec3 =
      let boxed_add_five = box_add_n 5 in
      let n = boxed_add_five.fn 10 in
      [n, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_26 {
        int tag;
        int lctor_29_0;
    };
    float add_0_int_to_float_to_float_24(int x_1, float y_2) {
        float pf_34 = float(x_1);
        return (pf_34 + y_2);
    }
    float dapply_25(DFn_26 dfn_30, float da_31) {
        int ca_28 = dfn_30.lctor_29_0;
        return add_0_int_to_float_to_float_24(ca_28, da_31);
    }
    struct fn_box {
        DFn_26 fn;
    };
    fn_box box_add_n_3_int_to_fn_box_23(int x_4) {
        DFn_26 anf_32 = DFn_26(0, x_4);
        return fn_box(anf_32);
    }
    vec3 main_pure(vec2 pos_5) {
        fn_box boxed_add_five_6 = box_add_n_3_int_to_fn_box_23(5);
        DFn_26 anf_33 = boxed_add_five_6.fn;
        float n_7 = dapply_25(anf_33, 10.);
        return vec3(n_7, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "function in variant, match-bound var used with int arg" =
  test
    {|
    type cb = | CB of (float -> float)

    let add x y = x + y

    let main (pos : vec2) : vec3 =
      let f = CB (add 1.0) in
      let result = match f with
        | CB g -> g 10
      in
      [result, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_20 {
        int tag;
        float lctor_23_0;
    };
    float add_0_float_to_float_to_float_18(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    struct cb {
        int tag;
        DFn_20 CB_0;
    };
    float dapply_19(DFn_20 dfn_24, float da_25) {
        float ca_22 = dfn_24.lctor_23_0;
        return add_0_float_to_float_to_float_18(ca_22, da_25);
    }
    vec3 main_pure(vec2 pos_3) {
        DFn_20 anf_26 = DFn_20(0, 1.);
        cb f_4 = cb(0, anf_26);
        DFn_20 g_6 = f_4.CB_0;
        float result_5 = dapply_19(g_6, 10.);
        return vec3(result_5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
