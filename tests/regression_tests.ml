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
        float pf_5 = float(x_1);
        float y_2 = (pf_5 + 3.);
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
        float pf_3 = float(n_1);
        return vec3(pf_3, 0., 0.);
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
        float pf_7 = float(n);
        bool anf_6 = (pf_7 < 0.5);
        if (anf_6) {
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
        float anf_7 = p_1.x;
        float anf_8 = p_1.y;
        return vec3(anf_7, anf_8, 0.);
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
        float pf_10 = float(a_1);
        point p_2 = point(pf_10, 0.);
        float anf_8 = p_2.x;
        float anf_9 = p_2.y;
        return vec3(anf_8, anf_9, 0.);
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
        float anf_7 = p_1.snd;
        float anf_8 = p_1.snd;
        return vec3(anf_7, anf_8, 0.);
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
        color anf_6 = color(0, 1.);
        int _lv_tag_7 = anf_6.tag;
        switch (_lv_tag_7) {
            case 0: {
                float v_1 = anf_6.Gray_0;
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
    float f_0_r_box_float_to_float_9(r_box_float b_1) {
        return b_1.value;
    }
    vec3 main_pure(vec2 coord_2) {
        r_box_float anf_10 = r_box_float(1.);
        float anf_11 = f_0_r_box_float_to_float_9(anf_10);
        return vec3(anf_11, 0., 0.);
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
    bool f_0_r_box_bool_to_bool_19(r_box_bool b_1) {
        return b_1.value;
    }
    struct r_box_float {
        float value;
    };
    float f_0_r_box_float_to_float_20(r_box_float b_1) {
        return b_1.value;
    }
    vec3 main_pure(vec2 coord_2) {
        r_box_float anf_21 = r_box_float(1.);
        float a_3 = f_0_r_box_float_to_float_20(anf_21);
        r_box_bool anf_22 = r_box_bool(true);
        bool anf_23 = f_0_r_box_bool_to_bool_19(anf_22);
        int b_4;
        if (anf_23) {
            b_4 = 1;
        } else {
            b_4 = 2;
        }
        float pf_24 = float(b_4);
        return vec3(a_3, pf_24, 0.);
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
    bool is_some_0_v_option_float_to_bool_14(v_option_float o_1) {
        int _lv_tag_17 = o_1.tag;
        switch (_lv_tag_17) {
            case 0: {
                float _wc_2 = o_1.Some_0;
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
        v_option_float anf_15 = v_option_float(0, 1.);
        bool anf_16 = is_some_0_v_option_float_to_bool_14(anf_15);
        float b_4;
        if (anf_16) {
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
    float f_0_r_box_float_to_float_12(r_box_float b_1) {
        float a_2 = b_1.value;
        return a_2;
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_13 = r_box_float(1.);
        float anf_14 = f_0_r_box_float_to_float_12(anf_13);
        return vec3(anf_14, 0., 0.);
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
    bool f_0_r_box_bool_to_bool_20(r_box_bool b_1) {
        bool a_2 = b_1.value;
        return a_2;
    }
    struct r_box_float {
        float value;
    };
    float f_0_r_box_float_to_float_21(r_box_float b_1) {
        float a_2 = b_1.value;
        return a_2;
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_22 = r_box_float(1.);
        float x_4 = f_0_r_box_float_to_float_21(anf_22);
        r_box_bool anf_23 = r_box_bool(true);
        bool anf_24 = f_0_r_box_bool_to_bool_20(anf_23);
        int y_5;
        if (anf_24) {
            y_5 = 1;
        } else {
            y_5 = 2;
        }
        float pf_25 = float(y_5);
        return vec3(x_4, pf_25, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* [let x = b.value in x * 2.0 - x]'s type constrained to float through Broadcast *)
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
    float scale_0_r_box_float_to_float_14(r_box_float b_1) {
        float x_2 = b_1.value;
        return (x_2 * 2.);
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_15 = r_box_float(1.);
        float anf_16 = scale_0_r_box_float_to_float_14(anf_15);
        return vec3(anf_16, 0., 0.);
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
    float get_x_0_vec2_to_float_11(vec2 v_1) {
        float x_2 = v_1[0];
        return x_2;
    }
    vec3 main_pure(vec2 coord_3) {
        float anf_12 = get_x_0_vec2_to_float_11(coord_3);
        return vec3(anf_12, 0., 0.);
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
    r_box_vec3 f_1_13(bool x_2) {
        int _iter_16 = 0;
        while ((_iter_16 < 1000)) {
            if (x_2) {
                vec3 anf_14 = vec3(1., 1., 1.);
                return r_box_vec3(anf_14);
            } else {
                x_2 = true;
                int _iter_inc_17 = (_iter_16 + 1);
                _iter_16 = _iter_inc_17;
                continue;
            }
        }
        r_box_vec3 _tmp_18;
        return _tmp_18;
    }
    vec3 main_pure(vec2 coord_0) {
        r_box_vec3 anf_15 = f_1_13(false);
        return anf_15.v;
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
    struct DFn_19 {
        int tag;
    };
    struct DFn_24 {
        int tag;
        DFn_19 lctor_25_0;
    };
    vec3 f_4_float_to_vec3_16_17(float x_5) {
        return vec3(x_5, x_5, x_5);
    }
    vec3 dapply_18(DFn_19 dfn_26, float da_27) {
        return f_4_float_to_vec3_16_17(da_27);
    }
    vec3 blend_0(DFn_19 f_1, float w_2) {
        return dapply_18(f_1, w_2);
    }
    vec3 dapply_23(DFn_24 dfn_28, float da_29) {
        DFn_19 ca_22 = dfn_28.lctor_25_0;
        return blend_0(ca_22, da_29);
    }
    vec3 main_pure(vec2 coord_3) {
        DFn_19 anf_30 = DFn_19(0);
        DFn_24 a_6 = DFn_24(0, anf_30);
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
        vec3 anf_35 = (c_3 * t_5);
        vec3 anf_36 = (anf_35 + d_4);
        vec3 anf_37 = (6.28318 * anf_36);
        vec3 anf_38 = cos(anf_37);
        vec3 anf_39 = (b_2 * anf_38);
        return (a_1 + anf_39);
    }
    vec3 main_pure(vec2 coord_6) {
        vec3 anf_40 = vec3(0., 0., 0.);
        vec3 anf_41 = vec3(0., 0., 0.);
        vec3 anf_42 = vec3(0., 0., 0.);
        vec3 anf_43 = vec3(0., 0., 0.);
        return palette_0(anf_40, anf_41, anf_42, anf_43, 0.);
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
    struct DFn_17 {
        int tag;
        float lctor_18_0;
    };
    float adder_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float dapply_16(DFn_17 dfn_19, float da_20) {
        float ca_15 = dfn_19.lctor_18_0;
        return adder_0(ca_15, da_20);
    }
    const DFn_17 scene_3 = DFn_17(0, 0.5);
    vec3 main_pure(vec2 coord_4) {
        float anf_21 = coord_4[0];
        float d_5 = dapply_16(scene_3, anf_21);
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
    struct DFn_16 {
        int tag;
        int lctor_17_0;
    };
    const DFn_16 inc_0_int_to_int_11 = DFn_16(0, 1);
    int lam_12(int x_1, int y_2) {
        return (x_1 + y_2);
    }
    int dapply_15(DFn_16 dfn_18, int da_19) {
        int ca_14 = dfn_18.lctor_17_0;
        return lam_12(ca_14, da_19);
    }
    vec3 main_pure(vec2 uv_3) {
        int anf_20 = dapply_15(inc_0_int_to_int_11, 1);
        float pf_21 = float(anf_20);
        return vec3(pf_21, 1., 1.);
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
    struct DFn_16 {
        int tag;
        int lctor_17_0;
    };
    const DFn_16 inc_0_float_to_float_11 = DFn_16(0, 1);
    float lam_12(int x_1, float y_2) {
        float pf_21 = float(x_1);
        return (pf_21 + y_2);
    }
    float dapply_15(DFn_16 dfn_18, float da_19) {
        int ca_14 = dfn_18.lctor_17_0;
        return lam_12(ca_14, da_19);
    }
    vec3 main_pure(vec2 uv_3) {
        float anf_20 = dapply_15(inc_0_float_to_float_11, 1.);
        return vec3(anf_20, 0., 0.);
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
    struct DFn_27 {
        int tag;
        int lctor_28_0;
        float lctor_31_0;
    };
    const DFn_27 add_one_0_float_to_float_20 = DFn_27(0, 1, 0.);
    float lam_22(int k_1, float x_2) {
        float pf_37 = float(k_1);
        return (x_2 + pf_37);
    }
    float lam_23(float s_4, float x_5) {
        return (x_5 * s_4);
    }
    float dapply_26(DFn_27 dfn_32, float da_33) {
        int _lv_tag_36 = dfn_32.tag;
        switch (_lv_tag_36) {
            case 0: {
                int ca_25 = dfn_32.lctor_28_0;
                return lam_22(ca_25, da_33);
                break;
            }
            default: {
                float ca_30 = dfn_32.lctor_31_0;
                return lam_23(ca_30, da_33);
                break;
            }
        }
    }
    const DFn_27 scale_3_float_to_float_21 = DFn_27(1, 0, 2.);
    vec3 main_pure(vec2 uv_6) {
        float anf_34 = dapply_26(add_one_0_float_to_float_20, 3.);
        float anf_35 = dapply_26(scale_3_float_to_float_21, anf_34);
        return vec3(anf_35, 0., 0.);
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
    struct DFn_34 {
        int tag;
        vec3 lctor_35_0;
    };
    vec3 palette_0_vec3_to_int_to_vec3_28(vec3 a_1, int t_2) {
        float pf_41 = float(t_2);
        vec3 anf_38 = (a_1 * pf_41);
        return cos(anf_38);
    }
    vec3 dapply_33(DFn_34 dfn_36, int da_37) {
        vec3 ca_32 = dfn_36.lctor_35_0;
        return palette_0_vec3_to_int_to_vec3_28(ca_32, da_37);
    }
    const DFn_34 warm_3_int_to_vec3_25 = DFn_34(0, vec3(0.5, 0.3, 0.1));
    vec3 main_pure(vec2 coord_4) {
        vec3 anf_40 = dapply_33(warm_3_int_to_vec3_25, 2);
        float a_5 = anf_40[0];
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
    struct DFn_15 {
        int tag;
        float lctor_16_0;
    };
    const DFn_15 add5_3 = DFn_15(0, 5.);
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float dapply_14(DFn_15 dfn_17, float da_18) {
        float ca_13 = dfn_17.lctor_16_0;
        return add_0(ca_13, da_18);
    }
    vec3 main_pure(vec2 coord_4) {
        float r_5 = dapply_14(add5_3, 3.);
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
        DFn_22 f_7 = DFn_22(0, 0.);
        float r_8 = dapply_21(f_7, 1.);
        return vec3(r_8, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* fully applied with int args - both promoted at call site *)
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
    struct DFn_23 {
        int tag;
        float lctor_24_0;
    };
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float addn_3(float n_4, float x_5) {
        return add_0(n_4, x_5);
    }
    float dapply_22(DFn_23 dfn_25, float da_26) {
        float ca_21 = dfn_25.lctor_24_0;
        return addn_3(ca_21, da_26);
    }
    vec3 main_pure(vec2 coord_6) {
        int n_7 = 5;
        float pf_27 = float(n_7);
        DFn_23 f_8 = DFn_23(0, pf_27);
        float r_9 = dapply_22(f_8, 1.);
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
    struct DFn_31 {
        int tag;
    };
    int lam_29(int t_5) {
        return (t_5 + 1);
    }
    int dapply_30(DFn_31 dfn_33, int da_34) {
        return lam_29(da_34);
    }
    int app_t_2_28(int t_1, DFn_31 f_3) {
        return dapply_30(f_3, t_1);
    }
    vec2 func_0_int_to_vec2_int_27(int t_1) {
        DFn_31 anf_35 = DFn_31(0);
        int x_4 = app_t_2_28(t_1, anf_35);
        float pf_36 = float(x_4);
        float pf_37 = float(x_4);
        return vec2(pf_36, pf_37);
    }
    vec3 main_pure(vec2 uv_6) {
        vec2 result_7 = func_0_int_to_vec2_int_27(0);
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
    struct DFn_29 {
        int tag;
    };
    float lam_27(float t_5) {
        return (t_5 + 1.);
    }
    float dapply_28(DFn_29 dfn_31, float da_32) {
        return lam_27(da_32);
    }
    float app_t_2_26(float t_1, DFn_29 f_3) {
        return dapply_28(f_3, t_1);
    }
    vec3 func_0_float_to_vec3_25(float t_1) {
        DFn_29 anf_33 = DFn_29(0);
        float x_4 = app_t_2_26(t_1, anf_33);
        return vec3(x_4, x_4, x_4);
    }
    vec3 main_pure(vec2 uv_6) {
        return func_0_float_to_vec3_25(0.);
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
    struct DFn_25 {
        int tag;
        float lctor_28_0;
    };
    float add_0_float_to_float_to_float_23(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float dapply_24(DFn_25 dfn_29, float da_30) {
        float ca_27 = dfn_29.lctor_28_0;
        return add_0_float_to_float_to_float_23(ca_27, da_30);
    }
    struct fn_box {
        DFn_25 fn;
    };
    fn_box box_add_n_3(float x_4) {
        DFn_25 anf_31 = DFn_25(0, x_4);
        return fn_box(anf_31);
    }
    vec3 main_pure(vec2 pos_5) {
        fn_box boxed_add_five_6 = box_add_n_3(5.);
        DFn_25 anf_32 = boxed_add_five_6.fn;
        float n_7 = dapply_24(anf_32, 10.);
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
    struct DFn_21 {
        int tag;
        float lctor_24_0;
    };
    float add_0_float_to_float_to_float_19(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    struct cb {
        int tag;
        DFn_21 CB_0;
    };
    float dapply_20(DFn_21 dfn_25, float da_26) {
        float ca_23 = dfn_25.lctor_24_0;
        return add_0_float_to_float_to_float_19(ca_23, da_26);
    }
    vec3 main_pure(vec2 pos_3) {
        DFn_21 anf_27 = DFn_21(0, 1.);
        cb f_4 = cb(0, anf_27);
        DFn_21 g_6 = f_4.CB_0;
        float result_5 = dapply_20(g_6, 10.);
        return vec3(result_5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "struct pattern matching on non-concrete types" =
  test
    {|
    type box['a] = { value : 'a }

    let unbox = function
      | { value = v } -> v

    let main (uv : vec2) : vec3 =
      let n = unbox { value = 1.5 } in
      [n, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct r_box_float {
        float value;
    };
    float unbox_0_r_box_float_to_float_14(r_box_float _fn_arg_1) {
        float v_2 = _fn_arg_1.value;
        return v_2;
    }
    vec3 main_pure(vec2 uv_3) {
        r_box_float anf_15 = r_box_float(1.5);
        float n_4 = unbox_0_r_box_float_to_float_14(anf_15);
        return vec3(n_4, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - vec broadcast against polymorphic param with int-typed ret" =
  test
    {|
    let mandelbrot c =
      let rec go z i =
        if i > 10 then i
        else
          let zy = z.0 * z.1 in
          let z' = [zy, zy] + c in
          go z' (i + 1)
      in
      go [0, 0] 0

    let main (uv : vec2) =
      let a = mandelbrot uv in
      [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    int go_2_42(vec2 c_1, vec2 z_3, int i_4) {
        int _iter_49 = 0;
        while ((_iter_49 < 1000)) {
            bool anf_43 = (i_4 > 10);
            if (anf_43) {
                return i_4;
            } else {
                float anf_44 = z_3[0];
                float anf_45 = z_3[1];
                float zy_5 = (anf_44 * anf_45);
                vec2 anf_46 = vec2(zy_5, zy_5);
                vec2 z_prime_6 = (anf_46 + c_1);
                int anf_47 = (i_4 + 1);
                c_1 = c_1;
                z_3 = z_prime_6;
                i_4 = anf_47;
                int _iter_inc_50 = (_iter_49 + 1);
                _iter_49 = _iter_inc_50;
                continue;
            }
        }
        return 0;
    }
    int mandelbrot_0_vec2_to_int_40(vec2 c_1) {
        vec2 anf_48 = vec2(0., 0.);
        return go_2_42(c_1, anf_48, 0);
    }
    vec3 main_pure(vec2 uv_7) {
        int a_8 = mandelbrot_0_vec2_to_int_40(uv_7);
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "regression - wrong DFn return type" =
  test
    {|
    type sdf = vec2 -> float
    let constant (r : float) : sdf = fun p -> r
    let union (f : sdf) _ r = f r
    let dup f g x = f (g x) (g x)
    let scene : sdf = dup union constant 0.3
    let main (coord : vec2) = [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_48 {
        int tag;
    };
    struct DFn_50 {
        int tag;
    };
    struct DFn_46 {
        int tag;
        DFn_50 lctor_54_0;
        float lctor_54_1;
        DFn_50 lctor_58_0;
        float lctor_58_1;
    };
    struct DFn_64 {
        int tag;
        DFn_48 lctor_65_0;
        DFn_46 lctor_65_1;
        DFn_46 lctor_65_2;
    };
    float constant_0(float r_1, vec2 p_2) {
        return r_1;
    }
    float dapply_49(DFn_50 dfn_74, float da_75, vec2 da_76) {
        return constant_0(da_75, da_76);
    }
    float dapply_45(DFn_46 dfn_68, vec2 da_69) {
        int _lv_tag_83 = dfn_68.tag;
        switch (_lv_tag_83) {
            case 0: {
                DFn_50 ca_51 = dfn_68.lctor_54_0;
                float ca_52 = dfn_68.lctor_54_1;
                return dapply_49(ca_51, ca_52, da_69);
                break;
            }
            default: {
                DFn_50 ca_55 = dfn_68.lctor_58_0;
                float ca_56 = dfn_68.lctor_58_1;
                return dapply_49(ca_55, ca_56, da_69);
                break;
            }
        }
    }
    DFn_64 dup_7_vec2_to_float_to_vec2_to_float_to_vec2_to_float_to_float_to_vec2_to_float_to_float_to_vec2_to_float_44(DFn_48 f_8, DFn_50 g_9, float x_10) {
        DFn_50 _tmp_84;
        DFn_46 anf_79 = DFn_46(0, g_9, x_10, _tmp_84, 0.);
        DFn_50 _tmp_85;
        DFn_46 anf_80 = DFn_46(1, _tmp_85, 0., g_9, x_10);
        return DFn_64(0, f_8, anf_79, anf_80);
    }
    DFn_64 scene_11() {
        DFn_48 anf_81 = DFn_48(0);
        DFn_50 anf_82 = DFn_50(0);
        return dup_7_vec2_to_float_to_vec2_to_float_to_vec2_to_float_to_float_to_vec2_to_float_to_float_to_vec2_to_float_44(anf_81, anf_82, 0.3);
    }
    float union_3_vec2_to_float_to_vec2_to_float_to_vec2_to_float_43(DFn_46 f_4, DFn_46 _x_5, vec2 r_6) {
        return dapply_45(f_4, r_6);
    }
    float dapply_47(DFn_48 dfn_70, DFn_46 da_71, DFn_46 da_72, vec2 da_73) {
        return union_3_vec2_to_float_to_vec2_to_float_to_vec2_to_float_43(da_71, da_72, da_73);
    }
    float dapply_63(DFn_64 dfn_77, vec2 da_78) {
        DFn_48 ca_59 = dfn_77.lctor_65_0;
        DFn_46 ca_60 = dfn_77.lctor_65_1;
        DFn_46 ca_61 = dfn_77.lctor_65_2;
        return dapply_47(ca_59, ca_60, ca_61, da_78);
    }
    vec3 main_pure(vec2 coord_12) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "main type nomangle if type not concrete" =
  test "let main uv = [0, 0, 0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv_0) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
