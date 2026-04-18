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
    float f_0_r_box_float_to_float_7(r_box_float b_1) {
        return b_1.value;
    }
    vec3 main_pure(vec2 coord_2) {
        r_box_float anf_8 = r_box_float(1.);
        float anf_9 = f_0_r_box_float_to_float_7(anf_8);
        return vec3(anf_9, 0., 0.);
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
    bool f_0_r_box_bool_to_bool_15(r_box_bool b_1) {
        return b_1.value;
    }
    struct r_box_float {
        float value;
    };
    float f_0_r_box_float_to_float_16(r_box_float b_1) {
        return b_1.value;
    }
    vec3 main_pure(vec2 coord_2) {
        r_box_float anf_17 = r_box_float(1.);
        float a_3 = f_0_r_box_float_to_float_16(anf_17);
        r_box_bool anf_18 = r_box_bool(true);
        bool anf_19 = f_0_r_box_bool_to_bool_15(anf_18);
        int b_4;
        if (anf_19) {
            b_4 = 1;
        } else {
            b_4 = 2;
        }
        float pf_20 = float(b_4);
        return vec3(a_3, pf_20, 0.);
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
    bool is_some_0_v_option_float_to_bool_11(v_option_float o_1) {
        int _lv_tag_14 = o_1.tag;
        switch (_lv_tag_14) {
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
        v_option_float anf_12 = v_option_float(0, 1.);
        bool anf_13 = is_some_0_v_option_float_to_bool_11(anf_12);
        float b_4;
        if (anf_13) {
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
    float f_0_r_box_float_to_float_10(r_box_float b_1) {
        float a_2 = b_1.value;
        return a_2;
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_11 = r_box_float(1.);
        float anf_12 = f_0_r_box_float_to_float_10(anf_11);
        return vec3(anf_12, 0., 0.);
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
    bool f_0_r_box_bool_to_bool_16(r_box_bool b_1) {
        bool a_2 = b_1.value;
        return a_2;
    }
    struct r_box_float {
        float value;
    };
    float f_0_r_box_float_to_float_17(r_box_float b_1) {
        float a_2 = b_1.value;
        return a_2;
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_18 = r_box_float(1.);
        float x_4 = f_0_r_box_float_to_float_17(anf_18);
        r_box_bool anf_19 = r_box_bool(true);
        bool anf_20 = f_0_r_box_bool_to_bool_16(anf_19);
        int y_5;
        if (anf_20) {
            y_5 = 1;
        } else {
            y_5 = 2;
        }
        float pf_21 = float(y_5);
        return vec3(x_4, pf_21, 0.);
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
    float scale_0_r_box_float_to_float_12(r_box_float b_1) {
        float x_2 = b_1.value;
        return (x_2 * 2.);
    }
    vec3 main_pure(vec2 coord_3) {
        r_box_float anf_13 = r_box_float(1.);
        float anf_14 = scale_0_r_box_float_to_float_12(anf_13);
        return vec3(anf_14, 0., 0.);
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
    float get_x_0_vec2_to_float_9(vec2 v_1) {
        float x_2 = v_1[0];
        return x_2;
    }
    vec3 main_pure(vec2 coord_3) {
        float anf_10 = get_x_0_vec2_to_float_9(coord_3);
        return vec3(anf_10, 0., 0.);
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
    r_box_vec3 f_1_9(bool x_2) {
        int _iter_12 = 0;
        while ((_iter_12 < 1000)) {
            if (x_2) {
                vec3 anf_10 = vec3(1., 1., 1.);
                return r_box_vec3(anf_10);
            } else {
                x_2 = true;
                int _iter_inc_13 = (_iter_12 + 1);
                _iter_12 = _iter_inc_13;
                continue;
            }
        }
        r_box_vec3 _tmp_14;
        return _tmp_14;
    }
    vec3 main_pure(vec2 coord_0) {
        r_box_vec3 anf_11 = f_1_9(false);
        return anf_11.v;
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
    struct DFn_13 {
        int tag;
    };
    struct DFn_18 {
        int tag;
        DFn_13 lctor_19_0;
    };
    vec3 dapply_12(DFn_13 dfn_20, float da_21) {
        return vec3(da_21, da_21, da_21);
    }
    vec3 blend_0(DFn_13 f_1, float w_2) {
        return dapply_12(f_1, w_2);
    }
    vec3 dapply_17(DFn_18 dfn_22, float da_23) {
        DFn_13 ca_16 = dfn_22.lctor_19_0;
        return blend_0(ca_16, da_23);
    }
    vec3 main_pure(vec2 coord_3) {
        DFn_13 f_4_float_to_vec3_11 = DFn_13(0);
        DFn_18 a_6 = DFn_18(0, f_4_float_to_vec3_11);
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
        vec3 anf_20 = (c_3 * t_5);
        vec3 anf_21 = (anf_20 + d_4);
        vec3 anf_22 = (6.28318 * anf_21);
        vec3 anf_23 = cos(anf_22);
        vec3 anf_24 = (b_2 * anf_23);
        return (a_1 + anf_24);
    }
    vec3 main_pure(vec2 coord_6) {
        vec3 anf_25 = vec3(0., 0., 0.);
        vec3 anf_26 = vec3(0., 0., 0.);
        vec3 anf_27 = vec3(0., 0., 0.);
        vec3 anf_28 = vec3(0., 0., 0.);
        return palette_0(anf_25, anf_26, anf_27, anf_28, 0.);
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
    struct DFn_14 {
        int tag;
        float lctor_15_0;
    };
    float adder_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float dapply_13(DFn_14 dfn_16, float da_17) {
        float ca_12 = dfn_16.lctor_15_0;
        return adder_0(ca_12, da_17);
    }
    const DFn_14 scene_3 = DFn_14(0, 0.5);
    vec3 main_pure(vec2 coord_4) {
        float anf_18 = coord_4[0];
        float d_5 = dapply_13(scene_3, anf_18);
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
    struct DFn_11 {
        int tag;
        int lctor_12_0;
    };
    int dapply_10(DFn_11 dfn_13, int da_14) {
        int x_1 = dfn_13.lctor_12_0;
        return (x_1 + da_14);
    }
    const DFn_11 inc_0_int_to_int_9 = DFn_11(0, 1);
    vec3 main_pure(vec2 uv_3) {
        int anf_15 = dapply_10(inc_0_int_to_int_9, 1);
        float pf_16 = float(anf_15);
        return vec3(pf_16, 1., 1.);
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
    struct DFn_11 {
        int tag;
        int lctor_12_0;
    };
    float dapply_10(DFn_11 dfn_13, float da_14) {
        int x_1 = dfn_13.lctor_12_0;
        float pf_16 = float(x_1);
        return (pf_16 + da_14);
    }
    const DFn_11 inc_0_float_to_float_9 = DFn_11(0, 1);
    vec3 main_pure(vec2 uv_3) {
        float anf_15 = dapply_10(inc_0_float_to_float_9, 1.);
        return vec3(anf_15, 0., 0.);
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
    struct DFn_20 {
        int tag;
        int lctor_21_0;
        float lctor_22_0;
    };
    const DFn_20 add_one_0_float_to_float_17 = DFn_20(0, 1, 0.);
    float dapply_19(DFn_20 dfn_23, float da_24) {
        int _lv_tag_27 = dfn_23.tag;
        switch (_lv_tag_27) {
            case 0: {
                int k_1 = dfn_23.lctor_21_0;
                float pf_28 = float(k_1);
                return (da_24 + pf_28);
                break;
            }
            default: {
                float s_4 = dfn_23.lctor_22_0;
                return (da_24 * s_4);
                break;
            }
        }
    }
    const DFn_20 scale_3_float_to_float_18 = DFn_20(1, 0, 2.);
    vec3 main_pure(vec2 uv_6) {
        float anf_25 = dapply_19(add_one_0_float_to_float_17, 3.);
        float anf_26 = dapply_19(scale_3_float_to_float_18, anf_25);
        return vec3(anf_26, 0., 0.);
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
    struct DFn_24 {
        int tag;
        vec3 lctor_25_0;
    };
    vec3 palette_0_vec3_to_int_to_vec3_20(vec3 a_1, int t_2) {
        float pf_31 = float(t_2);
        vec3 anf_28 = (a_1 * pf_31);
        return cos(anf_28);
    }
    vec3 dapply_23(DFn_24 dfn_26, int da_27) {
        vec3 ca_22 = dfn_26.lctor_25_0;
        return palette_0_vec3_to_int_to_vec3_20(ca_22, da_27);
    }
    const DFn_24 warm_3_int_to_vec3_19 = DFn_24(0, vec3(0.5, 0.3, 0.1));
    vec3 main_pure(vec2 coord_4) {
        vec3 anf_30 = dapply_23(warm_3_int_to_vec3_19, 2);
        float a_5 = anf_30[0];
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
    struct DFn_12 {
        int tag;
        float lctor_13_0;
    };
    const DFn_12 add5_3 = DFn_12(0, 5.);
    float add_0(float x_1, float y_2) {
        return (x_1 + y_2);
    }
    float dapply_11(DFn_12 dfn_14, float da_15) {
        float ca_10 = dfn_14.lctor_13_0;
        return add_0(ca_10, da_15);
    }
    vec3 main_pure(vec2 coord_4) {
        float r_5 = dapply_11(add5_3, 3.);
        return vec3(r_5, r_5, r_5);
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
    struct DFn_21 {
        int tag;
    };
    int dapply_20(DFn_21 dfn_23, int da_24) {
        return (da_24 + 1);
    }
    int app_t_2_25(int t_1, DFn_21 f_3) {
        return dapply_20(f_3, t_1);
    }
    vec2 func_0_int_to_vec2_19(int t_1) {
        DFn_21 anf_26 = DFn_21(0);
        int x_4 = app_t_2_25(t_1, anf_26);
        float pf_27 = float(x_4);
        float pf_28 = float(x_4);
        return vec2(pf_27, pf_28);
    }
    vec3 main_pure(vec2 uv_6) {
        vec2 result_7 = func_0_int_to_vec2_19(0);
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
    struct DFn_20 {
        int tag;
    };
    float dapply_19(DFn_20 dfn_22, float da_23) {
        return (da_23 + 1.);
    }
    float app_t_2_24(float t_1, DFn_20 f_3) {
        return dapply_19(f_3, t_1);
    }
    vec3 func_0_float_to_vec3_18(float t_1) {
        DFn_20 anf_25 = DFn_20(0);
        float x_4 = app_t_2_24(t_1, anf_25);
        return vec3(x_4, x_4, x_4);
    }
    vec3 main_pure(vec2 uv_6) {
        return func_0_float_to_vec3_18(0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
