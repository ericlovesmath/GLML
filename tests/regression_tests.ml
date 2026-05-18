open Runner

let%expect_test "int promotion edge cases" =
  (* int variable inferred *)
  test_term " let x = 5 in let y = x + 3.0 in [y, y, y]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        float anf = float(5);
        float y = (anf + 3.);
        return vec3(y, y, y);
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
    vec3 main_pure(vec2 coord) {
        float anf = float(2);
        return vec3(anf, 0., 0.);
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
    vec3 main_pure(vec2 u) {
        float anf = float(n);
        bool anf_0 = (anf < 0.5);
        if (anf_0) {
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
    vec3 main_pure(vec2 u) {
        point p = point(1., 2.);
        float anf = p.x;
        float anf_0 = p.y;
        return vec3(anf, anf_0, 0.);
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
    vec3 main_pure(vec2 u) {
        float anf = float(3);
        point p = point(anf, 0.);
        float anf_0 = p.x;
        float anf_1 = p.y;
        return vec3(anf_0, anf_1, 0.);
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
    struct pair {
        bool fst;
        float snd;
    };
    vec3 main_pure(vec2 u) {
        pair p = pair(true, 2.);
        float anf = p.snd;
        float anf_0 = p.snd;
        return vec3(anf, anf_0, 0.);
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
    vec3 main_pure(vec2 u) {
        color anf = color(0, 1.);
        int _lv_tag = anf.tag;
        switch (_lv_tag) {
            case 0: {
                float _lv_Gray_0 = anf.Gray_0;
                return vec3(_lv_Gray_0, _lv_Gray_0, _lv_Gray_0);
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
    struct box {
        float value;
    };
    float f_m(box b) {
        return b.value;
    }
    vec3 main_pure(vec2 coord) {
        box anf = box(1.);
        float anf_0 = f_m(anf);
        return vec3(anf_0, 0., 0.);
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
    struct box {
        bool value;
    };
    struct box_0 {
        float value;
    };
    bool f_m(box b) {
        return b.value;
    }
    float f_m_0(box_0 b) {
        return b.value;
    }
    vec3 main_pure(vec2 coord) {
        box_0 anf = box_0(1.);
        float a = f_m_0(anf);
        box anf_0 = box(true);
        bool anf_1 = f_m(anf_0);
        int b_0;
        if (anf_1) {
            b_0 = 1;
        } else {
            b_0 = 2;
        }
        float anf_2 = float(b_0);
        return vec3(a, anf_2, 0.);
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
    struct option {
        int tag;
        float Some_0;
    };
    bool is_some_m(option o) {
        int _lv_tag = o.tag;
        switch (_lv_tag) {
            case 0: {
                return true;
                break;
            }
            default: {
                return false;
                break;
            }
        }
    }
    vec3 main_pure(vec2 coord) {
        option anf = option(0, 1.);
        bool anf_0 = is_some_m(anf);
        float b;
        if (anf_0) {
            b = 1.;
        } else {
            b = 0.;
        }
        return vec3(b, 0., 0.);
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
    vec3 main_pure(vec2 coord) {
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
    struct box {
        float value;
    };
    float f_m(box b) {
        float a = b.value;
        return a;
    }
    vec3 main_pure(vec2 coord) {
        box anf = box(1.);
        float anf_0 = f_m(anf);
        return vec3(anf_0, 0., 0.);
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
    struct box {
        bool value;
    };
    struct box_0 {
        float value;
    };
    bool f_m(box b) {
        bool a = b.value;
        return a;
    }
    float f_m_0(box_0 b) {
        float a = b.value;
        return a;
    }
    vec3 main_pure(vec2 coord) {
        box_0 anf = box_0(1.);
        float x = f_m_0(anf);
        box anf_0 = box(true);
        bool anf_1 = f_m(anf_0);
        int y;
        if (anf_1) {
            y = 1;
        } else {
            y = 2;
        }
        float anf_2 = float(y);
        return vec3(x, anf_2, 0.);
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
    struct box {
        float value;
    };
    float scale_m(box b) {
        float x = b.value;
        return (x * 2.);
    }
    vec3 main_pure(vec2 coord) {
        box anf = box(1.);
        float anf_0 = scale_m(anf);
        return vec3(anf_0, 0., 0.);
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
    float get_x_m(vec2 v) {
        float x = v[0];
        return x;
    }
    vec3 main_pure(vec2 coord) {
        float anf = get_x_m(coord);
        return vec3(anf, 0., 0.);
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
    struct box {
        vec3 v;
    };
    box f_0(bool x) {
        int _iter = 0;
        while ((_iter < 1000)) {
            if (x) {
                vec3 anf = vec3(1., 1., 1.);
                return box(anf);
            } else {
                x = true;
                int _iter_inc = (_iter + 1);
                _iter = _iter_inc;
                continue;
            }
        }
        box _tmp;
        return _tmp;
    }
    vec3 main_pure(vec2 coord) {
        box anf_0 = f_0(false);
        return anf_0.v;
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
    struct DFn {
        int tag;
    };
    struct DFn_0 {
        int tag;
        DFn lctor_0;
    };
    vec3 main_pure(vec2 coord) {
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
    vec3 palette(vec3 a, vec3 b, vec3 c, vec3 d, float t) {
        vec3 anf = (c * t);
        vec3 anf_0 = (anf + d);
        vec3 anf_1 = (6.28318 * anf_0);
        vec3 anf_2 = cos(anf_1);
        vec3 anf_3 = (b * anf_2);
        return (a + anf_3);
    }
    vec3 main_pure(vec2 coord) {
        vec3 anf_4 = vec3(0., 0., 0.);
        vec3 anf_5 = vec3(0., 0., 0.);
        vec3 anf_6 = vec3(0., 0., 0.);
        vec3 anf_7 = vec3(0., 0., 0.);
        return palette(anf_4, anf_5, anf_6, anf_7, 0.);
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
    float add(float x, float y) {
        return (x + y);
    }
    vec3 main_pure(vec2 coord) {
        float r = add(1., 2.);
        return vec3(r, r, r);
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
    struct DFn {
        int tag;
        float lctor_0;
    };
    float adder(float x, float y) {
        return (x + y);
    }
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        return adder(_lv_lctor_0, da);
    }
    const DFn scene = DFn(0, 0.5);
    vec3 main_pure(vec2 coord) {
        float anf = coord[0];
        float d = dapply(scene, anf);
        return vec3(d, 0., 0.);
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
    const vec3 x = vec3(float(10), float(10), float(10));
    vec3 main_pure(vec2 coord) {
        return x;
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
    struct DFn {
        int tag;
        int lctor_0;
    };
    const DFn inc_m = DFn(0, 1);
    int lam(int x, int y) {
        return (x + y);
    }
    int dapply(DFn dfn, int da) {
        int _lv_lctor_0 = dfn.lctor_0;
        return lam(_lv_lctor_0, da);
    }
    vec3 main_pure(vec2 uv) {
        int anf = dapply(inc_m, 1);
        float anf_0 = float(anf);
        return vec3(anf_0, 1., 1.);
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
    struct DFn {
        int tag;
        int lctor_0;
    };
    const DFn inc_m = DFn(0, 1);
    float lam(int x, float y) {
        float anf = float(x);
        return (anf + y);
    }
    float dapply(DFn dfn, float da) {
        int _lv_lctor_0 = dfn.lctor_0;
        return lam(_lv_lctor_0, da);
    }
    vec3 main_pure(vec2 uv) {
        float anf_0 = dapply(inc_m, 1.);
        return vec3(anf_0, 0., 0.);
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
    struct DFn {
        int tag;
        int lctor_0;
        float lctor_0_0;
    };
    const DFn add_one_m = DFn(0, 1, 0.);
    float lam(int k, float x) {
        float anf = float(k);
        return (x + anf);
    }
    float lam_0(float s, float x_0) {
        return (x_0 * s);
    }
    float dapply(DFn dfn, float da) {
        int _lv_tag = dfn.tag;
        switch (_lv_tag) {
            case 0: {
                int _lv_lctor_0 = dfn.lctor_0;
                return lam(_lv_lctor_0, da);
                break;
            }
            default: {
                float _lv_lctor_0_0 = dfn.lctor_0_0;
                return lam_0(_lv_lctor_0_0, da);
                break;
            }
        }
    }
    const DFn scale_m = DFn(1, 0, 2.);
    vec3 main_pure(vec2 uv) {
        float anf_0 = dapply(add_one_m, 3.);
        float anf_1 = dapply(scale_m, anf_0);
        return vec3(anf_1, 0., 0.);
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
    struct DFn {
        int tag;
        vec3 lctor_0;
    };
    vec3 palette_m(vec3 a, int t) {
        float anf = float(t);
        vec3 anf_0 = (a * anf);
        return cos(anf_0);
    }
    vec3 dapply(DFn dfn, int da) {
        vec3 _lv_lctor_0 = dfn.lctor_0;
        return palette_m(_lv_lctor_0, da);
    }
    const DFn warm_m = DFn(0, vec3(0.5, 0.3, 0.1));
    vec3 main_pure(vec2 coord) {
        vec3 anf_2 = dapply(warm_m, 2);
        float a_0 = anf_2[0];
        return vec3(a_0, 0., 0.);
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
    struct DFn {
        int tag;
        float lctor_0;
    };
    float add(float x, float y) {
        return (x + y);
    }
    const DFn add5 = DFn(0, 5.);
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        return add(_lv_lctor_0, da);
    }
    vec3 main_pure(vec2 coord) {
        float r = dapply(add5, 3.);
        return vec3(r, r, r);
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
    struct DFn {
        int tag;
        float lctor_0;
    };
    float add(float x, float y) {
        return (x + y);
    }
    float addn(float n, float x_0) {
        return add(n, x_0);
    }
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        return addn(_lv_lctor_0, da);
    }
    vec3 main_pure(vec2 coord) {
        DFn f = DFn(0, 0.);
        float r = dapply(f, 1.);
        return vec3(r, 0., 0.);
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
    float add(float x, float y) {
        return (x + y);
    }
    float addn(float n, float x_0) {
        return add(n, x_0);
    }
    vec3 main_pure(vec2 coord) {
        float r = addn(0., 1.);
        return vec3(r, 0., 0.);
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
    struct DFn {
        int tag;
        float lctor_0;
    };
    float add(float x, float y) {
        return (x + y);
    }
    float addn(float n, float x_0) {
        return add(n, x_0);
    }
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        return addn(_lv_lctor_0, da);
    }
    vec3 main_pure(vec2 coord) {
        float anf = float(5);
        DFn f = DFn(0, anf);
        float r = dapply(f, 1.);
        return vec3(r, 0., 0.);
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
      [result.0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    int lam(int t_0) {
        return (t_0 + 1);
    }
    int dapply(DFn dfn, int da) {
        return lam(da);
    }
    int app_t_0(int t, DFn f) {
        return dapply(f, t);
    }
    vec2 func_m(int t) {
        DFn anf = DFn(0);
        int x = app_t_0(t, anf);
        float anf_0 = float(x);
        float anf_1 = float(x);
        return vec2(anf_0, anf_1);
    }
    vec3 main_pure(vec2 uv) {
        vec2 result = func_m(0);
        float anf_2 = result[0];
        return vec3(anf_2, 0., 0.);
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
    struct DFn {
        int tag;
    };
    float lam(float t_0) {
        return (t_0 + 1.);
    }
    float dapply(DFn dfn, float da) {
        return lam(da);
    }
    float app_t_0(float t, DFn f) {
        return dapply(f, t);
    }
    vec3 func_m(float t) {
        DFn anf = DFn(0);
        float x = app_t_0(t, anf);
        return vec3(x, x, x);
    }
    vec3 main_pure(vec2 uv) {
        return func_m(0.);
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
    struct DFn {
        int tag;
        float lctor_0;
    };
    float add_m(float x, float y) {
        return (x + y);
    }
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        return add_m(_lv_lctor_0, da);
    }
    struct fn_box {
        DFn fn;
    };
    fn_box box_add_n(float x_0) {
        DFn anf = DFn(0, x_0);
        return fn_box(anf);
    }
    vec3 main_pure(vec2 pos) {
        fn_box boxed_add_five = box_add_n(5.);
        DFn anf_0 = boxed_add_five.fn;
        float n = dapply(anf_0, 10.);
        return vec3(n, 0., 0.);
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
    struct DFn {
        int tag;
        float lctor_0;
    };
    float add_m(float x, float y) {
        return (x + y);
    }
    struct cb {
        int tag;
        DFn CB_0;
    };
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        return add_m(_lv_lctor_0, da);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf = DFn(0, 1.);
        cb f = cb(0, anf);
        DFn _lv_CB_0 = f.CB_0;
        float result = dapply(_lv_CB_0, 10.);
        return vec3(result, 0., 0.);
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
    struct box {
        float value;
    };
    float unbox_m(box _fn_arg) {
        float _lv_r_value = _fn_arg.value;
        return _lv_r_value;
    }
    vec3 main_pure(vec2 uv) {
        box anf = box(1.5);
        float n = unbox_m(anf);
        return vec3(n, 0., 0.);
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
    vec3 main_pure(vec2 uv) {
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
    vec3 main_pure(vec2 coord) {
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
    vec3 main_pure(vec2 uv) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "defunctionalize unifies int/float arrow flavors" =
  test
    {|
    type albedo_fn = vec3 -> vec3
    type material = Phong of albedo_fn * float

    let make_waves (c : vec3) : albedo_fn = fun p -> c
    let add_noise (strength : float) : albedo_fn = fun p -> [0, 0, 0]

    let scene_mat (p : vec3) : material =
      let waves = make_waves [1.0, 0.2, 0.5] in
      let noisy_waves = add_noise 0.15 in
      Phong (noisy_waves, 64.0)

    let main (uv : vec2) = [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let pick (f : vec3 -> vec3) (p : vec3) : vec3 = f p

    let main (uv : vec2) =
      let a = pick (fun p -> [0, 0, 0]) [1.0, 0.0, 0.0] in
      let b = pick (fun p -> p) [0.0, 1.0, 0.0] in
      a + b
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    vec3 lam(vec3 p_0) {
        return vec3(0., 0., 0.);
    }
    vec3 lam_0(vec3 p_1) {
        return p_1;
    }
    vec3 dapply(DFn dfn, vec3 da) {
        int _lv_tag = dfn.tag;
        switch (_lv_tag) {
            case 0: {
                return lam(da);
                break;
            }
            default: {
                return lam_0(da);
                break;
            }
        }
    }
    vec3 pick(DFn f, vec3 p) {
        return dapply(f, p);
    }
    vec3 main_pure(vec2 uv) {
        DFn anf = DFn(0);
        vec3 anf_0 = vec3(1., 0., 0.);
        vec3 a = pick(anf, anf_0);
        DFn anf_1 = DFn(1);
        vec3 anf_2 = vec3(0., 1., 0.);
        vec3 b = pick(anf_1, anf_2);
        return (a + b);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let make (b : bool) : vec3 -> vec3 =
      if b then fun p -> [0, 0, 0] else fun p -> p

    let main (uv : vec2) =
      let f = make (uv.0 > 0.5) in
      f [1.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    vec3 lam(vec3 p_0) {
        return vec3(0., 0., 0.);
    }
    vec3 lam_0(vec3 p) {
        return p;
    }
    vec3 dapply(DFn dfn, vec3 da) {
        int _lv_tag = dfn.tag;
        switch (_lv_tag) {
            case 0: {
                return lam(da);
                break;
            }
            default: {
                return lam_0(da);
                break;
            }
        }
    }
    DFn make(bool b) {
        if (b) {
            return DFn(0);
        } else {
            return DFn(1);
        }
    }
    vec3 main_pure(vec2 uv) {
        float anf = uv[0];
        bool anf_0 = (anf > 0.5);
        DFn f = make(anf_0);
        vec3 anf_1 = vec3(1., 0., 0.);
        return dapply(f, anf_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "promote ints through variant constructor coerce" =
  test
    {|
    type option['a] = Some of 'a | None
    let main uv =
      let x : option[float] = Some 5 in
      [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct option {
        int tag;
        float Some_0;
    };
    vec3 main_pure(vec2 uv) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "unused HOF with fn-typed param emits empty DFn typedef" =
  test
    {|
    let add_noise (f : vec3 -> vec3) : vec3 -> vec3 =
      fun p -> [0, 0, 0]

    let main (coord : vec2) = [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "HOF that applies param but is never called drops wrapper" =
  test
    {|
    let test (f : vec3 -> vec3) =
      fun p ->
        let n = f p in
        [0, 0, 0]

    let main uv =
      let f = test (fun a -> a) in
      f [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    struct DFn_0 {
        int tag;
        DFn lctor_0;
    };
    vec3 test(DFn f, vec3 p) {
        return vec3(0., 0., 0.);
    }
    vec3 dapply_0(DFn_0 dfn_0, vec3 da_0) {
        DFn _lv_lctor_0 = dfn_0.lctor_0;
        return test(_lv_lctor_0, da_0);
    }
    vec3 main_pure(vec2 uv) {
        DFn anf = DFn(0);
        DFn_0 f_0 = DFn_0(0, anf);
        vec3 anf_0 = vec3(0., 0., 0.);
        return dapply_0(f_0, anf_0);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "dot accepts mixed int/float vec args" =
  test
    {|
    let f : vec3 -> vec3 =
      fun p ->
        let n = #dot(p, [0, 0, 0]) in
        [n, n, n]

    let main uv = [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "DFn promotion when partial-app result flows to HOF param" =
  test
    {|
    type fn = vec3 -> vec3
    let id (f : fn) : fn = fun p -> f p
    let scene_mat (p : vec3) : fn =
      let waves p = p in
      let noisy_waves = id waves in
      noisy_waves
    let eval_material (mat : fn) : vec3 = [0.0, 0.0, 0.0]
    let main (coord : vec2) =
      let ro = [0.0, 0.0, 0.0] in
      let mat = scene_mat ro in
      let col = eval_material mat in
      col
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    struct DFn_0 {
        int tag;
        DFn lctor_0;
    };
    vec3 eval_material(DFn_0 mat) {
        return vec3(0., 0., 0.);
    }
    DFn_0 scene_mat(vec3 p_0) {
        DFn anf = DFn(0);
        DFn_0 noisy_waves = DFn_0(0, anf);
        return noisy_waves;
    }
    vec3 main_pure(vec2 coord) {
        vec3 ro = vec3(0., 0., 0.);
        DFn_0 mat_0 = scene_mat(ro);
        vec3 col = eval_material(mat_0);
        return col;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "DFn promotion when consume is declared before producer" =
  (* Same shape as the test above, but [consume] is declared before [main]'s
     [id waves] partial application that creates [DFn_0]. The post-pass must
     re-resolve [consume]'s param against the now-up-to-date [by_arrow]. *)
  test
    {|
    type fn = vec3 -> vec3
    let id (f : fn) : fn = fun p -> f p
    let waves (p : vec3) : vec3 = p
    let consume (m : fn) : vec3 = [0.0, 0.0, 0.0]
    let main (coord : vec2) =
      let nw = id waves in
      consume nw
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    struct DFn_0 {
        int tag;
        DFn lctor_0;
    };
    vec3 consume(DFn_0 m) {
        return vec3(0., 0., 0.);
    }
    vec3 main_pure(vec2 coord) {
        DFn anf = DFn(0);
        DFn_0 nw = DFn_0(0, anf);
        return consume(nw);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
