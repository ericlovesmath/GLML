open Runner

let%expect_test "int broadcasting with vecs and builtins" =
  (* int * vec3 literal *)
  test_term "let n = 2 in n * [0.5, 0.5, 0.5]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        int n = 2;
        float anf = float(2);
        vec3 anf_0 = vec3(0.5, 0.5, 0.5);
        return (anf * anf_0);
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
    vec3 main_pure(vec2 u) {
        float anf = float(n);
        vec3 anf_0 = vec3(0.5, 0.5, 0.5);
        return (anf * anf_0);
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
    vec3 main_pure(vec2 coord) {
        int n = 2;
        float anf = float(2);
        vec3 anf_0 = vec3(0.1, 0.2, 0.3);
        return (anf + anf_0);
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
    vec3 main_pure(vec2 u) {
        float anf = float(n);
        float r = sin(anf);
        return vec3(r, r, r);
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
    vec3 main_pure(vec2 coord) {
        float r = abs(5.);
        return vec3(r, r, r);
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
    vec3 main_pure(vec2 coord) {
        float r = min(1., 2.);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
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
    struct box {
        float value;
    };
    float f(box b) {
        return b.value;
    }
    vec3 main_pure(vec2 coord) {
        box anf = box(1.);
        float anf_0 = f(anf);
        return vec3(anf_0, 0., 0.);
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
    struct pair {
        float fst;
        int snd;
    };
    float get_fst(pair p) {
        return p.fst;
    }
    vec3 main_pure(vec2 coord) {
        pair p_0 = pair(1., 0);
        float anf = get_fst(p_0);
        return vec3(anf, 0., 0.);
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
    struct box {
        float value;
    };
    vec3 main_pure(vec2 coord) {
        box b = box(1.);
        float anf = b.value;
        return vec3(anf, 0., 0.);
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
    struct box {
        float value;
    };
    float get1(box b) {
        return b.value;
    }
    float get2(box b_0) {
        return b_0.value;
    }
    vec3 main_pure(vec2 coord) {
        box anf = box(1.);
        float anf_0 = get1(anf);
        box anf_1 = box(2.);
        float anf_2 = get2(anf_1);
        float anf_3 = (anf_0 + anf_2);
        return vec3(anf_3, 0., 0.);
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
    [typecheck] at 3:5-3:49: wrong number of type args
      name: box
      |
    3 |     let f (b: box[float, int]) : float = b.value
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
    struct box {
        float value;
    };
    struct box_0 {
        box value;
    };
    struct point {
        box_0 x;
        box y;
    };
    vec3 main_pure(vec2 coord) {
        box anf = box(1.);
        box_0 anf_0 = box_0(anf);
        box anf_1 = box(2.);
        point b = point(anf_0, anf_1);
        box_0 anf_2 = b.x;
        box anf_3 = anf_2.value;
        float anf_4 = anf_3.value;
        return vec3(anf_4, 0., 0.);
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
    struct option {
        int tag;
        int Some_0;
    };
    int unwrap(option opt, int default_0) {
        int _lv_tag = opt.tag;
        switch (_lv_tag) {
            case 0: {
                int _lv_Some_0 = opt.Some_0;
                int x = _lv_Some_0;
                return _lv_Some_0;
                break;
            }
            default: {
                return default_0;
                break;
            }
        }
    }
    vec3 main_pure(vec2 uv) {
        option anf = option(0, 10);
        int a = unwrap(anf, 10);
        option anf_0 = option(1, 0);
        int b = unwrap(anf_0, 5);
        option anf_1 = option(1, 0);
        int c = unwrap(anf_1, 5);
        float anf_2 = float(a);
        float anf_3 = float(b);
        float anf_4 = float(c);
        return vec3(anf_2, anf_3, anf_4);
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
    struct result {
        int tag;
        float Ok_0;
        int Err_0;
    };
    float unwrap(result r, float default_0) {
        int _lv_tag = r.tag;
        switch (_lv_tag) {
            case 0: {
                float _lv_Ok_0 = r.Ok_0;
                float x = _lv_Ok_0;
                return _lv_Ok_0;
                break;
            }
            default: {
                int _lv_Err_0 = r.Err_0;
                return default_0;
                break;
            }
        }
    }
    vec3 main_pure(vec2 uv) {
        result anf = result(0, 5.4, 0);
        float a = unwrap(anf, 5.);
        result anf_0 = result(1, 0., 2);
        float b = unwrap(anf_0, 2.3);
        float anf_1 = uv[0];
        return vec3(anf_1, a, b);
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
    struct option {
        int tag;
        int Some_0;
    };
    struct option_0 {
        int tag;
        bool Some_0;
    };
    int unwrap_m(option opt, int default_0) {
        int _lv_tag = opt.tag;
        switch (_lv_tag) {
            case 0: {
                int _lv_Some_0 = opt.Some_0;
                int x = _lv_Some_0;
                return _lv_Some_0;
                break;
            }
            default: {
                return default_0;
                break;
            }
        }
    }
    bool unwrap_m_0(option_0 opt, bool default_0) {
        int _lv_tag_0 = opt.tag;
        switch (_lv_tag_0) {
            case 0: {
                bool _lv_Some_0_0 = opt.Some_0;
                bool x = _lv_Some_0_0;
                return _lv_Some_0_0;
                break;
            }
            default: {
                return default_0;
                break;
            }
        }
    }
    vec3 main_pure(vec2 coord) {
        option_0 anf = option_0(0, true);
        bool x_0 = unwrap_m_0(anf, false);
        option anf_0 = option(0, 5);
        int y = unwrap_m(anf_0, 5);
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
        float a_0 = f_m_0(anf);
        box anf_0 = box(true);
        bool anf_1 = f_m(anf_0);
        int b_0;
        if (anf_1) {
            b_0 = 1;
        } else {
            b_0 = 2;
        }
        float anf_2 = float(b_0);
        return vec3(a_0, anf_2, 0.);
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
    float f_m(float x, float y) {
        return (x * y);
    }
    vec3 main_pure(vec2 coord) {
        float anf = f_m(1., 2.);
        return vec3(anf, 0., 0.);
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
    float scale_m(float x) {
        float anf = (x * 2.);
        return (anf - 1.);
    }
    vec2 scale_m_0(vec2 x) {
        vec2 anf_0 = (x * 2.);
        return (anf_0 - 1.);
    }
    vec3 main_pure(vec2 coord) {
        float anf_1 = scale_m(1.);
        vec2 anf_2 = vec2(anf_1, 2.);
        vec2 v = scale_m_0(anf_2);
        float anf_3 = v[0];
        float anf_4 = v[1];
        return vec3(anf_3, anf_4, 0.);
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
    float f_m(vec3 v) {
        vec3 anf = floor(v);
        vec3 anf_0 = (v - anf);
        return anf_0[0];
    }
    float f_m_0(vec2 v) {
        vec2 anf_1 = floor(v);
        vec2 anf_2 = (v - anf_1);
        return anf_2[0];
    }
    vec3 main_pure(vec2 coord) {
        vec2 anf_3 = vec2(0.5, 1.5);
        float a = f_m_0(anf_3);
        vec3 anf_4 = vec3(0.5, 1.5, 2.5);
        float b = f_m(anf_4);
        return vec3(a, b, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "parametric annotations" =
  test
    {|
    let id (x : 'a) : 'a = x
    let main (coord : vec2) =
      let a = id 1.0 in
      let b = id [1.0, 2.0, 3.0] in
      [a, b.0, b.1]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 id_m(vec3 x) {
        return x;
    }
    float id_m_0(float x) {
        return x;
    }
    vec3 main_pure(vec2 coord) {
        float a = id_m_0(1.);
        vec3 anf = vec3(1., 2., 3.);
        vec3 b = id_m(anf);
        float anf_0 = b[0];
        float anf_1 = b[1];
        return vec3(a, anf_0, anf_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "where Numeric clause" =
  test
    {|
    let dbl (x : 'a) : 'a where Num 'a = x + x
    let main (coord : vec2) =
      let a = dbl 1.0 in
      let b = dbl [1.0, 2.0, 3.0] in
      [a, b.0, b.1]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 dbl_m(vec3 x) {
        return (x + x);
    }
    float dbl_m_0(float x) {
        return (x + x);
    }
    vec3 main_pure(vec2 coord) {
        float a = dbl_m_0(1.);
        vec3 anf = vec3(1., 2., 3.);
        vec3 b = dbl_m(anf);
        float anf_0 = b[0];
        float anf_1 = b[1];
        return vec3(a, anf_0, anf_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "where broadcast" =
  test
    {|
    let add2 (a : 'a) (b : 'b) : 'r where ('a + 'b -> 'r) = a + b
    let main (coord : vec2) =
      let v = add2 [1.0, 2.0, 3.0] 0.5 in
      v
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 add2_m(vec3 a, float b) {
        return (a + b);
    }
    vec3 main_pure(vec2 coord) {
        vec3 anf = vec3(1., 2., 3.);
        vec3 v = add2_m(anf, 0.5);
        return v;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "where mul broadcast" =
  test
    {|
    let scale (m : 'a) (v : 'b) : 'r where ('a * 'b -> 'r) = m * v
    let main (coord : vec2) =
      let v = scale 2.0 [1.0, 2.0, 3.0] in
      v
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 scale_m(float m, vec3 v) {
        return (m * v);
    }
    vec3 main_pure(vec2 coord) {
        vec3 anf = vec3(1., 2., 3.);
        vec3 v_0 = scale_m(2., anf);
        return v_0;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "multiple where clauses" =
  test
    {|
    let combine (a : 'a) (b : 'b) (c : 'c) : 'r
        where (Num 'c, 'a + 'b -> 'r)
      = a + b + c
    let main (coord : vec2) =
      let v = combine [1.0, 2.0, 3.0] 0.5 1.0 in
      v
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 combine_m(vec3 a, float b, float c) {
        vec3 anf = (a + b);
        return (anf + c);
    }
    vec3 main_pure(vec2 coord) {
        vec3 anf_0 = vec3(1., 2., 3.);
        vec3 v = combine_m(anf_0, 0.5, 1.);
        return v;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "parens around individual where clauses" =
  test
    {|
    let combine (a : 'a) (b : 'b) (c : 'c) : 'r
      where (Num 'c), ('a + 'b -> 'r)
      = a + b + c
    let main (coord : vec2) =
      let v = combine [1.0, 2.0, 3.0] 0.5 1.0 in
      v
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 combine_m(vec3 a, float b, float c) {
        vec3 anf = (a + b);
        return (anf + c);
    }
    vec3 main_pure(vec2 coord) {
        vec3 anf_0 = vec3(1., 2., 3.);
        vec3 v = combine_m(anf_0, 0.5, 1.);
        return v;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "where clause on non-toplevel let" =
  test
    {|
    let main (coord : vec2) =
      let combine (a : 'a) (b : 'b) (c : 'c) : 'r
        where Num 'c, 'a + 'b -> 'r
        = a + b + c in
      combine [1.0, 2.0, 3.0] 0.5 1.0
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 combine_m_0(vec3 a, float b, float c) {
        vec3 anf = (a + b);
        return (anf + c);
    }
    vec3 main_pure(vec2 coord) {
        vec3 anf_0 = vec3(1., 2., 3.);
        return combine_m_0(anf_0, 0.5, 1.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "where clause on non-toplevel let - unknown class" =
  test
    {|
    let main (coord : vec2) =
      let f (x : 'a) : 'a where Foo 'a = x in
      [f 0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [parser] at 3:33-3:36: expected one of: `(`, identifier, type keyword/variable but found `Foo`
      in: "let expression at 3:7-3:10"
      in: "a top-level definition at 2:5-2:8"
      |
    3 |       let f (x : 'a) : 'a where Foo 'a = x in
      |                                 ^^^
    |}]
;;
