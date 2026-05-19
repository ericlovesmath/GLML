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
    vec3 main_pure(vec2 coord) {
        return vec3(1., 0., 0.);
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
    vec3 main_pure(vec2 coord) {
        return vec3(1., 0., 0.);
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
        return vec3(1., 0., 0.);
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
    vec3 main_pure(vec2 coord) {
        return vec3(3., 0., 0.);
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
        return vec3(1., 0., 0.);
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
                return _lv_Ok_0;
                break;
            }
            default: {
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
      let x = unwrap (Some 1.0) 2.0 in
      let y = unwrap (Some 5) 5 in
      [ y, x, 0 ]
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
        float Some_0;
    };
    vec3 main_pure(vec2 coord) {
        float anf_1 = float(5);
        return vec3(anf_1, 1., 0.);
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
    vec3 main_pure(vec2 coord) {
        float anf_2 = float(1);
        return vec3(1., anf_2, 0.);
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
    vec3 main_pure(vec2 coord) {
        return vec3(2., 0., 0.);
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
    vec3 main_pure(vec2 coord) {
        return vec3(1., 3., 0.);
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
    vec3 main_pure(vec2 coord) {
        vec2 anf_3 = vec2(0.5, 1.5);
        vec2 anf_1_0 = floor(anf_3);
        vec2 anf_2_0 = (anf_3 - anf_1_0);
        float a = anf_2_0[0];
        vec3 anf_4 = vec3(0.5, 1.5, 2.5);
        vec3 anf_5 = floor(anf_4);
        vec3 anf_0_0 = (anf_4 - anf_5);
        float b = anf_0_0[0];
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
    vec3 main_pure(vec2 coord) {
        return vec3(1., 1., 2.);
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
    vec3 main_pure(vec2 coord) {
        return vec3(2., 2., 4.);
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
    vec3 main_pure(vec2 coord) {
        vec3 v = vec3(1.5, 2.5, 3.5);
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
    vec3 main_pure(vec2 coord) {
        vec3 v_0 = vec3(2., 4., 6.);
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
    vec3 main_pure(vec2 coord) {
        vec3 v = vec3(2.5, 3.5, 4.5);
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
    vec3 main_pure(vec2 coord) {
        vec3 v = vec3(2.5, 3.5, 4.5);
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
    vec3 main_pure(vec2 coord) {
        return vec3(2.5, 3.5, 4.5);
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
