open Runner

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
    vec3 main_pure(vec2 coord) {
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
    [typecheck] at 4:15-5:22: non-exhaustive match
      missing: (witness false)
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
    [typecheck] at 4:15-7:23: redundant match arm
      id: 1
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
    vec3 main_pure(vec2 coord) {
        float x;
        if (b) {
            x = 0.;
        } else {
            x = 1.;
        }
        return vec3(x, 0., 0.);
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
    vec3 main_pure(vec2 coord) {
        float x;
        switch (n) {
            case 0: {
                x = 0.;
                break;
            }
            case 1: {
                x = 1.;
                break;
            }
            default: {
                x = 2.;
                break;
            }
        }
        return vec3(x, 0., 0.);
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
    [typecheck] at 4:15-6:19: non-exhaustive match
      missing: (witness 1)
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
    [typecheck] at 4:15-7:19: redundant match arm
      id: 1
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
    vec3 main_pure(vec2 coord) {
        bool _lv_cmp_0 = (x == 1.);
        float c;
        if (_lv_cmp_0) {
            c = 0.;
        } else {
            bool _lv_cmp = (x == 2.5);
            if (_lv_cmp) {
                c = 1.;
            } else {
                c = 2.;
            }
        }
        return vec3(c, 0., 0.);
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
    vec3 main_pure(vec2 coord) {
        bool _lv_cmp_0 = (x == 0.);
        if (_lv_cmp_0) {
            return vec3(1., 0., 0.);
        } else {
            bool _lv_cmp = (x == 1.);
            if (_lv_cmp) {
                return vec3(0., 1., 0.);
            } else {
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
    [typecheck] at 4:7-6:33: non-exhaustive match
      missing: (witness 2.)
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
    [typecheck] at 4:7-7:31: redundant match arm
      id: 1
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
    [typecheck] at 4:7-7:31: redundant match arm
      id: 1
      |
    4 |       match x with
    5 |         | 0.0 -> [1.0, 0.0, 0.0]
    6 |         | -0.0 -> [0.0, 1.0, 0.0]
    7 |         | _ -> [0.0, 0.0, 1.0]
      |
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
    float area(shape s) {
        int _lv_tag = s.tag;
        switch (_lv_tag) {
            case 0: {
                float _lv_Circle_0 = s.Circle_0;
                float anf = (3.14159 * _lv_Circle_0);
                return (anf * _lv_Circle_0);
                break;
            }
            case 1: {
                float _lv_Rect_0 = s.Rect_0;
                float _lv_Rect_1 = s.Rect_1;
                return (_lv_Rect_0 * _lv_Rect_1);
                break;
            }
            default: {
                return 0.;
                break;
            }
        }
    }
    vec3 main_pure(vec2 coord) {
        shape anf_0 = shape(0, 2., 0., 0.);
        float a = area(anf_0);
        shape anf_1 = shape(1, 0., 3., 4.);
        float b = area(anf_1);
        shape anf_2 = shape(2, 0., 0., 0.);
        float c = area(anf_2);
        return vec3(a, b, c);
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
    vec3 main_pure(vec2 coord) {
        return vec3(5., 5., 5.);
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
      missing: (witness (Green))
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

let%expect_test "struct pattern matching" =
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) : vec3 =
      match { x = 1.0, y = 2.0 } with
      | { x = a, y = b } -> [a, b, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 2., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Partial: bind one field, ignore rest with _ *)
  test
    {|
    type rgb = { r : float, g : float, b : float }

    let main (uv : vec2) : vec3 =
      let c : rgb = { r = 1.0, g = 0.5, b = 0.0 } in
      match c with
      | { r = red, _ } -> [red, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type box['a] = { value : 'a }

    let main (uv : vec2) : vec3 =
      let b = { value = 1.5 } in
      match b with
      | { value = v, _ } -> [v, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1.5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Error non-exhaustive *)
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) : vec3 =
      let p : point = { x = 1.0, y = 2.0 } in
      match p with
      | { x = a } -> [a, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 6:7-7:35: non-exhaustive record pat
      |
    6 |       match p with
    7 |       | { x = a } -> [a, 0.0, 0.0]
      |
    |}];
  (* Error unknown field *)
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) : vec3 =
      let p : point = { x = 1.0, y = 2.0 } in
      match p with
      | { x = a, z = b } -> [a, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 6:7-7:42: unknown constructor/field in pattern
      |
    6 |       match p with
    7 |       | { x = a, z = b } -> [a, 0.0, 0.0]
      |
    |}];
  (* Error:duplicate field *)
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) : vec3 =
      let p : point = { x = 1.0, y = 2.0 } in
      match p with
      | { x = a, x = b } -> [a, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 6:7-7:42: duplicate field
      fname: x
      |
    6 |       match p with
    7 |       | { x = a, x = b } -> [a, 0.0, 0.0]
      |
    |}];
  (* Field Punning *)
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) : vec3 =
      match { x = 1.0, y = 2.0 } with
      | { x = a, y } -> [a, y, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 2., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "bracket pattern matching" =
  test
    {|
    let f = function
      | [x, _, z] -> x + z

    let g = function
      | [[a, b], [c, d]] -> a + d

    let h (v : vec2) : float = match v with | [x, y] -> x + y

    let main (coord : vec2) : vec3 =
      let a = f [coord.0, coord.1, 0.0] in
      let b = g [[1.0, 0.0], [0.0, 1.0]] in
      let c = h coord in
      [a, b, c]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        float anf = coord[0];
        float a_0 = (anf + 0.);
        float _lv_v0_3_0 = coord[0];
        float _lv_v1_3_0 = coord[1];
        float c_0 = (_lv_v0_3_0 + _lv_v1_3_0);
        return vec3(a_0, 2., c_0);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "let pattern binding" =
  test
    {|
    type wrapper = Wrap of float

    let f w =
      let Wrap v = w in
      let (Wrap v') = w in
      v + v'

    let main (uv : vec2) =
      let (x : float) = 2.0 in
      let [u, v] = uv in
      [x, f (Wrap 1.0), u + v]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        float _lv_v0 = uv[0];
        float _lv_v1 = uv[1];
        float anf_1 = (_lv_v0 + _lv_v1);
        return vec3(2., 2., anf_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "nested pattern matching" =
  test
    {|
    type option['a] = Some of 'a | None

    let x a = match a with
      | Some 1 -> 1
      | None -> 1

    let main (coord : vec2) : vec3 =
      let _ = x None in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-6:18: non-exhaustive match
      missing: (witness (Some 0))
      |
    4 |     let x a = match a with
    5 |       | Some 1 -> 1
    6 |       | None -> 1
      |
    |}]
;;

let%expect_test "nested pattern matching with polymorphism" =
  test
    {|
    type option['a] = Some of 'a | None

    let f a = match a with
      | Some (Some y) -> y
      | Some None -> 0.0
      | None -> 1.0

    let main (coord : vec2) : vec3 =
      let v = f (Some (Some 0.5)) in
      [v, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        return vec3(0.5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "nested pattern matching with literals in records" =
  test
    {|
    type point = { x : float, y : float }

    let main (coord : vec2) : vec3 =
      let p = { x = 0.0, y = 0.5 } in
      let v = match p with
        | { x = 0.0, y } -> y
        | { x, y } -> x + y
      in
      [v, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        return vec3(0.5, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "nested pattern matching with duplicate arms" =
  test
    {|
    type option['a] = Some of 'a | None

    let f a = match a with
      | Some 1 -> 1
      | Some 1 -> 2
      | Some _ -> 0
      | None -> -1

    let main (coord : vec2) : vec3 =
      let _ = f (Some 0) in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-8:19: redundant match arm
      id: 1
      |
    4 |     let f a = match a with
    5 |       | Some 1 -> 1
    6 |       | Some 1 -> 2
    7 |       | Some _ -> 0
    8 |       | None -> -1
      |
    |}]
;;

let%expect_test "nested pattern matching with non exhaustive bool" =
  test
    {|
    type option['a] = Some of 'a | None

    let f a = match a with
      | Some true -> 1.0
      | None -> 3.0

    let main (coord : vec2) : vec3 =
      let _ = f None in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-6:20: non-exhaustive match
      missing: (witness (Some false))
      |
    4 |     let f a = match a with
    5 |       | Some true -> 1.0
    6 |       | None -> 3.0
      |
    |}]
;;

let%expect_test "pattern match exhaustiveness edge cases" =
  test
    {|
    let main (coord : vec2) : vec3 =
      let n = 0 in
      let _ = match n with
        | 0 -> 0
        | 1 -> 1
      in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-6:17: non-exhaustive match
      missing: (witness 2)
      |
    4 |       let _ = match n with
    5 |         | 0 -> 0
    6 |         | 1 -> 1
      |
    |}];
  test
    {|
    let main (coord : vec2) : vec3 =
      let n = 7 in
      let _ = match n with
        | 0 -> 0
        | x -> x
      in
      [0.0, 0.0, 0.0]
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
    |}];
  test
    {|
    let main (coord : vec2) : vec3 =
      let f = 0.0 in
      let _ = match f with
        | 0.0 -> 1.0
        | 1.0 -> 2.0
      in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-6:21: non-exhaustive match
      missing: (witness 2.)
      |
    4 |       let _ = match f with
    5 |         | 0.0 -> 1.0
    6 |         | 1.0 -> 2.0
      |
    |}];
  test
    {|
    let main (coord : vec2) : vec3 =
      let b = true in
      let _ = match b with
        | true -> 1.0
      in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-5:22: non-exhaustive match
      missing: (witness false)
      |
    4 |       let _ = match b with
    5 |         | true -> 1.0
      |
    |}];
  test
    {|
    type option['a] = Some of 'a | None

    let main (coord : vec2) : vec3 =
      let v = match Some 1.0 with
        | Some _ -> 1.0
        | None -> 0.0
      in
      [v, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        return vec3(1., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type point = { x : float, y : float }

    let main (coord : vec2) : vec3 =
      let p = { x = 0.0, y = 0.5 } in
      let _ = match p with
        | { x = 0.0, y } -> y
      in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 6:15-7:30: non-exhaustive match
      missing: (witness (record (x 1.) (y 0.)))
      |
    6 |       let _ = match p with
    7 |         | { x = 0.0, y } -> y
      |
    |}];
  test
    {|
    let main (coord : vec2) : vec3 =
      let v = [0.0, 1.0] in
      let _ = match v with
        | [0.0, _] -> 0.0
      in
      [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    [typecheck] at 4:15-5:26: non-exhaustive match
      missing: (witness (bracket 1. 0.))
      |
    4 |       let _ = match v with
    5 |         | [0.0, _] -> 0.0
      |
    |}]
;;
