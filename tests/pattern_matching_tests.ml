open Runner

let%expect_test "bool match" =
  test
    {|
    #extern bool b
    let main (coord : vec2) = let c = (match b with
      | true -> [1.0, 0.0, 0.0]
      | false -> [0.0, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform bool b;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        vec3 c;
        if (b) {
            c = vec3(1., 0., 0.);
        } else {
            c = vec3(0., 0., 0.);
        }
        float anf = c[0];
        float anf_0 = c[1];
        float anf_1 = c[2];
        fragColor = vec4(anf, anf_0, anf_1, 1.);
    }
    |}];
  test
    {|
    #extern bool b
    let main (coord : vec2) = let x = match b with
        | true -> 1.0
      in
      [x, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:39-4:22: non-exhaustive match
      missing: (witness false)
      |
    3 |     let main (coord : vec2) = let x = match b with
    4 |         | true -> 1.0
      |
    |}];
  test
    {|
    #extern bool b
    let main (coord : vec2) = let x = match b with
        | true -> 1.0
        | true -> 1.0
        | false -> 1.0
      in
      [x, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:39-6:23: redundant match arm
      id: 1
      |
    3 |     let main (coord : vec2) = let x = match b with
    4 |         | true -> 1.0
    5 |         | true -> 1.0
    6 |         | false -> 1.0
      |
    |}];
  test
    {|
    #extern bool b
    let main (coord : vec2) = let x = match b with
        | false -> 1.0
        | _ -> 0.0
      in
      [x, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform bool b;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float x;
        if (b) {
            x = 0.;
        } else {
            x = 1.;
        }
        fragColor = vec4(x, 0., 0., 1.);
    }
    |}]
;;

let%expect_test "int match" =
  test
    {|
    #extern int n
    let main (coord : vec2) = let x = match n with
        | 0 -> 0.0
        | 1 -> 1.0
        | _ -> 2.0
      in
      [x, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform int n;
    void main() {
        vec2 coord = gl_FragCoord.xy;
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
        fragColor = vec4(x, 0., 0., 1.);
    }
    |}];
  test
    {|
    #extern int n
    let main (coord : vec2) = let x = match n with
        | 0 -> 0.0
        | 4 -> 0.0
      in
      [x, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:39-5:19: non-exhaustive match
      missing: (witness 1)
      |
    3 |     let main (coord : vec2) = let x = match n with
    4 |         | 0 -> 0.0
    5 |         | 4 -> 0.0
      |
    |}];
  test
    {|
    #extern int n
    let main (coord : vec2) = let x = match n with
        | 0 -> 0.0
        | 0 -> 1.0
        | k -> 0.0
      in
      [x, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:39-6:19: redundant match arm
      id: 1
      |
    3 |     let main (coord : vec2) = let x = match n with
    4 |         | 0 -> 0.0
    5 |         | 0 -> 1.0
    6 |         | k -> 0.0
      |
    |}]
;;

let%expect_test "float match" =
  test
    {|
    #extern float x
    let main (coord : vec2) = let c = match x with
        | 1.0 -> 0.0
        | 2.5 -> 1.0
        | _ -> 2.0
      in
      [c, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float x;
    void main() {
        vec2 coord = gl_FragCoord.xy;
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
        fragColor = vec4(c, 0., 0., 1.);
    }
    |}];
  (* Float match in return position *)
  test
    {|
    #extern float x
    let main (coord : vec2) = let c = (match x with
        | 0.0 -> [1.0, 0.0, 0.0]
        | 1.0 -> [0.0, 1.0, 0.0]
        | _ -> [0.0, 0.0, 1.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float x;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        bool _lv_cmp_0 = (x == 0.);
        vec3 c;
        if (_lv_cmp_0) {
            c = vec3(1., 0., 0.);
        } else {
            bool _lv_cmp = (x == 1.);
            if (_lv_cmp) {
                c = vec3(0., 1., 0.);
            } else {
                c = vec3(0., 0., 1.);
            }
        }
        float anf = c[0];
        float anf_0 = c[1];
        float anf_1 = c[2];
        fragColor = vec4(anf, anf_0, anf_1, 1.);
    }
    |}];
  test
    {|
    #extern float x
    let main (coord : vec2) = let c = (match x with
        | 0.0 -> [1.0, 0.0, 0.0]
        | 1.0 -> [0.0, 1.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:39-5:34: non-exhaustive match
      missing: (witness 2.)
      |
    3 |     let main (coord : vec2) = let c = (match x with
    4 |         | 0.0 -> [1.0, 0.0, 0.0]
    5 |         | 1.0 -> [0.0, 1.0, 0.0]) in [c.0, c.1, c.2, 1.0]
      |
    |}];
  test
    {|
    #extern float x
    let main (coord : vec2) = let c = (match x with
        | 0.0 -> [1.0, 0.0, 0.0]
        | 0.0 -> [0.0, 1.0, 0.0]
        | _ -> [0.0, 0.0, 1.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:39-6:32: redundant match arm
      id: 1
      |
    3 |     let main (coord : vec2) = let c = (match x with
    4 |         | 0.0 -> [1.0, 0.0, 0.0]
    5 |         | 0.0 -> [0.0, 1.0, 0.0]
    6 |         | _ -> [0.0, 0.0, 1.0]) in [c.0, c.1, c.2, 1.0]
      |
    |}];
  test
    {|
    #extern float x
    let main (coord : vec2) = let c = (match x with
        | 0.0 -> [1.0, 0.0, 0.0]
        | -0.0 -> [0.0, 1.0, 0.0]
        | _ -> [0.0, 0.0, 1.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:39-6:32: redundant match arm
      id: 1
      |
    3 |     let main (coord : vec2) = let c = (match x with
    4 |         | 0.0 -> [1.0, 0.0, 0.0]
    5 |         | -0.0 -> [0.0, 1.0, 0.0]
    6 |         | _ -> [0.0, 0.0, 1.0]) in [c.0, c.1, c.2, 1.0]
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

    let main (coord : vec2) = let a = area (Circle 2.0) in
      let b = area (Rect (3.0, 4.0)) in
      let c = area Empty in
      [a, b, c, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(12.56636, 12., 0., 1.);
    }
    |}]
;;

let%expect_test "variant match in let binding" =
  test
    {|
    type opt =
      | Some of float
      | None

    let main (coord : vec2) = let x = Some 5.0 in
      let v = match x with
        | Some f -> f
        | None -> 0.0
      in
      [v, v, v, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(5., 5., 5., 1.);
    }
    |}]
;;

let%expect_test "variant exhaustive checking and incorrect maching" =
  test
    {|
    type color = | Red | Green | Blue

    let main (coord : vec2) = let v = match Red with
        | Red -> 1.0
        | Blue -> 2.0
      in
      [v, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 4:39-6:22: non-exhaustive match
      missing: (witness (Green))
      |
    4 |     let main (coord : vec2) = let v = match Red with
    5 |         | Red -> 1.0
    6 |         | Blue -> 2.0
      |
    |}];
  test
    {|
    type shape =
      | Circle of float
      | Empty

    let main (coord : vec2) = let s = Circle (1.0, 2.0) in
      [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 6:39-6:56: wrong number of args to constructor
      ctor: Circle
      |
    6 |     let main (coord : vec2) = let s = Circle (1.0, 2.0) in
      |                                       ^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "struct pattern matching" =
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) = let c = (match { x = 1.0, y = 2.0 } with
      | { x = a, y = b } -> [a, b, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(1., 2., 0., 1.);
    }
    |}];
  (* Partial: bind one field, ignore rest with _ *)
  test
    {|
    type rgb = { r : float, g : float, b : float }

    let main (uv : vec2) = let c = (let c : rgb = { r = 1.0, g = 0.5, b = 0.0 } in
      match c with
      | { r = red, _ } -> [red, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(1., 0., 0., 1.);
    }
    |}];
  test
    {|
    type box['a] = { value : 'a }

    let main (uv : vec2) = let c = (let b = { value = 1.5 } in
      match b with
      | { value = v, _ } -> [v, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(1.5, 0., 0., 1.);
    }
    |}];
  (* Error non-exhaustive *)
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) = let c = (let p : point = { x = 1.0, y = 2.0 } in
      match p with
      | { x = a } -> [a, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 5:7-6:35: non-exhaustive record pat
      |
    5 |       match p with
    6 |       | { x = a } -> [a, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
      |
    |}];
  (* Error unknown field *)
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) = let c = (let p : point = { x = 1.0, y = 2.0 } in
      match p with
      | { x = a, z = b } -> [a, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 5:7-6:42: unknown constructor/field in pattern
      |
    5 |       match p with
    6 |       | { x = a, z = b } -> [a, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
      |
    |}];
  (* Error:duplicate field *)
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) = let c = (let p : point = { x = 1.0, y = 2.0 } in
      match p with
      | { x = a, x = b } -> [a, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 5:7-6:42: duplicate field
      fname: x
      |
    5 |       match p with
    6 |       | { x = a, x = b } -> [a, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
      |
    |}];
  (* Field Punning *)
  test
    {|
    type point = { x : float, y : float }

    let main (uv : vec2) = let c = (match { x = 1.0, y = 2.0 } with
      | { x = a, y } -> [a, y, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(1., 2., 0., 1.);
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

    let main (coord : vec2) = let a = f [coord.0, coord.1, 0.0] in
      let b = g [[1.0, 0.0], [0.0, 1.0]] in
      let c = h coord in
      [a, b, c, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float anf = coord[0];
        float anf_0 = coord[1];
        float c_0 = (anf + anf_0);
        fragColor = vec4(anf, 2., c_0, 1.);
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

    let main (uv : vec2) = let (x : float) = 2.0 in
      let [u, v] = uv in
      [x, f (Wrap 1.0), u + v, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        float _lv_v0 = uv[0];
        float _lv_v1 = uv[1];
        float anf_1 = (_lv_v0 + _lv_v1);
        fragColor = vec4(2., 2., anf_1, 1.);
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

    let main (coord : vec2) = let _ = x None in
      [0.0, 0.0, 0.0, 1.0]
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

    let main (coord : vec2) = let v = f (Some (Some 0.5)) in
      [v, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0.5, 0., 0., 1.);
    }
    |}]
;;

let%expect_test "nested pattern matching with literals in records" =
  test
    {|
    type point = { x : float, y : float }

    let main (coord : vec2) = let p = { x = 0.0, y = 0.5 } in
      let v = match p with
        | { x = 0.0, y } -> y
        | { x, y } -> x + y
      in
      [v, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0.5, 0., 0., 1.);
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

    let main (coord : vec2) = let _ = f (Some 0) in
      [0.0, 0.0, 0.0, 1.0]
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

    let main (coord : vec2) = let _ = f None in
      [0.0, 0.0, 0.0, 1.0]
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
    let main (coord : vec2) = let n = 0 in
      let _ = match n with
        | 0 -> 0
        | 1 -> 1
      in
      [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:15-5:17: non-exhaustive match
      missing: (witness 2)
      |
    3 |       let _ = match n with
    4 |         | 0 -> 0
    5 |         | 1 -> 1
      |
    |}];
  test
    {|
    let main (coord : vec2) = let n = 7 in
      let _ = match n with
        | 0 -> 0
        | x -> x
      in
      [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}];
  test
    {|
    let main (coord : vec2) = let f = 0.0 in
      let _ = match f with
        | 0.0 -> 1.0
        | 1.0 -> 2.0
      in
      [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:15-5:21: non-exhaustive match
      missing: (witness 2.)
      |
    3 |       let _ = match f with
    4 |         | 0.0 -> 1.0
    5 |         | 1.0 -> 2.0
      |
    |}];
  test
    {|
    let main (coord : vec2) = let b = true in
      let _ = match b with
        | true -> 1.0
      in
      [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:15-4:22: non-exhaustive match
      missing: (witness false)
      |
    3 |       let _ = match b with
    4 |         | true -> 1.0
      |
    |}];
  test
    {|
    type option['a] = Some of 'a | None

    let main (coord : vec2) = let v = match Some 1.0 with
        | Some _ -> 1.0
        | None -> 0.0
      in
      [v, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 0., 0., 1.);
    }
    |}];
  test
    {|
    type point = { x : float, y : float }

    let main (coord : vec2) = let p = { x = 0.0, y = 0.5 } in
      let _ = match p with
        | { x = 0.0, y } -> y
      in
      [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 5:15-6:30: non-exhaustive match
      missing: (witness (record (x 1.) (y 0.)))
      |
    5 |       let _ = match p with
    6 |         | { x = 0.0, y } -> y
      |
    |}];
  test
    {|
    let main (coord : vec2) = let v = [0.0, 1.0] in
      let _ = match v with
        | [0.0, _] -> 0.0
      in
      [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:15-4:26: non-exhaustive match
      missing: (witness (bracket 1. 0.))
      |
    3 |       let _ = match v with
    4 |         | [0.0, _] -> 0.0
      |
    |}]
;;
