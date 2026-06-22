open Runner

let%expect_test "simple tests for compile_stlc" =
  test_term "let x = 2.0 in [ 12.0 * x + 10.0, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(34., 0., 0., 1.);
    }
    |}];
  test_term "if true && false then [ 1.0, 0.0, 0.0 ] else [ 0.0, 0.0, 0.0 ]";
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
    #extern float n
    let f = fun (x : float) -> x + n
    let main = fun (u : vec2) -> [f 10.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float n;
    void main() {
        vec2 u = gl_FragCoord.xy;
        float anf = (10. + n);
        fragColor = vec4(anf, 0., 0., 1.);
    }
    |}];
  test
    {|
    #extern float n
    let f (x : float) = x + n
    let main (u : vec2) = [f 10.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float n;
    void main() {
        vec2 u = gl_FragCoord.xy;
        float anf = (10. + n);
        fragColor = vec4(anf, 0., 0., 1.);
    }
    |}]
;;

let%expect_test "generic vectors and matrices" =
  test
    {|
    let main (u : vec2) = let m = [ [1.0, 0.0, 0.0], [ 0.0, 1.0, 0.0 ], [ 0.0, 0.0, 1.0] ] in
      let n = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]] in
      let v = [ 1.0, 2.0 ] in
      [(m.0).0, (n.0).0, v.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(1., 1., 1., 1.);
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
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 0., 0., 1.);
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
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 0., 0., 1.);
    }
    |}];
  test_term "[0.0, 0.0, 0.0].4";
  [%expect
    {|
    [constraint solver] at 1:35-1:54: vec index out of bounds
      n: 3
      i: 4
      |
    1 | let main (coord : vec2) = let c = ([0.0, 0.0, 0.0].4) in [c.0, c.1, c.2, 1.0]
      |                                   ^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "builtins" =
  test_term "let v = [ 1.0, 2.0, 3.0 ] in [ #sin 1.0, #dot v v, #length v ]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0.8414709848078965, 14., 3.7416573867739413, 1.);
    }
    |}];
  test_term "#cross [1.0, 2.0, 3.0] [0.0, 2.0, 5.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(4., -5., 2., 1.);
    }
    |}];
  test_term "#cross [ 1.0, 1.0 ] [ 0.0, 0.0 ]";
  [%expect
    {|
    [constraint solver] at 1:36-1:55: type mismatch
      ty: (vec 2 'v_2)
      ty': (vec 3 float)
      |
    1 | let main (coord : vec2) = let c = (#cross [ 1.0, 1.0 ] [ 0.0, 0.0 ]) in [c.0, c.1, c.2, 1.0]
      |                                    ^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "multi argument functions / lambdas" =
  test
    {|
    let f (x : float) (y : float) = x + y
    let g = fun (x : float) (y : float) -> x - y
    let main (u : vec2) = [f 10.0 5.0, g 0.0 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(15., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "lambda lifting" =
  test
    {|
    let main (u : vec2) = let x = 10.0 in
      let y = 5.0 in
      let add (z : float) = x + y + z in
      [add 1.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(16., 0., 0., 1.);
    }
    |}];
  test
    {|
    let main (u : vec2) = let c = (let f (x : float) =
        let g (y : float) = x + y in
        ([ g 1.0, 0.0, 0.0 ])
      in
      f 10.0) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(11., 0., 0., 1.);
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
    [typecheck] at 2:5-4:8: main must have type vec2 -> vec4
      ty: ((vec 2 float) -> (float -> float))
      |
    2 |     let main (u : vec2) =
    3 |       let f = fun (x : float) -> x + 1.0 in
    4 |       f
      |
    |}];
  test
    {|
    let apply_f (f : float -> float) (x : float) = f x
    let main (u : vec2) = [apply_f (fun x -> x + 1) 10.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(11., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "recursive functions" =
  test
    {|
    let rec fact (n : int) (acc : int) : int =
      if n = 0 then acc else fact (n - 1) (acc * n)

    let main (u : vec2) = let num = fact 5 1 in
      [num, 0., 0., 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    int fact(int n, int acc) {
        int _iter = 0;
        while (true) {
            bool _lim_cond = (_iter < 1000);
            if (_lim_cond) {
                bool anf = (n == 0);
                if (anf) {
                    return acc;
                } else {
                    int anf_0 = (n - 1);
                    int anf_1 = (acc * n);
                    int _iter_inc = (_iter + 1);
                    _iter = _iter_inc;
                    n = anf_0;
                    acc = anf_1;
                    continue;
                }
            } else {
                return 0;
            }
        }
    }
    void main() {
        vec2 u = gl_FragCoord.xy;
        int num = fact(5, 1);
        float anf_2 = float(num);
        fragColor = vec4(anf_2, 0., 0., 1.);
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

    let main (u: vec2) = let p = { x = 1.0, y = 2.0 } in
      let c = make_red p in
      [c.r, c.g, c.b, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(2., 0., 0., 1.);
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

    let main (u: vec2) = let p = { x = 1.0, y = 2.0 } in
      let c = make_red p in
      [c.r, c.g, c.b, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(1., 0., 0., 1.);
    }
    |}];
  test
    {|
    type point = { x: float, y: float }

    let main (u: vec2) = let p = { x = 1.0, z = 2.0 } in
      [p.x, p.x, p.x, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 4:34-4:54: record does not match any known struct
      provided_fields: (x z)
      |
    4 |     let main (u: vec2) = let p = { x = 1.0, z = 2.0 } in
      |                                  ^^^^^^^^^^^^^^^^^^^^
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

    let main (u: vec2) = let seg = make_seg 1.0 in
      let c = seg.end.x in
      [c, c, c, 1.0]
    |}
  in
  test test_program;
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u_0 = gl_FragCoord.xy;
        fragColor = vec4(1., 1., 1., 1.);
    }
    |}]
;;

let%expect_test "monomorphization tests" =
  test
    {|
    let id x = x
    let main (coord : vec2) = let c = (let a = id 1.0 in
      let b = id true in
      if b then [a, 0.0, 0.0] else [0.0, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
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
    let main (coord : vec2) = let c = (let id x = x in
      let a = id 1.0 in
      let b = id true in
      if b then [a, 0.0, 0.0] else [0.0, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
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
  (* Unused polymorphic function *)
  test
    {|
    let id x = x
    let main (coord : vec2) = [1.0, 0.0, 0.0, 1.0]
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
  (* Validate no duplication of polymorphic function *)
  test
    {|
    let main (coord : vec2) = let id x = x in
      let a = id 1.0 in
      let b = id 2.0 in
      [a, b, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 2., 0., 1.);
    }
    |}]
;;

let%expect_test "advanced monomorphization example" =
  test
    {|
    let id x = x
    let const x y = x
    let main (coord : vec2) = let a = id 1.0 in
      let b = const 2.0 true in
      [a, b, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 2., 0., 1.);
    }
    |}]
;;

let%expect_test "toplevel constant (atomic only)" =
  test
    {|
    let pi = 3.14159

    let main (u : vec2) = [pi, pi, pi, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(3.14159, 3.14159, 3.14159, 1.);
    }
    |}];
  test
    {|
    let x = #sin 1.0 + #cos 2.0

    let main (u : vec2) = [x, x, x, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(0.4253241482607541, 0.4253241482607541, 0.4253241482607541, 1.);
    }
    |}]
;;

let%expect_test "promotion of ints to floats" =
  test
    {|
    let main (u : vec2) = let b = 1 + 2 in
      let a = b + 2. in
      [b, a, 3, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(3., 5., 3., 1.);
    }
    |}]
;;

let%expect_test "defunctionalization" =
  (* Named function reference as higher-order argument *)
  test
    {|
    let apply f x = f x
    let double n = n * 2.0
    let main (pos : vec2) = let r = apply double pos.0 in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float anf_0 = pos[0];
        float r = (anf_0 * 2.);
        fragColor = vec4(r, r, r, 1.);
    }
    |}];
  (* Lambda in argument position *)
  test
    {|
    let apply f x = f x
    let main (pos : vec2) = let r = apply (fun y -> y + 1.0) (pos.0) in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float anf_0 = pos[0];
        float r = (anf_0 + 1.);
        fragColor = vec4(r, r, r, 1.);
    }
    |}];
  (* Lambda capturing a free variable (closure) *)
  test
    {|
    let apply f x = f x
    let main (pos : vec2) = let px = pos.0 in
      let r = apply (fun y -> px + y) pos.1 in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float px = pos[0];
        float anf_0 = pos[1];
        float r = (px + anf_0);
        fragColor = vec4(r, r, r, 1.);
    }
    |}];
  (* Lambda stored in let binding, used as value *)
  test
    {|
    let apply (f : float -> float) (x : float) = f x
    let main (pos : vec2) = let scale = fun (y : float) -> y * pos.0 in
      let r = apply scale (pos.1) in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float anf_1 = pos[1];
        float anf_3 = pos[0];
        float r = (anf_1 * anf_3);
        fragColor = vec4(r, r, r, 1.);
    }
    |}];
  (* Three named functions of the same type (3-case switch) *)
  test
    {|
    let apply (f : float -> float) (x : float) = f x
    let double n = n * 2.0
    let triple n = n * 3.0
    let quadruple n = n * 4.0
    let main (pos : vec2) = let a = apply double pos.0 in
      let b = apply triple pos.1 in
      let c = apply quadruple pos.0 in
      [a, b, c, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float anf_0 = pos[0];
        float a = (anf_0 * 2.);
        float anf_2 = pos[1];
        float b = (anf_2 * 3.);
        float c = (anf_0 * 4.);
        fragColor = vec4(a, b, c, 1.);
    }
    |}];
  (* HOF with binary function type *)
  test
    {|
    let apply2 f x y = f x y
    let add a b = a + b
    let main (pos : vec2) = let r = apply2 add pos.0 pos.1 in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float anf_0 = pos[0];
        float anf_1 = pos[1];
        float r = (anf_0 + anf_1);
        fragColor = vec4(r, r, r, 1.);
    }
    |}];
  (* map! *)
  test
    {|
    let map f v = [f v.0, f v.1, f v.2]
    let main (uv : vec2) = let c = (map (fun x -> x * 2) [0, 1, 2]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(0., 2., 4., 1.);
    }
    |}]
;;

let%expect_test "defunctionalization - returning closures" =
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) = let f = addn 0. in
      let r = f 1. in
      [r, 0, 0, 1.0]
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
    let main (coord : vec2) = let addn (n : float) = fun (x : float) -> n + x in
      let f = addn 0. in
      let r = f 1. in
      [r, 0, 0, 1.0]
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
    let addn (n : float) = fun (x : float) -> n + x
    let main (coord : vec2) = let f = addn 1. in
      let g = f in
      let r = g 2. in
      [r, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(3., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "defunctionalization - partial application of first-class functions" =
  (* Simple test *)
  test
    {|
      let main (pos : vec2) = let add = fun (a : float) (b : float) -> a + b in
        let f = add in
        let g = f pos.0 in
        let r = g pos.1 in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float anf_0 = pos[0];
        float anf_2 = pos[1];
        float r = (anf_0 + anf_2);
        fragColor = vec4(r, r, r, 1.);
    }
    |}];
  (* Global function as value with multi-level partial application *)
  test
    {|
      let add3 (a : float) (b : float) (c : float) = a + b + c
      let main (pos : vec2) = let f : float -> float -> float -> float = add3 in
        let g = f 1.0 in
        let h = g 2.0 in
        let r = h pos.0 in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float anf_3 = pos[0];
        float r = (3. + anf_3);
        fragColor = vec4(r, r, r, 1.);
    }
    |}];
  (* Partial application of first-class function passed to HOF *)
  test
    {|
      let apply f x = f x
      let add (a : float) (b : float) = a + b
      let main (pos : vec2) = let add_as_value : float -> float -> float = add in
        let r = apply (add_as_value pos.0) pos.1 in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        float anf_0 = pos[0];
        float anf_2 = pos[1];
        float r = (anf_0 + anf_2);
        fragColor = vec4(r, r, r, 1.);
    }
    |}];
  test
    {|
    let mkinc n =
      let x = 1 in
      fun y -> x + y

    let main (uv : vec2) = let c = (let inc = mkinc 0 in
      inc 2 * [1, 1, 1]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(3., 3., 3., 1.);
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
    let main (u : vec2) = [f 0, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}];
  test
    {|
    type option['a] = Some of 'a | None
    type a = option[int]
    type b = a

    let f (n : a) : b = n
    let main (u : vec2) = [0, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "parametrized type aliases" =
  test
    {|
    type id['a] = 'a
    let f (n : id[int]) : id[int] = n
    let main (u : vec2) = [f 0, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}];
  test
    {|
    type either['a, 'b] = ('a, 'b)
    let f (p : either[int, float]) : float =
      let (_, y) = p in
      y
    let main (u : vec2) = [f (1, 2.0), 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(2., 0., 0., 1.);
    }
    |}];
  test
    {|
    type box['a] = 'a
    type boxed_int = box[int]
    let f (n : boxed_int) : box[int] = n
    let main (u : vec2) = [f 0, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "parametrized type aliases error cases" =
  test
    {|
    type id['a] = 'a
    let f (n : id) : int = n
    let main (u : vec2) = [f 0, 0, 0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:5-3:29: type alias requires type arguments
      name: id
      params: (a)
      |
    3 |     let f (n : id) : int = n
      |     ^^^^^^^^^^^^^^^^^^^^^^^^
    |}];
  test
    {|
    type id['a] = 'a
    let f (n : id[int, int]) : int = n
    let main (u : vec2) = [f 0, 0, 0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:5-3:39: wrong number of type args
      name: id
      |
    3 |     let f (n : id[int, int]) : int = n
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "toplevel complex consts / promotion to zero-arg functions" =
  test
    {|
    #extern float u_scale
    let scale = u_scale
    let pi = 3.14159
    let main (coord : vec2) = [pi, pi, pi, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(3.14159, 3.14159, 3.14159, 1.);
    }
    |}];
  test
    {|
    type v = { a : float }
    let base = 2 + 1.0
    let derived = { a = base * 2.0 }.a
    let main (coord : vec2) = [derived, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(6., 0., 0., 1.);
    }
    |}];
  test
    {|
    #extern bool u_flag
    let chosen = if u_flag then 1.0 else 0.0
    let main (coord : vec2) = [chosen, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform bool u_flag;
    float chosen() {
        if (u_flag) {
            return 1.;
        } else {
            return 0.;
        }
    }
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float _lc = chosen();
        fragColor = vec4(_lc, 0., 0., 1.);
    }
    |}]
;;

let%expect_test "ints in float contexts" =
  (* int literal passed directly as float argument *)
  test_term "let f (x : float) = x * 2.0 in [f 3, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(6., 0., 0., 1.);
    }
    |}];
  (* int literal in arithmetic with float - promotes left operand *)
  test_term "let x = 1 + 2.0 in [x, x, x]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(3., 3., 3., 1.);
    }
    |}];
  (* int variable passed to annotated float param *)
  test_term "let n = 4 in let f (x : float) = x + 1.0 in [f n, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(5., 0., 0., 1.);
    }
    |}];
  (* int literals in vec3 literal *)
  test_term "[1, 2, 3]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 2., 3., 1.);
    }
    |}];
  (* int literal broadcast-multiplied with float *)
  test_term "let v = [1.0, 2.0, 3.0] in [2 * v.0, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(2., 0., 0., 1.);
    }
    |}];
  (* int extern in float arithmetic *)
  test
    {|
    #extern int n
    let main (u : vec2) = [n + 1.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform int n;
    void main() {
        vec2 u = gl_FragCoord.xy;
        float anf = float(n);
        float anf_0 = (anf + 1.);
        fragColor = vec4(anf_0, 0., 0., 1.);
    }
    |}];
  (* int from if-expression used in float context *)
  test_term "let r = if true then 1 else 2 in [r, 0.0, 0.0]";
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
  (* int literal passed to builtin expecting float *)
  test_term "let s = #sin 0 in [s, 0.0, 0.0]";
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
  (* two int literals passed to two float params *)
  test_term "let f (x : float) (y : float) = x + y in [f 1 2, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(3., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "function keyword desugaring" =
  test
    {|
    type option['a] = Some of 'a | None

    let f = function
      | Some x -> x + 1.0
      | None -> 0.0

    let g = function | true -> 1.0 | false -> 0.0

    let apply_fn (f : 'a -> 'b) (x : 'a) : 'b = f x

    let main (u : vec2) = let h = apply_fn (function | true -> 1 | false -> 0) in
      [f (Some 5.0), g true, h true, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(6., 1., 1., 1.);
    }
    |}]
;;

let%expect_test "pipe operator" =
  test
    {|
    let f (x : float) : float = x + 1.0
    let g (x : float) : float = x * 2.0
    let main (u : vec2) = let n = 1.0 |> fun x -> x * 2.0 in
      [2.0 |> f |> g, n, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(6., 2., 0., 1.);
    }
    |}]
;;

let%expect_test "functions in records / structs" =
  test
    {|
    type fn_box = { fn : float -> float }

    let main (pos : vec2) = let b = { fn = fun x -> x * 2.0 } in
      let r = b.fn 3.0 in
      [r, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        fragColor = vec4(6., 0., 0., 1.);
    }
    |}];
  test
    {|
    type fn_box = { fn : float -> float }

    let apply (f : float -> float) (x : float) : float = f x

    let main (pos : vec2) = let b = { fn = fun x -> x * 3.0 } in
      let r = apply b.fn 4.0 in
      [r, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        fragColor = vec4(12., 0., 0., 1.);
    }
    |}];
  test
    {|
    type callback = CB of (float -> float) | NoCB

    let apply (f : float -> float) (x : float) : float = f x

    let main (pos : vec2) = let cb = CB (fun x -> x * 2.0) in
      let r = match cb with
        | CB f -> apply f 6.0
        | NoCB -> 0.0
      in
      [r, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        fragColor = vec4(12., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "curried builtins" =
  test
    {|
    let main (coord : vec2) = let m = #min in
      let a = m coord.0 0.5 in
      let cap = #min 0.5 in
      [a, cap coord.1, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float anf = coord[0];
        float a = min(anf, 0.5);
        float anf_1 = coord[1];
        float anf_2 = min(0.5, anf_1);
        fragColor = vec4(a, anf_2, 0., 1.);
    }
    |}];
  test
    {|
    let apply (f : float -> float) (x : float) = f x

    let main (coord : vec2) = [apply #sin coord.0, apply #cos coord.1, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float anf_0 = coord[0];
        float anf_1 = sin(anf_0);
        float anf_3 = coord[1];
        float anf_4 = cos(anf_3);
        fragColor = vec4(anf_1, anf_4, 0., 1.);
    }
    |}]
;;

let%expect_test "curried binary operators and pipe" =
  test
    {|
    let app = (|>)

    let main (coord : vec2) = let add = (+) in
      let double = (*) 2 in
      [add coord.0 1.0, app coord.1 #cos, double coord.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float anf_0 = coord[0];
        float anf_1 = (anf_0 + 1.);
        float anf_2 = coord[1];
        float anf_4 = cos(anf_2);
        float anf_7 = (2. * anf_0);
        fragColor = vec4(anf_1, anf_4, anf_7, 1.);
    }
    |}]
;;

let%expect_test "fragment derivative builtins" =
  test_term
    "let g = #dFdx coord in let h = #dFdy coord in [ #fwidth g.0, h.1, #dFdx g.1 ]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        vec2 g = dFdx(coord);
        vec2 h = dFdy(coord);
        float anf = g[0];
        float anf_0 = fwidth(anf);
        float anf_1 = h[1];
        float anf_2 = g[1];
        float anf_3 = dFdx(anf_2);
        fragColor = vec4(anf_0, anf_1, anf_3, 1.);
    }
    |}];
  (* derivative of constant is zero *)
  test_term "[ #dFdx 5.0, #dFdy 2.0, #fwidth 7.0 ]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}]
;;
