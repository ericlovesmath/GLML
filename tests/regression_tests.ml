open Runner

let%expect_test "int promotion edge cases" =
  (* int variable inferred *)
  test_term " let x = 5 in let y = x + 3.0 in [y, y, y]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(8., 8., 8., 1.);
    }
    |}];
  (* int variable in vec *)
  test_term "let n = 2 in [n, 0.0, 0.0]";
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
  (* int compared with float *)
  test
    {|
    #extern int n
    let main (u : vec2) = let c = (if n < 0.5 then [1.0, 0.0, 0.0] else [0.0, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
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
        bool anf_0 = (anf < 0.5);
        vec3 c;
        if (anf_0) {
            c = vec3(1., 0., 0.);
        } else {
            c = vec3(0., 0., 0.);
        }
        float anf_1 = c[0];
        float anf_2 = c[1];
        float anf_3 = c[2];
        fragColor = vec4(anf_1, anf_2, anf_3, 1.);
    }
    |}];
  (* int literal in struct with float field *)
  test
    {|
    type point = { x: float, y: float }
    let main (u : vec2) = let p = { x = 1, y = 2 } in
      [p.x, p.y, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(1., 2., 0., 1.);
    }
    |}];
  (* int variable in struct with float field *)
  test
    {|
    type point = { x: float, y: float }
    let main (u : vec2) = let a = 3 in
      let p = { x = a, y = 0.0 } in
      [p.x, p.y, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(3., 0., 0., 1.);
    }
    |}];
  (* parametrized struct where non-param field is float, value is int *)
  test
    {|
    type pair['a] = { fst: 'a, snd: float }
    let main (u : vec2) = let p = { fst = true, snd = 2 } in
      [p.snd, p.snd, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 u = gl_FragCoord.xy;
        fragColor = vec4(2., 2., 0., 1.);
    }
    |}];
  (* variant constructor float/int *)
  test
    {|
    type color = Gray of float | Black
    let main (u : vec2) = let c = (match Gray 1 with
      | Gray v -> [v, v, v]
      | Black -> [0.0, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
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

let%expect_test "regression - polymorphic struct type in function" =
  test
    {|
    type box['a] = { value: 'a }
    let f (b: box['a]) : 'a = b.value
    let main (coord: vec2) = [f { value = 1.0 }, 0.0, 0.0, 1.0]
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
    type box['a] = { value: 'a }
    let f b = b.value
    let main (coord: vec2) = let a = f { value = 1.0 } in
      let b = if f { value = true } then 1 else 2 in
      [a, b, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 1., 0., 1.);
    }
    |}]
;;

let%expect_test "regression - polymorphic variant type in function" =
  test
    {|
    type option['a] = Some of 'a | None

    let is_some o = match o with | Some _ -> true | None -> false

    let main (coord: vec2) = let b = if is_some (Some 1.0) then 1.0 else 0.0 in
      [b, 0.0, 0.0, 1.0]
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

    let main (coord : vec2) = let y = unwrap None None in
      [0, 0, 0, 1.0]
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
    |}]
;;

let%expect_test "field access in let binding (unannotated)" =
  (* Regression tests for over-generalization of let-bound variables *)
  test
    {|
    type box['a] = { value: 'a }
    let f b = let a = b.value in a
    let main (coord: vec2) = [f { value = 1.0 }, 0.0, 0.0, 1.0]
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
  (* Polymorphic usage *)
  test
    {|
    type box['a] = { value: 'a }
    let f b = let a = b.value in a
    let main (coord: vec2) = let x = f { value = 1.0 } in
      let y = if f { value = true } then 1 else 2 in
      [x, y, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 1., 0., 1.);
    }
    |}];
  (* [let x = b.value in x * 2.0 - x]'s type constrained to float through Broadcast *)
  test
    {|
    type box['a] = { value: 'a }
    let scale b = let x = b.value in x * 2.0
    let main (coord: vec2) = [scale { value = 1.0 }, 0.0, 0.0, 1.0]
    |};
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
  (* Same test with IndexAccess *)
  test
    {|
    let get_x v = let x = v.0 in x
    let main (coord: vec2) = [get_x coord, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float x_0 = coord[0];
        fragColor = vec4(x_0, 0., 0., 1.);
    }
    |}]
;;

let%expect_test "regression - placeholder structs and variants in tail position" =
  test
    {|
    type box['a] = { v: 'a }
    let main (coord: vec2) = let c = (let rec f x = if x then { v = [1, 1, 1] } else f true in
      (f false).v) in [c.0, c.1, c.2, 1.0]
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
        while (true) {
            bool _lim_cond = (_iter < 1000);
            if (_lim_cond) {
                if (x) {
                    vec3 anf = vec3(1., 1., 1.);
                    return box(anf);
                } else {
                    int _iter_inc = (_iter + 1);
                    _iter = _iter_inc;
                    x = true;
                    continue;
                }
            } else {
                vec3 _zero = vec3(0., 0., 0.);
                box _zero_0 = box(_zero);
                return _zero_0;
            }
        }
    }
    void main() {
        vec2 coord = gl_FragCoord.xy;
        box anf_0 = f_0(false);
        vec3 c = anf_0.v;
        float anf_1 = c[0];
        float anf_2 = c[1];
        float anf_3 = c[2];
        fragColor = vec4(anf_1, anf_2, anf_3, 1.);
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
    let main (coord : vec2) = let f = fun x -> [x, x, x] in
      let a = blend f in
      [0, 0, 0, 1.0]
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
    |}]
;;

let%expect_test "return type annotation for function-returning functions" =
  (* palette: 4 params, return type is (float -> vec3) *)
  test
    {|
    let palette (a : vec3) (b : vec3) (c : vec3) (d : vec3) : (float -> vec3) =
      fun t -> a + b * #cos (6.28318 * (c * t + d))
    let main (coord : vec2) = let c = (palette [0.,0.,0.] [0.,0.,0.] [0.,0.,0.] [0.,0.,0.] 0.) in [c.0, c.1, c.2, 1.0]
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
  (* simple: one param, return type is (float -> float) *)
  test
    {|
    let add (x : float) : (float -> float) = fun y -> x + y
    let main (coord : vec2) = let r = add 1. 2. in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(3., 3., 3., 1.);
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

    let main (coord : vec2) = let d = scene coord.0 in
      [d, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float anf = coord[0];
        float d = (0.5 + anf);
        fragColor = vec4(d, 0., 0., 1.);
    }
    |}]
;;

let%expect_test "toplevel vectors of ints are treated as consts with builtin #floats" =
  test
    {|
    let a = 10
    let x = [a, a, a]
    let main (coord : vec2) = let c = (x) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(10., 10., 10., 1.);
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

    let main (uv : vec2) = [inc 1, 1, 1, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(2., 1., 1., 1.);
    }
    |}];
  (* Called with float argument *)
  test
    {|
    let inc =
      let x = 1 in
      fun y -> x + y

    let main (uv : vec2) = [inc 1.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(2., 0., 0., 1.);
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

    let main (uv : vec2) = [scale (add_one 3.0), 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(8., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "regression - partial application stored as top level value" =
  test
    {|
    let palette (a : vec3) = fun t -> #cos (a * t)
    let warm = palette [0.5, 0.3, 0.1]
    let main (coord : vec2) = let a = (warm 2).0 in
      [a, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        vec3 anf_0_0 = vec3(1., 0.6, 0.2);
        vec3 anf_2 = cos(anf_0_0);
        float a_0 = anf_2[0];
        fragColor = vec4(a_0, 0., 0., 1.);
    }
    |}];
  test
    {|
    let add (x : float) (y : float) = x + y
    let add5 = add 5.0
    let main (coord : vec2) = let r = add5 3.0 in
      [r, r, r, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(8., 8., 8., 1.);
    }
    |}]
;;

let%expect_test "regression - int promotion through closures / partial application" =
  (* int literal passed to float param via partial application through closure *)
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) = let f = addn 0 in
      let r = f 1 in
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
  (* fully applied with int args - both promoted at call site *)
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) = let r = addn 0 1 in
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
  (* int variable captured in closure then passed to float param *)
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) = let n = 5 in
      let f = addn n in
      let r = f 1 in
      [r, 0, 0, 1.0]
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
    let main (uv : vec2) = let result = func 0 in
      [result.0, 0, 0, 1.0]
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
  (* Float variant: t inferred as float *)
  test
    {|
    let func t =
      let app_t f = f t in
      let x = app_t (fun t -> t + 1.0) in
      [x, x, x]
    let main (uv : vec2) = let c = (func 0.0) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(1., 1., 1., 1.);
    }
    |}]
;;

let%expect_test "closures in records / structs" =
  test
    {|
    type fn_box = { fn : float -> float }

    let add x y = x + y

    let box_add_n x = { fn = add x }

    let main (pos : vec2) = let boxed_add_five = box_add_n 5 in
      let n = boxed_add_five.fn 10 in
      [n, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        fragColor = vec4(15., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "function in variant, match-bound var used with int arg" =
  test
    {|
    type cb = | CB of (float -> float)

    let add x y = x + y

    let main (pos : vec2) = let f = CB (add 1.0) in
      let result = match f with
        | CB g -> g 10
      in
      [result, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 pos = gl_FragCoord.xy;
        fragColor = vec4(11., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "struct pattern matching on non-concrete types" =
  test
    {|
    type box['a] = { value : 'a }

    let unbox = function
      | { value = v } -> v

    let main (uv : vec2) = let n = unbox { value = 1.5 } in
      [n, 0, 0, 1.0]
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

    let main (uv : vec2) = let a = mandelbrot uv in
      [0, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
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
    let main (coord : vec2) = [0, 0, 0, 1.0]
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
    |}]
;;

let%expect_test "main type nomangle if type not concrete" =
  test "let main uv = [0, 0, 0, 1.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
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

    let main (uv : vec2) = [0, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}];
  test
    {|
    let pick (f : vec3 -> vec3) (p : vec3) : vec3 = f p

    let main (uv : vec2) = let c = (let a = pick (fun p -> [0, 0, 0]) [1.0, 0.0, 0.0] in
      let b = pick (fun p -> p) [0.0, 1.0, 0.0] in
      a + b) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(0., 1., 0., 1.);
    }
    |}];
  test
    {|
    let make (b : bool) : vec3 -> vec3 =
      if b then fun p -> [0, 0, 0] else fun p -> p

    let main (uv : vec2) = let c = (let f = make (uv.0 > 0.5) in
      f [1.0, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_0 {
        int tag;
    };
    void main() {
        vec2 uv = gl_FragCoord.xy;
        float anf = uv[0];
        bool anf_0 = (anf > 0.5);
        DFn_0 anf_1;
        if (anf_0) {
            anf_1 = DFn_0(0);
        } else {
            anf_1 = DFn_0(1);
        }
        vec3 anf_2 = vec3(1., 0., 0.);
        int _lv_tag_0 = anf_1.tag;
        vec3 c;
        switch (_lv_tag_0) {
            case 0: {
                c = vec3(0., 0., 0.);
                break;
            }
            default: {
                c = anf_2;
                break;
            }
        }
        float anf_3 = c[0];
        float anf_4 = c[1];
        float anf_5 = c[2];
        fragColor = vec4(anf_3, anf_4, anf_5, 1.);
    }
    |}]
;;

let%expect_test "promote ints through variant constructor coerce" =
  test
    {|
    type option['a] = Some of 'a | None
    let main uv = let x : option[float] = Some 5 in
      [0, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "unused HOF with fn-typed param emits empty DFn typedef" =
  test
    {|
    let add_noise (f : vec3 -> vec3) : vec3 -> vec3 =
      fun p -> [0, 0, 0]

    let main (coord : vec2) = [0, 0, 0, 1.0]
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
    |}]
;;

let%expect_test "HOF that applies param but is never called drops wrapper" =
  test
    {|
    let test (f : vec3 -> vec3) =
      fun p ->
        let n = f p in
        [0, 0, 0]

    let main uv = let c = (let f = test (fun a -> a) in
      f [0, 0, 0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "dot accepts mixed int/float vec args" =
  test
    {|
    let f : vec3 -> vec3 =
      fun p ->
        let n = #dot p [0, 0, 0] in
        [n, n, n]

    let main uv = [0, 0, 0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 uv = gl_FragCoord.xy;
        fragColor = vec4(0., 0., 0., 1.);
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
    let main (coord : vec2) = let c = (let ro = [0.0, 0.0, 0.0] in
      let mat = scene_mat ro in
      let col = eval_material mat in
      col) in [c.0, c.1, c.2, 1.0]
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
    let main (coord : vec2) = let c = (let nw = id waves in
      consume nw) in [c.0, c.1, c.2, 1.0]
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
    |}]
;;

let%expect_test "DFn promotion of fields in user-declared variant type" =
  (* Bug with function type in variant *)
  test
    {|
    #extern float u_pick

    type fn = vec3 -> vec3

    type material =
      | Lambert of fn
      | Phong of fn * float

    let make_waves (s : float) : fn = fun p -> [0.0, 0.0, 0.0]
    let add_noise (b : fn) : fn = fun p -> [0.0, 0.0, 0.0]

    let pick (cond : bool) : material =
      if cond then
        let w = make_waves 3.0 in
        let nw = add_noise w in
        Phong (nw, 64.0)
      else
        Lambert (make_waves 2.0)

    let eval_mat (m : material) (p : vec3) : vec3 =
      match m with
      | Lambert a -> a p
      | Phong (a, s) -> a p * s

    let main (coord : vec2) = let c = (let m = pick (u_pick > 0.5) in
      eval_mat m [0.0, 0.0, 0.0]) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_1 {
        int tag;
        float lctor_2_0;
        float lctor_4_0;
    };
    struct DFn_0 {
        int tag;
        float lctor_2_0;
        DFn_1 lctor_3_0;
        float lctor_4_0;
    };
    vec3 dapply_0(DFn_0 dfn_0, vec3 da_0) {
        int _lv_tag = dfn_0.tag;
        switch (_lv_tag) {
            case 0: {
                return vec3(0., 0., 0.);
                break;
            }
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                return vec3(0., 0., 0.);
                break;
            }
        }
    }
    struct material {
        int tag;
        DFn_0 Lambert_0;
        DFn_0 Phong_0;
        float Phong_1;
    };
    uniform float u_pick;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        bool anf_8 = (u_pick > 0.5);
        material m_0;
        if (anf_8) {
            DFn_1 anf_0_0 = DFn_1(0, 0., 0.);
            DFn_0 anf_1_0 = DFn_0(0, 0., anf_0_0, 0.);
            DFn_1 anf_2_0 = DFn_1(0, 3., 0.);
            DFn_0 anf_3_0 = DFn_0(1, 0., anf_2_0, 0.);
            m_0 = material(1, anf_1_0, anf_3_0, 64.);
        } else {
            DFn_1 anf_4_0 = DFn_1(0, 0., 0.);
            DFn_0 anf_5_0 = DFn_0(2, 0., anf_4_0, 2.);
            DFn_0 anf_7_0 = DFn_0(0, 0., anf_4_0, 0.);
            m_0 = material(0, anf_5_0, anf_7_0, 0.);
        }
        vec3 anf_9 = vec3(0., 0., 0.);
        int _lv_tag_0_0 = m_0.tag;
        vec3 c;
        switch (_lv_tag_0_0) {
            case 0: {
                DFn_0 _lv_Lambert_0_0 = m_0.Lambert_0;
                c = dapply_0(_lv_Lambert_0_0, anf_9);
                break;
            }
            default: {
                DFn_0 _lv_Phong_0_0 = m_0.Phong_0;
                float _lv_Phong_1_0 = m_0.Phong_1;
                vec3 anf_13 = dapply_0(_lv_Phong_0_0, anf_9);
                c = (anf_13 * _lv_Phong_1_0);
                break;
            }
        }
        float anf_10 = c[0];
        float anf_11 = c[1];
        float anf_12 = c[2];
        fragColor = vec4(anf_10, anf_11, anf_12, 1.);
    }
    |}]
;;

let%expect_test "placeholder is wrong type" =
  (* I was too lazy to minimize this more sorry *)
  test
    {|
    type option['a] = Some of 'a | None
    type hit = { t : float, n : vec3, m : vec3 }
    let ro = [0, 0, 0]
    let sphere_t (r : float) : option[float] =
      let b = #dot ro ro in
      if b < 0 then None
      else if b > 0.1 then Some b
      else if b > 0.01 then Some b
      else None
    let closer (best : option[hit]) : option[hit] =
      match sphere_t 1.0 with
      | None -> best
      | Some t -> best
    let trace (n : int) : option[hit] = closer None
    let main (coord : vec2) = let c = (match trace 1 with
      | None -> ro
      | Some h -> ro) in [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct hit {
        float t;
        vec3 n;
        vec3 m;
    };
    struct option {
        int tag;
        float Some_0;
    };
    struct option_0 {
        int tag;
        hit Some_0;
    };
    const vec3 ro = vec3(0., 0., 0.);
    void main() {
        vec2 coord = gl_FragCoord.xy;
        vec3 anf_2_0 = vec3(0., 0., 0.);
        hit anf_4_0 = hit(0., anf_2_0, anf_2_0);
        option_0 anf_5_0 = option_0(1, anf_4_0);
        float b_2 = dot(ro, ro);
        bool anf_11 = (b_2 < 0.);
        option _lv_scrut_2;
        if (anf_11) {
            _lv_scrut_2 = option(1, 0.);
        } else {
            bool anf_0_2 = (b_2 > 0.1);
            if (anf_0_2) {
                _lv_scrut_2 = option(0, b_2);
            } else {
                bool anf_1_2 = (b_2 > 0.01);
                if (anf_1_2) {
                    _lv_scrut_2 = option(0, b_2);
                } else {
                    _lv_scrut_2 = option(1, 0.);
                }
            }
        }
        int _lv_tag_2 = _lv_scrut_2.tag;
        option_0 _lv_scrut_0;
        switch (_lv_tag_2) {
            case 1: {
                _lv_scrut_0 = anf_5_0;
                break;
            }
            default: {
                _lv_scrut_0 = anf_5_0;
                break;
            }
        }
        int _lv_tag_0 = _lv_scrut_0.tag;
        vec3 c;
        switch (_lv_tag_0) {
            case 1: {
                c = ro;
                break;
            }
            default: {
                c = ro;
                break;
            }
        }
        float anf_6 = c[0];
        float anf_7 = c[1];
        float anf_8 = c[2];
        fragColor = vec4(anf_6, anf_7, anf_8, 1.);
    }
    |}]
;;
