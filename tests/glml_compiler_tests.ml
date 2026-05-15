open Runner

let%expect_test "simple tests for compile_stlc" =
  test_term "let x = 2.0 in [ 12.0 * x + 10.0, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        float x = 2.;
        float anf = (12. * x);
        float anf_0 = (anf + 10.);
        return vec3(anf_0, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "if true && false then [ 1.0, 0.0, 0.0 ] else [ 0.0, 0.0, 0.0 ]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        bool anf = (true && false);
        if (anf) {
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
    #extern float n
    let f = fun (x : float) -> x + n
    let main = fun (u : vec2) -> [f 10.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float n;
    float f(float x) {
        return (x + n);
    }
    vec3 main_pure(vec2 u) {
        float anf = f(10.);
        return vec3(anf, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    #extern float n
    let f (x : float) = x + n
    let main (u : vec2) = [ f 10.0, 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float n;
    float f(float x) {
        return (x + n);
    }
    vec3 main_pure(vec2 u) {
        float anf = f(10.);
        return vec3(anf, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "generic vectors and matrices" =
  test
    {|
    let main (u : vec2) =
      let m = [ [1.0, 0.0, 0.0], [ 0.0, 1.0, 0.0 ], [ 0.0, 0.0, 1.0] ] in
      let m = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]] in
      let v = [ 1.0, 2.0 ] in
      [ 1.0, 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 u) {
        vec3 anf = vec3(1., 0., 0.);
        vec3 anf_0 = vec3(0., 1., 0.);
        vec3 anf_1 = vec3(0., 0., 1.);
        mat3 m = mat3(anf, anf_0, anf_1);
        vec2 anf_2 = vec2(1., 2.);
        vec2 anf_3 = vec2(3., 4.);
        vec2 anf_4 = vec2(5., 6.);
        mat3x2 m_0 = mat3x2(anf_2, anf_3, anf_4);
        vec2 v = vec2(1., 2.);
        return vec3(1., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
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
    vec3 main_pure(vec2 coord) {
        vec3 v = vec3(1., 2., 3.);
        float anf = v[0];
        return vec3(anf, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
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
    vec3 main_pure(vec2 coord) {
        vec3 anf = vec3(1., 0., 0.);
        vec3 anf_0 = vec3(0., 1., 0.);
        vec3 anf_1 = vec3(0., 0., 1.);
        mat3 m = mat3(anf, anf_0, anf_1);
        vec3 c = m[0];
        float anf_2 = c[0];
        float anf_3 = c[1];
        float anf_4 = c[2];
        return vec3(anf_2, anf_3, anf_4);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "[0.0, 0.0, 0.0].4";
  [%expect
    {|
    [constraint solver] at 1:27-1:44: vec index out of bounds
      n: 3
      i: 4
      |
    1 | let main (coord : vec2) = [0.0, 0.0, 0.0].4
      |                           ^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "builtins" =
  test_term "let v = [ 1.0, 2.0, 3.0 ] in [ #sin(1.0), #dot(v, v), #length(v) ]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        vec3 v = vec3(1., 2., 3.);
        float anf = sin(1.);
        float anf_0 = dot(v, v);
        float anf_1 = length(v);
        return vec3(anf, anf_0, anf_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "#cross([1.0, 2.0, 3.0], [0.0, 2.0, 5.0])";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        vec3 anf = vec3(1., 2., 3.);
        vec3 anf_0 = vec3(0., 2., 5.);
        return cross(anf, anf_0);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test_term "#cross([ 1.0, 1.0 ], [ 0.0, 0.0 ])";
  [%expect
    {|
    [constraint solver] at 1:27-1:61: type mismatch
      ty: (vec 2 'v)
      ty': (vec 3 float)
      |
    1 | let main (coord : vec2) = #cross([ 1.0, 1.0 ], [ 0.0, 0.0 ])
      |                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "multi argument functions / lambdas" =
  test
    {|
    let f (x : float) (y : float) = x + y
    let g = fun (x : float) (y : float) -> x - y
    let main (u : vec2) = [ f 10.0 5.0, g 0.0 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float f(float x, float y) {
        return (x + y);
    }
    float g(float x_0, float y_0) {
        return (x_0 - y_0);
    }
    vec3 main_pure(vec2 u) {
        float anf = f(10., 5.);
        float anf_0 = g(0., 0.);
        return vec3(anf, anf_0, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "lambda lifting" =
  test
    {|
    let main (u : vec2) =
      let x = 10.0 in
      let y = 5.0 in
      let add (z : float) = x + y + z in
      [ add 1.0, 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float add_0(float x, float y, float z) {
        float anf = (x + y);
        return (anf + z);
    }
    vec3 main_pure(vec2 u) {
        float x = 10.;
        float y = 5.;
        float anf_0 = add_0(x, y, 1.);
        return vec3(anf_0, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let main (u : vec2) =
      let f (x : float) =
        let g (y : float) = x + y in
        ([ g 1.0, 0.0, 0.0 ])
      in
      f 10.0
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float g_0(float x, float y) {
        return (x + y);
    }
    vec3 f_0(float x) {
        float anf = g_0(x, 1.);
        return vec3(anf, 0., 0.);
    }
    vec3 main_pure(vec2 u) {
        return f_0(10.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
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
    [typecheck] at 2:5-4:8: main must have type vec2 -> vec3
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
    let main (u : vec2) =
      [ apply_f (fun x -> x + 1) 10.0, 0.0, 0.0 ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    float lam(float x_0) {
        return (x_0 + 1.);
    }
    float dapply(DFn dfn, float da) {
        return lam(da);
    }
    float apply_f(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 u) {
        DFn anf = DFn(0);
        float anf_0 = apply_f(anf, 10.);
        return vec3(anf_0, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "recursive functions" =
  test
    {|
    let rec fact (n : int) (acc : int) : int =
      if n = 0 then acc else fact (n - 1) (acc * n)

    let main (u : vec2) =
      let num = fact 5 1 in
      [ 0., 0., 0. ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    int fact(int n, int acc) {
        int _iter = 0;
        while ((_iter < 1000)) {
            bool anf = (n == 0);
            if (anf) {
                return acc;
            } else {
                int anf_0 = (n - 1);
                int anf_1 = (acc * n);
                n = anf_0;
                acc = anf_1;
                int _iter_inc = (_iter + 1);
                _iter = _iter_inc;
                continue;
            }
        }
        return 0;
    }
    vec3 main_pure(vec2 u) {
        int num = fact(5, 1);
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
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

    let main (u: vec2) =
      let p = { x = 1.0, y = 2.0 } in
      let c = make_red p in
      [c.r, c.g, c.b]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct color {
        float r;
        float g;
        float b;
    };
    struct point {
        float x;
        float y;
    };
    color make_red(point p) {
        float p_y = p.y;
        return color(p_y, 0., 0.);
    }
    vec3 main_pure(vec2 u) {
        point p_0 = point(1., 2.);
        color c = make_red(p_0);
        float anf = c.r;
        float anf_0 = c.g;
        float anf_1 = c.b;
        return vec3(anf, anf_0, anf_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
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

    let main (u: vec2) =
      let p = { x = 1.0, y = 2.0 } in
      let c = make_red p in
      [c.r, c.g, c.b]
  |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct color {
        float r;
        float g;
        float b;
    };
    struct point {
        float x;
        float y;
    };
    color make_red(point p) {
        color col;
        if (true) {
            col = color(1., 0., 0.);
        } else {
            col = color(0., 0., 1.);
        }
        return col;
    }
    vec3 main_pure(vec2 u) {
        point p_0 = point(1., 2.);
        color c = make_red(p_0);
        float anf = c.r;
        float anf_0 = c.g;
        float anf_1 = c.b;
        return vec3(anf, anf_0, anf_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type point = { x: float, y: float }

    let main (u: vec2) =
      let p = { x = 1.0, z = 2.0 } in
      [p.x, p.x, p.x]
    |};
  [%expect
    {|
    [typecheck] at 5:15-5:35: record does not match any known struct
      provided_fields: (x z)
      |
    5 |       let p = { x = 1.0, z = 2.0 } in
      |               ^^^^^^^^^^^^^^^^^^^^
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

    let main (u: vec2) =
      let seg = make_seg 1.0 in
      let c = seg.end.x in
      [c, c, c]
    |}
  in
  test test_program;
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct point {
        float x;
        float y;
    };
    struct segment {
        point start;
        point end;
    };
    segment make_seg(float u) {
        segment s;
        if (true) {
            point anf = point(0., 0.);
            point anf_0 = point(1., 1.);
            s = segment(anf, anf_0);
        } else {
            point anf_1 = point(1., 1.);
            point anf_2 = point(0., 0.);
            s = segment(anf_1, anf_2);
        }
        return s;
    }
    vec3 main_pure(vec2 u_0) {
        segment seg = make_seg(1.);
        point anf_3 = seg.end;
        float c = anf_3.x;
        return vec3(c, c, c);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "monomorphization tests" =
  test
    {|
    let id x = x
    let main (coord : vec2) =
      let a = id 1.0 in
      let b = id true in
      if b then [a, 0.0, 0.0] else [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    bool id_m(bool x) {
        return x;
    }
    float id_m_0(float x) {
        return x;
    }
    vec3 main_pure(vec2 coord) {
        float a = id_m_0(1.);
        bool b = id_m(true);
        if (b) {
            return vec3(a, 0., 0.);
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
    let main (coord : vec2) =
      let id x = x in
      let a = id 1.0 in
      let b = id true in
      if b then [a, 0.0, 0.0] else [0.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float id_m_0_0(float x) {
        return x;
    }
    bool id_m_1(bool x) {
        return x;
    }
    vec3 main_pure(vec2 coord) {
        float a = id_m_0_0(1.);
        bool b = id_m_1(true);
        if (b) {
            return vec3(a, 0., 0.);
        } else {
            return vec3(0., 0., 0.);
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Unused polymorphic function *)
  test
    {|
    let id x = x
    let main (coord : vec2) = [1.0, 0.0, 0.0]
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
  (* Validate no duplication of polymorphic function *)
  test
    {|
    let main (coord : vec2) =
      let id x = x in
      let a = id 1.0 in
      let b = id 2.0 in
      [a, b, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float id_m_0(float x) {
        return x;
    }
    vec3 main_pure(vec2 coord) {
        float a = id_m_0(1.);
        float b = id_m_0(2.);
        return vec3(a, b, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "advanced monomorphization example" =
  test
    {|
    let id x = x
    let const x y = x
    let main (coord : vec2) =
      let a = id 1.0 in
      let b = const 2.0 true in
      [a, b, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float const_0_m(float x_0, bool y) {
        return x_0;
    }
    float id_m(float x) {
        return x;
    }
    vec3 main_pure(vec2 coord) {
        float a = id_m(1.);
        float b = const_0_m(2., true);
        return vec3(a, b, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "toplevel constant (atomic only)" =
  test
    {|
    let pi = 3.14159

    let main (u : vec2) = [pi, pi, pi]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const float pi = 3.14159;
    vec3 main_pure(vec2 u) {
        return vec3(pi, pi, pi);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let x = #sin(1.0) + #cos(2.0)

    let main (u : vec2) = [x, x, x]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const float x = (sin(1.) + cos(2.));
    vec3 main_pure(vec2 u) {
        return vec3(x, x, x);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "promotion of ints to floats" =
  test
    {|
    let main (u : vec2) =
      let b = 1 + 2 in
      let a = b + 2. in
      [b, a, 3]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 u) {
        int b = (1 + 2);
        float anf = float(b);
        float a = (anf + 2.);
        float anf_0 = float(b);
        return vec3(anf_0, a, 3.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "defunctionalization" =
  (* Named function reference as higher-order argument *)
  test
    {|
    let apply f x = f x
    let double n = n * 2.0
    let main (pos : vec2) =
      let r = apply double pos.0 in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    float double_0_m(float n) {
        return (n * 2.);
    }
    float dapply(DFn dfn, float da) {
        return double_0_m(da);
    }
    float apply_m(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf = DFn(0);
        float anf_0 = pos[0];
        float r = apply_m(anf, anf_0);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Lambda in argument position *)
  test
    {|
    let apply f x = f x
    let main (pos : vec2) =
      let r = apply (fun y -> y + 1.0) (pos.0) in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    float lam(float y) {
        return (y + 1.);
    }
    float dapply(DFn dfn, float da) {
        return lam(da);
    }
    float apply_m(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf = DFn(0);
        float anf_0 = pos[0];
        float r = apply_m(anf, anf_0);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Lambda capturing a free variable (closure) *)
  test
    {|
    let apply f x = f x
    let main (pos : vec2) =
      let px = pos.0 in
      let r = apply (fun y -> px + y) pos.1 in
      [ r, r, r ]
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
    float lam(float px, float y) {
        return (px + y);
    }
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        float ca = _lv_lctor_0;
        return lam(ca, da);
    }
    float apply_m(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 pos) {
        float px = pos[0];
        DFn anf = DFn(0, px);
        float anf_0 = pos[1];
        float r = apply_m(anf, anf_0);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Lambda stored in let binding, used as value *)
  test
    {|
    let apply (f : float -> float) (x : float) = f x
    let main (pos : vec2) =
      let scale = fun (y : float) -> y * pos.0 in
      let r = apply scale (pos.1) in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
        vec2 lctor_0;
    };
    float scale_0(vec2 pos, float y) {
        float anf = pos[0];
        return (y * anf);
    }
    float dapply(DFn dfn, float da) {
        vec2 _lv_lctor_0 = dfn.lctor_0;
        vec2 ca = _lv_lctor_0;
        return scale_0(ca, da);
    }
    float apply(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf_0 = DFn(0, pos);
        float anf_1 = pos[1];
        float r = apply(anf_0, anf_1);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Three named functions of the same type (3-case switch) *)
  test
    {|
    let apply (f : float -> float) (x : float) = f x
    let double n = n * 2.0
    let triple n = n * 3.0
    let quadruple n = n * 4.0
    let main (pos : vec2) =
      let a = apply double pos.0 in
      let b = apply triple pos.1 in
      let c = apply quadruple pos.0 in
      [ a, b, c ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    float double_0_m(float n) {
        return (n * 2.);
    }
    float quadruple_m(float n_1) {
        return (n_1 * 4.);
    }
    float triple_m(float n_0) {
        return (n_0 * 3.);
    }
    float dapply(DFn dfn, float da) {
        int _lv_tag = dfn.tag;
        switch (_lv_tag) {
            case 0: {
                return double_0_m(da);
                break;
            }
            case 1: {
                return triple_m(da);
                break;
            }
            default: {
                return quadruple_m(da);
                break;
            }
        }
    }
    float apply(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf = DFn(0);
        float anf_0 = pos[0];
        float a = apply(anf, anf_0);
        DFn anf_1 = DFn(1);
        float anf_2 = pos[1];
        float b = apply(anf_1, anf_2);
        DFn anf_3 = DFn(2);
        float anf_4 = pos[0];
        float c = apply(anf_3, anf_4);
        return vec3(a, b, c);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* HOF with binary function type *)
  test
    {|
    let apply2 f x y = f x y
    let add a b = a + b
    let main (pos : vec2) =
      let r = apply2 add pos.0 pos.1 in
      [ r, r, r ]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    float add_m(float a, float b) {
        return (a + b);
    }
    float dapply(DFn dfn, float da, float da_0) {
        return add_m(da, da_0);
    }
    float apply2_m(DFn f, float x, float y) {
        return dapply(f, x, y);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf = DFn(0);
        float anf_0 = pos[0];
        float anf_1 = pos[1];
        float r = apply2_m(anf, anf_0, anf_1);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* map! *)
  test
    {|
    let map f v = [f v.0, f v.1, f v.2]
    let main (uv : vec2) =
      let color = map (fun x -> x * 2) [0, 1, 2] in
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
    float lam(float x) {
        return (x * 2.);
    }
    float dapply(DFn dfn, float da) {
        return lam(da);
    }
    vec3 map_m(DFn f, vec3 v) {
        float anf = v[0];
        float anf_0 = dapply(f, anf);
        float anf_1 = v[1];
        float anf_2 = dapply(f, anf_1);
        float anf_3 = v[2];
        float anf_4 = dapply(f, anf_3);
        return vec3(anf_0, anf_2, anf_4);
    }
    vec3 main_pure(vec2 uv) {
        DFn anf_5 = DFn(0);
        vec3 anf_6 = vec3(0., 1., 2.);
        vec3 color = map_m(anf_5, anf_6);
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "defunctionalization - returning closures" =
  test
    {|
    let add (x : float) (y : float) = x + y
    let addn (n : float) = fun (x : float) -> add n x
    let main (coord : vec2) =
      let f = addn 0. in
      let r = f 1. in
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
        float ca = _lv_lctor_0;
        return addn(ca, da);
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
  test
    {|
    let main (coord : vec2) =
      let addn (n : float) = fun (x : float) -> n + x in
      let f = addn 0. in
      let r = f 1. in
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
    float addn_0(float n, float x) {
        return (n + x);
    }
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        float ca = _lv_lctor_0;
        return addn_0(ca, da);
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
  test
    {|
    let addn (n : float) = fun (x : float) -> n + x
    let main (coord : vec2) =
      let f = addn 1. in
      let g = f in
      let r = g 2. in
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
    float addn(float n, float x) {
        return (n + x);
    }
    float dapply(DFn dfn, float da) {
        float _lv_lctor_0 = dfn.lctor_0;
        float ca = _lv_lctor_0;
        return addn(ca, da);
    }
    vec3 main_pure(vec2 coord) {
        DFn f = DFn(0, 1.);
        DFn g = f;
        float r = dapply(g, 2.);
        return vec3(r, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "defunctionalization - partial application of first-class functions" =
  (* Simple test *)
  test
    {|
      let main (pos : vec2) =
        let add = fun (a : float) (b : float) -> a + b in
        let f = add in
        let g = f pos.0 in
        let r = g pos.1 in
        [r, r, r]
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
        float lctor_1;
    };
    float add_0(float a, float b) {
        return (a + b);
    }
    float dapply(DFn dfn, float da, float da_0) {
        return add_0(da, da_0);
    }
    float dapply_0(DFn_0 dfn_0, float da_1) {
        DFn _lv_lctor_0 = dfn_0.lctor_0;
        float _lv_lctor_1 = dfn_0.lctor_1;
        float ca_0 = _lv_lctor_1;
        DFn ca = _lv_lctor_0;
        return dapply(ca, ca_0, da_1);
    }
    vec3 main_pure(vec2 pos) {
        DFn f = DFn(0);
        float anf = pos[0];
        DFn_0 g = DFn_0(0, f, anf);
        float anf_0 = pos[1];
        float r = dapply_0(g, anf_0);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Global function as value with multi-level partial application *)
  test
    {|
      let add3 (a : float) (b : float) (c : float) = a + b + c
      let main (pos : vec2) =
        let f : float -> float -> float -> float = add3 in
        let g = f 1.0 in
        let h = g 2.0 in
        let r = h pos.0 in
        [r, r, r]
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
        float lctor_1;
    };
    struct DFn_1 {
        int tag;
        DFn_0 lctor_0_0;
        float lctor_0_1;
    };
    float add3(float a, float b, float c) {
        float anf = (a + b);
        return (anf + c);
    }
    float dapply(DFn dfn, float da, float da_0, float da_1) {
        return add3(da, da_0, da_1);
    }
    float dapply_0(DFn_0 dfn_0, float da_2, float da_3) {
        DFn _lv_lctor_0 = dfn_0.lctor_0;
        float _lv_lctor_1 = dfn_0.lctor_1;
        float ca_0 = _lv_lctor_1;
        DFn ca = _lv_lctor_0;
        return dapply(ca, ca_0, da_2, da_3);
    }
    float dapply_1(DFn_1 dfn_1, float da_4) {
        DFn_0 _lv_lctor_0_0 = dfn_1.lctor_0_0;
        float _lv_lctor_0_1 = dfn_1.lctor_0_1;
        float ca_2 = _lv_lctor_0_1;
        DFn_0 ca_1 = _lv_lctor_0_0;
        return dapply_0(ca_1, ca_2, da_4);
    }
    vec3 main_pure(vec2 pos) {
        DFn f = DFn(0);
        DFn_0 g = DFn_0(0, f, 1.);
        DFn_1 h = DFn_1(0, g, 2.);
        float anf_0 = pos[0];
        float r = dapply_1(h, anf_0);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* Partial application of first-class function passed to HOF *)
  test
    {|
      let apply f x = f x
      let add (a : float) (b : float) = a + b
      let main (pos : vec2) =
        let add_as_value : float -> float -> float = add in
        let r = apply (add_as_value pos.0) pos.1 in
        [r, r, r]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn_0 {
        int tag;
    };
    struct DFn {
        int tag;
        DFn_0 lctor_0;
        float lctor_1;
    };
    float add(float a, float b) {
        return (a + b);
    }
    float dapply_0(DFn_0 dfn_0, float da_0, float da_1) {
        return add(da_0, da_1);
    }
    float dapply(DFn dfn, float da) {
        DFn_0 _lv_lctor_0 = dfn.lctor_0;
        float _lv_lctor_1 = dfn.lctor_1;
        float ca_0 = _lv_lctor_1;
        DFn_0 ca = _lv_lctor_0;
        return dapply_0(ca, ca_0, da);
    }
    float apply_m(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 pos) {
        DFn_0 add_as_value = DFn_0(0);
        float anf = pos[0];
        DFn anf_0 = DFn(0, add_as_value, anf);
        float anf_1 = pos[1];
        float r = apply_m(anf_0, anf_1);
        return vec3(r, r, r);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    let mkinc n =
      let x = 1 in
      fun y -> x + y

    let main (uv : vec2) =
      let inc = mkinc 0 in
      inc 2 * [1, 1, 1]
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
    int lam(int x, int y) {
        return (x + y);
    }
    int dapply(DFn dfn, int da) {
        int _lv_lctor_0 = dfn.lctor_0;
        int ca = _lv_lctor_0;
        return lam(ca, da);
    }
    DFn mkinc_m(int n) {
        int x = 1;
        return DFn(0, x);
    }
    vec3 main_pure(vec2 uv) {
        DFn inc_m = mkinc_m(0);
        int anf = dapply(inc_m, 2);
        float anf_0 = float(anf);
        vec3 anf_1 = vec3(1., 1., 1.);
        return (anf_0 * anf_1);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
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
    let main (u : vec2) = [f 0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    int f(int n) {
        return n;
    }
    vec3 main_pure(vec2 u) {
        int anf = f(0);
        float anf_0 = float(anf);
        return vec3(anf_0, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type option['a] = Some of 'a | None
    type a = option[int]
    type b = a

    let f (n : a) : b = n
    let main (u : vec2) = [0, 0, 0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 u) {
        return vec3(0., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "toplevel complex consts / promotion to zero-arg functions" =
  test
    {|
    #extern float u_scale
    let scale = u_scale
    let pi = 3.14159
    let main (coord : vec2) = [pi, pi, pi]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const float pi = 3.14159;
    vec3 main_pure(vec2 coord) {
        return vec3(pi, pi, pi);
    }
    uniform float u_scale;
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type v = { a : float }
    let base = 2 + 1.0
    let derived = { a = base * 2.0 }.a
    let main (coord : vec2) = [derived, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const float base = (2. + 1.);
    struct v_4 {
        float a;
    };
    const float derived = v_4((base * 2.)).a;
    vec3 main_pure(vec2 coord) {
        return vec3(derived, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    #extern bool u_flag
    let chosen = if u_flag then 1.0 else 0.0
    let main (coord : vec2) = [chosen, 0.0, 0.0]
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
    vec3 main_pure(vec2 coord) {
        float _lc = chosen();
        return vec3(_lc, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
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
    float f_0(float x) {
        return (x * 2.);
    }
    vec3 main_pure(vec2 coord) {
        float anf = f_0(3.);
        return vec3(anf, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int literal in arithmetic with float - promotes left operand *)
  test_term "let x = 1 + 2.0 in [x, x, x]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        float x = (1. + 2.);
        return vec3(x, x, x);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int variable passed to annotated float param *)
  test_term "let n = 4 in let f (x : float) = x + 1.0 in [f n, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float f_0(float x) {
        return (x + 1.);
    }
    vec3 main_pure(vec2 coord) {
        int n = 4;
        float anf = float(n);
        float anf_0 = f_0(anf);
        return vec3(anf_0, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int literals in vec3 literal *)
  test_term "[1, 2, 3]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        return vec3(1., 2., 3.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int literal broadcast-multiplied with float *)
  test_term "let v = [1.0, 2.0, 3.0] in [2 * v.0, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        vec3 v = vec3(1., 2., 3.);
        float anf = v[0];
        float anf_0 = (2. * anf);
        return vec3(anf_0, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int extern in float arithmetic *)
  test
    {|
    #extern int n
    let main (u : vec2) = [n + 1.0, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform int n;
    vec3 main_pure(vec2 u) {
        float anf = float(n);
        float anf_0 = (anf + 1.);
        return vec3(anf_0, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int from if-expression used in float context *)
  test_term "let r = if true then 1 else 2 in [r, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        int r;
        if (true) {
            r = 1;
        } else {
            r = 2;
        }
        float anf = float(r);
        return vec3(anf, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* int literal passed to builtin expecting float *)
  test_term "let s = #sin(0) in [s, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        float s = sin(0.);
        return vec3(s, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  (* two int literals passed to two float params *)
  test_term "let f (x : float) (y : float) = x + y in [f 1 2, 0.0, 0.0]";
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float f_0(float x, float y) {
        return (x + y);
    }
    vec3 main_pure(vec2 coord) {
        float anf = f_0(1., 2.);
        return vec3(anf, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
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

    let main (u : vec2) = 
      let h = apply_fn (function | true -> 1 | false -> 0) in
      [f (Some 5.0), g true, h true]
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
    float g(bool _fn_arg_0) {
        if (_fn_arg_0) {
            return 1.;
        } else {
            return 0.;
        }
    }
    int lam(bool _fn_arg_1) {
        if (_fn_arg_1) {
            return 1;
        } else {
            return 0;
        }
    }
    int dapply(DFn dfn, bool da) {
        return lam(da);
    }
    int apply_fn_m(DFn f_0, bool x_0) {
        return dapply(f_0, x_0);
    }
    int dapply_0(DFn_0 dfn_0, bool da_0) {
        DFn _lv_lctor_0 = dfn_0.lctor_0;
        DFn ca = _lv_lctor_0;
        return apply_fn_m(ca, da_0);
    }
    struct option {
        int tag;
        float Some_0;
    };
    float f(option _fn_arg) {
        int _lv_tag = _fn_arg.tag;
        switch (_lv_tag) {
            case 0: {
                float _lv_Some_0 = _fn_arg.Some_0;
                float x = _lv_Some_0;
                return (x + 1.);
                break;
            }
            default: {
                return 0.;
                break;
            }
        }
    }
    vec3 main_pure(vec2 u) {
        DFn anf = DFn(0);
        DFn_0 h = DFn_0(0, anf);
        option anf_0 = option(0, 5.);
        float anf_1 = f(anf_0);
        float anf_2 = g(true);
        int anf_3 = dapply_0(h, true);
        float anf_4 = float(anf_3);
        return vec3(anf_1, anf_2, anf_4);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "pipe operator" =
  test
    {|
    let f (x : float) : float = x + 1.0
    let g (x : float) : float = x * 2.0
    let main (u : vec2) =
      let n = 1.0 |> fun x -> x * 2.0 in
      [2.0 |> f |> g, n, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float f(float x) {
        return (x + 1.);
    }
    float g(float x_0) {
        return (x_0 * 2.);
    }
    float lam(float x_1) {
        return (x_1 * 2.);
    }
    vec3 main_pure(vec2 u) {
        float n = lam(1.);
        float anf = f(2.);
        float anf_0 = g(anf);
        return vec3(anf_0, n, 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;

let%expect_test "functions in records / structs" =
  test
    {|
    type fn_box = { fn : float -> float }

    let main (pos : vec2) : vec3 =
      let b = { fn = fun x -> x * 2.0 } in
      let r = b.fn 3.0 in
      [r, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    struct fn_box {
        DFn fn;
    };
    float lam(float x) {
        return (x * 2.);
    }
    float dapply(DFn dfn, float da) {
        return lam(da);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf = DFn(0);
        fn_box b = fn_box(anf);
        DFn anf_0 = b.fn;
        float r = dapply(anf_0, 3.);
        return vec3(r, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type fn_box = { fn : float -> float }

    let apply (f : float -> float) (x : float) : float = f x

    let main (pos : vec2) : vec3 =
      let b = { fn = fun x -> x * 3.0 } in
      let r = apply b.fn 4.0 in
      [r, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    struct fn_box {
        DFn fn;
    };
    float lam(float x_0) {
        return (x_0 * 3.);
    }
    float dapply(DFn dfn, float da) {
        return lam(da);
    }
    float apply(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf = DFn(0);
        fn_box b = fn_box(anf);
        DFn anf_0 = b.fn;
        float r = apply(anf_0, 4.);
        return vec3(r, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}];
  test
    {|
    type callback = CB of (float -> float) | NoCB

    let apply (f : float -> float) (x : float) : float = f x

    let main (pos : vec2) : vec3 =
      let cb = CB (fun x -> x * 2.0) in
      let r = match cb with
        | CB f -> apply f 6.0
        | NoCB -> 0.0
      in
      [r, 0.0, 0.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct DFn {
        int tag;
    };
    struct callback {
        int tag;
        DFn CB_0;
    };
    float lam(float x_0) {
        return (x_0 * 2.);
    }
    float dapply(DFn dfn, float da) {
        return lam(da);
    }
    float apply(DFn f, float x) {
        return dapply(f, x);
    }
    vec3 main_pure(vec2 pos) {
        DFn anf = DFn(0);
        callback cb = callback(0, anf);
        int _lv_tag = cb.tag;
        float r;
        switch (_lv_tag) {
            case 0: {
                DFn _lv_CB_0 = cb.CB_0;
                DFn f_0 = _lv_CB_0;
                r = apply(f_0, 6.);
                break;
            }
            default: {
                r = 0.;
                break;
            }
        }
        return vec3(r, 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
