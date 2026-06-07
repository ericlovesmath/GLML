open Core

let dump = [ Glml_compiler.Passes.Lower_tuples ]
let test = Runner.test ~dump
let test_term = Runner.test_term ~dump

let%expect_test "basic tuple construction and destructure" =
  test
    {|
    let main (uv : vec2) =
      let p = (1.0, 2.0) in
      let (x, y) = p in
      [x, y, 0.0]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 float) (_1 float)))) : tuple)
      ((Define Nonrec main
        ((lambda uv
          ((let p ((record (1.) (2.)))
            ((match (p) ((record (_0 x) (_1 y)) ((vec3 (x) (y) (0.)))))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 2., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "nested tuple destructure" =
  test
    {|
    let main (uv : vec2) =
      let p = ((1.0, 2.0), 3.0) in
      let ((a, b), c) = p in
      [a, b, c]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 float) (_1 float)))) : tuple)
      ((TypeDef tuple_0 (RecordDecl ((_0 tuple) (_1 float)))) : tuple_0)
      ((Define Nonrec main
        ((lambda uv
          ((let p ((record ((record (1.) (2.))) (3.)))
            ((match (p)
              ((record (_0 (record (_0 a) (_1 b))) (_1 c)) ((vec3 (a) (b) (c)))))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 2., 3.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "function returning a tuple" =
  test
    {|
    let swap (p : (float, int)) =
      let (a, b) = p in
      (b, a)
    let main (uv : vec2) =
      let (x, y) = swap (1.0, 2) in
      [y, 0.0, 0.0]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 float) (_1 int)))) : tuple)
      ((TypeDef tuple_0 (RecordDecl ((_0 int) (_1 float)))) : tuple_0)
      ((Define Nonrec swap
        ((lambda p ((match (p) ((record (_0 a) (_1 b)) ((record (b) (a)))))))))
       : (tuple -> tuple_0))
      ((Define Nonrec main
        ((lambda uv
          ((match ((app (swap) ((record (1.) (2)))))
            ((record (_0 x) (_1 y)) ((vec3 (y) (0.) (0.)))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 0., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "tuple of vectors" =
  test
    {|
    let main (uv : vec2) =
      let p = ([1.0, 0.0, 0.0], [0.0, 1.0, 0.0]) in
      let (a, b) = p in
      a + b
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 (vec 3 float)) (_1 (vec 3 float))))) :
       tuple)
      ((Define Nonrec main
        ((lambda uv
          ((let p ((record ((vec3 (1.) (0.) (0.))) ((vec3 (0.) (1.) (0.)))))
            ((match (p) ((record (_0 a) (_1 b)) ((+ (a) (b)))))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 1., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "wildcard inside tuple pattern" =
  test
    {|
    let main (uv : vec2) =
      let (_, y, _) = (1.0, 2.0, 3.0) in
      [y, y, y]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 float) (_1 float) (_2 float)))) : tuple)
      ((Define Nonrec main
        ((lambda uv
          ((match ((record (1.) (2.) (3.)))
            ((record (_0 _) (_1 y) (_2 _)) ((vec3 (y) (y) (y)))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(2., 2., 2.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "tuple of int promoted to float in vec" =
  test
    {|
    let main (uv : vec2) =
      let (a, b, c) = (1, 2, 3) in
      [a, b, c]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 int) (_1 int) (_2 int)))) : tuple)
      ((Define Nonrec main
        ((lambda uv
          ((match ((record (1) (2) (3)))
            ((record (_0 a) (_1 b) (_2 c))
             ((vec3 ((float (a))) ((float (b))) ((float (c)))))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 2., 3.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "multiple tuples in one file" =
  test
    {|
    let main (uv : vec2) =
      let (a, b) = (1.0, 2.0) in
      let (c, d, e) = (3.0, 4.0, 5.0) in
      [a + c, b + d, e]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 float) (_1 float)))) : tuple)
      ((TypeDef tuple_0 (RecordDecl ((_0 float) (_1 float) (_2 float)))) :
       tuple_0)
      ((Define Nonrec main
        ((lambda uv
          ((match ((record (1.) (2.)))
            ((record (_0 a) (_1 b))
             ((match ((record (3.) (4.) (5.)))
               ((record (_0 c) (_1 d) (_2 e))
                ((vec3 ((+ (a) (c))) ((+ (b) (d))) (e))))))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(4., 6., 5.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "multiple of same tuple type in one file" =
  test
    {|
    let main (uv : vec2) =
      let (a, b) = (1.0, 2.0) in
      let (c, d) = (3.0, 4.0) in
      [a + c, b + d, 0.0]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 float) (_1 float)))) : tuple)
      ((Define Nonrec main
        ((lambda uv
          ((match ((record (1.) (2.)))
            ((record (_0 a) (_1 b))
             ((match ((record (3.) (4.)))
               ((record (_0 c) (_1 d)) ((vec3 ((+ (a) (c))) ((+ (b) (d))) (0.))))))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(4., 6., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "tuple in record field" =
  test
    {|
    type pair_box = { values : (float, float) }
    let main (uv : vec2) =
      let b = { values = (1.0, 2.0) } in
      let (x, y) = b.values in
      [x, y, 0.0]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 float) (_1 float)))) : tuple)
      ((TypeDef pair_box (RecordDecl ((values tuple)))) : pair_box)
      ((Define Nonrec main
        ((lambda uv
          ((let b ((record ((record (1.) (2.)))))
            ((match ((. (b) values))
              ((record (_0 x) (_1 y)) ((vec3 (x) (y) (0.)))))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 uv) {
        return vec3(1., 2., 0.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "type errors" =
  test
    {|
    let main (uv : vec2) =
      let (a, b) = (1.0, 2.0, 3.0) in
      [a, b, 0.0]
    |};
  [%expect
    {|
    [constraint solver] at 3:7-4:18: type mismatch
      ty: (tuple float float float)
      ty': (tuple 'v_0 'v_1)
      |
    3 |       let (a, b) = (1.0, 2.0, 3.0) in
    4 |       [a, b, 0.0]
      |
  |}];
  test
    {|
    let main (uv : vec2) =
      let (a, b) = (1.0, true) in
      [a, b, 0.0]
    |};
  [%expect
    {|
    [constraint solver] at 4:7-4:18: type mismatch
      ty: bool
      ty': float
      |
    4 |       [a, b, 0.0]
      |       ^^^^^^^^^^^
    |}]
;;

let%expect_test "validate 1 tuple doesn't exist" =
  test_term "let (x) = 1.0 in [x, x, x]";
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((Define Nonrec main ((lambda coord ((let x (1.) ((vec3 (x) (x) (x))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 main_pure(vec2 coord) {
        return vec3(1., 1., 1.);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;

let%expect_test "no clash with records with named fields like _0/_1" =
  test
    {|
    #extern float u
    type holder = { _0 : float, pair : (float, float) }
    let main (uv : vec2) =
      let h = { _0 = u, pair = (u + 1.0, u + 2.0) } in
      let (a, b) = h.pair in
      [h._0, a, b]
    |};
  [%expect
    {|
    ===== lower tuples =====
    (Program
     (((TypeDef tuple (RecordDecl ((_0 float) (_1 float)))) : tuple)
      ((TypeDef holder (RecordDecl ((_0 float) (pair tuple)))) : holder)
      ((Extern u) : float)
      ((Define Nonrec main
        ((lambda uv
          ((let h ((record (u) ((record ((+ (u) (1.))) ((+ (u) (2.)))))))
            ((match ((. (h) pair))
              ((record (_0 a) (_1 b)) ((vec3 ((. (h) _0)) (a) (b)))))))))))
       : ((vec 2 float) -> (vec 3 float)))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform float u;
    vec3 main_pure(vec2 uv) {
        float anf = (u + 1.);
        float anf_0 = (u + 2.);
        return vec3(u, anf, anf_0);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = vec4(color.xyz, 1.);
    }
    |}]
;;
