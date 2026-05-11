open Runner

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
