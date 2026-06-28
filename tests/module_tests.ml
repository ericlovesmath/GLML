open Runner

let%expect_test "qualified access and open" =
  test
    {|
    module Vec = struct
      let len (p : vec2) : float = #sqrt (#dot p p)
      let scale (p : vec2) (k : float) : vec2 = [p.0 * k, p.1 * k]
    end

    open Vec

    let main (coord : vec2) =
      let l = len (scale coord 2.0) in
      let q = Vec.len coord in
      [l, q, l, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float anf_0_0 = coord[0];
        float anf_1_0 = (anf_0_0 * 2.);
        float anf_2_0 = coord[1];
        float anf_3_0 = (anf_2_0 * 2.);
        vec2 anf_4 = vec2(anf_1_0, anf_3_0);
        float anf_6 = dot(anf_4, anf_4);
        float l = sqrt(anf_6);
        float anf_5 = dot(coord, coord);
        float q = sqrt(anf_5);
        fragColor = vec4(l, q, l, 1.);
    }
    |}]
;;

let%expect_test "qualified function used as HoF" =
  test
    {|
    module M = struct
      let double (x : float) : float = x * 2.0
    end

    let apply (f : float -> float) (x : float) : float = f x

    let main (coord : vec2) = [apply M.double 3.0, 0.0, 0.0, 1.0]
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

let%expect_test "module member does not capture a top-level binding" =
  test
    {|
    let k = 10.0

    module M = struct
      let k = 2.0
      let scaled (x : float) : float = x * k
    end

    let main (coord : vec2) = [k, M.k, M.scaled 1.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(10., 2., 2., 1.);
    }
    |}]
;;

let%expect_test "a local binding shadows an opened name" =
  test
    {|
    module M = struct
      let v = 1.0
    end

    open M

    let main (coord : vec2) =
      let v = 9.0 in
      [v, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(9., 0., 0., 1.);
    }
    |}]
;;

let%expect_test "main may not be defined inside a module" =
  test
    {|
    module M = struct
      let main (c : vec2) = [c.0, c.1, 0.0, 1.0]
    end
    let main (c : vec2) = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 3:7-3:49: main may not be defined inside a module
      |
    3 |       let main (c : vec2) = [c.0, c.1, 0.0, 1.0]
      |       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "module error tests" =
  test
    {|
    module M = struct let x = 1.0 end
    let main (c : vec2) = let _ = N.x in [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 3:35-3:38: unknown module
      m: N
      |
    3 |     let main (c : vec2) = let _ = N.x in [0.0, 0.0, 0.0, 1.0]
      |                                   ^^^
    |}];
  test
    {|
    module M = struct let x = 1.0 end
    let main (c : vec2) = let _ = M.y in [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 3:35-3:38: unknown module mem
      m: M
      x: y
      |
    3 |     let main (c : vec2) = let _ = M.y in [0.0, 0.0, 0.0, 1.0]
      |                                   ^^^
    |}];
  test
    {|
    module M = struct
      let x = 1.0
      let x = 2.0
    end
    let main (c : vec2) = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 2:5-5:8: duplicate module member
      mname: M
      dup: x
      |
    2 |     module M = struct
    3 |       let x = 1.0
    4 |       let x = 2.0
    5 |     end
      |
    |}];
  test
    {|
    module M = struct let x = 1.0 end
    module M = struct let y = 2.0 end
    let main (c : vec2) = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 3:5-3:38: duplicate module
      mname: M
      |
    3 |     module M = struct let y = 2.0 end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "type member / qualified type" =
  test
    {|
    module M = struct
      type t = vec4
      type u = t
      let mk (x : float) : u = [x, x, x, 1.0]
    end
    let main (coord : vec2) : M.t = M.mk 1.0
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(1., 1., 1., 1.);
    }
    |}]
;;

let%expect_test "module type alias" =
  test
    {|
    type color = { r : float, g : float, b : float }
    module M = struct
      type c = color
      let mk (x : float) : c = { r = x, g = x, b = x }
    end
    let main (coord : vec2) : vec4 =
      let c : M.c = M.mk 0.5 in
      [c.r, c.g, c.b, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0.5, 0.5, 0.5, 1.);
    }
    |}]
;;

let%expect_test "qualified type member name shadowing" =
  test
    ~dump:[ Uniquify ]
    {|
    type t = float
    module M = struct
      type t = vec4
      let mk (x : float) : t = [x, x, x, 1.0]
    end
    let main (coord : vec2) : M.t =
      let a : t = 0.5 in
      M.mk a
    |};
  [%expect
    {|
    ===== uniquify =====
    (Program
     ((TypeDef t (AliasDecl float)) (TypeDef M_t (AliasDecl (vec 4 float)))
      (Define Nonrec mk (: (float -> M_t)) (lambda (x (float)) (vec4 x x x 1.)))
      (Define Nonrec main (: ((vec 2 float) -> M_t))
       (lambda (coord ((vec 2 float))) (let a (: t) 0.5 (app mk a))))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0.5, 0.5, 0.5, 1.);
    }
    |}]
;;

let%expect_test "unknown qualified type member" =
  test
    {|
    module M = struct type t = vec4 end
    let main (coord : vec2) : M.cinna = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 3:5-3:61: unknown module type
      m: M
      tn: cinna
      |
    3 |     let main (coord : vec2) : M.cinna = [0.0, 0.0, 0.0, 1.0]
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;
