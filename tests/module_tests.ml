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
     ((TypeDef t (AliasDecl float))
      (Module M (TypeDef t (AliasDecl (vec 4 float)))
       (Define Nonrec mk (: (float -> t)) (lambda (x (float)) (vec4 x x x 1.))))
      (Define Nonrec main (: ((vec 2 float) -> M.t))
       (lambda (coord ((vec 2 float))) (let a (: t) 0.5 (app M.mk a))))))

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0.5, 0.5, 0.5, 1.);
    }
    |}]
;;

let%expect_test "transparent ascription" =
  test
    {|
    module type COLOR = sig
      type t = vec3
      val mk : float -> float -> float -> t
    end

    module Color : COLOR = struct
      type t = vec3
      let secret (x : float) : float = x * 2.0
      let mk (r : float) (g : float) (b : float) : t = [secret r, g, b]
    end

    let main (coord : vec2) : vec4 =
      let c : Color.t = Color.mk 0.25 0.5 0.75 in
      [c.0, c.1, c.2, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0.5, 0.5, 0.75, 1.);
    }
    |}]
;;

let%expect_test "ascription hides members and types" =
  test
    {|
    module type S = sig val pub : float -> float end
    module M : S = struct
      let priv (x : float) : float = x + 1.0
      let pub (x : float) : float = priv x
    end
    let main (coord : vec2) : vec4 = [M.priv 0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 7:39-7:45: unknown module mem
      m: M
      x: priv
      |
    7 |     let main (coord : vec2) : vec4 = [M.priv 0.0, 0.0, 0.0, 1.0]
      |                                       ^^^^^^
  |}];
  test
    {|
    module type S = sig val mk : float -> vec3 end
    module M : S = struct
      type secret_t = vec3
      let mk (x : float) : secret_t = [x, x, x]
    end
    let main (coord : vec2) : vec4 =
      let z : M.secret_t = M.mk 0.0 in
      [z.0, z.1, z.2, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 8:7-9:27: unknown module type
      m: M
      tn: secret_t
      |
    8 |       let z : M.secret_t = M.mk 0.0 in
    9 |       [z.0, z.1, z.2, 1.0]
      |
  |}];
  test
    {|
    module M : sig val pub : float -> float end = struct
      let priv (x : float) : float = x + 1.0
      let pub (x : float) : float = priv x
    end
    open M
    let main (coord : vec2) : vec4 = [pub 0.0, 0.0, 0.0, 1.0]
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

let%expect_test "polymorphic spec matched by polymorphic impl" =
  test
    {|
    module type S = sig
      val id : 'a -> 'a
      val f : float -> float
    end
    module M : S = struct
      let id x = x
      let f x = x
    end
    let main (coord : vec2) : vec4 = [M.id 0.5, M.f 0.25, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(0.5, 0.25, 0., 1.);
    }
    |}]
;;

let%expect_test "signature mismatch errors" =
  (* missing member *)
  test
    {|
    module type S = sig val f : float -> float end
    module M : S = struct let g (x : float) : float = x end
    let main (coord : vec2) : vec4 = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:5-3:60: module does not implement val
      mname: M
      x: f
      |
    3 |     module M : S = struct let g (x : float) : float = x end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}];
  (* wrong-typed member *)
  test
    {|
    module type S = sig val f : float -> vec3 end
    module M : S = struct let f (x : float) : float = x end
    let main (coord : vec2) : vec4 = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:5-3:60: signature mismatch: val has wrong type
      mname: M
      x: f
      |
    3 |     module M : S = struct let f (x : float) : float = x end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}];
  (* Over-general spec *)
  test
    {|
    module type S = sig val f : 'a -> 'a -> 'a end
    module M : S = struct let f (a : float) (b : float) : float = a + b end
    let main (coord : vec2) : vec4 = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:5-3:76: signature mismatch: val has wrong type
      mname: M
      x: f
      |
    3 |     module M : S = struct let f (a : float) (b : float) : float = a + b end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}];
  (* Unconstrainable spec *)
  test
    {|
    module type S = sig val f : 'a -> 'a end
    module M : S = struct let f x = x + x end
    let main (coord : vec2) : vec4 = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:5-3:46: signature mismatch: val has wrong type
      mname: M
      x: f
      |
    3 |     module M : S = struct let f x = x + x end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}];
  (* Manifest type mismatch *)
  test
    {|
    module type S = sig type t = vec4 end
    module M : S = struct type t = vec3 end
    let main (coord : vec2) : vec4 = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [typecheck] at 3:5-3:44: signature mismatch: manifest type
      mname: M
      t: t
      |
    3 |     module M : S = struct type t = vec3 end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "manifest type equality success" =
  test
    {|
    module type S = sig
      type t = vec3
      val zero : t
    end
    module M : S = struct
      type t = vec3
      let zero : t = [0.0, 0.0, 0.0]
    end
    let main (coord : vec2) : vec4 =
      let z : M.t = M.zero in
      [z.0, z.1, z.2, 1.0]
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

let%expect_test "unknown signature name" =
  test
    {|
    module M : Nonexistent = struct let f (x : float) : float = x end
    let main (coord : vec2) : vec4 = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 2:5-2:70: unknown signature
      sname: Nonexistent
      |
    2 |     module M : Nonexistent = struct let f (x : float) : float = x end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "duplicate signature component is rejected" =
  test
    {|
    module type S = sig val f : float -> float  val f : float -> float end
    module M : S = struct let f (x : float) : float = x end
    let main (coord : vec2) : vec4 = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 2:5-2:75: duplicate signature val
      dup: f
      |
    2 |     module type S = sig val f : float -> float  val f : float -> float end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}];
  test
    {|
    module type S = sig type t = vec3  type t = vec4 end
    module M : S = struct type t = vec3 end
    let main (coord : vec2) : vec4 = [0.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    [uniquify] at 2:5-2:57: duplicate signature type
      dup: t
      |
    2 |     module type S = sig type t = vec3  type t = vec4 end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "recursive module member" =
  test
    {|
    module M = struct
      let rec count (n : float) : float = if n < 1.0 then 0.0 else count (n - 1.0)
      let rec loop n = if n < 1.0 then 0.0 else loop (n - 1.0)
    end
    let main (coord : vec2) : vec4 = [M.count 3.0, M.loop 2.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float count(float n) {
        for (int i = 0; (i < 1000); i = (i + 1)) {
            bool anf = (n < 1.);
            if (anf) {
                return 0.;
            } else {
                float anf_0 = (n - 1.);
                n = anf_0;
                continue;
            }
        }
        return 0.;
    }
    float loop_m(float n_0) {
        for (int i_0 = 0; (i_0 < 1000); i_0 = (i_0 + 1)) {
            bool anf_1 = (n_0 < 1.);
            if (anf_1) {
                return 0.;
            } else {
                float anf_2 = (n_0 - 1.);
                n_0 = anf_2;
                continue;
            }
        }
        return 0.;
    }
    void main() {
        vec2 coord = gl_FragCoord.xy;
        float anf_3 = count(3.);
        float anf_4 = loop_m(2.);
        fragColor = vec4(anf_3, anf_4, 0., 1.);
    }
    |}]
;;

let%expect_test "cross-module test" =
  test
    {|
    module M = struct let two (x : float) : float = x * 2.0 end
    module N = struct let four (x : float) : float = M.two (M.two x) end
    let main (coord : vec2) : vec4 = [N.four 1.0, 0.0, 0.0, 1.0]
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec4 fragColor;
    void main() {
        vec2 coord = gl_FragCoord.xy;
        fragColor = vec4(4., 0., 0., 1.);
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
