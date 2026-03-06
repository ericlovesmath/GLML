open Core
open Glml_compiler

let examples_dir = "../examples"

let%expect_test "compile examples" =
  let glml_files = Stdlib.Sys.readdir examples_dir in
  Array.iter glml_files ~f:(fun file ->
    let content = In_channel.read_all (Filename.concat examples_dir file) in
    Printf.printf "====== COMPILING EXAMPLE %s ======\n\n" file;
    let dump =
      let handler pass sexp =
        let pass = Passes.to_string pass in
        let data = Sexp.to_string_hum sexp in
        Printf.printf "=== %s (%s) ===\n%s\n\n" pass file data
      in
      Passes.all
      |> List.map ~f:(fun pass -> pass, handler pass)
      |> Passes.Map.of_alist_exn
    in
    match compile ~dump content with
    | Ok _ -> ()
    | Error err -> print_s [%message "ERROR" (file : string) (err : Error.t)]);
  [%expect
    {|
    ====== COMPILING EXAMPLE checkerboard.glml ======

    === stlc (checkerboard.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv
       (lambda (coord (vec 2))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec main
       (lambda (coord (vec 2))
        (let uv (app get_uv coord)
         (let size 5.
          (let cx (floor (+ (* (index uv 0) size) (* u_time 2.)))
           (let cy (floor (* (index uv 1) size))
            (let checker_sum (+ cx cy)
             (let is_even (- checker_sum (* (floor (/ checker_sum 2.)) 2.))
              (if (< is_even 0.5) (vec3 0.2 0.2 0.2) (vec3 0.8 0.8 0.8))))))))))))

    === uniquify (checkerboard.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv_3
       (lambda (coord_0 (vec 2))
        (let top_1 (- (* 2. coord_0) u_resolution)
         (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_1 bot_2)))))
      (Define Nonrec main
       (lambda (coord_4 (vec 2))
        (let uv_5 (app get_uv_3 coord_4)
         (let size_6 5.
          (let cx_7 (floor (+ (* (index uv_5 0) size_6) (* u_time 2.)))
           (let cy_8 (floor (* (index uv_5 1) size_6))
            (let checker_sum_9 (+ cx_7 cy_8)
             (let is_even_10
              (- checker_sum_9 (* (floor (/ checker_sum_9 2.)) 2.))
              (if (< is_even_10 0.5) (vec3 0.2 0.2 0.2) (vec3 0.8 0.8 0.8))))))))))))

    === typecheck (checkerboard.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_3
        ((lambda (coord_0 (vec 2))
          ((let top_1
            ((- ((* (2. : float) (coord_0 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_2
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_1 : (vec 2)) (bot_2 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        ((lambda (coord_4 (vec 2))
          ((let uv_5
            ((app (get_uv_3 : ((vec 2) -> (vec 2))) (coord_4 : (vec 2))) :
             (vec 2))
            ((let size_6 (5. : float)
              ((let cx_7
                ((floor
                  ((+
                    ((* ((index (uv_5 : (vec 2)) 0) : float) (size_6 : float)) :
                     float)
                    ((* (u_time : float) (2. : float)) : float))
                   : float))
                 : float)
                ((let cy_8
                  ((floor
                    ((* ((index (uv_5 : (vec 2)) 1) : float) (size_6 : float)) :
                     float))
                   : float)
                  ((let checker_sum_9 ((+ (cx_7 : float) (cy_8 : float)) : float)
                    ((let is_even_10
                      ((- (checker_sum_9 : float)
                        ((*
                          ((floor
                            ((/ (checker_sum_9 : float) (2. : float)) : float))
                           : float)
                          (2. : float))
                         : float))
                       : float)
                      ((if ((< (is_even_10 : float) (0.5 : float)) : bool)
                        ((vec3 (0.2 : float) (0.2 : float) (0.2 : float)) :
                         (vec 3))
                        ((vec3 (0.8 : float) (0.8 : float) (0.8 : float)) :
                         (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (checkerboard.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_3
        (lambda ((coord_0 (vec 2)))
         (let top_1 (- (* 2. coord_0) u_resolution)
          (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_1 bot_2)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_3 coord_4)
          (let size_6 5.
           (let cx_7 (floor (+ (* (index uv_5 0) size_6) (* u_time 2.)))
            (let cy_8 (floor (* (index uv_5 1) size_6))
             (let checker_sum_9 (+ cx_7 cy_8)
              (let is_even_10
               (- checker_sum_9 (* (floor (/ checker_sum_9 2.)) 2.))
               (if (< is_even_10 0.5) (vec3 0.2 0.2 0.2) (vec3 0.8 0.8 0.8))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let top_1 (- (* 2. coord_0) u_resolution)
         (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_1 bot_2)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_3 coord_4)
         (let size_6 5.
          (let cx_7 (floor (+ (* (index uv_5 0) size_6) (* u_time 2.)))
           (let cy_8 (floor (* (index uv_5 1) size_6))
            (let checker_sum_9 (+ cx_7 cy_8)
             (let is_even_10
              (- checker_sum_9 (* (floor (/ checker_sum_9 2.)) 2.))
              (if (< is_even_10 0.5) (vec3 0.2 0.2 0.2) (vec3 0.8 0.8 0.8))))))))))
      : ((vec 2) -> (vec 3))))

    === anf (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let anf_11 (* 2. coord_0)
         (let top_1 (- anf_11 u_resolution)
          (let anf_12 (index u_resolution 0)
           (let anf_13 (index u_resolution 1)
            (let bot_2 (min anf_12 anf_13) (return (/ top_1 bot_2)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_3 coord_4)
         (let size_6 5.
          (let anf_14 (index uv_5 0)
           (let anf_15 (* anf_14 size_6)
            (let anf_16 (* u_time 2.)
             (let anf_17 (+ anf_15 anf_16)
              (let cx_7 (floor anf_17)
               (let anf_18 (index uv_5 1)
                (let anf_19 (* anf_18 size_6)
                 (let cy_8 (floor anf_19)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_20 (/ checker_sum_9 2.)
                    (let anf_21 (floor anf_20)
                     (let anf_22 (* anf_21 2.)
                      (let is_even_10 (- checker_sum_9 anf_22)
                       (let anf_23 (< is_even_10 0.5)
                        (return
                         (if anf_23 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let anf_11 (* 2. coord_0)
         (let top_1 (- anf_11 u_resolution)
          (let anf_12 (index u_resolution 0)
           (let anf_13 (index u_resolution 1)
            (let bot_2 (min anf_12 anf_13) (return (/ top_1 bot_2)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_3 coord_4)
         (let size_6 5.
          (let anf_14 (index uv_5 0)
           (let anf_15 (* anf_14 size_6)
            (let anf_16 (* u_time 2.)
             (let anf_17 (+ anf_15 anf_16)
              (let cx_7 (floor anf_17)
               (let anf_18 (index uv_5 1)
                (let anf_19 (* anf_18 size_6)
                 (let cy_8 (floor anf_19)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_20 (/ checker_sum_9 2.)
                    (let anf_21 (floor anf_20)
                     (let anf_22 (* anf_21 2.)
                      (let is_even_10 (- checker_sum_9 anf_22)
                       (let anf_23 (< is_even_10 0.5)
                        (return
                         (if anf_23 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (checkerboard.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name get_uv_3) (desc ()) (params (((TyVec 2) coord_0)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_11 (* 2. coord_0))
         (set () vec2 top_1 (- anf_11 u_resolution))
         (set () float anf_12 (index u_resolution 0))
         (set () float anf_13 (index u_resolution 1))
         (set () float bot_2 (min anf_12 anf_13)) (return (/ top_1 bot_2)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_3 coord_4)) (set () float size_6 5.)
         (set () float anf_14 (index uv_5 0))
         (set () float anf_15 (* anf_14 size_6))
         (set () float anf_16 (* u_time 2.))
         (set () float anf_17 (+ anf_15 anf_16))
         (set () float cx_7 (floor anf_17)) (set () float anf_18 (index uv_5 1))
         (set () float anf_19 (* anf_18 size_6))
         (set () float cy_8 (floor anf_19))
         (set () float checker_sum_9 (+ cx_7 cy_8))
         (set () float anf_20 (/ checker_sum_9 2.))
         (set () float anf_21 (floor anf_20)) (set () float anf_22 (* anf_21 2.))
         (set () float is_even_10 (- checker_sum_9 anf_22))
         (set () bool anf_23 (< is_even_10 0.5))
         (if anf_23 (Block (return (vec3 0.2 0.2 0.2)))
          (Block (return (vec3 0.8 0.8 0.8)))))))))

    === patch main (checkerboard.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_3) (desc ()) (params (((TyVec 2) coord_0)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_11 (* 2. coord_0))
         (set () vec2 top_1 (- anf_11 u_resolution))
         (set () float anf_12 (index u_resolution 0))
         (set () float anf_13 (index u_resolution 1))
         (set () float bot_2 (min anf_12 anf_13)) (return (/ top_1 bot_2)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_3 coord_4)) (set () float size_6 5.)
         (set () float anf_14 (index uv_5 0))
         (set () float anf_15 (* anf_14 size_6))
         (set () float anf_16 (* u_time 2.))
         (set () float anf_17 (+ anf_15 anf_16))
         (set () float cx_7 (floor anf_17)) (set () float anf_18 (index uv_5 1))
         (set () float anf_19 (* anf_18 size_6))
         (set () float cy_8 (floor anf_19))
         (set () float checker_sum_9 (+ cx_7 cy_8))
         (set () float anf_20 (/ checker_sum_9 2.))
         (set () float anf_21 (floor anf_20)) (set () float anf_22 (* anf_21 2.))
         (set () float is_even_10 (- checker_sum_9 anf_22))
         (set () bool anf_23 (< is_even_10 0.5))
         (if anf_23 (Block (return (vec3 0.2 0.2 0.2)))
          (Block (return (vec3 0.8 0.8 0.8)))))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE mouse_circle.glml ======

    === stlc (mouse_circle.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern (vec 2) u_mouse)
      (Extern float u_time)
      (Define Nonrec get_uv
       (lambda (coord (vec 2))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec main
       (lambda (coord (vec 2))
        (let uv (app get_uv coord)
         (let mouseUV (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
          (let radius (+ (* (sin (* u_time 2.)) 0.1) 0.15)
           (if (< (distance uv mouseUV) radius) (vec3 0. 0. 0.5)
            (vec3 0.5 0.5 1.)))))))))

    === uniquify (mouse_circle.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern (vec 2) u_mouse)
      (Extern float u_time)
      (Define Nonrec get_uv_3
       (lambda (coord_0 (vec 2))
        (let top_1 (- (* 2. coord_0) u_resolution)
         (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_1 bot_2)))))
      (Define Nonrec main
       (lambda (coord_4 (vec 2))
        (let uv_5 (app get_uv_3 coord_4)
         (let mouseUV_6
          (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
          (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
           (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
            (vec3 0.5 0.5 1.)))))))))

    === typecheck (mouse_circle.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
      ((Extern u_time) : float)
      ((Define Nonrec get_uv_3
        ((lambda (coord_0 (vec 2))
          ((let top_1
            ((- ((* (2. : float) (coord_0 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_2
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_1 : (vec 2)) (bot_2 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        ((lambda (coord_4 (vec 2))
          ((let uv_5
            ((app (get_uv_3 : ((vec 2) -> (vec 2))) (coord_4 : (vec 2))) :
             (vec 2))
            ((let mouseUV_6
              ((/
                ((- ((* (2. : float) (u_mouse : (vec 2))) : (vec 2))
                  (u_resolution : (vec 2)))
                 : (vec 2))
                ((index (u_resolution : (vec 2)) 1) : float))
               : (vec 2))
              ((let radius_7
                ((+
                  ((* ((sin ((* (u_time : float) (2. : float)) : float)) : float)
                    (0.1 : float))
                   : float)
                  (0.15 : float))
                 : float)
                ((if
                  ((< ((distance (uv_5 : (vec 2)) (mouseUV_6 : (vec 2))) : float)
                    (radius_7 : float))
                   : bool)
                  ((vec3 (0. : float) (0. : float) (0.5 : float)) : (vec 3))
                  ((vec3 (0.5 : float) (0.5 : float) (1. : float)) : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (mouse_circle.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
      ((Extern u_time) : float)
      ((Define Nonrec get_uv_3
        (lambda ((coord_0 (vec 2)))
         (let top_1 (- (* 2. coord_0) u_resolution)
          (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_1 bot_2)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_3 coord_4)
          (let mouseUV_6
           (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
           (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
            (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
             (vec3 0.5 0.5 1.)))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let top_1 (- (* 2. coord_0) u_resolution)
         (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_1 bot_2)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_3 coord_4)
         (let mouseUV_6
          (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
          (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
           (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
            (vec3 0.5 0.5 1.)))))))
      : ((vec 2) -> (vec 3))))

    === anf (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let anf_8 (* 2. coord_0)
         (let top_1 (- anf_8 u_resolution)
          (let anf_9 (index u_resolution 0)
           (let anf_10 (index u_resolution 1)
            (let bot_2 (min anf_9 anf_10) (return (/ top_1 bot_2)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_3 coord_4)
         (let anf_11 (* 2. u_mouse)
          (let anf_12 (- anf_11 u_resolution)
           (let anf_13 (index u_resolution 1)
            (let mouseUV_6 (/ anf_12 anf_13)
             (let anf_14 (* u_time 2.)
              (let anf_15 (sin anf_14)
               (let anf_16 (* anf_15 0.1)
                (let radius_7 (+ anf_16 0.15)
                 (let anf_17 (distance uv_5 mouseUV_6)
                  (let anf_18 (< anf_17 radius_7)
                   (return
                    (if anf_18 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let anf_8 (* 2. coord_0)
         (let top_1 (- anf_8 u_resolution)
          (let anf_9 (index u_resolution 0)
           (let anf_10 (index u_resolution 1)
            (let bot_2 (min anf_9 anf_10) (return (/ top_1 bot_2)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_3 coord_4)
         (let anf_11 (* 2. u_mouse)
          (let anf_12 (- anf_11 u_resolution)
           (let anf_13 (index u_resolution 1)
            (let mouseUV_6 (/ anf_12 anf_13)
             (let anf_14 (* u_time 2.)
              (let anf_15 (sin anf_14)
               (let anf_16 (* anf_15 0.1)
                (let radius_7 (+ anf_16 0.15)
                 (let anf_17 (distance uv_5 mouseUV_6)
                  (let anf_18 (< anf_17 radius_7)
                   (return
                    (if anf_18 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (mouse_circle.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform (TyVec 2) u_mouse)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_3) (desc ()) (params (((TyVec 2) coord_0)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_8 (* 2. coord_0))
         (set () vec2 top_1 (- anf_8 u_resolution))
         (set () float anf_9 (index u_resolution 0))
         (set () float anf_10 (index u_resolution 1))
         (set () float bot_2 (min anf_9 anf_10)) (return (/ top_1 bot_2)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_3 coord_4))
         (set () vec2 anf_11 (* 2. u_mouse))
         (set () vec2 anf_12 (- anf_11 u_resolution))
         (set () float anf_13 (index u_resolution 1))
         (set () vec2 mouseUV_6 (/ anf_12 anf_13))
         (set () float anf_14 (* u_time 2.)) (set () float anf_15 (sin anf_14))
         (set () float anf_16 (* anf_15 0.1))
         (set () float radius_7 (+ anf_16 0.15))
         (set () float anf_17 (distance uv_5 mouseUV_6))
         (set () bool anf_18 (< anf_17 radius_7))
         (if anf_18 (Block (return (vec3 0. 0. 0.5)))
          (Block (return (vec3 0.5 0.5 1.)))))))))

    === patch main (mouse_circle.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform (TyVec 2) u_mouse) (Global Uniform TyFloat u_time)
      (Function (name get_uv_3) (desc ()) (params (((TyVec 2) coord_0)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_8 (* 2. coord_0))
         (set () vec2 top_1 (- anf_8 u_resolution))
         (set () float anf_9 (index u_resolution 0))
         (set () float anf_10 (index u_resolution 1))
         (set () float bot_2 (min anf_9 anf_10)) (return (/ top_1 bot_2)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_3 coord_4))
         (set () vec2 anf_11 (* 2. u_mouse))
         (set () vec2 anf_12 (- anf_11 u_resolution))
         (set () float anf_13 (index u_resolution 1))
         (set () vec2 mouseUV_6 (/ anf_12 anf_13))
         (set () float anf_14 (* u_time 2.)) (set () float anf_15 (sin anf_14))
         (set () float anf_16 (* anf_15 0.1))
         (set () float radius_7 (+ anf_16 0.15))
         (set () float anf_17 (distance uv_5 mouseUV_6))
         (set () bool anf_18 (< anf_17 radius_7))
         (if anf_18 (Block (return (vec3 0. 0. 0.5)))
          (Block (return (vec3 0.5 0.5 1.)))))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE rainbow.glml ======

    === stlc (rainbow.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv
       (lambda (coord (vec 2))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec main
       (lambda (coord (vec 2))
        (let uv (app get_uv coord)
         (let wave (+ (* 5. (+ (index uv 0) (index uv 1))) u_time)
          (let r (+ (* (sin wave) 0.3) 0.7)
           (let g (+ (* (sin (+ wave 2.)) 0.3) 0.7)
            (let b (+ (* (sin (+ wave 4.)) 0.3) 0.7) (vec3 r g b))))))))))

    === uniquify (rainbow.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv_3
       (lambda (coord_0 (vec 2))
        (let top_1 (- (* 2. coord_0) u_resolution)
         (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_1 bot_2)))))
      (Define Nonrec main
       (lambda (coord_4 (vec 2))
        (let uv_5 (app get_uv_3 coord_4)
         (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
          (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
           (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
            (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))))

    === typecheck (rainbow.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_3
        ((lambda (coord_0 (vec 2))
          ((let top_1
            ((- ((* (2. : float) (coord_0 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_2
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_1 : (vec 2)) (bot_2 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        ((lambda (coord_4 (vec 2))
          ((let uv_5
            ((app (get_uv_3 : ((vec 2) -> (vec 2))) (coord_4 : (vec 2))) :
             (vec 2))
            ((let wave_6
              ((+
                ((* (5. : float)
                  ((+ ((index (uv_5 : (vec 2)) 0) : float)
                    ((index (uv_5 : (vec 2)) 1) : float))
                   : float))
                 : float)
                (u_time : float))
               : float)
              ((let r_7
                ((+ ((* ((sin (wave_6 : float)) : float) (0.3 : float)) : float)
                  (0.7 : float))
                 : float)
                ((let g_8
                  ((+
                    ((*
                      ((sin ((+ (wave_6 : float) (2. : float)) : float)) : float)
                      (0.3 : float))
                     : float)
                    (0.7 : float))
                   : float)
                  ((let b_9
                    ((+
                      ((*
                        ((sin ((+ (wave_6 : float) (4. : float)) : float)) :
                         float)
                        (0.3 : float))
                       : float)
                      (0.7 : float))
                     : float)
                    ((vec3 (r_7 : float) (g_8 : float) (b_9 : float)) : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (rainbow.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_3
        (lambda ((coord_0 (vec 2)))
         (let top_1 (- (* 2. coord_0) u_resolution)
          (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_1 bot_2)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_3 coord_4)
          (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
           (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
            (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
             (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let top_1 (- (* 2. coord_0) u_resolution)
         (let bot_2 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_1 bot_2)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_3 coord_4)
         (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
          (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
           (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
            (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))
      : ((vec 2) -> (vec 3))))

    === anf (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let anf_10 (* 2. coord_0)
         (let top_1 (- anf_10 u_resolution)
          (let anf_11 (index u_resolution 0)
           (let anf_12 (index u_resolution 1)
            (let bot_2 (min anf_11 anf_12) (return (/ top_1 bot_2)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_3 coord_4)
         (let anf_13 (index uv_5 0)
          (let anf_14 (index uv_5 1)
           (let anf_15 (+ anf_13 anf_14)
            (let anf_16 (* 5. anf_15)
             (let wave_6 (+ anf_16 u_time)
              (let anf_17 (sin wave_6)
               (let anf_18 (* anf_17 0.3)
                (let r_7 (+ anf_18 0.7)
                 (let anf_19 (+ wave_6 2.)
                  (let anf_20 (sin anf_19)
                   (let anf_21 (* anf_20 0.3)
                    (let g_8 (+ anf_21 0.7)
                     (let anf_22 (+ wave_6 4.)
                      (let anf_23 (sin anf_22)
                       (let anf_24 (* anf_23 0.3)
                        (let b_9 (+ anf_24 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_3) (args ((coord_0 (vec 2))))
       (body
        (let anf_10 (* 2. coord_0)
         (let top_1 (- anf_10 u_resolution)
          (let anf_11 (index u_resolution 0)
           (let anf_12 (index u_resolution 1)
            (let bot_2 (min anf_11 anf_12) (return (/ top_1 bot_2)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_3 coord_4)
         (let anf_13 (index uv_5 0)
          (let anf_14 (index uv_5 1)
           (let anf_15 (+ anf_13 anf_14)
            (let anf_16 (* 5. anf_15)
             (let wave_6 (+ anf_16 u_time)
              (let anf_17 (sin wave_6)
               (let anf_18 (* anf_17 0.3)
                (let r_7 (+ anf_18 0.7)
                 (let anf_19 (+ wave_6 2.)
                  (let anf_20 (sin anf_19)
                   (let anf_21 (* anf_20 0.3)
                    (let g_8 (+ anf_21 0.7)
                     (let anf_22 (+ wave_6 4.)
                      (let anf_23 (sin anf_22)
                       (let anf_24 (* anf_23 0.3)
                        (let b_9 (+ anf_24 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (rainbow.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name get_uv_3) (desc ()) (params (((TyVec 2) coord_0)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_10 (* 2. coord_0))
         (set () vec2 top_1 (- anf_10 u_resolution))
         (set () float anf_11 (index u_resolution 0))
         (set () float anf_12 (index u_resolution 1))
         (set () float bot_2 (min anf_11 anf_12)) (return (/ top_1 bot_2)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_3 coord_4))
         (set () float anf_13 (index uv_5 0))
         (set () float anf_14 (index uv_5 1))
         (set () float anf_15 (+ anf_13 anf_14))
         (set () float anf_16 (* 5. anf_15))
         (set () float wave_6 (+ anf_16 u_time))
         (set () float anf_17 (sin wave_6)) (set () float anf_18 (* anf_17 0.3))
         (set () float r_7 (+ anf_18 0.7)) (set () float anf_19 (+ wave_6 2.))
         (set () float anf_20 (sin anf_19)) (set () float anf_21 (* anf_20 0.3))
         (set () float g_8 (+ anf_21 0.7)) (set () float anf_22 (+ wave_6 4.))
         (set () float anf_23 (sin anf_22)) (set () float anf_24 (* anf_23 0.3))
         (set () float b_9 (+ anf_24 0.7)) (return (vec3 r_7 g_8 b_9)))))))

    === patch main (rainbow.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_3) (desc ()) (params (((TyVec 2) coord_0)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_10 (* 2. coord_0))
         (set () vec2 top_1 (- anf_10 u_resolution))
         (set () float anf_11 (index u_resolution 0))
         (set () float anf_12 (index u_resolution 1))
         (set () float bot_2 (min anf_11 anf_12)) (return (/ top_1 bot_2)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_3 coord_4))
         (set () float anf_13 (index uv_5 0))
         (set () float anf_14 (index uv_5 1))
         (set () float anf_15 (+ anf_13 anf_14))
         (set () float anf_16 (* 5. anf_15))
         (set () float wave_6 (+ anf_16 u_time))
         (set () float anf_17 (sin wave_6)) (set () float anf_18 (* anf_17 0.3))
         (set () float r_7 (+ anf_18 0.7)) (set () float anf_19 (+ wave_6 2.))
         (set () float anf_20 (sin anf_19)) (set () float anf_21 (* anf_20 0.3))
         (set () float g_8 (+ anf_21 0.7)) (set () float anf_22 (+ wave_6 4.))
         (set () float anf_23 (sin anf_22)) (set () float anf_24 (* anf_23 0.3))
         (set () float b_9 (+ anf_24 0.7)) (return (vec3 r_7 g_8 b_9)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))
    |}]
;;
