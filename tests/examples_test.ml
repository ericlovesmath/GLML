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
      (Define Nonrec get_uv_0
       (lambda (coord_1 (vec 2))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define Nonrec main
       (lambda (coord_4 (vec 2))
        (let uv_5 (app get_uv_0 coord_4)
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
      ((Define Nonrec get_uv_0
        ((lambda (coord_1 (vec 2))
          ((let top_2
            ((- ((* (2. : float) (coord_1 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : (vec 2)) (bot_3 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        ((lambda (coord_4 (vec 2))
          ((let uv_5
            ((app (get_uv_0 : ((vec 2) -> (vec 2))) (coord_4 : (vec 2))) :
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
      ((Define Nonrec get_uv_0
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0 coord_4)
          (let size_6 5.
           (let cx_7 (floor (+ (* (index uv_5 0) size_6) (* u_time 2.)))
            (let cy_8 (floor (* (index uv_5 1) size_6))
             (let checker_sum_9 (+ cx_7 cy_8)
              (let is_even_10
               (- checker_sum_9 (* (floor (/ checker_sum_9 2.)) 2.))
               (if (< is_even_10 0.5) (vec3 0.2 0.2 0.2) (vec3 0.8 0.8 0.8))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda_lift (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0 coord_4)
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
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_11 (* 2. coord_1)
         (let top_2 (- anf_11 u_resolution)
          (let anf_12 (index u_resolution 0)
           (let anf_13 (index u_resolution 1)
            (let bot_3 (min anf_12 anf_13) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
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

    === tail_call (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_11 (* 2. coord_1)
         (let top_2 (- anf_11 u_resolution)
          (let anf_12 (index u_resolution 0)
           (let anf_13 (index u_resolution 1)
            (let bot_3 (min anf_12 anf_13) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
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
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_11 (* 2. coord_1))
         (set () vec2 top_2 (- anf_11 u_resolution))
         (set () float anf_12 (index u_resolution 0))
         (set () float anf_13 (index u_resolution 1))
         (set () float bot_3 (min anf_12 anf_13)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4)) (set () float size_6 5.)
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

    === patch_main (checkerboard.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_11 (* 2. coord_1))
         (set () vec2 top_2 (- anf_11 u_resolution))
         (set () float anf_12 (index u_resolution 0))
         (set () float anf_13 (index u_resolution 1))
         (set () float bot_3 (min anf_12 anf_13)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4)) (set () float size_6 5.)
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

    ====== COMPILING EXAMPLE mandelbrot.glml ======

    === stlc (mandelbrot.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv
       (lambda (coord (vec 2))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define
       (Rec 1000 (float -> (float -> (float -> (float -> (float -> float))))))
       mandel
       (lambda (zx float)
        (lambda (zy float)
         (lambda (cx float)
          (lambda (cy float)
           (lambda (i float)
            (if (|| (> (length (vec2 zx zy)) 2.) (> i 150.)) i
             (let next_zx (+ (- (* zx zx) (* zy zy)) cx)
              (let next_zy (+ (* (* 2. zx) zy) cy)
               (app (app (app (app (app mandel next_zx) next_zy) cx) cy)
                (+ i 1.)))))))))))
      (Define Nonrec main
       (lambda (coord (vec 2))
        (let uv (app get_uv coord)
         (let zoom (exp (+ (* (sin (* u_time 0.4)) 4.5) 3.5))
          (let cx (+ -0.7453 (/ (index uv 0) zoom))
           (let cy (+ 0.1127 (/ (index uv 1) zoom))
            (let iter (app (app (app (app (app mandel 0.) 0.) cx) cy) 0.)
             (if (> iter 149.) (vec3 0. 0. 0.)
              (let n (/ iter 150.)
               (let r (+ (* (sin (+ (* n 10.) u_time)) 0.5) 0.5)
                (let g (+ (* (sin (+ (* n 20.) u_time)) 0.5) 0.5)
                 (let b (+ (* (sin (+ (* n 30.) u_time)) 0.5) 0.5) (vec3 r g b)))))))))))))))

    === uniquify (mandelbrot.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv_0
       (lambda (coord_1 (vec 2))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define
       (Rec 1000 (float -> (float -> (float -> (float -> (float -> float))))))
       mandel_4
       (lambda (zx_5 float)
        (lambda (zy_6 float)
         (lambda (cx_7 float)
          (lambda (cy_8 float)
           (lambda (i_9 float)
            (if (|| (> (length (vec2 zx_5 zy_6)) 2.) (> i_9 150.)) i_9
             (let next_zx_10 (+ (- (* zx_5 zx_5) (* zy_6 zy_6)) cx_7)
              (let next_zy_11 (+ (* (* 2. zx_5) zy_6) cy_8)
               (app
                (app (app (app (app mandel_4 next_zx_10) next_zy_11) cx_7) cy_8)
                (+ i_9 1.)))))))))))
      (Define Nonrec main
       (lambda (coord_12 (vec 2))
        (let uv_13 (app get_uv_0 coord_12)
         (let zoom_14 (exp (+ (* (sin (* u_time 0.4)) 4.5) 3.5))
          (let cx_15 (+ -0.7453 (/ (index uv_13 0) zoom_14))
           (let cy_16 (+ 0.1127 (/ (index uv_13 1) zoom_14))
            (let iter_17
             (app (app (app (app (app mandel_4 0.) 0.) cx_15) cy_16) 0.)
             (if (> iter_17 149.) (vec3 0. 0. 0.)
              (let n_18 (/ iter_17 150.)
               (let r_19 (+ (* (sin (+ (* n_18 10.) u_time)) 0.5) 0.5)
                (let g_20 (+ (* (sin (+ (* n_18 20.) u_time)) 0.5) 0.5)
                 (let b_21 (+ (* (sin (+ (* n_18 30.) u_time)) 0.5) 0.5)
                  (vec3 r_19 g_20 b_21)))))))))))))))

    === typecheck (mandelbrot.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda (coord_1 (vec 2))
          ((let top_2
            ((- ((* (2. : float) (coord_1 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : (vec 2)) (bot_3 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define
        (Rec 1000 (float -> (float -> (float -> (float -> (float -> float))))))
        mandel_4
        ((lambda (zx_5 float)
          ((lambda (zy_6 float)
            ((lambda (cx_7 float)
              ((lambda (cy_8 float)
                ((lambda (i_9 float)
                  ((if
                    ((||
                      ((>
                        ((length
                          ((vec2 (zx_5 : float) (zy_6 : float)) : (vec 2)))
                         : float)
                        (2. : float))
                       : bool)
                      ((> (i_9 : float) (150. : float)) : bool))
                     : bool)
                    (i_9 : float)
                    ((let next_zx_10
                      ((+
                        ((- ((* (zx_5 : float) (zx_5 : float)) : float)
                          ((* (zy_6 : float) (zy_6 : float)) : float))
                         : float)
                        (cx_7 : float))
                       : float)
                      ((let next_zy_11
                        ((+
                          ((* ((* (2. : float) (zx_5 : float)) : float)
                            (zy_6 : float))
                           : float)
                          (cy_8 : float))
                         : float)
                        ((app
                          ((app
                            ((app
                              ((app
                                ((app
                                  (mandel_4 :
                                   (float ->
                                    (float ->
                                     (float -> (float -> (float -> float))))))
                                  (next_zx_10 : float))
                                 :
                                 (float ->
                                  (float -> (float -> (float -> float)))))
                                (next_zy_11 : float))
                               : (float -> (float -> (float -> float))))
                              (cx_7 : float))
                             : (float -> (float -> float)))
                            (cy_8 : float))
                           : (float -> float))
                          ((+ (i_9 : float) (1. : float)) : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : (float -> float)))
               : (float -> (float -> float))))
             : (float -> (float -> (float -> float)))))
           : (float -> (float -> (float -> (float -> float))))))
         : (float -> (float -> (float -> (float -> (float -> float)))))))
       : (float -> (float -> (float -> (float -> (float -> float))))))
      ((Define Nonrec main
        ((lambda (coord_12 (vec 2))
          ((let uv_13
            ((app (get_uv_0 : ((vec 2) -> (vec 2))) (coord_12 : (vec 2))) :
             (vec 2))
            ((let zoom_14
              ((exp
                ((+
                  ((*
                    ((sin ((* (u_time : float) (0.4 : float)) : float)) : float)
                    (4.5 : float))
                   : float)
                  (3.5 : float))
                 : float))
               : float)
              ((let cx_15
                ((+ (-0.7453 : float)
                  ((/ ((index (uv_13 : (vec 2)) 0) : float) (zoom_14 : float)) :
                   float))
                 : float)
                ((let cy_16
                  ((+ (0.1127 : float)
                    ((/ ((index (uv_13 : (vec 2)) 1) : float) (zoom_14 : float))
                     : float))
                   : float)
                  ((let iter_17
                    ((app
                      ((app
                        ((app
                          ((app
                            ((app
                              (mandel_4 :
                               (float ->
                                (float -> (float -> (float -> (float -> float))))))
                              (0. : float))
                             : (float -> (float -> (float -> (float -> float)))))
                            (0. : float))
                           : (float -> (float -> (float -> float))))
                          (cx_15 : float))
                         : (float -> (float -> float)))
                        (cy_16 : float))
                       : (float -> float))
                      (0. : float))
                     : float)
                    ((if ((> (iter_17 : float) (149. : float)) : bool)
                      ((vec3 (0. : float) (0. : float) (0. : float)) : (vec 3))
                      ((let n_18 ((/ (iter_17 : float) (150. : float)) : float)
                        ((let r_19
                          ((+
                            ((*
                              ((sin
                                ((+ ((* (n_18 : float) (10. : float)) : float)
                                  (u_time : float))
                                 : float))
                               : float)
                              (0.5 : float))
                             : float)
                            (0.5 : float))
                           : float)
                          ((let g_20
                            ((+
                              ((*
                                ((sin
                                  ((+ ((* (n_18 : float) (20. : float)) : float)
                                    (u_time : float))
                                   : float))
                                 : float)
                                (0.5 : float))
                               : float)
                              (0.5 : float))
                             : float)
                            ((let b_21
                              ((+
                                ((*
                                  ((sin
                                    ((+
                                      ((* (n_18 : float) (30. : float)) : float)
                                      (u_time : float))
                                     : float))
                                   : float)
                                  (0.5 : float))
                                 : float)
                                (0.5 : float))
                               : float)
                              ((vec3 (r_19 : float) (g_20 : float)
                                (b_21 : float))
                               : (vec 3)))
                             : (vec 3)))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (mandelbrot.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define
        (Rec 1000 (float -> (float -> (float -> (float -> (float -> float))))))
        mandel_4
        (lambda ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float))
         (if (|| (> (length (vec2 zx_5 zy_6)) 2.) (> i_9 150.)) i_9
          (let next_zx_10 (+ (- (* zx_5 zx_5) (* zy_6 zy_6)) cx_7)
           (let next_zy_11 (+ (* (* 2. zx_5) zy_6) cy_8)
            (app mandel_4 next_zx_10 next_zy_11 cx_7 cy_8 (+ i_9 1.)))))))
       : (float -> (float -> (float -> (float -> (float -> float))))))
      ((Define Nonrec main
        (lambda ((coord_12 (vec 2)))
         (let uv_13 (app get_uv_0 coord_12)
          (let zoom_14 (exp (+ (* (sin (* u_time 0.4)) 4.5) 3.5))
           (let cx_15 (+ -0.7453 (/ (index uv_13 0) zoom_14))
            (let cy_16 (+ 0.1127 (/ (index uv_13 1) zoom_14))
             (let iter_17 (app mandel_4 0. 0. cx_15 cy_16 0.)
              (if (> iter_17 149.) (vec3 0. 0. 0.)
               (let n_18 (/ iter_17 150.)
                (let r_19 (+ (* (sin (+ (* n_18 10.) u_time)) 0.5) 0.5)
                 (let g_20 (+ (* (sin (+ (* n_18 20.) u_time)) 0.5) 0.5)
                  (let b_21 (+ (* (sin (+ (* n_18 30.) u_time)) 0.5) 0.5)
                   (vec3 r_19 g_20 b_21)))))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda_lift (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define
       (Rec 1000 (float -> (float -> (float -> (float -> (float -> float))))))
       (name mandel_4)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (if (|| (> (length (vec2 zx_5 zy_6)) 2.) (> i_9 150.)) i_9
         (let next_zx_10 (+ (- (* zx_5 zx_5) (* zy_6 zy_6)) cx_7)
          (let next_zy_11 (+ (* (* 2. zx_5) zy_6) cy_8)
           (app mandel_4 next_zx_10 next_zy_11 cx_7 cy_8 (+ i_9 1.)))))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define Nonrec (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (app get_uv_0 coord_12)
         (let zoom_14 (exp (+ (* (sin (* u_time 0.4)) 4.5) 3.5))
          (let cx_15 (+ -0.7453 (/ (index uv_13 0) zoom_14))
           (let cy_16 (+ 0.1127 (/ (index uv_13 1) zoom_14))
            (let iter_17 (app mandel_4 0. 0. cx_15 cy_16 0.)
             (if (> iter_17 149.) (vec3 0. 0. 0.)
              (let n_18 (/ iter_17 150.)
               (let r_19 (+ (* (sin (+ (* n_18 10.) u_time)) 0.5) 0.5)
                (let g_20 (+ (* (sin (+ (* n_18 20.) u_time)) 0.5) 0.5)
                 (let b_21 (+ (* (sin (+ (* n_18 30.) u_time)) 0.5) 0.5)
                  (vec3 r_19 g_20 b_21)))))))))))))
      : ((vec 2) -> (vec 3))))

    === anf (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_22 (* 2. coord_1)
         (let top_2 (- anf_22 u_resolution)
          (let anf_23 (index u_resolution 0)
           (let anf_24 (index u_resolution 1)
            (let bot_3 (min anf_23 anf_24) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define
       (Rec 1000 (float -> (float -> (float -> (float -> (float -> float))))))
       (name mandel_4)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let anf_25 (vec2 zx_5 zy_6)
         (let anf_26 (length anf_25)
          (let anf_27 (> anf_26 2.)
           (let anf_28 (> i_9 150.)
            (let anf_29 (|| anf_27 anf_28)
             (return
              (if anf_29 (return i_9)
               (let anf_30 (* zx_5 zx_5)
                (let anf_31 (* zy_6 zy_6)
                 (let anf_32 (- anf_30 anf_31)
                  (let next_zx_10 (+ anf_32 cx_7)
                   (let anf_33 (* 2. zx_5)
                    (let anf_34 (* anf_33 zy_6)
                     (let next_zy_11 (+ anf_34 cy_8)
                      (let anf_35 (+ i_9 1.)
                       (return (mandel_4 next_zx_10 next_zy_11 cx_7 cy_8 anf_35)))))))))))))))))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define Nonrec (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0 coord_12)
         (let anf_36 (* u_time 0.4)
          (let anf_37 (sin anf_36)
           (let anf_38 (* anf_37 4.5)
            (let anf_39 (+ anf_38 3.5)
             (let zoom_14 (exp anf_39)
              (let anf_40 (index uv_13 0)
               (let anf_41 (/ anf_40 zoom_14)
                (let cx_15 (+ -0.7453 anf_41)
                 (let anf_42 (index uv_13 1)
                  (let anf_43 (/ anf_42 zoom_14)
                   (let cy_16 (+ 0.1127 anf_43)
                    (let iter_17 (mandel_4 0. 0. cx_15 cy_16 0.)
                     (let anf_44 (> iter_17 149.)
                      (return
                       (if anf_44 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_45 (* n_18 10.)
                          (let anf_46 (+ anf_45 u_time)
                           (let anf_47 (sin anf_46)
                            (let anf_48 (* anf_47 0.5)
                             (let r_19 (+ anf_48 0.5)
                              (let anf_49 (* n_18 20.)
                               (let anf_50 (+ anf_49 u_time)
                                (let anf_51 (sin anf_50)
                                 (let anf_52 (* anf_51 0.5)
                                  (let g_20 (+ anf_52 0.5)
                                   (let anf_53 (* n_18 30.)
                                    (let anf_54 (+ anf_53 u_time)
                                     (let anf_55 (sin anf_54)
                                      (let anf_56 (* anf_55 0.5)
                                       (let b_21 (+ anf_56 0.5)
                                        (return (vec3 r_19 g_20 b_21))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_22 (* 2. coord_1)
         (let top_2 (- anf_22 u_resolution)
          (let anf_23 (index u_resolution 0)
           (let anf_24 (index u_resolution 1)
            (let bot_3 (min anf_23 anf_24) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name mandel_4)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let _iter_57 0
         (while (< _iter_57 1000)
          (let anf_25 (vec2 zx_5 zy_6)
           (let anf_26 (length anf_25)
            (let anf_27 (> anf_26 2.)
             (let anf_28 (> i_9 150.)
              (let anf_29 (|| anf_27 anf_28)
               (return
                (if anf_29 (return i_9)
                 (let anf_30 (* zx_5 zx_5)
                  (let anf_31 (* zy_6 zy_6)
                   (let anf_32 (- anf_30 anf_31)
                    (let next_zx_10 (+ anf_32 cx_7)
                     (let anf_33 (* 2. zx_5)
                      (let anf_34 (* anf_33 zy_6)
                       (let next_zy_11 (+ anf_34 cy_8)
                        (let anf_35 (+ i_9 1.)
                         (set zx_5 next_zx_10
                          (set zy_6 next_zy_11
                           (set cx_7 cx_7
                            (set cy_8 cy_8
                             (set i_9 anf_35
                              (let _iter_inc_58 (+ _iter_57 1)
                               (set _iter_57 _iter_inc_58 continue))))))))))))))))))))))
          (return 0.)))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0 coord_12)
         (let anf_36 (* u_time 0.4)
          (let anf_37 (sin anf_36)
           (let anf_38 (* anf_37 4.5)
            (let anf_39 (+ anf_38 3.5)
             (let zoom_14 (exp anf_39)
              (let anf_40 (index uv_13 0)
               (let anf_41 (/ anf_40 zoom_14)
                (let cx_15 (+ -0.7453 anf_41)
                 (let anf_42 (index uv_13 1)
                  (let anf_43 (/ anf_42 zoom_14)
                   (let cy_16 (+ 0.1127 anf_43)
                    (let iter_17 (mandel_4 0. 0. cx_15 cy_16 0.)
                     (let anf_44 (> iter_17 149.)
                      (return
                       (if anf_44 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_45 (* n_18 10.)
                          (let anf_46 (+ anf_45 u_time)
                           (let anf_47 (sin anf_46)
                            (let anf_48 (* anf_47 0.5)
                             (let r_19 (+ anf_48 0.5)
                              (let anf_49 (* n_18 20.)
                               (let anf_50 (+ anf_49 u_time)
                                (let anf_51 (sin anf_50)
                                 (let anf_52 (* anf_51 0.5)
                                  (let g_20 (+ anf_52 0.5)
                                   (let anf_53 (* n_18 30.)
                                    (let anf_54 (+ anf_53 u_time)
                                     (let anf_55 (sin anf_54)
                                      (let anf_56 (* anf_55 0.5)
                                       (let b_21 (+ anf_56 0.5)
                                        (return (vec3 r_19 g_20 b_21))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (mandelbrot.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_22 (* 2. coord_1))
         (set () vec2 top_2 (- anf_22 u_resolution))
         (set () float anf_23 (index u_resolution 0))
         (set () float anf_24 (index u_resolution 1))
         (set () float bot_3 (min anf_23 anf_24)) (return (/ top_2 bot_3)))))
      (Function (name mandel_4) (desc ())
       (params
        ((TyFloat zx_5) (TyFloat zy_6) (TyFloat cx_7) (TyFloat cy_8)
         (TyFloat i_9)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_57 0)
         (while (< _iter_57 1000)
          (Block (set () vec2 anf_25 (vec2 zx_5 zy_6))
           (set () float anf_26 (length anf_25))
           (set () bool anf_27 (> anf_26 2.)) (set () bool anf_28 (> i_9 150.))
           (set () bool anf_29 (|| anf_27 anf_28))
           (if anf_29 (Block (return i_9))
            (Block (set () float anf_30 (* zx_5 zx_5))
             (set () float anf_31 (* zy_6 zy_6))
             (set () float anf_32 (- anf_30 anf_31))
             (set () float next_zx_10 (+ anf_32 cx_7))
             (set () float anf_33 (* 2. zx_5))
             (set () float anf_34 (* anf_33 zy_6))
             (set () float next_zy_11 (+ anf_34 cy_8))
             (set () float anf_35 (+ i_9 1.)) (set zx_5 next_zx_10)
             (set zy_6 next_zy_11) (set cx_7 cx_7) (set cy_8 cy_8)
             (set i_9 anf_35) (set () int _iter_inc_58 (+ _iter_57 1))
             (set _iter_57 _iter_inc_58) continue))))
         (return 0.))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_12)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_13 (get_uv_0 coord_12))
         (set () float anf_36 (* u_time 0.4)) (set () float anf_37 (sin anf_36))
         (set () float anf_38 (* anf_37 4.5))
         (set () float anf_39 (+ anf_38 3.5)) (set () float zoom_14 (exp anf_39))
         (set () float anf_40 (index uv_13 0))
         (set () float anf_41 (/ anf_40 zoom_14))
         (set () float cx_15 (+ -0.7453 anf_41))
         (set () float anf_42 (index uv_13 1))
         (set () float anf_43 (/ anf_42 zoom_14))
         (set () float cy_16 (+ 0.1127 anf_43))
         (set () float iter_17 (mandel_4 0. 0. cx_15 cy_16 0.))
         (set () bool anf_44 (> iter_17 149.))
         (if anf_44 (Block (return (vec3 0. 0. 0.)))
          (Block (set () float n_18 (/ iter_17 150.))
           (set () float anf_45 (* n_18 10.))
           (set () float anf_46 (+ anf_45 u_time))
           (set () float anf_47 (sin anf_46))
           (set () float anf_48 (* anf_47 0.5))
           (set () float r_19 (+ anf_48 0.5)) (set () float anf_49 (* n_18 20.))
           (set () float anf_50 (+ anf_49 u_time))
           (set () float anf_51 (sin anf_50))
           (set () float anf_52 (* anf_51 0.5))
           (set () float g_20 (+ anf_52 0.5)) (set () float anf_53 (* n_18 30.))
           (set () float anf_54 (+ anf_53 u_time))
           (set () float anf_55 (sin anf_54))
           (set () float anf_56 (* anf_55 0.5))
           (set () float b_21 (+ anf_56 0.5)) (return (vec3 r_19 g_20 b_21)))))))))

    === patch_main (mandelbrot.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_22 (* 2. coord_1))
         (set () vec2 top_2 (- anf_22 u_resolution))
         (set () float anf_23 (index u_resolution 0))
         (set () float anf_24 (index u_resolution 1))
         (set () float bot_3 (min anf_23 anf_24)) (return (/ top_2 bot_3)))))
      (Function (name mandel_4) (desc ())
       (params
        ((TyFloat zx_5) (TyFloat zy_6) (TyFloat cx_7) (TyFloat cy_8)
         (TyFloat i_9)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_57 0)
         (while (< _iter_57 1000)
          (Block (set () vec2 anf_25 (vec2 zx_5 zy_6))
           (set () float anf_26 (length anf_25))
           (set () bool anf_27 (> anf_26 2.)) (set () bool anf_28 (> i_9 150.))
           (set () bool anf_29 (|| anf_27 anf_28))
           (if anf_29 (Block (return i_9))
            (Block (set () float anf_30 (* zx_5 zx_5))
             (set () float anf_31 (* zy_6 zy_6))
             (set () float anf_32 (- anf_30 anf_31))
             (set () float next_zx_10 (+ anf_32 cx_7))
             (set () float anf_33 (* 2. zx_5))
             (set () float anf_34 (* anf_33 zy_6))
             (set () float next_zy_11 (+ anf_34 cy_8))
             (set () float anf_35 (+ i_9 1.)) (set zx_5 next_zx_10)
             (set zy_6 next_zy_11) (set cx_7 cx_7) (set cy_8 cy_8)
             (set i_9 anf_35) (set () int _iter_inc_58 (+ _iter_57 1))
             (set _iter_57 _iter_inc_58) continue))))
         (return 0.))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_12)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_13 (get_uv_0 coord_12))
         (set () float anf_36 (* u_time 0.4)) (set () float anf_37 (sin anf_36))
         (set () float anf_38 (* anf_37 4.5))
         (set () float anf_39 (+ anf_38 3.5)) (set () float zoom_14 (exp anf_39))
         (set () float anf_40 (index uv_13 0))
         (set () float anf_41 (/ anf_40 zoom_14))
         (set () float cx_15 (+ -0.7453 anf_41))
         (set () float anf_42 (index uv_13 1))
         (set () float anf_43 (/ anf_42 zoom_14))
         (set () float cy_16 (+ 0.1127 anf_43))
         (set () float iter_17 (mandel_4 0. 0. cx_15 cy_16 0.))
         (set () bool anf_44 (> iter_17 149.))
         (if anf_44 (Block (return (vec3 0. 0. 0.)))
          (Block (set () float n_18 (/ iter_17 150.))
           (set () float anf_45 (* n_18 10.))
           (set () float anf_46 (+ anf_45 u_time))
           (set () float anf_47 (sin anf_46))
           (set () float anf_48 (* anf_47 0.5))
           (set () float r_19 (+ anf_48 0.5)) (set () float anf_49 (* n_18 20.))
           (set () float anf_50 (+ anf_49 u_time))
           (set () float anf_51 (sin anf_50))
           (set () float anf_52 (* anf_51 0.5))
           (set () float g_20 (+ anf_52 0.5)) (set () float anf_53 (* n_18 30.))
           (set () float anf_54 (+ anf_53 u_time))
           (set () float anf_55 (sin anf_54))
           (set () float anf_56 (* anf_55 0.5))
           (set () float b_21 (+ anf_56 0.5)) (return (vec3 r_19 g_20 b_21)))))))
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
      (Define Nonrec get_uv_0
       (lambda (coord_1 (vec 2))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define Nonrec main
       (lambda (coord_4 (vec 2))
        (let uv_5 (app get_uv_0 coord_4)
         (let mouseUV_6
          (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
          (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
           (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
            (vec3 0.5 0.5 1.)))))))))

    === typecheck (mouse_circle.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
      ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda (coord_1 (vec 2))
          ((let top_2
            ((- ((* (2. : float) (coord_1 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : (vec 2)) (bot_3 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        ((lambda (coord_4 (vec 2))
          ((let uv_5
            ((app (get_uv_0 : ((vec 2) -> (vec 2))) (coord_4 : (vec 2))) :
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
      ((Define Nonrec get_uv_0
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0 coord_4)
          (let mouseUV_6
           (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
           (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
            (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
             (vec3 0.5 0.5 1.)))))))
       : ((vec 2) -> (vec 3)))))

    === lambda_lift (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0 coord_4)
         (let mouseUV_6
          (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
          (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
           (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
            (vec3 0.5 0.5 1.)))))))
      : ((vec 2) -> (vec 3))))

    === anf (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_8 (* 2. coord_1)
         (let top_2 (- anf_8 u_resolution)
          (let anf_9 (index u_resolution 0)
           (let anf_10 (index u_resolution 1)
            (let bot_3 (min anf_9 anf_10) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
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

    === tail_call (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_8 (* 2. coord_1)
         (let top_2 (- anf_8 u_resolution)
          (let anf_9 (index u_resolution 0)
           (let anf_10 (index u_resolution 1)
            (let bot_3 (min anf_9 anf_10) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
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
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_8 (* 2. coord_1))
         (set () vec2 top_2 (- anf_8 u_resolution))
         (set () float anf_9 (index u_resolution 0))
         (set () float anf_10 (index u_resolution 1))
         (set () float bot_3 (min anf_9 anf_10)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4))
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

    === patch_main (mouse_circle.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform (TyVec 2) u_mouse) (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_8 (* 2. coord_1))
         (set () vec2 top_2 (- anf_8 u_resolution))
         (set () float anf_9 (index u_resolution 0))
         (set () float anf_10 (index u_resolution 1))
         (set () float bot_3 (min anf_9 anf_10)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4))
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
      (Define Nonrec get_uv_0
       (lambda (coord_1 (vec 2))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define Nonrec main
       (lambda (coord_4 (vec 2))
        (let uv_5 (app get_uv_0 coord_4)
         (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
          (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
           (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
            (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))))

    === typecheck (rainbow.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda (coord_1 (vec 2))
          ((let top_2
            ((- ((* (2. : float) (coord_1 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : (vec 2)) (bot_3 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        ((lambda (coord_4 (vec 2))
          ((let uv_5
            ((app (get_uv_0 : ((vec 2) -> (vec 2))) (coord_4 : (vec 2))) :
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
      ((Define Nonrec get_uv_0
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0 coord_4)
          (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
           (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
            (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
             (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda_lift (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0 coord_4)
         (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
          (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
           (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
            (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))
      : ((vec 2) -> (vec 3))))

    === anf (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_10 (* 2. coord_1)
         (let top_2 (- anf_10 u_resolution)
          (let anf_11 (index u_resolution 0)
           (let anf_12 (index u_resolution 1)
            (let bot_3 (min anf_11 anf_12) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
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

    === tail_call (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_10 (* 2. coord_1)
         (let top_2 (- anf_10 u_resolution)
          (let anf_11 (index u_resolution 0)
           (let anf_12 (index u_resolution 1)
            (let bot_3 (min anf_11 anf_12) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
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
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_10 (* 2. coord_1))
         (set () vec2 top_2 (- anf_10 u_resolution))
         (set () float anf_11 (index u_resolution 0))
         (set () float anf_12 (index u_resolution 1))
         (set () float bot_3 (min anf_11 anf_12)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4))
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

    === patch_main (rainbow.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_10 (* 2. coord_1))
         (set () vec2 top_2 (- anf_10 u_resolution))
         (set () float anf_11 (index u_resolution 0))
         (set () float anf_12 (index u_resolution 1))
         (set () float bot_3 (min anf_11 anf_12)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4))
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

    ====== COMPILING EXAMPLE recursion.glml ======

    === stlc (recursion.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv
       (lambda (coord (vec 2))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec rotate
       (lambda (angle float)
        (let s (sin angle) (let c (cos angle) (mat2x2 c (* -1. s) s c)))))
      (Define (Rec 1000 (float -> (float -> float))) gcd
       (lambda (a float)
        (lambda (b float)
         (if (< a 0.05) b
          (if (< b 0.05) a
           (if (> a b) (app (app gcd (- a b)) b) (app (app gcd a) (- b a))))))))
      (Define Nonrec main
       (lambda (coord (vec 2))
        (let uv (app get_uv coord)
         (let uv (* (app rotate u_time) uv)
          (let x (abs (* (* (index uv 0) (sin (* u_time 2.))) 2.))
           (let y (abs (* (* (index uv 1) (sin (* u_time 2.))) 2.))
            (let res (app (app gcd x) y) (vec3 res (* res 0.5) (- 1. res)))))))))))

    === uniquify (recursion.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv_0
       (lambda (coord_1 (vec 2))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define Nonrec rotate_4
       (lambda (angle_5 float)
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5) (mat2x2 c_7 (* -1. s_6) s_6 c_7)))))
      (Define (Rec 1000 (float -> (float -> float))) gcd_8
       (lambda (a_9 float)
        (lambda (b_10 float)
         (if (< a_9 0.05) b_10
          (if (< b_10 0.05) a_9
           (if (> a_9 b_10) (app (app gcd_8 (- a_9 b_10)) b_10)
            (app (app gcd_8 a_9) (- b_10 a_9))))))))
      (Define Nonrec main
       (lambda (coord_11 (vec 2))
        (let uv_12 (app get_uv_0 coord_11)
         (let uv_13 (* (app rotate_4 u_time) uv_12)
          (let x_14 (abs (* (* (index uv_13 0) (sin (* u_time 2.))) 2.))
           (let y_15 (abs (* (* (index uv_13 1) (sin (* u_time 2.))) 2.))
            (let res_16 (app (app gcd_8 x_14) y_15)
             (vec3 res_16 (* res_16 0.5) (- 1. res_16)))))))))))

    === typecheck (recursion.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda (coord_1 (vec 2))
          ((let top_2
            ((- ((* (2. : float) (coord_1 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : (vec 2)) (bot_3 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec rotate_4
        ((lambda (angle_5 float)
          ((let s_6 ((sin (angle_5 : float)) : float)
            ((let c_7 ((cos (angle_5 : float)) : float)
              ((mat2x2 (c_7 : float) ((* (-1. : float) (s_6 : float)) : float)
                (s_6 : float) (c_7 : float))
               : (mat 2 2)))
             : (mat 2 2)))
           : (mat 2 2)))
         : (float -> (mat 2 2))))
       : (float -> (mat 2 2)))
      ((Define (Rec 1000 (float -> (float -> float))) gcd_8
        ((lambda (a_9 float)
          ((lambda (b_10 float)
            ((if ((< (a_9 : float) (0.05 : float)) : bool) (b_10 : float)
              ((if ((< (b_10 : float) (0.05 : float)) : bool) (a_9 : float)
                ((if ((> (a_9 : float) (b_10 : float)) : bool)
                  ((app
                    ((app (gcd_8 : (float -> (float -> float)))
                      ((- (a_9 : float) (b_10 : float)) : float))
                     : (float -> float))
                    (b_10 : float))
                   : float)
                  ((app
                    ((app (gcd_8 : (float -> (float -> float))) (a_9 : float)) :
                     (float -> float))
                    ((- (b_10 : float) (a_9 : float)) : float))
                   : float))
                 : float))
               : float))
             : float))
           : (float -> float)))
         : (float -> (float -> float))))
       : (float -> (float -> float)))
      ((Define Nonrec main
        ((lambda (coord_11 (vec 2))
          ((let uv_12
            ((app (get_uv_0 : ((vec 2) -> (vec 2))) (coord_11 : (vec 2))) :
             (vec 2))
            ((let uv_13
              ((*
                ((app (rotate_4 : (float -> (mat 2 2))) (u_time : float)) :
                 (mat 2 2))
                (uv_12 : (vec 2)))
               : (vec 2))
              ((let x_14
                ((abs
                  ((*
                    ((* ((index (uv_13 : (vec 2)) 0) : float)
                      ((sin ((* (u_time : float) (2. : float)) : float)) : float))
                     : float)
                    (2. : float))
                   : float))
                 : float)
                ((let y_15
                  ((abs
                    ((*
                      ((* ((index (uv_13 : (vec 2)) 1) : float)
                        ((sin ((* (u_time : float) (2. : float)) : float)) :
                         float))
                       : float)
                      (2. : float))
                     : float))
                   : float)
                  ((let res_16
                    ((app
                      ((app (gcd_8 : (float -> (float -> float))) (x_14 : float))
                       : (float -> float))
                      (y_15 : float))
                     : float)
                    ((vec3 (res_16 : float)
                      ((* (res_16 : float) (0.5 : float)) : float)
                      ((- (1. : float) (res_16 : float)) : float))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (recursion.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec rotate_4
        (lambda ((angle_5 float))
         (let s_6 (sin angle_5)
          (let c_7 (cos angle_5) (mat2x2 c_7 (* -1. s_6) s_6 c_7)))))
       : (float -> (mat 2 2)))
      ((Define (Rec 1000 (float -> (float -> float))) gcd_8
        (lambda ((a_9 float) (b_10 float))
         (if (< a_9 0.05) b_10
          (if (< b_10 0.05) a_9
           (if (> a_9 b_10) (app gcd_8 (- a_9 b_10) b_10)
            (app gcd_8 a_9 (- b_10 a_9)))))))
       : (float -> (float -> float)))
      ((Define Nonrec main
        (lambda ((coord_11 (vec 2)))
         (let uv_12 (app get_uv_0 coord_11)
          (let uv_13 (* (app rotate_4 u_time) uv_12)
           (let x_14 (abs (* (* (index uv_13 0) (sin (* u_time 2.))) 2.))
            (let y_15 (abs (* (* (index uv_13 1) (sin (* u_time 2.))) 2.))
             (let res_16 (app gcd_8 x_14 y_15)
              (vec3 res_16 (* res_16 0.5) (- 1. res_16)))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda_lift (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5) (mat2x2 c_7 (* -1. s_6) s_6 c_7)))))
      : (float -> (mat 2 2)))
     ((Define (Rec 1000 (float -> (float -> float))) (name gcd_8)
       (args ((a_9 float) (b_10 float)))
       (body
        (if (< a_9 0.05) b_10
         (if (< b_10 0.05) a_9
          (if (> a_9 b_10) (app gcd_8 (- a_9 b_10) b_10)
           (app gcd_8 a_9 (- b_10 a_9)))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (app get_uv_0 coord_11)
         (let uv_13 (* (app rotate_4 u_time) uv_12)
          (let x_14 (abs (* (* (index uv_13 0) (sin (* u_time 2.))) 2.))
           (let y_15 (abs (* (* (index uv_13 1) (sin (* u_time 2.))) 2.))
            (let res_16 (app gcd_8 x_14 y_15)
             (vec3 res_16 (* res_16 0.5) (- 1. res_16)))))))))
      : ((vec 2) -> (vec 3))))

    === anf (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_17 (* 2. coord_1)
         (let top_2 (- anf_17 u_resolution)
          (let anf_18 (index u_resolution 0)
           (let anf_19 (index u_resolution 1)
            (let bot_3 (min anf_18 anf_19) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_20 (* -1. s_6) (return (mat2x2 c_7 anf_20 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (Rec 1000 (float -> (float -> float))) (name gcd_8)
       (args ((a_9 float) (b_10 float)))
       (body
        (let anf_21 (< a_9 0.05)
         (return
          (if anf_21 (return b_10)
           (let anf_22 (< b_10 0.05)
            (return
             (if anf_22 (return a_9)
              (let anf_23 (> a_9 b_10)
               (return
                (if anf_23 (let anf_24 (- a_9 b_10) (return (gcd_8 anf_24 b_10)))
                 (let anf_25 (- b_10 a_9) (return (gcd_8 a_9 anf_25))))))))))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_26 (rotate_4 u_time)
          (let uv_13 (* anf_26 uv_12)
           (let anf_27 (index uv_13 0)
            (let anf_28 (* u_time 2.)
             (let anf_29 (sin anf_28)
              (let anf_30 (* anf_27 anf_29)
               (let anf_31 (* anf_30 2.)
                (let x_14 (abs anf_31)
                 (let anf_32 (index uv_13 1)
                  (let anf_33 (* u_time 2.)
                   (let anf_34 (sin anf_33)
                    (let anf_35 (* anf_32 anf_34)
                     (let anf_36 (* anf_35 2.)
                      (let y_15 (abs anf_36)
                       (let res_16 (gcd_8 x_14 y_15)
                        (let anf_37 (* res_16 0.5)
                         (let anf_38 (- 1. res_16)
                          (return (vec3 res_16 anf_37 anf_38))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_17 (* 2. coord_1)
         (let top_2 (- anf_17 u_resolution)
          (let anf_18 (index u_resolution 0)
           (let anf_19 (index u_resolution 1)
            (let bot_3 (min anf_18 anf_19) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_20 (* -1. s_6) (return (mat2x2 c_7 anf_20 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (name gcd_8) (args ((a_9 float) (b_10 float)))
       (body
        (let _iter_39 0
         (while (< _iter_39 1000)
          (let anf_21 (< a_9 0.05)
           (return
            (if anf_21 (return b_10)
             (let anf_22 (< b_10 0.05)
              (return
               (if anf_22 (return a_9)
                (let anf_23 (> a_9 b_10)
                 (return
                  (if anf_23
                   (let anf_24 (- a_9 b_10)
                    (set a_9 anf_24
                     (set b_10 b_10
                      (let _iter_inc_40 (+ _iter_39 1)
                       (set _iter_39 _iter_inc_40 continue)))))
                   (let anf_25 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_25
                      (let _iter_inc_41 (+ _iter_39 1)
                       (set _iter_39 _iter_inc_41 continue))))))))))))))
          (return 0.)))))
      : (float -> (float -> float)))
     ((Define (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_26 (rotate_4 u_time)
          (let uv_13 (* anf_26 uv_12)
           (let anf_27 (index uv_13 0)
            (let anf_28 (* u_time 2.)
             (let anf_29 (sin anf_28)
              (let anf_30 (* anf_27 anf_29)
               (let anf_31 (* anf_30 2.)
                (let x_14 (abs anf_31)
                 (let anf_32 (index uv_13 1)
                  (let anf_33 (* u_time 2.)
                   (let anf_34 (sin anf_33)
                    (let anf_35 (* anf_32 anf_34)
                     (let anf_36 (* anf_35 2.)
                      (let y_15 (abs anf_36)
                       (let res_16 (gcd_8 x_14 y_15)
                        (let anf_37 (* res_16 0.5)
                         (let anf_38 (- 1. res_16)
                          (return (vec3 res_16 anf_37 anf_38))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (recursion.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_17 (* 2. coord_1))
         (set () vec2 top_2 (- anf_17 u_resolution))
         (set () float anf_18 (index u_resolution 0))
         (set () float anf_19 (index u_resolution 1))
         (set () float bot_3 (min anf_18 anf_19)) (return (/ top_2 bot_3)))))
      (Function (name rotate_4) (desc ()) (params ((TyFloat angle_5)))
       (ret_type (TyMat 2 2))
       (body
        ((set () float s_6 (sin angle_5)) (set () float c_7 (cos angle_5))
         (set () float anf_20 (* -1. s_6)) (return (mat2 c_7 anf_20 s_6 c_7)))))
      (Function (name gcd_8) (desc ()) (params ((TyFloat a_9) (TyFloat b_10)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_39 0)
         (while (< _iter_39 1000)
          (Block (set () bool anf_21 (< a_9 0.05))
           (if anf_21 (Block (return b_10))
            (Block (set () bool anf_22 (< b_10 0.05))
             (if anf_22 (Block (return a_9))
              (Block (set () bool anf_23 (> a_9 b_10))
               (if anf_23
                (Block (set () float anf_24 (- a_9 b_10)) (set a_9 anf_24)
                 (set b_10 b_10) (set () int _iter_inc_40 (+ _iter_39 1))
                 (set _iter_39 _iter_inc_40) continue)
                (Block (set () float anf_25 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_25) (set () int _iter_inc_41 (+ _iter_39 1))
                 (set _iter_39 _iter_inc_41) continue))))))))
         (return 0.))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_12 (get_uv_0 coord_11))
         (set () mat2 anf_26 (rotate_4 u_time))
         (set () vec2 uv_13 (* anf_26 uv_12))
         (set () float anf_27 (index uv_13 0))
         (set () float anf_28 (* u_time 2.)) (set () float anf_29 (sin anf_28))
         (set () float anf_30 (* anf_27 anf_29))
         (set () float anf_31 (* anf_30 2.)) (set () float x_14 (abs anf_31))
         (set () float anf_32 (index uv_13 1))
         (set () float anf_33 (* u_time 2.)) (set () float anf_34 (sin anf_33))
         (set () float anf_35 (* anf_32 anf_34))
         (set () float anf_36 (* anf_35 2.)) (set () float y_15 (abs anf_36))
         (set () float res_16 (gcd_8 x_14 y_15))
         (set () float anf_37 (* res_16 0.5)) (set () float anf_38 (- 1. res_16))
         (return (vec3 res_16 anf_37 anf_38)))))))

    === patch_main (recursion.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_17 (* 2. coord_1))
         (set () vec2 top_2 (- anf_17 u_resolution))
         (set () float anf_18 (index u_resolution 0))
         (set () float anf_19 (index u_resolution 1))
         (set () float bot_3 (min anf_18 anf_19)) (return (/ top_2 bot_3)))))
      (Function (name rotate_4) (desc ()) (params ((TyFloat angle_5)))
       (ret_type (TyMat 2 2))
       (body
        ((set () float s_6 (sin angle_5)) (set () float c_7 (cos angle_5))
         (set () float anf_20 (* -1. s_6)) (return (mat2 c_7 anf_20 s_6 c_7)))))
      (Function (name gcd_8) (desc ()) (params ((TyFloat a_9) (TyFloat b_10)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_39 0)
         (while (< _iter_39 1000)
          (Block (set () bool anf_21 (< a_9 0.05))
           (if anf_21 (Block (return b_10))
            (Block (set () bool anf_22 (< b_10 0.05))
             (if anf_22 (Block (return a_9))
              (Block (set () bool anf_23 (> a_9 b_10))
               (if anf_23
                (Block (set () float anf_24 (- a_9 b_10)) (set a_9 anf_24)
                 (set b_10 b_10) (set () int _iter_inc_40 (+ _iter_39 1))
                 (set _iter_39 _iter_inc_40) continue)
                (Block (set () float anf_25 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_25) (set () int _iter_inc_41 (+ _iter_39 1))
                 (set _iter_39 _iter_inc_41) continue))))))))
         (return 0.))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_12 (get_uv_0 coord_11))
         (set () mat2 anf_26 (rotate_4 u_time))
         (set () vec2 uv_13 (* anf_26 uv_12))
         (set () float anf_27 (index uv_13 0))
         (set () float anf_28 (* u_time 2.)) (set () float anf_29 (sin anf_28))
         (set () float anf_30 (* anf_27 anf_29))
         (set () float anf_31 (* anf_30 2.)) (set () float x_14 (abs anf_31))
         (set () float anf_32 (index uv_13 1))
         (set () float anf_33 (* u_time 2.)) (set () float anf_34 (sin anf_33))
         (set () float anf_35 (* anf_32 anf_34))
         (set () float anf_36 (* anf_35 2.)) (set () float y_15 (abs anf_36))
         (set () float res_16 (gcd_8 x_14 y_15))
         (set () float anf_37 (* res_16 0.5)) (set () float anf_38 (- 1. res_16))
         (return (vec3 res_16 anf_37 anf_38)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE warped_noise.glml ======

    === stlc (warped_noise.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec fract (lambda (v (vec 4)) (- v (floor v))))
      (Define Nonrec smoothstep
       (lambda (edge0 float)
        (lambda (edge1 float)
         (lambda (x float)
          (let t (clamp (/ (- x edge0) (- edge1 edge0)) 0. 1.)
           (* (* t t) (- 3. (* 2. t))))))))
      (Define Nonrec smoothNoise
       (lambda (p (vec 2))
        (let i (floor p)
         (let pf (- p i)
          (let inter (* (* pf pf) (- 3. (* 2. pf)))
           (let v4 (vec4 0. 1. 27. 28.)
            (let seed (+ (+ v4 (index i 0)) (* (index i 1) 27.))
             (let hash (app fract (* (sin (% seed 6.2831853)) 200000.))
              (let col0 (vec2 (index hash 0) (index hash 1))
               (let col1 (vec2 (index hash 2) (index hash 3))
                (let res_v
                 (+ (* col0 (- 1. (index inter 1))) (* col1 (index inter 1)))
                 (dot res_v (vec2 (- 1. (index inter 0)) (index inter 0))))))))))))))
      (Define Nonrec fractalNoise
       (lambda (p (vec 2))
        (+
         (+
          (+ (* (app smoothNoise p) 0.5333)
           (* (app smoothNoise (* p 2.)) 0.2667))
          (* (app smoothNoise (* p 4.)) 0.1333))
         (* (app smoothNoise (* p 8.)) 0.0667))))
      (Define Nonrec warpedNoise
       (lambda (p (vec 2))
        (let m (* (vec2 u_time (- 0. u_time)) 0.5)
         (let x (app fractalNoise (+ p m))
          (let y (app fractalNoise (+ (+ p (vec2 (index m 1) (index m 0))) x))
           (let z (app fractalNoise (+ (- (- p m) x) y))
            (let warp (+ (+ (vec2 x y) (vec2 y z)) (vec2 z x))
             (let mag (* (length (vec3 x y z)) 0.25)
              (app fractalNoise (+ (+ p warp) mag))))))))))
      (Define Nonrec main
       (lambda (coord (vec 2))
        (let uv (/ (- coord (* u_resolution 0.5)) (index u_resolution 1))
         (let n (app warpedNoise (* uv 6.))
          (let n2 (app warpedNoise (- (* uv 6.) 0.02))
           (let bump (* (/ (max (- n2 n) 0.) 0.02) 0.7071)
            (let bump2 (* (/ (max (- n n2) 0.) 0.02) 0.7071)
             (let b1 (+ (* bump bump) (* (pow bump 4.) 0.5))
              (let b2 (+ (* bump2 bump2) (* (pow bump2 4.) 0.5))
               (let base_col
                (+ (* (* (vec3 1. 0.7 0.6) (vec3 b1 (* (+ b1 b2) 0.4) b2)) 0.3)
                 0.5)
                (let col (* (* n n) base_col)
                 (let spot1_dist (length (- uv 0.65))
                  (let spot2_dist (length (+ uv 0.5))
                   (let a (* (vec3 0.8 0.4 1.) 0.35)
                    (let b
                     (* (vec3 1. 0.5 0.2)
                      (app (app (app smoothstep 0.) 1.) (- 1. spot1_dist)))
                     (let c
                      (* (vec3 0.2 0.4 1.)
                       (app (app (app smoothstep 0.) 1.) (- 1. spot2_dist)))
                      (let spot_logic (+ a (* (+ b c) 5.))
                       (let final_col (* col spot_logic)
                        (sqrt (max final_col 0.))))))))))))))))))))))

    === uniquify (warped_noise.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec fract_0 (lambda (v_1 (vec 4)) (- v_1 (floor v_1))))
      (Define Nonrec smoothstep_2
       (lambda (edge0_3 float)
        (lambda (edge1_4 float)
         (lambda (x_5 float)
          (let t_6 (clamp (/ (- x_5 edge0_3) (- edge1_4 edge0_3)) 0. 1.)
           (* (* t_6 t_6) (- 3. (* 2. t_6))))))))
      (Define Nonrec smoothNoise_7
       (lambda (p_8 (vec 2))
        (let i_9 (floor p_8)
         (let pf_10 (- p_8 i_9)
          (let inter_11 (* (* pf_10 pf_10) (- 3. (* 2. pf_10)))
           (let v4_12 (vec4 0. 1. 27. 28.)
            (let seed_13 (+ (+ v4_12 (index i_9 0)) (* (index i_9 1) 27.))
             (let hash_14 (app fract_0 (* (sin (% seed_13 6.2831853)) 200000.))
              (let col0_15 (vec2 (index hash_14 0) (index hash_14 1))
               (let col1_16 (vec2 (index hash_14 2) (index hash_14 3))
                (let res_v_17
                 (+ (* col0_15 (- 1. (index inter_11 1)))
                  (* col1_16 (index inter_11 1)))
                 (dot res_v_17
                  (vec2 (- 1. (index inter_11 0)) (index inter_11 0))))))))))))))
      (Define Nonrec fractalNoise_18
       (lambda (p_19 (vec 2))
        (+
         (+
          (+ (* (app smoothNoise_7 p_19) 0.5333)
           (* (app smoothNoise_7 (* p_19 2.)) 0.2667))
          (* (app smoothNoise_7 (* p_19 4.)) 0.1333))
         (* (app smoothNoise_7 (* p_19 8.)) 0.0667))))
      (Define Nonrec warpedNoise_20
       (lambda (p_21 (vec 2))
        (let m_22 (* (vec2 u_time (- 0. u_time)) 0.5)
         (let x_23 (app fractalNoise_18 (+ p_21 m_22))
          (let y_24
           (app fractalNoise_18
            (+ (+ p_21 (vec2 (index m_22 1) (index m_22 0))) x_23))
           (let z_25 (app fractalNoise_18 (+ (- (- p_21 m_22) x_23) y_24))
            (let warp_26
             (+ (+ (vec2 x_23 y_24) (vec2 y_24 z_25)) (vec2 z_25 x_23))
             (let mag_27 (* (length (vec3 x_23 y_24 z_25)) 0.25)
              (app fractalNoise_18 (+ (+ p_21 warp_26) mag_27))))))))))
      (Define Nonrec main
       (lambda (coord_28 (vec 2))
        (let uv_29 (/ (- coord_28 (* u_resolution 0.5)) (index u_resolution 1))
         (let n_30 (app warpedNoise_20 (* uv_29 6.))
          (let n2_31 (app warpedNoise_20 (- (* uv_29 6.) 0.02))
           (let bump_32 (* (/ (max (- n2_31 n_30) 0.) 0.02) 0.7071)
            (let bump2_33 (* (/ (max (- n_30 n2_31) 0.) 0.02) 0.7071)
             (let b1_34 (+ (* bump_32 bump_32) (* (pow bump_32 4.) 0.5))
              (let b2_35 (+ (* bump2_33 bump2_33) (* (pow bump2_33 4.) 0.5))
               (let base_col_36
                (+
                 (*
                  (* (vec3 1. 0.7 0.6)
                   (vec3 b1_34 (* (+ b1_34 b2_35) 0.4) b2_35))
                  0.3)
                 0.5)
                (let col_37 (* (* n_30 n_30) base_col_36)
                 (let spot1_dist_38 (length (- uv_29 0.65))
                  (let spot2_dist_39 (length (+ uv_29 0.5))
                   (let a_40 (* (vec3 0.8 0.4 1.) 0.35)
                    (let b_41
                     (* (vec3 1. 0.5 0.2)
                      (app (app (app smoothstep_2 0.) 1.) (- 1. spot1_dist_38)))
                     (let c_42
                      (* (vec3 0.2 0.4 1.)
                       (app (app (app smoothstep_2 0.) 1.) (- 1. spot2_dist_39)))
                      (let spot_logic_43 (+ a_40 (* (+ b_41 c_42) 5.))
                       (let final_col_44 (* col_37 spot_logic_43)
                        (sqrt (max final_col_44 0.))))))))))))))))))))))

    === typecheck (warped_noise.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec fract_0
        ((lambda (v_1 (vec 4))
          ((- (v_1 : (vec 4)) ((floor (v_1 : (vec 4))) : (vec 4))) : (vec 4)))
         : ((vec 4) -> (vec 4))))
       : ((vec 4) -> (vec 4)))
      ((Define Nonrec smoothstep_2
        ((lambda (edge0_3 float)
          ((lambda (edge1_4 float)
            ((lambda (x_5 float)
              ((let t_6
                ((clamp
                  ((/ ((- (x_5 : float) (edge0_3 : float)) : float)
                    ((- (edge1_4 : float) (edge0_3 : float)) : float))
                   : float)
                  (0. : float) (1. : float))
                 : float)
                ((* ((* (t_6 : float) (t_6 : float)) : float)
                  ((- (3. : float) ((* (2. : float) (t_6 : float)) : float)) :
                   float))
                 : float))
               : float))
             : (float -> float)))
           : (float -> (float -> float))))
         : (float -> (float -> (float -> float)))))
       : (float -> (float -> (float -> float))))
      ((Define Nonrec smoothNoise_7
        ((lambda (p_8 (vec 2))
          ((let i_9 ((floor (p_8 : (vec 2))) : (vec 2))
            ((let pf_10 ((- (p_8 : (vec 2)) (i_9 : (vec 2))) : (vec 2))
              ((let inter_11
                ((* ((* (pf_10 : (vec 2)) (pf_10 : (vec 2))) : (vec 2))
                  ((- (3. : float)
                    ((* (2. : float) (pf_10 : (vec 2))) : (vec 2)))
                   : (vec 2)))
                 : (vec 2))
                ((let v4_12
                  ((vec4 (0. : float) (1. : float) (27. : float) (28. : float)) :
                   (vec 4))
                  ((let seed_13
                    ((+
                      ((+ (v4_12 : (vec 4)) ((index (i_9 : (vec 2)) 0) : float))
                       : (vec 4))
                      ((* ((index (i_9 : (vec 2)) 1) : float) (27. : float)) :
                       float))
                     : (vec 4))
                    ((let hash_14
                      ((app (fract_0 : ((vec 4) -> (vec 4)))
                        ((*
                          ((sin
                            ((% (seed_13 : (vec 4)) (6.2831853 : float)) :
                             (vec 4)))
                           : (vec 4))
                          (200000. : float))
                         : (vec 4)))
                       : (vec 4))
                      ((let col0_15
                        ((vec2 ((index (hash_14 : (vec 4)) 0) : float)
                          ((index (hash_14 : (vec 4)) 1) : float))
                         : (vec 2))
                        ((let col1_16
                          ((vec2 ((index (hash_14 : (vec 4)) 2) : float)
                            ((index (hash_14 : (vec 4)) 3) : float))
                           : (vec 2))
                          ((let res_v_17
                            ((+
                              ((* (col0_15 : (vec 2))
                                ((- (1. : float)
                                  ((index (inter_11 : (vec 2)) 1) : float))
                                 : float))
                               : (vec 2))
                              ((* (col1_16 : (vec 2))
                                ((index (inter_11 : (vec 2)) 1) : float))
                               : (vec 2)))
                             : (vec 2))
                            ((dot (res_v_17 : (vec 2))
                              ((vec2
                                ((- (1. : float)
                                  ((index (inter_11 : (vec 2)) 0) : float))
                                 : float)
                                ((index (inter_11 : (vec 2)) 0) : float))
                               : (vec 2)))
                             : float))
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : float))
               : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec fractalNoise_18
        ((lambda (p_19 (vec 2))
          ((+
            ((+
              ((+
                ((*
                  ((app (smoothNoise_7 : ((vec 2) -> float)) (p_19 : (vec 2))) :
                   float)
                  (0.5333 : float))
                 : float)
                ((*
                  ((app (smoothNoise_7 : ((vec 2) -> float))
                    ((* (p_19 : (vec 2)) (2. : float)) : (vec 2)))
                   : float)
                  (0.2667 : float))
                 : float))
               : float)
              ((*
                ((app (smoothNoise_7 : ((vec 2) -> float))
                  ((* (p_19 : (vec 2)) (4. : float)) : (vec 2)))
                 : float)
                (0.1333 : float))
               : float))
             : float)
            ((*
              ((app (smoothNoise_7 : ((vec 2) -> float))
                ((* (p_19 : (vec 2)) (8. : float)) : (vec 2)))
               : float)
              (0.0667 : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec warpedNoise_20
        ((lambda (p_21 (vec 2))
          ((let m_22
            ((*
              ((vec2 (u_time : float)
                ((- (0. : float) (u_time : float)) : float))
               : (vec 2))
              (0.5 : float))
             : (vec 2))
            ((let x_23
              ((app (fractalNoise_18 : ((vec 2) -> float))
                ((+ (p_21 : (vec 2)) (m_22 : (vec 2))) : (vec 2)))
               : float)
              ((let y_24
                ((app (fractalNoise_18 : ((vec 2) -> float))
                  ((+
                    ((+ (p_21 : (vec 2))
                      ((vec2 ((index (m_22 : (vec 2)) 1) : float)
                        ((index (m_22 : (vec 2)) 0) : float))
                       : (vec 2)))
                     : (vec 2))
                    (x_23 : float))
                   : (vec 2)))
                 : float)
                ((let z_25
                  ((app (fractalNoise_18 : ((vec 2) -> float))
                    ((+
                      ((- ((- (p_21 : (vec 2)) (m_22 : (vec 2))) : (vec 2))
                        (x_23 : float))
                       : (vec 2))
                      (y_24 : float))
                     : (vec 2)))
                   : float)
                  ((let warp_26
                    ((+
                      ((+ ((vec2 (x_23 : float) (y_24 : float)) : (vec 2))
                        ((vec2 (y_24 : float) (z_25 : float)) : (vec 2)))
                       : (vec 2))
                      ((vec2 (z_25 : float) (x_23 : float)) : (vec 2)))
                     : (vec 2))
                    ((let mag_27
                      ((*
                        ((length
                          ((vec3 (x_23 : float) (y_24 : float) (z_25 : float)) :
                           (vec 3)))
                         : float)
                        (0.25 : float))
                       : float)
                      ((app (fractalNoise_18 : ((vec 2) -> float))
                        ((+ ((+ (p_21 : (vec 2)) (warp_26 : (vec 2))) : (vec 2))
                          (mag_27 : float))
                         : (vec 2)))
                       : float))
                     : float))
                   : float))
                 : float))
               : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec main
        ((lambda (coord_28 (vec 2))
          ((let uv_29
            ((/
              ((- (coord_28 : (vec 2))
                ((* (u_resolution : (vec 2)) (0.5 : float)) : (vec 2)))
               : (vec 2))
              ((index (u_resolution : (vec 2)) 1) : float))
             : (vec 2))
            ((let n_30
              ((app (warpedNoise_20 : ((vec 2) -> float))
                ((* (uv_29 : (vec 2)) (6. : float)) : (vec 2)))
               : float)
              ((let n2_31
                ((app (warpedNoise_20 : ((vec 2) -> float))
                  ((- ((* (uv_29 : (vec 2)) (6. : float)) : (vec 2))
                    (0.02 : float))
                   : (vec 2)))
                 : float)
                ((let bump_32
                  ((*
                    ((/
                      ((max ((- (n2_31 : float) (n_30 : float)) : float)
                        (0. : float))
                       : float)
                      (0.02 : float))
                     : float)
                    (0.7071 : float))
                   : float)
                  ((let bump2_33
                    ((*
                      ((/
                        ((max ((- (n_30 : float) (n2_31 : float)) : float)
                          (0. : float))
                         : float)
                        (0.02 : float))
                       : float)
                      (0.7071 : float))
                     : float)
                    ((let b1_34
                      ((+ ((* (bump_32 : float) (bump_32 : float)) : float)
                        ((* ((pow (bump_32 : float) (4. : float)) : float)
                          (0.5 : float))
                         : float))
                       : float)
                      ((let b2_35
                        ((+ ((* (bump2_33 : float) (bump2_33 : float)) : float)
                          ((* ((pow (bump2_33 : float) (4. : float)) : float)
                            (0.5 : float))
                           : float))
                         : float)
                        ((let base_col_36
                          ((+
                            ((*
                              ((*
                                ((vec3 (1. : float) (0.7 : float) (0.6 : float))
                                 : (vec 3))
                                ((vec3 (b1_34 : float)
                                  ((*
                                    ((+ (b1_34 : float) (b2_35 : float)) : float)
                                    (0.4 : float))
                                   : float)
                                  (b2_35 : float))
                                 : (vec 3)))
                               : (vec 3))
                              (0.3 : float))
                             : (vec 3))
                            (0.5 : float))
                           : (vec 3))
                          ((let col_37
                            ((* ((* (n_30 : float) (n_30 : float)) : float)
                              (base_col_36 : (vec 3)))
                             : (vec 3))
                            ((let spot1_dist_38
                              ((length
                                ((- (uv_29 : (vec 2)) (0.65 : float)) : (vec 2)))
                               : float)
                              ((let spot2_dist_39
                                ((length
                                  ((+ (uv_29 : (vec 2)) (0.5 : float)) : (vec 2)))
                                 : float)
                                ((let a_40
                                  ((*
                                    ((vec3 (0.8 : float) (0.4 : float)
                                      (1. : float))
                                     : (vec 3))
                                    (0.35 : float))
                                   : (vec 3))
                                  ((let b_41
                                    ((*
                                      ((vec3 (1. : float) (0.5 : float)
                                        (0.2 : float))
                                       : (vec 3))
                                      ((app
                                        ((app
                                          ((app
                                            (smoothstep_2 :
                                             (float ->
                                              (float -> (float -> float))))
                                            (0. : float))
                                           : (float -> (float -> float)))
                                          (1. : float))
                                         : (float -> float))
                                        ((- (1. : float) (spot1_dist_38 : float))
                                         : float))
                                       : float))
                                     : (vec 3))
                                    ((let c_42
                                      ((*
                                        ((vec3 (0.2 : float) (0.4 : float)
                                          (1. : float))
                                         : (vec 3))
                                        ((app
                                          ((app
                                            ((app
                                              (smoothstep_2 :
                                               (float ->
                                                (float -> (float -> float))))
                                              (0. : float))
                                             : (float -> (float -> float)))
                                            (1. : float))
                                           : (float -> float))
                                          ((- (1. : float)
                                            (spot2_dist_39 : float))
                                           : float))
                                         : float))
                                       : (vec 3))
                                      ((let spot_logic_43
                                        ((+ (a_40 : (vec 3))
                                          ((*
                                            ((+ (b_41 : (vec 3))
                                              (c_42 : (vec 3)))
                                             : (vec 3))
                                            (5. : float))
                                           : (vec 3)))
                                         : (vec 3))
                                        ((let final_col_44
                                          ((* (col_37 : (vec 3))
                                            (spot_logic_43 : (vec 3)))
                                           : (vec 3))
                                          ((sqrt
                                            ((max (final_col_44 : (vec 3))
                                              (0. : float))
                                             : (vec 3)))
                                           : (vec 3)))
                                         : (vec 3)))
                                       : (vec 3)))
                                     : (vec 3)))
                                   : (vec 3)))
                                 : (vec 3)))
                               : (vec 3)))
                             : (vec 3)))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (warped_noise.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec fract_0 (lambda ((v_1 (vec 4))) (- v_1 (floor v_1)))) :
       ((vec 4) -> (vec 4)))
      ((Define Nonrec smoothstep_2
        (lambda ((edge0_3 float) (edge1_4 float) (x_5 float))
         (let t_6 (clamp (/ (- x_5 edge0_3) (- edge1_4 edge0_3)) 0. 1.)
          (* (* t_6 t_6) (- 3. (* 2. t_6))))))
       : (float -> (float -> (float -> float))))
      ((Define Nonrec smoothNoise_7
        (lambda ((p_8 (vec 2)))
         (let i_9 (floor p_8)
          (let pf_10 (- p_8 i_9)
           (let inter_11 (* (* pf_10 pf_10) (- 3. (* 2. pf_10)))
            (let v4_12 (vec4 0. 1. 27. 28.)
             (let seed_13 (+ (+ v4_12 (index i_9 0)) (* (index i_9 1) 27.))
              (let hash_14 (app fract_0 (* (sin (% seed_13 6.2831853)) 200000.))
               (let col0_15 (vec2 (index hash_14 0) (index hash_14 1))
                (let col1_16 (vec2 (index hash_14 2) (index hash_14 3))
                 (let res_v_17
                  (+ (* col0_15 (- 1. (index inter_11 1)))
                   (* col1_16 (index inter_11 1)))
                  (dot res_v_17
                   (vec2 (- 1. (index inter_11 0)) (index inter_11 0))))))))))))))
       : ((vec 2) -> float))
      ((Define Nonrec fractalNoise_18
        (lambda ((p_19 (vec 2)))
         (+
          (+
           (+ (* (app smoothNoise_7 p_19) 0.5333)
            (* (app smoothNoise_7 (* p_19 2.)) 0.2667))
           (* (app smoothNoise_7 (* p_19 4.)) 0.1333))
          (* (app smoothNoise_7 (* p_19 8.)) 0.0667))))
       : ((vec 2) -> float))
      ((Define Nonrec warpedNoise_20
        (lambda ((p_21 (vec 2)))
         (let m_22 (* (vec2 u_time (- 0. u_time)) 0.5)
          (let x_23 (app fractalNoise_18 (+ p_21 m_22))
           (let y_24
            (app fractalNoise_18
             (+ (+ p_21 (vec2 (index m_22 1) (index m_22 0))) x_23))
            (let z_25 (app fractalNoise_18 (+ (- (- p_21 m_22) x_23) y_24))
             (let warp_26
              (+ (+ (vec2 x_23 y_24) (vec2 y_24 z_25)) (vec2 z_25 x_23))
              (let mag_27 (* (length (vec3 x_23 y_24 z_25)) 0.25)
               (app fractalNoise_18 (+ (+ p_21 warp_26) mag_27))))))))))
       : ((vec 2) -> float))
      ((Define Nonrec main
        (lambda ((coord_28 (vec 2)))
         (let uv_29 (/ (- coord_28 (* u_resolution 0.5)) (index u_resolution 1))
          (let n_30 (app warpedNoise_20 (* uv_29 6.))
           (let n2_31 (app warpedNoise_20 (- (* uv_29 6.) 0.02))
            (let bump_32 (* (/ (max (- n2_31 n_30) 0.) 0.02) 0.7071)
             (let bump2_33 (* (/ (max (- n_30 n2_31) 0.) 0.02) 0.7071)
              (let b1_34 (+ (* bump_32 bump_32) (* (pow bump_32 4.) 0.5))
               (let b2_35 (+ (* bump2_33 bump2_33) (* (pow bump2_33 4.) 0.5))
                (let base_col_36
                 (+
                  (*
                   (* (vec3 1. 0.7 0.6)
                    (vec3 b1_34 (* (+ b1_34 b2_35) 0.4) b2_35))
                   0.3)
                  0.5)
                 (let col_37 (* (* n_30 n_30) base_col_36)
                  (let spot1_dist_38 (length (- uv_29 0.65))
                   (let spot2_dist_39 (length (+ uv_29 0.5))
                    (let a_40 (* (vec3 0.8 0.4 1.) 0.35)
                     (let b_41
                      (* (vec3 1. 0.5 0.2)
                       (app smoothstep_2 0. 1. (- 1. spot1_dist_38)))
                      (let c_42
                       (* (vec3 0.2 0.4 1.)
                        (app smoothstep_2 0. 1. (- 1. spot2_dist_39)))
                       (let spot_logic_43 (+ a_40 (* (+ b_41 c_42) 5.))
                        (let final_col_44 (* col_37 spot_logic_43)
                         (sqrt (max final_col_44 0.))))))))))))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda_lift (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name fract_0) (args ((v_1 (vec 4))))
       (body (- v_1 (floor v_1))))
      : ((vec 4) -> (vec 4)))
     ((Define Nonrec (name smoothstep_2)
       (args ((edge0_3 float) (edge1_4 float) (x_5 float)))
       (body
        (let t_6 (clamp (/ (- x_5 edge0_3) (- edge1_4 edge0_3)) 0. 1.)
         (* (* t_6 t_6) (- 3. (* 2. t_6))))))
      : (float -> (float -> (float -> float))))
     ((Define Nonrec (name smoothNoise_7) (args ((p_8 (vec 2))))
       (body
        (let i_9 (floor p_8)
         (let pf_10 (- p_8 i_9)
          (let inter_11 (* (* pf_10 pf_10) (- 3. (* 2. pf_10)))
           (let v4_12 (vec4 0. 1. 27. 28.)
            (let seed_13 (+ (+ v4_12 (index i_9 0)) (* (index i_9 1) 27.))
             (let hash_14 (app fract_0 (* (sin (% seed_13 6.2831853)) 200000.))
              (let col0_15 (vec2 (index hash_14 0) (index hash_14 1))
               (let col1_16 (vec2 (index hash_14 2) (index hash_14 3))
                (let res_v_17
                 (+ (* col0_15 (- 1. (index inter_11 1)))
                  (* col1_16 (index inter_11 1)))
                 (dot res_v_17
                  (vec2 (- 1. (index inter_11 0)) (index inter_11 0))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name fractalNoise_18) (args ((p_19 (vec 2))))
       (body
        (+
         (+
          (+ (* (app smoothNoise_7 p_19) 0.5333)
           (* (app smoothNoise_7 (* p_19 2.)) 0.2667))
          (* (app smoothNoise_7 (* p_19 4.)) 0.1333))
         (* (app smoothNoise_7 (* p_19 8.)) 0.0667))))
      : ((vec 2) -> float))
     ((Define Nonrec (name warpedNoise_20) (args ((p_21 (vec 2))))
       (body
        (let m_22 (* (vec2 u_time (- 0. u_time)) 0.5)
         (let x_23 (app fractalNoise_18 (+ p_21 m_22))
          (let y_24
           (app fractalNoise_18
            (+ (+ p_21 (vec2 (index m_22 1) (index m_22 0))) x_23))
           (let z_25 (app fractalNoise_18 (+ (- (- p_21 m_22) x_23) y_24))
            (let warp_26
             (+ (+ (vec2 x_23 y_24) (vec2 y_24 z_25)) (vec2 z_25 x_23))
             (let mag_27 (* (length (vec3 x_23 y_24 z_25)) 0.25)
              (app fractalNoise_18 (+ (+ p_21 warp_26) mag_27))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name main) (args ((coord_28 (vec 2))))
       (body
        (let uv_29 (/ (- coord_28 (* u_resolution 0.5)) (index u_resolution 1))
         (let n_30 (app warpedNoise_20 (* uv_29 6.))
          (let n2_31 (app warpedNoise_20 (- (* uv_29 6.) 0.02))
           (let bump_32 (* (/ (max (- n2_31 n_30) 0.) 0.02) 0.7071)
            (let bump2_33 (* (/ (max (- n_30 n2_31) 0.) 0.02) 0.7071)
             (let b1_34 (+ (* bump_32 bump_32) (* (pow bump_32 4.) 0.5))
              (let b2_35 (+ (* bump2_33 bump2_33) (* (pow bump2_33 4.) 0.5))
               (let base_col_36
                (+
                 (*
                  (* (vec3 1. 0.7 0.6)
                   (vec3 b1_34 (* (+ b1_34 b2_35) 0.4) b2_35))
                  0.3)
                 0.5)
                (let col_37 (* (* n_30 n_30) base_col_36)
                 (let spot1_dist_38 (length (- uv_29 0.65))
                  (let spot2_dist_39 (length (+ uv_29 0.5))
                   (let a_40 (* (vec3 0.8 0.4 1.) 0.35)
                    (let b_41
                     (* (vec3 1. 0.5 0.2)
                      (app smoothstep_2 0. 1. (- 1. spot1_dist_38)))
                     (let c_42
                      (* (vec3 0.2 0.4 1.)
                       (app smoothstep_2 0. 1. (- 1. spot2_dist_39)))
                      (let spot_logic_43 (+ a_40 (* (+ b_41 c_42) 5.))
                       (let final_col_44 (* col_37 spot_logic_43)
                        (sqrt (max final_col_44 0.))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === anf (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name fract_0) (args ((v_1 (vec 4))))
       (body (let anf_45 (floor v_1) (return (- v_1 anf_45)))))
      : ((vec 4) -> (vec 4)))
     ((Define Nonrec (name smoothstep_2)
       (args ((edge0_3 float) (edge1_4 float) (x_5 float)))
       (body
        (let anf_46 (- x_5 edge0_3)
         (let anf_47 (- edge1_4 edge0_3)
          (let anf_48 (/ anf_46 anf_47)
           (let t_6 (clamp anf_48 0. 1.)
            (let anf_49 (* t_6 t_6)
             (let anf_50 (* 2. t_6)
              (let anf_51 (- 3. anf_50) (return (* anf_49 anf_51)))))))))))
      : (float -> (float -> (float -> float))))
     ((Define Nonrec (name smoothNoise_7) (args ((p_8 (vec 2))))
       (body
        (let i_9 (floor p_8)
         (let pf_10 (- p_8 i_9)
          (let anf_52 (* pf_10 pf_10)
           (let anf_53 (* 2. pf_10)
            (let anf_54 (- 3. anf_53)
             (let inter_11 (* anf_52 anf_54)
              (let v4_12 (vec4 0. 1. 27. 28.)
               (let anf_55 (index i_9 0)
                (let anf_56 (+ v4_12 anf_55)
                 (let anf_57 (index i_9 1)
                  (let anf_58 (* anf_57 27.)
                   (let seed_13 (+ anf_56 anf_58)
                    (let anf_59 (% seed_13 6.2831853)
                     (let anf_60 (sin anf_59)
                      (let anf_61 (* anf_60 200000.)
                       (let hash_14 (fract_0 anf_61)
                        (let anf_62 (index hash_14 0)
                         (let anf_63 (index hash_14 1)
                          (let col0_15 (vec2 anf_62 anf_63)
                           (let anf_64 (index hash_14 2)
                            (let anf_65 (index hash_14 3)
                             (let col1_16 (vec2 anf_64 anf_65)
                              (let anf_66 (index inter_11 1)
                               (let anf_67 (- 1. anf_66)
                                (let anf_68 (* col0_15 anf_67)
                                 (let anf_69 (index inter_11 1)
                                  (let anf_70 (* col1_16 anf_69)
                                   (let res_v_17 (+ anf_68 anf_70)
                                    (let anf_71 (index inter_11 0)
                                     (let anf_72 (- 1. anf_71)
                                      (let anf_73 (index inter_11 0)
                                       (let anf_74 (vec2 anf_72 anf_73)
                                        (return (dot res_v_17 anf_74))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name fractalNoise_18) (args ((p_19 (vec 2))))
       (body
        (let anf_75 (smoothNoise_7 p_19)
         (let anf_76 (* anf_75 0.5333)
          (let anf_77 (* p_19 2.)
           (let anf_78 (smoothNoise_7 anf_77)
            (let anf_79 (* anf_78 0.2667)
             (let anf_80 (+ anf_76 anf_79)
              (let anf_81 (* p_19 4.)
               (let anf_82 (smoothNoise_7 anf_81)
                (let anf_83 (* anf_82 0.1333)
                 (let anf_84 (+ anf_80 anf_83)
                  (let anf_85 (* p_19 8.)
                   (let anf_86 (smoothNoise_7 anf_85)
                    (let anf_87 (* anf_86 0.0667) (return (+ anf_84 anf_87)))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name warpedNoise_20) (args ((p_21 (vec 2))))
       (body
        (let anf_88 (- 0. u_time)
         (let anf_89 (vec2 u_time anf_88)
          (let m_22 (* anf_89 0.5)
           (let anf_90 (+ p_21 m_22)
            (let x_23 (fractalNoise_18 anf_90)
             (let anf_91 (index m_22 1)
              (let anf_92 (index m_22 0)
               (let anf_93 (vec2 anf_91 anf_92)
                (let anf_94 (+ p_21 anf_93)
                 (let anf_95 (+ anf_94 x_23)
                  (let y_24 (fractalNoise_18 anf_95)
                   (let anf_96 (- p_21 m_22)
                    (let anf_97 (- anf_96 x_23)
                     (let anf_98 (+ anf_97 y_24)
                      (let z_25 (fractalNoise_18 anf_98)
                       (let anf_99 (vec2 x_23 y_24)
                        (let anf_100 (vec2 y_24 z_25)
                         (let anf_101 (+ anf_99 anf_100)
                          (let anf_102 (vec2 z_25 x_23)
                           (let warp_26 (+ anf_101 anf_102)
                            (let anf_103 (vec3 x_23 y_24 z_25)
                             (let anf_104 (length anf_103)
                              (let mag_27 (* anf_104 0.25)
                               (let anf_105 (+ p_21 warp_26)
                                (let anf_106 (+ anf_105 mag_27)
                                 (return (fractalNoise_18 anf_106)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name main) (args ((coord_28 (vec 2))))
       (body
        (let anf_107 (* u_resolution 0.5)
         (let anf_108 (- coord_28 anf_107)
          (let anf_109 (index u_resolution 1)
           (let uv_29 (/ anf_108 anf_109)
            (let anf_110 (* uv_29 6.)
             (let n_30 (warpedNoise_20 anf_110)
              (let anf_111 (* uv_29 6.)
               (let anf_112 (- anf_111 0.02)
                (let n2_31 (warpedNoise_20 anf_112)
                 (let anf_113 (- n2_31 n_30)
                  (let anf_114 (max anf_113 0.)
                   (let anf_115 (/ anf_114 0.02)
                    (let bump_32 (* anf_115 0.7071)
                     (let anf_116 (- n_30 n2_31)
                      (let anf_117 (max anf_116 0.)
                       (let anf_118 (/ anf_117 0.02)
                        (let bump2_33 (* anf_118 0.7071)
                         (let anf_119 (* bump_32 bump_32)
                          (let anf_120 (pow bump_32 4.)
                           (let anf_121 (* anf_120 0.5)
                            (let b1_34 (+ anf_119 anf_121)
                             (let anf_122 (* bump2_33 bump2_33)
                              (let anf_123 (pow bump2_33 4.)
                               (let anf_124 (* anf_123 0.5)
                                (let b2_35 (+ anf_122 anf_124)
                                 (let anf_125 (vec3 1. 0.7 0.6)
                                  (let anf_126 (+ b1_34 b2_35)
                                   (let anf_127 (* anf_126 0.4)
                                    (let anf_128 (vec3 b1_34 anf_127 b2_35)
                                     (let anf_129 (* anf_125 anf_128)
                                      (let anf_130 (* anf_129 0.3)
                                       (let base_col_36 (+ anf_130 0.5)
                                        (let anf_131 (* n_30 n_30)
                                         (let col_37 (* anf_131 base_col_36)
                                          (let anf_132 (- uv_29 0.65)
                                           (let spot1_dist_38 (length anf_132)
                                            (let anf_133 (+ uv_29 0.5)
                                             (let spot2_dist_39 (length anf_133)
                                              (let anf_134 (vec3 0.8 0.4 1.)
                                               (let a_40 (* anf_134 0.35)
                                                (let anf_135 (vec3 1. 0.5 0.2)
                                                 (let anf_136
                                                  (- 1. spot1_dist_38)
                                                  (let anf_137
                                                   (smoothstep_2 0. 1. anf_136)
                                                   (let b_41 (* anf_135 anf_137)
                                                    (let anf_138
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_139
                                                      (- 1. spot2_dist_39)
                                                      (let anf_140
                                                       (smoothstep_2 0. 1.
                                                        anf_139)
                                                       (let c_42
                                                        (* anf_138 anf_140)
                                                        (let anf_141
                                                         (+ b_41 c_42)
                                                         (let anf_142
                                                          (* anf_141 5.)
                                                          (let spot_logic_43
                                                           (+ a_40 anf_142)
                                                           (let final_col_44
                                                            (* col_37
                                                             spot_logic_43)
                                                            (let anf_143
                                                             (max final_col_44
                                                              0.)
                                                             (return
                                                              (sqrt anf_143)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name fract_0) (args ((v_1 (vec 4))))
       (body (let anf_45 (floor v_1) (return (- v_1 anf_45)))))
      : ((vec 4) -> (vec 4)))
     ((Define (name smoothstep_2)
       (args ((edge0_3 float) (edge1_4 float) (x_5 float)))
       (body
        (let anf_46 (- x_5 edge0_3)
         (let anf_47 (- edge1_4 edge0_3)
          (let anf_48 (/ anf_46 anf_47)
           (let t_6 (clamp anf_48 0. 1.)
            (let anf_49 (* t_6 t_6)
             (let anf_50 (* 2. t_6)
              (let anf_51 (- 3. anf_50) (return (* anf_49 anf_51)))))))))))
      : (float -> (float -> (float -> float))))
     ((Define (name smoothNoise_7) (args ((p_8 (vec 2))))
       (body
        (let i_9 (floor p_8)
         (let pf_10 (- p_8 i_9)
          (let anf_52 (* pf_10 pf_10)
           (let anf_53 (* 2. pf_10)
            (let anf_54 (- 3. anf_53)
             (let inter_11 (* anf_52 anf_54)
              (let v4_12 (vec4 0. 1. 27. 28.)
               (let anf_55 (index i_9 0)
                (let anf_56 (+ v4_12 anf_55)
                 (let anf_57 (index i_9 1)
                  (let anf_58 (* anf_57 27.)
                   (let seed_13 (+ anf_56 anf_58)
                    (let anf_59 (% seed_13 6.2831853)
                     (let anf_60 (sin anf_59)
                      (let anf_61 (* anf_60 200000.)
                       (let hash_14 (fract_0 anf_61)
                        (let anf_62 (index hash_14 0)
                         (let anf_63 (index hash_14 1)
                          (let col0_15 (vec2 anf_62 anf_63)
                           (let anf_64 (index hash_14 2)
                            (let anf_65 (index hash_14 3)
                             (let col1_16 (vec2 anf_64 anf_65)
                              (let anf_66 (index inter_11 1)
                               (let anf_67 (- 1. anf_66)
                                (let anf_68 (* col0_15 anf_67)
                                 (let anf_69 (index inter_11 1)
                                  (let anf_70 (* col1_16 anf_69)
                                   (let res_v_17 (+ anf_68 anf_70)
                                    (let anf_71 (index inter_11 0)
                                     (let anf_72 (- 1. anf_71)
                                      (let anf_73 (index inter_11 0)
                                       (let anf_74 (vec2 anf_72 anf_73)
                                        (return (dot res_v_17 anf_74))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name fractalNoise_18) (args ((p_19 (vec 2))))
       (body
        (let anf_75 (smoothNoise_7 p_19)
         (let anf_76 (* anf_75 0.5333)
          (let anf_77 (* p_19 2.)
           (let anf_78 (smoothNoise_7 anf_77)
            (let anf_79 (* anf_78 0.2667)
             (let anf_80 (+ anf_76 anf_79)
              (let anf_81 (* p_19 4.)
               (let anf_82 (smoothNoise_7 anf_81)
                (let anf_83 (* anf_82 0.1333)
                 (let anf_84 (+ anf_80 anf_83)
                  (let anf_85 (* p_19 8.)
                   (let anf_86 (smoothNoise_7 anf_85)
                    (let anf_87 (* anf_86 0.0667) (return (+ anf_84 anf_87)))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name warpedNoise_20) (args ((p_21 (vec 2))))
       (body
        (let anf_88 (- 0. u_time)
         (let anf_89 (vec2 u_time anf_88)
          (let m_22 (* anf_89 0.5)
           (let anf_90 (+ p_21 m_22)
            (let x_23 (fractalNoise_18 anf_90)
             (let anf_91 (index m_22 1)
              (let anf_92 (index m_22 0)
               (let anf_93 (vec2 anf_91 anf_92)
                (let anf_94 (+ p_21 anf_93)
                 (let anf_95 (+ anf_94 x_23)
                  (let y_24 (fractalNoise_18 anf_95)
                   (let anf_96 (- p_21 m_22)
                    (let anf_97 (- anf_96 x_23)
                     (let anf_98 (+ anf_97 y_24)
                      (let z_25 (fractalNoise_18 anf_98)
                       (let anf_99 (vec2 x_23 y_24)
                        (let anf_100 (vec2 y_24 z_25)
                         (let anf_101 (+ anf_99 anf_100)
                          (let anf_102 (vec2 z_25 x_23)
                           (let warp_26 (+ anf_101 anf_102)
                            (let anf_103 (vec3 x_23 y_24 z_25)
                             (let anf_104 (length anf_103)
                              (let mag_27 (* anf_104 0.25)
                               (let anf_105 (+ p_21 warp_26)
                                (let anf_106 (+ anf_105 mag_27)
                                 (return (fractalNoise_18 anf_106)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name main) (args ((coord_28 (vec 2))))
       (body
        (let anf_107 (* u_resolution 0.5)
         (let anf_108 (- coord_28 anf_107)
          (let anf_109 (index u_resolution 1)
           (let uv_29 (/ anf_108 anf_109)
            (let anf_110 (* uv_29 6.)
             (let n_30 (warpedNoise_20 anf_110)
              (let anf_111 (* uv_29 6.)
               (let anf_112 (- anf_111 0.02)
                (let n2_31 (warpedNoise_20 anf_112)
                 (let anf_113 (- n2_31 n_30)
                  (let anf_114 (max anf_113 0.)
                   (let anf_115 (/ anf_114 0.02)
                    (let bump_32 (* anf_115 0.7071)
                     (let anf_116 (- n_30 n2_31)
                      (let anf_117 (max anf_116 0.)
                       (let anf_118 (/ anf_117 0.02)
                        (let bump2_33 (* anf_118 0.7071)
                         (let anf_119 (* bump_32 bump_32)
                          (let anf_120 (pow bump_32 4.)
                           (let anf_121 (* anf_120 0.5)
                            (let b1_34 (+ anf_119 anf_121)
                             (let anf_122 (* bump2_33 bump2_33)
                              (let anf_123 (pow bump2_33 4.)
                               (let anf_124 (* anf_123 0.5)
                                (let b2_35 (+ anf_122 anf_124)
                                 (let anf_125 (vec3 1. 0.7 0.6)
                                  (let anf_126 (+ b1_34 b2_35)
                                   (let anf_127 (* anf_126 0.4)
                                    (let anf_128 (vec3 b1_34 anf_127 b2_35)
                                     (let anf_129 (* anf_125 anf_128)
                                      (let anf_130 (* anf_129 0.3)
                                       (let base_col_36 (+ anf_130 0.5)
                                        (let anf_131 (* n_30 n_30)
                                         (let col_37 (* anf_131 base_col_36)
                                          (let anf_132 (- uv_29 0.65)
                                           (let spot1_dist_38 (length anf_132)
                                            (let anf_133 (+ uv_29 0.5)
                                             (let spot2_dist_39 (length anf_133)
                                              (let anf_134 (vec3 0.8 0.4 1.)
                                               (let a_40 (* anf_134 0.35)
                                                (let anf_135 (vec3 1. 0.5 0.2)
                                                 (let anf_136
                                                  (- 1. spot1_dist_38)
                                                  (let anf_137
                                                   (smoothstep_2 0. 1. anf_136)
                                                   (let b_41 (* anf_135 anf_137)
                                                    (let anf_138
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_139
                                                      (- 1. spot2_dist_39)
                                                      (let anf_140
                                                       (smoothstep_2 0. 1.
                                                        anf_139)
                                                       (let c_42
                                                        (* anf_138 anf_140)
                                                        (let anf_141
                                                         (+ b_41 c_42)
                                                         (let anf_142
                                                          (* anf_141 5.)
                                                          (let spot_logic_43
                                                           (+ a_40 anf_142)
                                                           (let final_col_44
                                                            (* col_37
                                                             spot_logic_43)
                                                            (let anf_143
                                                             (max final_col_44
                                                              0.)
                                                             (return
                                                              (sqrt anf_143)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (warped_noise.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name fract_0) (desc ()) (params (((TyVec 4) v_1)))
       (ret_type (TyVec 4))
       (body ((set () vec4 anf_45 (floor v_1)) (return (- v_1 anf_45)))))
      (Function (name smoothstep_2) (desc ())
       (params ((TyFloat edge0_3) (TyFloat edge1_4) (TyFloat x_5)))
       (ret_type TyFloat)
       (body
        ((set () float anf_46 (- x_5 edge0_3))
         (set () float anf_47 (- edge1_4 edge0_3))
         (set () float anf_48 (/ anf_46 anf_47))
         (set () float t_6 (clamp anf_48 0. 1.))
         (set () float anf_49 (* t_6 t_6)) (set () float anf_50 (* 2. t_6))
         (set () float anf_51 (- 3. anf_50)) (return (* anf_49 anf_51)))))
      (Function (name smoothNoise_7) (desc ()) (params (((TyVec 2) p_8)))
       (ret_type TyFloat)
       (body
        ((set () vec2 i_9 (floor p_8)) (set () vec2 pf_10 (- p_8 i_9))
         (set () vec2 anf_52 (* pf_10 pf_10)) (set () vec2 anf_53 (* 2. pf_10))
         (set () vec2 anf_54 (- 3. anf_53))
         (set () vec2 inter_11 (* anf_52 anf_54))
         (set () vec4 v4_12 (vec4 0. 1. 27. 28.))
         (set () float anf_55 (index i_9 0))
         (set () vec4 anf_56 (+ v4_12 anf_55))
         (set () float anf_57 (index i_9 1)) (set () float anf_58 (* anf_57 27.))
         (set () vec4 seed_13 (+ anf_56 anf_58))
         (set () vec4 anf_59 (% seed_13 6.2831853))
         (set () vec4 anf_60 (sin anf_59))
         (set () vec4 anf_61 (* anf_60 200000.))
         (set () vec4 hash_14 (fract_0 anf_61))
         (set () float anf_62 (index hash_14 0))
         (set () float anf_63 (index hash_14 1))
         (set () vec2 col0_15 (vec2 anf_62 anf_63))
         (set () float anf_64 (index hash_14 2))
         (set () float anf_65 (index hash_14 3))
         (set () vec2 col1_16 (vec2 anf_64 anf_65))
         (set () float anf_66 (index inter_11 1))
         (set () float anf_67 (- 1. anf_66))
         (set () vec2 anf_68 (* col0_15 anf_67))
         (set () float anf_69 (index inter_11 1))
         (set () vec2 anf_70 (* col1_16 anf_69))
         (set () vec2 res_v_17 (+ anf_68 anf_70))
         (set () float anf_71 (index inter_11 0))
         (set () float anf_72 (- 1. anf_71))
         (set () float anf_73 (index inter_11 0))
         (set () vec2 anf_74 (vec2 anf_72 anf_73))
         (return (dot res_v_17 anf_74)))))
      (Function (name fractalNoise_18) (desc ()) (params (((TyVec 2) p_19)))
       (ret_type TyFloat)
       (body
        ((set () float anf_75 (smoothNoise_7 p_19))
         (set () float anf_76 (* anf_75 0.5333)) (set () vec2 anf_77 (* p_19 2.))
         (set () float anf_78 (smoothNoise_7 anf_77))
         (set () float anf_79 (* anf_78 0.2667))
         (set () float anf_80 (+ anf_76 anf_79)) (set () vec2 anf_81 (* p_19 4.))
         (set () float anf_82 (smoothNoise_7 anf_81))
         (set () float anf_83 (* anf_82 0.1333))
         (set () float anf_84 (+ anf_80 anf_83)) (set () vec2 anf_85 (* p_19 8.))
         (set () float anf_86 (smoothNoise_7 anf_85))
         (set () float anf_87 (* anf_86 0.0667)) (return (+ anf_84 anf_87)))))
      (Function (name warpedNoise_20) (desc ()) (params (((TyVec 2) p_21)))
       (ret_type TyFloat)
       (body
        ((set () float anf_88 (- 0. u_time))
         (set () vec2 anf_89 (vec2 u_time anf_88))
         (set () vec2 m_22 (* anf_89 0.5)) (set () vec2 anf_90 (+ p_21 m_22))
         (set () float x_23 (fractalNoise_18 anf_90))
         (set () float anf_91 (index m_22 1))
         (set () float anf_92 (index m_22 0))
         (set () vec2 anf_93 (vec2 anf_91 anf_92))
         (set () vec2 anf_94 (+ p_21 anf_93))
         (set () vec2 anf_95 (+ anf_94 x_23))
         (set () float y_24 (fractalNoise_18 anf_95))
         (set () vec2 anf_96 (- p_21 m_22)) (set () vec2 anf_97 (- anf_96 x_23))
         (set () vec2 anf_98 (+ anf_97 y_24))
         (set () float z_25 (fractalNoise_18 anf_98))
         (set () vec2 anf_99 (vec2 x_23 y_24))
         (set () vec2 anf_100 (vec2 y_24 z_25))
         (set () vec2 anf_101 (+ anf_99 anf_100))
         (set () vec2 anf_102 (vec2 z_25 x_23))
         (set () vec2 warp_26 (+ anf_101 anf_102))
         (set () vec3 anf_103 (vec3 x_23 y_24 z_25))
         (set () float anf_104 (length anf_103))
         (set () float mag_27 (* anf_104 0.25))
         (set () vec2 anf_105 (+ p_21 warp_26))
         (set () vec2 anf_106 (+ anf_105 mag_27))
         (return (fractalNoise_18 anf_106)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_28)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 anf_107 (* u_resolution 0.5))
         (set () vec2 anf_108 (- coord_28 anf_107))
         (set () float anf_109 (index u_resolution 1))
         (set () vec2 uv_29 (/ anf_108 anf_109))
         (set () vec2 anf_110 (* uv_29 6.))
         (set () float n_30 (warpedNoise_20 anf_110))
         (set () vec2 anf_111 (* uv_29 6.))
         (set () vec2 anf_112 (- anf_111 0.02))
         (set () float n2_31 (warpedNoise_20 anf_112))
         (set () float anf_113 (- n2_31 n_30))
         (set () float anf_114 (max anf_113 0.))
         (set () float anf_115 (/ anf_114 0.02))
         (set () float bump_32 (* anf_115 0.7071))
         (set () float anf_116 (- n_30 n2_31))
         (set () float anf_117 (max anf_116 0.))
         (set () float anf_118 (/ anf_117 0.02))
         (set () float bump2_33 (* anf_118 0.7071))
         (set () float anf_119 (* bump_32 bump_32))
         (set () float anf_120 (pow bump_32 4.))
         (set () float anf_121 (* anf_120 0.5))
         (set () float b1_34 (+ anf_119 anf_121))
         (set () float anf_122 (* bump2_33 bump2_33))
         (set () float anf_123 (pow bump2_33 4.))
         (set () float anf_124 (* anf_123 0.5))
         (set () float b2_35 (+ anf_122 anf_124))
         (set () vec3 anf_125 (vec3 1. 0.7 0.6))
         (set () float anf_126 (+ b1_34 b2_35))
         (set () float anf_127 (* anf_126 0.4))
         (set () vec3 anf_128 (vec3 b1_34 anf_127 b2_35))
         (set () vec3 anf_129 (* anf_125 anf_128))
         (set () vec3 anf_130 (* anf_129 0.3))
         (set () vec3 base_col_36 (+ anf_130 0.5))
         (set () float anf_131 (* n_30 n_30))
         (set () vec3 col_37 (* anf_131 base_col_36))
         (set () vec2 anf_132 (- uv_29 0.65))
         (set () float spot1_dist_38 (length anf_132))
         (set () vec2 anf_133 (+ uv_29 0.5))
         (set () float spot2_dist_39 (length anf_133))
         (set () vec3 anf_134 (vec3 0.8 0.4 1.))
         (set () vec3 a_40 (* anf_134 0.35))
         (set () vec3 anf_135 (vec3 1. 0.5 0.2))
         (set () float anf_136 (- 1. spot1_dist_38))
         (set () float anf_137 (smoothstep_2 0. 1. anf_136))
         (set () vec3 b_41 (* anf_135 anf_137))
         (set () vec3 anf_138 (vec3 0.2 0.4 1.))
         (set () float anf_139 (- 1. spot2_dist_39))
         (set () float anf_140 (smoothstep_2 0. 1. anf_139))
         (set () vec3 c_42 (* anf_138 anf_140))
         (set () vec3 anf_141 (+ b_41 c_42)) (set () vec3 anf_142 (* anf_141 5.))
         (set () vec3 spot_logic_43 (+ a_40 anf_142))
         (set () vec3 final_col_44 (* col_37 spot_logic_43))
         (set () vec3 anf_143 (max final_col_44 0.)) (return (sqrt anf_143)))))))

    === patch_main (warped_noise.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name fract_0) (desc ()) (params (((TyVec 4) v_1)))
       (ret_type (TyVec 4))
       (body ((set () vec4 anf_45 (floor v_1)) (return (- v_1 anf_45)))))
      (Function (name smoothstep_2) (desc ())
       (params ((TyFloat edge0_3) (TyFloat edge1_4) (TyFloat x_5)))
       (ret_type TyFloat)
       (body
        ((set () float anf_46 (- x_5 edge0_3))
         (set () float anf_47 (- edge1_4 edge0_3))
         (set () float anf_48 (/ anf_46 anf_47))
         (set () float t_6 (clamp anf_48 0. 1.))
         (set () float anf_49 (* t_6 t_6)) (set () float anf_50 (* 2. t_6))
         (set () float anf_51 (- 3. anf_50)) (return (* anf_49 anf_51)))))
      (Function (name smoothNoise_7) (desc ()) (params (((TyVec 2) p_8)))
       (ret_type TyFloat)
       (body
        ((set () vec2 i_9 (floor p_8)) (set () vec2 pf_10 (- p_8 i_9))
         (set () vec2 anf_52 (* pf_10 pf_10)) (set () vec2 anf_53 (* 2. pf_10))
         (set () vec2 anf_54 (- 3. anf_53))
         (set () vec2 inter_11 (* anf_52 anf_54))
         (set () vec4 v4_12 (vec4 0. 1. 27. 28.))
         (set () float anf_55 (index i_9 0))
         (set () vec4 anf_56 (+ v4_12 anf_55))
         (set () float anf_57 (index i_9 1)) (set () float anf_58 (* anf_57 27.))
         (set () vec4 seed_13 (+ anf_56 anf_58))
         (set () vec4 anf_59 (% seed_13 6.2831853))
         (set () vec4 anf_60 (sin anf_59))
         (set () vec4 anf_61 (* anf_60 200000.))
         (set () vec4 hash_14 (fract_0 anf_61))
         (set () float anf_62 (index hash_14 0))
         (set () float anf_63 (index hash_14 1))
         (set () vec2 col0_15 (vec2 anf_62 anf_63))
         (set () float anf_64 (index hash_14 2))
         (set () float anf_65 (index hash_14 3))
         (set () vec2 col1_16 (vec2 anf_64 anf_65))
         (set () float anf_66 (index inter_11 1))
         (set () float anf_67 (- 1. anf_66))
         (set () vec2 anf_68 (* col0_15 anf_67))
         (set () float anf_69 (index inter_11 1))
         (set () vec2 anf_70 (* col1_16 anf_69))
         (set () vec2 res_v_17 (+ anf_68 anf_70))
         (set () float anf_71 (index inter_11 0))
         (set () float anf_72 (- 1. anf_71))
         (set () float anf_73 (index inter_11 0))
         (set () vec2 anf_74 (vec2 anf_72 anf_73))
         (return (dot res_v_17 anf_74)))))
      (Function (name fractalNoise_18) (desc ()) (params (((TyVec 2) p_19)))
       (ret_type TyFloat)
       (body
        ((set () float anf_75 (smoothNoise_7 p_19))
         (set () float anf_76 (* anf_75 0.5333)) (set () vec2 anf_77 (* p_19 2.))
         (set () float anf_78 (smoothNoise_7 anf_77))
         (set () float anf_79 (* anf_78 0.2667))
         (set () float anf_80 (+ anf_76 anf_79)) (set () vec2 anf_81 (* p_19 4.))
         (set () float anf_82 (smoothNoise_7 anf_81))
         (set () float anf_83 (* anf_82 0.1333))
         (set () float anf_84 (+ anf_80 anf_83)) (set () vec2 anf_85 (* p_19 8.))
         (set () float anf_86 (smoothNoise_7 anf_85))
         (set () float anf_87 (* anf_86 0.0667)) (return (+ anf_84 anf_87)))))
      (Function (name warpedNoise_20) (desc ()) (params (((TyVec 2) p_21)))
       (ret_type TyFloat)
       (body
        ((set () float anf_88 (- 0. u_time))
         (set () vec2 anf_89 (vec2 u_time anf_88))
         (set () vec2 m_22 (* anf_89 0.5)) (set () vec2 anf_90 (+ p_21 m_22))
         (set () float x_23 (fractalNoise_18 anf_90))
         (set () float anf_91 (index m_22 1))
         (set () float anf_92 (index m_22 0))
         (set () vec2 anf_93 (vec2 anf_91 anf_92))
         (set () vec2 anf_94 (+ p_21 anf_93))
         (set () vec2 anf_95 (+ anf_94 x_23))
         (set () float y_24 (fractalNoise_18 anf_95))
         (set () vec2 anf_96 (- p_21 m_22)) (set () vec2 anf_97 (- anf_96 x_23))
         (set () vec2 anf_98 (+ anf_97 y_24))
         (set () float z_25 (fractalNoise_18 anf_98))
         (set () vec2 anf_99 (vec2 x_23 y_24))
         (set () vec2 anf_100 (vec2 y_24 z_25))
         (set () vec2 anf_101 (+ anf_99 anf_100))
         (set () vec2 anf_102 (vec2 z_25 x_23))
         (set () vec2 warp_26 (+ anf_101 anf_102))
         (set () vec3 anf_103 (vec3 x_23 y_24 z_25))
         (set () float anf_104 (length anf_103))
         (set () float mag_27 (* anf_104 0.25))
         (set () vec2 anf_105 (+ p_21 warp_26))
         (set () vec2 anf_106 (+ anf_105 mag_27))
         (return (fractalNoise_18 anf_106)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_28)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 anf_107 (* u_resolution 0.5))
         (set () vec2 anf_108 (- coord_28 anf_107))
         (set () float anf_109 (index u_resolution 1))
         (set () vec2 uv_29 (/ anf_108 anf_109))
         (set () vec2 anf_110 (* uv_29 6.))
         (set () float n_30 (warpedNoise_20 anf_110))
         (set () vec2 anf_111 (* uv_29 6.))
         (set () vec2 anf_112 (- anf_111 0.02))
         (set () float n2_31 (warpedNoise_20 anf_112))
         (set () float anf_113 (- n2_31 n_30))
         (set () float anf_114 (max anf_113 0.))
         (set () float anf_115 (/ anf_114 0.02))
         (set () float bump_32 (* anf_115 0.7071))
         (set () float anf_116 (- n_30 n2_31))
         (set () float anf_117 (max anf_116 0.))
         (set () float anf_118 (/ anf_117 0.02))
         (set () float bump2_33 (* anf_118 0.7071))
         (set () float anf_119 (* bump_32 bump_32))
         (set () float anf_120 (pow bump_32 4.))
         (set () float anf_121 (* anf_120 0.5))
         (set () float b1_34 (+ anf_119 anf_121))
         (set () float anf_122 (* bump2_33 bump2_33))
         (set () float anf_123 (pow bump2_33 4.))
         (set () float anf_124 (* anf_123 0.5))
         (set () float b2_35 (+ anf_122 anf_124))
         (set () vec3 anf_125 (vec3 1. 0.7 0.6))
         (set () float anf_126 (+ b1_34 b2_35))
         (set () float anf_127 (* anf_126 0.4))
         (set () vec3 anf_128 (vec3 b1_34 anf_127 b2_35))
         (set () vec3 anf_129 (* anf_125 anf_128))
         (set () vec3 anf_130 (* anf_129 0.3))
         (set () vec3 base_col_36 (+ anf_130 0.5))
         (set () float anf_131 (* n_30 n_30))
         (set () vec3 col_37 (* anf_131 base_col_36))
         (set () vec2 anf_132 (- uv_29 0.65))
         (set () float spot1_dist_38 (length anf_132))
         (set () vec2 anf_133 (+ uv_29 0.5))
         (set () float spot2_dist_39 (length anf_133))
         (set () vec3 anf_134 (vec3 0.8 0.4 1.))
         (set () vec3 a_40 (* anf_134 0.35))
         (set () vec3 anf_135 (vec3 1. 0.5 0.2))
         (set () float anf_136 (- 1. spot1_dist_38))
         (set () float anf_137 (smoothstep_2 0. 1. anf_136))
         (set () vec3 b_41 (* anf_135 anf_137))
         (set () vec3 anf_138 (vec3 0.2 0.4 1.))
         (set () float anf_139 (- 1. spot2_dist_39))
         (set () float anf_140 (smoothstep_2 0. 1. anf_139))
         (set () vec3 c_42 (* anf_138 anf_140))
         (set () vec3 anf_141 (+ b_41 c_42)) (set () vec3 anf_142 (* anf_141 5.))
         (set () vec3 spot_logic_43 (+ a_40 anf_142))
         (set () vec3 final_col_44 (* col_37 spot_logic_43))
         (set () vec3 anf_143 (max final_col_44 0.)) (return (sqrt anf_143)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))
    |}]
;;
