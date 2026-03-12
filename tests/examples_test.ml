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
       (lambda (coord ((vec 2)))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
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
       (lambda (coord_1 ((vec 2)))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define Nonrec main
       (lambda (coord_4 ((vec 2)))
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

    === monomorphize (checkerboard.glml) ===
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
        (let anf_32 (* 2. coord_1)
         (let top_2 (- anf_32 u_resolution)
          (let anf_33 (index u_resolution 0)
           (let anf_34 (index u_resolution 1)
            (let bot_3 (min anf_33 anf_34) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
         (let size_6 5.
          (let anf_35 (index uv_5 0)
           (let anf_36 (* anf_35 size_6)
            (let anf_37 (* u_time 2.)
             (let anf_38 (+ anf_36 anf_37)
              (let cx_7 (floor anf_38)
               (let anf_39 (index uv_5 1)
                (let anf_40 (* anf_39 size_6)
                 (let cy_8 (floor anf_40)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_41 (/ checker_sum_9 2.)
                    (let anf_42 (floor anf_41)
                     (let anf_43 (* anf_42 2.)
                      (let is_even_10 (- checker_sum_9 anf_43)
                       (let anf_44 (< is_even_10 0.5)
                        (return
                         (if anf_44 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_32 (* 2. coord_1)
         (let top_2 (- anf_32 u_resolution)
          (let anf_33 (index u_resolution 0)
           (let anf_34 (index u_resolution 1)
            (let bot_3 (min anf_33 anf_34) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
         (let size_6 5.
          (let anf_35 (index uv_5 0)
           (let anf_36 (* anf_35 size_6)
            (let anf_37 (* u_time 2.)
             (let anf_38 (+ anf_36 anf_37)
              (let cx_7 (floor anf_38)
               (let anf_39 (index uv_5 1)
                (let anf_40 (* anf_39 size_6)
                 (let cy_8 (floor anf_40)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_41 (/ checker_sum_9 2.)
                    (let anf_42 (floor anf_41)
                     (let anf_43 (* anf_42 2.)
                      (let is_even_10 (- checker_sum_9 anf_43)
                       (let anf_44 (< is_even_10 0.5)
                        (return
                         (if anf_44 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (checkerboard.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_32 (* 2. coord_1))
         (set () vec2 top_2 (- anf_32 u_resolution))
         (set () float anf_33 (index u_resolution 0))
         (set () float anf_34 (index u_resolution 1))
         (set () float bot_3 (min anf_33 anf_34)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4)) (set () float size_6 5.)
         (set () float anf_35 (index uv_5 0))
         (set () float anf_36 (* anf_35 size_6))
         (set () float anf_37 (* u_time 2.))
         (set () float anf_38 (+ anf_36 anf_37))
         (set () float cx_7 (floor anf_38)) (set () float anf_39 (index uv_5 1))
         (set () float anf_40 (* anf_39 size_6))
         (set () float cy_8 (floor anf_40))
         (set () float checker_sum_9 (+ cx_7 cy_8))
         (set () float anf_41 (/ checker_sum_9 2.))
         (set () float anf_42 (floor anf_41)) (set () float anf_43 (* anf_42 2.))
         (set () float is_even_10 (- checker_sum_9 anf_43))
         (set () bool anf_44 (< is_even_10 0.5))
         (if anf_44 (Block (return (vec3 0.2 0.2 0.2)))
          (Block (return (vec3 0.8 0.8 0.8)))))))))

    === patch_main (checkerboard.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_32 (* 2. coord_1))
         (set () vec2 top_2 (- anf_32 u_resolution))
         (set () float anf_33 (index u_resolution 0))
         (set () float anf_34 (index u_resolution 1))
         (set () float bot_3 (min anf_33 anf_34)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4)) (set () float size_6 5.)
         (set () float anf_35 (index uv_5 0))
         (set () float anf_36 (* anf_35 size_6))
         (set () float anf_37 (* u_time 2.))
         (set () float anf_38 (+ anf_36 anf_37))
         (set () float cx_7 (floor anf_38)) (set () float anf_39 (index uv_5 1))
         (set () float anf_40 (* anf_39 size_6))
         (set () float cy_8 (floor anf_40))
         (set () float checker_sum_9 (+ cx_7 cy_8))
         (set () float anf_41 (/ checker_sum_9 2.))
         (set () float anf_42 (floor anf_41)) (set () float anf_43 (* anf_42 2.))
         (set () float is_even_10 (- checker_sum_9 anf_43))
         (set () bool anf_44 (< is_even_10 0.5))
         (if anf_44 (Block (return (vec3 0.2 0.2 0.2)))
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
       (lambda (coord ((vec 2)))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define
       (Rec 1000 ((float -> (float -> (float -> (float -> (float -> float)))))))
       mandel
       (lambda (zx (float))
        (lambda (zy (float))
         (lambda (cx (float))
          (lambda (cy (float))
           (lambda (i (float))
            (if (|| (> (length (vec2 zx zy)) 2.) (> i 150.)) i
             (let next_zx (+ (- (* zx zx) (* zy zy)) cx)
              (let next_zy (+ (* (* 2. zx) zy) cy)
               (app (app (app (app (app mandel next_zx) next_zy) cx) cy)
                (+ i 1.)))))))))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
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
       (lambda (coord_1 ((vec 2)))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define
       (Rec 1000 ((float -> (float -> (float -> (float -> (float -> float)))))))
       mandel_4
       (lambda (zx_5 (float))
        (lambda (zy_6 (float))
         (lambda (cx_7 (float))
          (lambda (cy_8 (float))
           (lambda (i_9 (float))
            (if (|| (> (length (vec2 zx_5 zy_6)) 2.) (> i_9 150.)) i_9
             (let next_zx_10 (+ (- (* zx_5 zx_5) (* zy_6 zy_6)) cx_7)
              (let next_zy_11 (+ (* (* 2. zx_5) zy_6) cy_8)
               (app
                (app (app (app (app mandel_4 next_zx_10) next_zy_11) cx_7) cy_8)
                (+ i_9 1.)))))))))))
      (Define Nonrec main
       (lambda (coord_12 ((vec 2)))
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
        (Rec 1000 ((float -> (float -> (float -> (float -> (float -> float)))))))
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

    === monomorphize (mandelbrot.glml) ===
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
        (Rec 1000 ((float -> (float -> (float -> (float -> (float -> float)))))))
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
        (Rec 1000 ((float -> (float -> (float -> (float -> (float -> float)))))))
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
       (Rec 1000 ((float -> (float -> (float -> (float -> (float -> float)))))))
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
        (let anf_80 (* 2. coord_1)
         (let top_2 (- anf_80 u_resolution)
          (let anf_81 (index u_resolution 0)
           (let anf_82 (index u_resolution 1)
            (let bot_3 (min anf_81 anf_82) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define
       (Rec 1000 ((float -> (float -> (float -> (float -> (float -> float)))))))
       (name mandel_4)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let anf_83 (vec2 zx_5 zy_6)
         (let anf_84 (length anf_83)
          (let anf_85 (> anf_84 2.)
           (let anf_86 (> i_9 150.)
            (let anf_87 (|| anf_85 anf_86)
             (return
              (if anf_87 (return i_9)
               (let anf_88 (* zx_5 zx_5)
                (let anf_89 (* zy_6 zy_6)
                 (let anf_90 (- anf_88 anf_89)
                  (let next_zx_10 (+ anf_90 cx_7)
                   (let anf_91 (* 2. zx_5)
                    (let anf_92 (* anf_91 zy_6)
                     (let next_zy_11 (+ anf_92 cy_8)
                      (let anf_93 (+ i_9 1.)
                       (return (mandel_4 next_zx_10 next_zy_11 cx_7 cy_8 anf_93)))))))))))))))))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define Nonrec (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0 coord_12)
         (let anf_94 (* u_time 0.4)
          (let anf_95 (sin anf_94)
           (let anf_96 (* anf_95 4.5)
            (let anf_97 (+ anf_96 3.5)
             (let zoom_14 (exp anf_97)
              (let anf_98 (index uv_13 0)
               (let anf_99 (/ anf_98 zoom_14)
                (let cx_15 (+ -0.7453 anf_99)
                 (let anf_100 (index uv_13 1)
                  (let anf_101 (/ anf_100 zoom_14)
                   (let cy_16 (+ 0.1127 anf_101)
                    (let iter_17 (mandel_4 0. 0. cx_15 cy_16 0.)
                     (let anf_102 (> iter_17 149.)
                      (return
                       (if anf_102 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_103 (* n_18 10.)
                          (let anf_104 (+ anf_103 u_time)
                           (let anf_105 (sin anf_104)
                            (let anf_106 (* anf_105 0.5)
                             (let r_19 (+ anf_106 0.5)
                              (let anf_107 (* n_18 20.)
                               (let anf_108 (+ anf_107 u_time)
                                (let anf_109 (sin anf_108)
                                 (let anf_110 (* anf_109 0.5)
                                  (let g_20 (+ anf_110 0.5)
                                   (let anf_111 (* n_18 30.)
                                    (let anf_112 (+ anf_111 u_time)
                                     (let anf_113 (sin anf_112)
                                      (let anf_114 (* anf_113 0.5)
                                       (let b_21 (+ anf_114 0.5)
                                        (return (vec3 r_19 g_20 b_21))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_80 (* 2. coord_1)
         (let top_2 (- anf_80 u_resolution)
          (let anf_81 (index u_resolution 0)
           (let anf_82 (index u_resolution 1)
            (let bot_3 (min anf_81 anf_82) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name mandel_4)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let _iter_115 0
         (while (< _iter_115 1000)
          (let anf_83 (vec2 zx_5 zy_6)
           (let anf_84 (length anf_83)
            (let anf_85 (> anf_84 2.)
             (let anf_86 (> i_9 150.)
              (let anf_87 (|| anf_85 anf_86)
               (return
                (if anf_87 (return i_9)
                 (let anf_88 (* zx_5 zx_5)
                  (let anf_89 (* zy_6 zy_6)
                   (let anf_90 (- anf_88 anf_89)
                    (let next_zx_10 (+ anf_90 cx_7)
                     (let anf_91 (* 2. zx_5)
                      (let anf_92 (* anf_91 zy_6)
                       (let next_zy_11 (+ anf_92 cy_8)
                        (let anf_93 (+ i_9 1.)
                         (set zx_5 next_zx_10
                          (set zy_6 next_zy_11
                           (set cx_7 cx_7
                            (set cy_8 cy_8
                             (set i_9 anf_93
                              (let _iter_inc_116 (+ _iter_115 1)
                               (set _iter_115 _iter_inc_116 continue))))))))))))))))))))))
          (return 0.)))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0 coord_12)
         (let anf_94 (* u_time 0.4)
          (let anf_95 (sin anf_94)
           (let anf_96 (* anf_95 4.5)
            (let anf_97 (+ anf_96 3.5)
             (let zoom_14 (exp anf_97)
              (let anf_98 (index uv_13 0)
               (let anf_99 (/ anf_98 zoom_14)
                (let cx_15 (+ -0.7453 anf_99)
                 (let anf_100 (index uv_13 1)
                  (let anf_101 (/ anf_100 zoom_14)
                   (let cy_16 (+ 0.1127 anf_101)
                    (let iter_17 (mandel_4 0. 0. cx_15 cy_16 0.)
                     (let anf_102 (> iter_17 149.)
                      (return
                       (if anf_102 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_103 (* n_18 10.)
                          (let anf_104 (+ anf_103 u_time)
                           (let anf_105 (sin anf_104)
                            (let anf_106 (* anf_105 0.5)
                             (let r_19 (+ anf_106 0.5)
                              (let anf_107 (* n_18 20.)
                               (let anf_108 (+ anf_107 u_time)
                                (let anf_109 (sin anf_108)
                                 (let anf_110 (* anf_109 0.5)
                                  (let g_20 (+ anf_110 0.5)
                                   (let anf_111 (* n_18 30.)
                                    (let anf_112 (+ anf_111 u_time)
                                     (let anf_113 (sin anf_112)
                                      (let anf_114 (* anf_113 0.5)
                                       (let b_21 (+ anf_114 0.5)
                                        (return (vec3 r_19 g_20 b_21))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (mandelbrot.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_80 (* 2. coord_1))
         (set () vec2 top_2 (- anf_80 u_resolution))
         (set () float anf_81 (index u_resolution 0))
         (set () float anf_82 (index u_resolution 1))
         (set () float bot_3 (min anf_81 anf_82)) (return (/ top_2 bot_3)))))
      (Function (name mandel_4) (desc ())
       (params
        ((TyFloat zx_5) (TyFloat zy_6) (TyFloat cx_7) (TyFloat cy_8)
         (TyFloat i_9)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_115 0)
         (while (< _iter_115 1000)
          (Block (set () vec2 anf_83 (vec2 zx_5 zy_6))
           (set () float anf_84 (length anf_83))
           (set () bool anf_85 (> anf_84 2.)) (set () bool anf_86 (> i_9 150.))
           (set () bool anf_87 (|| anf_85 anf_86))
           (if anf_87 (Block (return i_9))
            (Block (set () float anf_88 (* zx_5 zx_5))
             (set () float anf_89 (* zy_6 zy_6))
             (set () float anf_90 (- anf_88 anf_89))
             (set () float next_zx_10 (+ anf_90 cx_7))
             (set () float anf_91 (* 2. zx_5))
             (set () float anf_92 (* anf_91 zy_6))
             (set () float next_zy_11 (+ anf_92 cy_8))
             (set () float anf_93 (+ i_9 1.)) (set zx_5 next_zx_10)
             (set zy_6 next_zy_11) (set cx_7 cx_7) (set cy_8 cy_8)
             (set i_9 anf_93) (set () int _iter_inc_116 (+ _iter_115 1))
             (set _iter_115 _iter_inc_116) continue))))
         (return 0.))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_12)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_13 (get_uv_0 coord_12))
         (set () float anf_94 (* u_time 0.4)) (set () float anf_95 (sin anf_94))
         (set () float anf_96 (* anf_95 4.5))
         (set () float anf_97 (+ anf_96 3.5)) (set () float zoom_14 (exp anf_97))
         (set () float anf_98 (index uv_13 0))
         (set () float anf_99 (/ anf_98 zoom_14))
         (set () float cx_15 (+ -0.7453 anf_99))
         (set () float anf_100 (index uv_13 1))
         (set () float anf_101 (/ anf_100 zoom_14))
         (set () float cy_16 (+ 0.1127 anf_101))
         (set () float iter_17 (mandel_4 0. 0. cx_15 cy_16 0.))
         (set () bool anf_102 (> iter_17 149.))
         (if anf_102 (Block (return (vec3 0. 0. 0.)))
          (Block (set () float n_18 (/ iter_17 150.))
           (set () float anf_103 (* n_18 10.))
           (set () float anf_104 (+ anf_103 u_time))
           (set () float anf_105 (sin anf_104))
           (set () float anf_106 (* anf_105 0.5))
           (set () float r_19 (+ anf_106 0.5))
           (set () float anf_107 (* n_18 20.))
           (set () float anf_108 (+ anf_107 u_time))
           (set () float anf_109 (sin anf_108))
           (set () float anf_110 (* anf_109 0.5))
           (set () float g_20 (+ anf_110 0.5))
           (set () float anf_111 (* n_18 30.))
           (set () float anf_112 (+ anf_111 u_time))
           (set () float anf_113 (sin anf_112))
           (set () float anf_114 (* anf_113 0.5))
           (set () float b_21 (+ anf_114 0.5)) (return (vec3 r_19 g_20 b_21)))))))))

    === patch_main (mandelbrot.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_80 (* 2. coord_1))
         (set () vec2 top_2 (- anf_80 u_resolution))
         (set () float anf_81 (index u_resolution 0))
         (set () float anf_82 (index u_resolution 1))
         (set () float bot_3 (min anf_81 anf_82)) (return (/ top_2 bot_3)))))
      (Function (name mandel_4) (desc ())
       (params
        ((TyFloat zx_5) (TyFloat zy_6) (TyFloat cx_7) (TyFloat cy_8)
         (TyFloat i_9)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_115 0)
         (while (< _iter_115 1000)
          (Block (set () vec2 anf_83 (vec2 zx_5 zy_6))
           (set () float anf_84 (length anf_83))
           (set () bool anf_85 (> anf_84 2.)) (set () bool anf_86 (> i_9 150.))
           (set () bool anf_87 (|| anf_85 anf_86))
           (if anf_87 (Block (return i_9))
            (Block (set () float anf_88 (* zx_5 zx_5))
             (set () float anf_89 (* zy_6 zy_6))
             (set () float anf_90 (- anf_88 anf_89))
             (set () float next_zx_10 (+ anf_90 cx_7))
             (set () float anf_91 (* 2. zx_5))
             (set () float anf_92 (* anf_91 zy_6))
             (set () float next_zy_11 (+ anf_92 cy_8))
             (set () float anf_93 (+ i_9 1.)) (set zx_5 next_zx_10)
             (set zy_6 next_zy_11) (set cx_7 cx_7) (set cy_8 cy_8)
             (set i_9 anf_93) (set () int _iter_inc_116 (+ _iter_115 1))
             (set _iter_115 _iter_inc_116) continue))))
         (return 0.))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_12)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_13 (get_uv_0 coord_12))
         (set () float anf_94 (* u_time 0.4)) (set () float anf_95 (sin anf_94))
         (set () float anf_96 (* anf_95 4.5))
         (set () float anf_97 (+ anf_96 3.5)) (set () float zoom_14 (exp anf_97))
         (set () float anf_98 (index uv_13 0))
         (set () float anf_99 (/ anf_98 zoom_14))
         (set () float cx_15 (+ -0.7453 anf_99))
         (set () float anf_100 (index uv_13 1))
         (set () float anf_101 (/ anf_100 zoom_14))
         (set () float cy_16 (+ 0.1127 anf_101))
         (set () float iter_17 (mandel_4 0. 0. cx_15 cy_16 0.))
         (set () bool anf_102 (> iter_17 149.))
         (if anf_102 (Block (return (vec3 0. 0. 0.)))
          (Block (set () float n_18 (/ iter_17 150.))
           (set () float anf_103 (* n_18 10.))
           (set () float anf_104 (+ anf_103 u_time))
           (set () float anf_105 (sin anf_104))
           (set () float anf_106 (* anf_105 0.5))
           (set () float r_19 (+ anf_106 0.5))
           (set () float anf_107 (* n_18 20.))
           (set () float anf_108 (+ anf_107 u_time))
           (set () float anf_109 (sin anf_108))
           (set () float anf_110 (* anf_109 0.5))
           (set () float g_20 (+ anf_110 0.5))
           (set () float anf_111 (* n_18 30.))
           (set () float anf_112 (+ anf_111 u_time))
           (set () float anf_113 (sin anf_112))
           (set () float anf_114 (* anf_113 0.5))
           (set () float b_21 (+ anf_114 0.5)) (return (vec3 r_19 g_20 b_21)))))))
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
       (lambda (coord ((vec 2)))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
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
       (lambda (coord_1 ((vec 2)))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define Nonrec main
       (lambda (coord_4 ((vec 2)))
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

    === monomorphize (mouse_circle.glml) ===
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
        (let anf_25 (* 2. coord_1)
         (let top_2 (- anf_25 u_resolution)
          (let anf_26 (index u_resolution 0)
           (let anf_27 (index u_resolution 1)
            (let bot_3 (min anf_26 anf_27) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
         (let anf_28 (* 2. u_mouse)
          (let anf_29 (- anf_28 u_resolution)
           (let anf_30 (index u_resolution 1)
            (let mouseUV_6 (/ anf_29 anf_30)
             (let anf_31 (* u_time 2.)
              (let anf_32 (sin anf_31)
               (let anf_33 (* anf_32 0.1)
                (let radius_7 (+ anf_33 0.15)
                 (let anf_34 (distance uv_5 mouseUV_6)
                  (let anf_35 (< anf_34 radius_7)
                   (return
                    (if anf_35 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_25 (* 2. coord_1)
         (let top_2 (- anf_25 u_resolution)
          (let anf_26 (index u_resolution 0)
           (let anf_27 (index u_resolution 1)
            (let bot_3 (min anf_26 anf_27) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
         (let anf_28 (* 2. u_mouse)
          (let anf_29 (- anf_28 u_resolution)
           (let anf_30 (index u_resolution 1)
            (let mouseUV_6 (/ anf_29 anf_30)
             (let anf_31 (* u_time 2.)
              (let anf_32 (sin anf_31)
               (let anf_33 (* anf_32 0.1)
                (let radius_7 (+ anf_33 0.15)
                 (let anf_34 (distance uv_5 mouseUV_6)
                  (let anf_35 (< anf_34 radius_7)
                   (return
                    (if anf_35 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (mouse_circle.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform (TyVec 2) u_mouse)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_25 (* 2. coord_1))
         (set () vec2 top_2 (- anf_25 u_resolution))
         (set () float anf_26 (index u_resolution 0))
         (set () float anf_27 (index u_resolution 1))
         (set () float bot_3 (min anf_26 anf_27)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4))
         (set () vec2 anf_28 (* 2. u_mouse))
         (set () vec2 anf_29 (- anf_28 u_resolution))
         (set () float anf_30 (index u_resolution 1))
         (set () vec2 mouseUV_6 (/ anf_29 anf_30))
         (set () float anf_31 (* u_time 2.)) (set () float anf_32 (sin anf_31))
         (set () float anf_33 (* anf_32 0.1))
         (set () float radius_7 (+ anf_33 0.15))
         (set () float anf_34 (distance uv_5 mouseUV_6))
         (set () bool anf_35 (< anf_34 radius_7))
         (if anf_35 (Block (return (vec3 0. 0. 0.5)))
          (Block (return (vec3 0.5 0.5 1.)))))))))

    === patch_main (mouse_circle.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform (TyVec 2) u_mouse) (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_25 (* 2. coord_1))
         (set () vec2 top_2 (- anf_25 u_resolution))
         (set () float anf_26 (index u_resolution 0))
         (set () float anf_27 (index u_resolution 1))
         (set () float bot_3 (min anf_26 anf_27)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4))
         (set () vec2 anf_28 (* 2. u_mouse))
         (set () vec2 anf_29 (- anf_28 u_resolution))
         (set () float anf_30 (index u_resolution 1))
         (set () vec2 mouseUV_6 (/ anf_29 anf_30))
         (set () float anf_31 (* u_time 2.)) (set () float anf_32 (sin anf_31))
         (set () float anf_33 (* anf_32 0.1))
         (set () float radius_7 (+ anf_33 0.15))
         (set () float anf_34 (distance uv_5 mouseUV_6))
         (set () bool anf_35 (< anf_34 radius_7))
         (if anf_35 (Block (return (vec3 0. 0. 0.5)))
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
       (lambda (coord ((vec 2)))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
        (let uv (app get_uv coord)
         (let wave (+ (* 5. (+ (index uv 0) (index uv 1))) u_time)
          (let r (+ (* (sin wave) 0.3) 0.7)
           (let g (+ (* (sin (+ wave 2.)) 0.3) 0.7)
            (let b (+ (* (sin (+ wave 4.)) 0.3) 0.7) (vec3 r g b))))))))))

    === uniquify (rainbow.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv_0
       (lambda (coord_1 ((vec 2)))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define Nonrec main
       (lambda (coord_4 ((vec 2)))
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

    === monomorphize (rainbow.glml) ===
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
        (let anf_33 (* 2. coord_1)
         (let top_2 (- anf_33 u_resolution)
          (let anf_34 (index u_resolution 0)
           (let anf_35 (index u_resolution 1)
            (let bot_3 (min anf_34 anf_35) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
         (let anf_36 (index uv_5 0)
          (let anf_37 (index uv_5 1)
           (let anf_38 (+ anf_36 anf_37)
            (let anf_39 (* 5. anf_38)
             (let wave_6 (+ anf_39 u_time)
              (let anf_40 (sin wave_6)
               (let anf_41 (* anf_40 0.3)
                (let r_7 (+ anf_41 0.7)
                 (let anf_42 (+ wave_6 2.)
                  (let anf_43 (sin anf_42)
                   (let anf_44 (* anf_43 0.3)
                    (let g_8 (+ anf_44 0.7)
                     (let anf_45 (+ wave_6 4.)
                      (let anf_46 (sin anf_45)
                       (let anf_47 (* anf_46 0.3)
                        (let b_9 (+ anf_47 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_33 (* 2. coord_1)
         (let top_2 (- anf_33 u_resolution)
          (let anf_34 (index u_resolution 0)
           (let anf_35 (index u_resolution 1)
            (let bot_3 (min anf_34 anf_35) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0 coord_4)
         (let anf_36 (index uv_5 0)
          (let anf_37 (index uv_5 1)
           (let anf_38 (+ anf_36 anf_37)
            (let anf_39 (* 5. anf_38)
             (let wave_6 (+ anf_39 u_time)
              (let anf_40 (sin wave_6)
               (let anf_41 (* anf_40 0.3)
                (let r_7 (+ anf_41 0.7)
                 (let anf_42 (+ wave_6 2.)
                  (let anf_43 (sin anf_42)
                   (let anf_44 (* anf_43 0.3)
                    (let g_8 (+ anf_44 0.7)
                     (let anf_45 (+ wave_6 4.)
                      (let anf_46 (sin anf_45)
                       (let anf_47 (* anf_46 0.3)
                        (let b_9 (+ anf_47 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (rainbow.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_33 (* 2. coord_1))
         (set () vec2 top_2 (- anf_33 u_resolution))
         (set () float anf_34 (index u_resolution 0))
         (set () float anf_35 (index u_resolution 1))
         (set () float bot_3 (min anf_34 anf_35)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4))
         (set () float anf_36 (index uv_5 0))
         (set () float anf_37 (index uv_5 1))
         (set () float anf_38 (+ anf_36 anf_37))
         (set () float anf_39 (* 5. anf_38))
         (set () float wave_6 (+ anf_39 u_time))
         (set () float anf_40 (sin wave_6)) (set () float anf_41 (* anf_40 0.3))
         (set () float r_7 (+ anf_41 0.7)) (set () float anf_42 (+ wave_6 2.))
         (set () float anf_43 (sin anf_42)) (set () float anf_44 (* anf_43 0.3))
         (set () float g_8 (+ anf_44 0.7)) (set () float anf_45 (+ wave_6 4.))
         (set () float anf_46 (sin anf_45)) (set () float anf_47 (* anf_46 0.3))
         (set () float b_9 (+ anf_47 0.7)) (return (vec3 r_7 g_8 b_9)))))))

    === patch_main (rainbow.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_33 (* 2. coord_1))
         (set () vec2 top_2 (- anf_33 u_resolution))
         (set () float anf_34 (index u_resolution 0))
         (set () float anf_35 (index u_resolution 1))
         (set () float bot_3 (min anf_34 anf_35)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0 coord_4))
         (set () float anf_36 (index uv_5 0))
         (set () float anf_37 (index uv_5 1))
         (set () float anf_38 (+ anf_36 anf_37))
         (set () float anf_39 (* 5. anf_38))
         (set () float wave_6 (+ anf_39 u_time))
         (set () float anf_40 (sin wave_6)) (set () float anf_41 (* anf_40 0.3))
         (set () float r_7 (+ anf_41 0.7)) (set () float anf_42 (+ wave_6 2.))
         (set () float anf_43 (sin anf_42)) (set () float anf_44 (* anf_43 0.3))
         (set () float g_8 (+ anf_44 0.7)) (set () float anf_45 (+ wave_6 4.))
         (set () float anf_46 (sin anf_45)) (set () float anf_47 (* anf_46 0.3))
         (set () float b_9 (+ anf_47 0.7)) (return (vec3 r_7 g_8 b_9)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE raymarch.glml ======

    === stlc (raymarch.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Extern (vec 2) u_mouse)
      (Define Nonrec rotate
       (lambda (p ((vec 2)))
        (lambda (angle (float))
         (let s (sin angle)
          (let c (cos angle)
           (vec2 (- (* (index p 0) c) (* (index p 1) s))
            (+ (* (index p 0) s) (* (index p 1) c))))))))
      (Define Nonrec sMin
       (lambda (a (float))
        (lambda (b (float))
         (let k 0.1
          (let h (clamp (+ 0.5 (/ (* 0.5 (- b a)) k)) 0. 1.)
           (- (mix b a h) (* (* k h) (- 1. h))))))))
      (Define Nonrec palette
       (lambda (t (float))
        (let cfg (vec3 0.3 0.416 0.557)
         (+ (* (cos (* (+ cfg t) 6.28318)) 0.5) 0.5))))
      (Define Nonrec sdTorus
       (lambda (p ((vec 3)))
        (lambda (t ((vec 2)))
         (let q
          (vec2 (- (length (vec2 (index p 0) (index p 2))) (index t 0))
           (index p 1))
          (- (length q) (index t 1))))))
      (Define Nonrec map
       (lambda (p ((vec 3)))
        (let angle (* u_time 2.)
         (let p_xy (app (app rotate (vec2 (index p 0) (index p 1))) angle)
          (let p' (vec3 (index p_xy 0) (index p_xy 1) (index p 2))
           (let p_yz (app (app rotate (vec2 (index p' 1) (index p' 2))) angle)
            (let p' (vec3 (index p' 0) (index p_yz 0) (index p_yz 1))
             (app (app sMin (app (app sdTorus p') (vec2 1. 0.3)))
              (app (app sdTorus p) (vec2 2. 0.5))))))))))
      (Define Nonrec march
       (lambda (ro ((vec 3)))
        (lambda (rd ((vec 3)))
         (let (rec 1000 ((float -> (int -> float)))) march
          (lambda (t (float))
           (lambda (steps (int))
            (if (> steps 80) t
             (let d (app map (+ ro (* rd t)))
              (if (< d 0.001) t
               (if (> t 100.) 100.1 (app (app march (+ t d)) (+ steps 1))))))))
          (app (app march 0.) 0)))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
        (let res_min (min (index u_resolution 0) (index u_resolution 1))
         (let uv (/ (- (* coord 2.) u_resolution) res_min)
          (let mouseUV (/ (- (* u_mouse 2.) u_resolution) res_min)
           (let ro_init (vec3 0. 0. -10.)
            (let rd_init (normalize (vec3 (index uv 0) (index uv 1) 1.))
             (let rotX (- 0. (index mouseUV 1))
              (let rotY (- 0. (index mouseUV 0))
               (let ro_yz
                (app (app rotate (vec2 (index ro_init 1) (index ro_init 2)))
                 rotX)
                (let rd_yz
                 (app (app rotate (vec2 (index rd_init 1) (index rd_init 2)))
                  rotX)
                 (let ro (vec3 (index ro_init 0) (index ro_yz 0) (index ro_yz 1))
                  (let rd
                   (vec3 (index rd_init 0) (index rd_yz 0) (index rd_yz 1))
                   (let ro_xz
                    (app (app rotate (vec2 (index ro 0) (index ro 2))) rotY)
                    (let rd_xz
                     (app (app rotate (vec2 (index rd 0) (index rd 2))) rotY)
                     (let ro (vec3 (index ro_xz 0) (index ro 1) (index ro_xz 1))
                      (let rd (vec3 (index rd_xz 0) (index rd 1) (index rd_xz 1))
                       (let t (app (app march ro) rd)
                        (let col
                         (if (> t 100.) (vec3 0.2 0.2 0.2)
                          (app palette (* t 0.3)))
                         (let glow (/ 0.02 (length (- uv mouseUV))) (+ col glow)))))))))))))))))))))))

    === uniquify (raymarch.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Extern (vec 2) u_mouse)
      (Define Nonrec rotate_0
       (lambda (p_1 ((vec 2)))
        (lambda (angle_2 (float))
         (let s_3 (sin angle_2)
          (let c_4 (cos angle_2)
           (vec2 (- (* (index p_1 0) c_4) (* (index p_1 1) s_3))
            (+ (* (index p_1 0) s_3) (* (index p_1 1) c_4))))))))
      (Define Nonrec sMin_5
       (lambda (a_6 (float))
        (lambda (b_7 (float))
         (let k_8 0.1
          (let h_9 (clamp (+ 0.5 (/ (* 0.5 (- b_7 a_6)) k_8)) 0. 1.)
           (- (mix b_7 a_6 h_9) (* (* k_8 h_9) (- 1. h_9))))))))
      (Define Nonrec palette_10
       (lambda (t_11 (float))
        (let cfg_12 (vec3 0.3 0.416 0.557)
         (+ (* (cos (* (+ cfg_12 t_11) 6.28318)) 0.5) 0.5))))
      (Define Nonrec sdTorus_13
       (lambda (p_14 ((vec 3)))
        (lambda (t_15 ((vec 2)))
         (let q_16
          (vec2 (- (length (vec2 (index p_14 0) (index p_14 2))) (index t_15 0))
           (index p_14 1))
          (- (length q_16) (index t_15 1))))))
      (Define Nonrec map_17
       (lambda (p_18 ((vec 3)))
        (let angle_19 (* u_time 2.)
         (let p_xy_20
          (app (app rotate_0 (vec2 (index p_18 0) (index p_18 1))) angle_19)
          (let p_prime_21
           (vec3 (index p_xy_20 0) (index p_xy_20 1) (index p_18 2))
           (let p_yz_22
            (app (app rotate_0 (vec2 (index p_prime_21 1) (index p_prime_21 2)))
             angle_19)
            (let p_prime_23
             (vec3 (index p_prime_21 0) (index p_yz_22 0) (index p_yz_22 1))
             (app (app sMin_5 (app (app sdTorus_13 p_prime_23) (vec2 1. 0.3)))
              (app (app sdTorus_13 p_18) (vec2 2. 0.5))))))))))
      (Define Nonrec march_24
       (lambda (ro_25 ((vec 3)))
        (lambda (rd_26 ((vec 3)))
         (let (rec 1000 ((float -> (int -> float)))) march_27
          (lambda (t_28 (float))
           (lambda (steps_29 (int))
            (if (> steps_29 80) t_28
             (let d_30 (app map_17 (+ ro_25 (* rd_26 t_28)))
              (if (< d_30 0.001) t_28
               (if (> t_28 100.) 100.1
                (app (app march_27 (+ t_28 d_30)) (+ steps_29 1))))))))
          (app (app march_27 0.) 0)))))
      (Define Nonrec main
       (lambda (coord_31 ((vec 2)))
        (let res_min_32 (min (index u_resolution 0) (index u_resolution 1))
         (let uv_33 (/ (- (* coord_31 2.) u_resolution) res_min_32)
          (let mouseUV_34 (/ (- (* u_mouse 2.) u_resolution) res_min_32)
           (let ro_init_35 (vec3 0. 0. -10.)
            (let rd_init_36 (normalize (vec3 (index uv_33 0) (index uv_33 1) 1.))
             (let rotX_37 (- 0. (index mouseUV_34 1))
              (let rotY_38 (- 0. (index mouseUV_34 0))
               (let ro_yz_39
                (app
                 (app rotate_0 (vec2 (index ro_init_35 1) (index ro_init_35 2)))
                 rotX_37)
                (let rd_yz_40
                 (app
                  (app rotate_0 (vec2 (index rd_init_36 1) (index rd_init_36 2)))
                  rotX_37)
                 (let ro_41
                  (vec3 (index ro_init_35 0) (index ro_yz_39 0)
                   (index ro_yz_39 1))
                  (let rd_42
                   (vec3 (index rd_init_36 0) (index rd_yz_40 0)
                    (index rd_yz_40 1))
                   (let ro_xz_43
                    (app (app rotate_0 (vec2 (index ro_41 0) (index ro_41 2)))
                     rotY_38)
                    (let rd_xz_44
                     (app (app rotate_0 (vec2 (index rd_42 0) (index rd_42 2)))
                      rotY_38)
                     (let ro_45
                      (vec3 (index ro_xz_43 0) (index ro_41 1)
                       (index ro_xz_43 1))
                      (let rd_46
                       (vec3 (index rd_xz_44 0) (index rd_42 1)
                        (index rd_xz_44 1))
                       (let t_47 (app (app march_24 ro_45) rd_46)
                        (let col_48
                         (if (> t_47 100.) (vec3 0.2 0.2 0.2)
                          (app palette_10 (* t_47 0.3)))
                         (let glow_49 (/ 0.02 (length (- uv_33 mouseUV_34)))
                          (+ col_48 glow_49)))))))))))))))))))))))

    === typecheck (raymarch.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Extern u_mouse) : (vec 2))
      ((Define Nonrec rotate_0
        ((lambda (p_1 (vec 2))
          ((lambda (angle_2 float)
            ((let s_3 ((sin (angle_2 : float)) : float)
              ((let c_4 ((cos (angle_2 : float)) : float)
                ((vec2
                  ((-
                    ((* ((index (p_1 : (vec 2)) 0) : float) (c_4 : float)) :
                     float)
                    ((* ((index (p_1 : (vec 2)) 1) : float) (s_3 : float)) :
                     float))
                   : float)
                  ((+
                    ((* ((index (p_1 : (vec 2)) 0) : float) (s_3 : float)) :
                     float)
                    ((* ((index (p_1 : (vec 2)) 1) : float) (c_4 : float)) :
                     float))
                   : float))
                 : (vec 2)))
               : (vec 2)))
             : (vec 2)))
           : (float -> (vec 2))))
         : ((vec 2) -> (float -> (vec 2)))))
       : ((vec 2) -> (float -> (vec 2))))
      ((Define Nonrec sMin_5
        ((lambda (a_6 float)
          ((lambda (b_7 float)
            ((let k_8 (0.1 : float)
              ((let h_9
                ((clamp
                  ((+ (0.5 : float)
                    ((/
                      ((* (0.5 : float)
                        ((- (b_7 : float) (a_6 : float)) : float))
                       : float)
                      (k_8 : float))
                     : float))
                   : float)
                  (0. : float) (1. : float))
                 : float)
                ((- ((mix (b_7 : float) (a_6 : float) (h_9 : float)) : float)
                  ((* ((* (k_8 : float) (h_9 : float)) : float)
                    ((- (1. : float) (h_9 : float)) : float))
                   : float))
                 : float))
               : float))
             : float))
           : (float -> float)))
         : (float -> (float -> float))))
       : (float -> (float -> float)))
      ((Define Nonrec palette_10
        ((lambda (t_11 float)
          ((let cfg_12
            ((vec3 (0.3 : float) (0.416 : float) (0.557 : float)) : (vec 3))
            ((+
              ((*
                ((cos
                  ((* ((+ (cfg_12 : (vec 3)) (t_11 : float)) : (vec 3))
                    (6.28318 : float))
                   : (vec 3)))
                 : (vec 3))
                (0.5 : float))
               : (vec 3))
              (0.5 : float))
             : (vec 3)))
           : (vec 3)))
         : (float -> (vec 3))))
       : (float -> (vec 3)))
      ((Define Nonrec sdTorus_13
        ((lambda (p_14 (vec 3))
          ((lambda (t_15 (vec 2))
            ((let q_16
              ((vec2
                ((-
                  ((length
                    ((vec2 ((index (p_14 : (vec 3)) 0) : float)
                      ((index (p_14 : (vec 3)) 2) : float))
                     : (vec 2)))
                   : float)
                  ((index (t_15 : (vec 2)) 0) : float))
                 : float)
                ((index (p_14 : (vec 3)) 1) : float))
               : (vec 2))
              ((- ((length (q_16 : (vec 2))) : float)
                ((index (t_15 : (vec 2)) 1) : float))
               : float))
             : float))
           : ((vec 2) -> float)))
         : ((vec 3) -> ((vec 2) -> float))))
       : ((vec 3) -> ((vec 2) -> float)))
      ((Define Nonrec map_17
        ((lambda (p_18 (vec 3))
          ((let angle_19 ((* (u_time : float) (2. : float)) : float)
            ((let p_xy_20
              ((app
                ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                  ((vec2 ((index (p_18 : (vec 3)) 0) : float)
                    ((index (p_18 : (vec 3)) 1) : float))
                   : (vec 2)))
                 : (float -> (vec 2)))
                (angle_19 : float))
               : (vec 2))
              ((let p_prime_21
                ((vec3 ((index (p_xy_20 : (vec 2)) 0) : float)
                  ((index (p_xy_20 : (vec 2)) 1) : float)
                  ((index (p_18 : (vec 3)) 2) : float))
                 : (vec 3))
                ((let p_yz_22
                  ((app
                    ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                      ((vec2 ((index (p_prime_21 : (vec 3)) 1) : float)
                        ((index (p_prime_21 : (vec 3)) 2) : float))
                       : (vec 2)))
                     : (float -> (vec 2)))
                    (angle_19 : float))
                   : (vec 2))
                  ((let p_prime_23
                    ((vec3 ((index (p_prime_21 : (vec 3)) 0) : float)
                      ((index (p_yz_22 : (vec 2)) 0) : float)
                      ((index (p_yz_22 : (vec 2)) 1) : float))
                     : (vec 3))
                    ((app
                      ((app (sMin_5 : (float -> (float -> float)))
                        ((app
                          ((app (sdTorus_13 : ((vec 3) -> ((vec 2) -> float)))
                            (p_prime_23 : (vec 3)))
                           : ((vec 2) -> float))
                          ((vec2 (1. : float) (0.3 : float)) : (vec 2)))
                         : float))
                       : (float -> float))
                      ((app
                        ((app (sdTorus_13 : ((vec 3) -> ((vec 2) -> float)))
                          (p_18 : (vec 3)))
                         : ((vec 2) -> float))
                        ((vec2 (2. : float) (0.5 : float)) : (vec 2)))
                       : float))
                     : float))
                   : float))
                 : float))
               : float))
             : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec march_24
        ((lambda (ro_25 (vec 3))
          ((lambda (rd_26 (vec 3))
            ((let (rec 1000 ((float -> (int -> float)))) march_27
              ((lambda (t_28 float)
                ((lambda (steps_29 int)
                  ((if ((> (steps_29 : int) (80 : int)) : bool) (t_28 : float)
                    ((let d_30
                      ((app (map_17 : ((vec 3) -> float))
                        ((+ (ro_25 : (vec 3))
                          ((* (rd_26 : (vec 3)) (t_28 : float)) : (vec 3)))
                         : (vec 3)))
                       : float)
                      ((if ((< (d_30 : float) (0.001 : float)) : bool)
                        (t_28 : float)
                        ((if ((> (t_28 : float) (100. : float)) : bool)
                          (100.1 : float)
                          ((app
                            ((app (march_27 : (float -> (int -> float)))
                              ((+ (t_28 : float) (d_30 : float)) : float))
                             : (int -> float))
                            ((+ (steps_29 : int) (1 : int)) : int))
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : (int -> float)))
               : (float -> (int -> float)))
              ((app
                ((app (march_27 : (float -> (int -> float))) (0. : float)) :
                 (int -> float))
                (0 : int))
               : float))
             : float))
           : ((vec 3) -> float)))
         : ((vec 3) -> ((vec 3) -> float))))
       : ((vec 3) -> ((vec 3) -> float)))
      ((Define Nonrec main
        ((lambda (coord_31 (vec 2))
          ((let res_min_32
            ((min ((index (u_resolution : (vec 2)) 0) : float)
              ((index (u_resolution : (vec 2)) 1) : float))
             : float)
            ((let uv_33
              ((/
                ((- ((* (coord_31 : (vec 2)) (2. : float)) : (vec 2))
                  (u_resolution : (vec 2)))
                 : (vec 2))
                (res_min_32 : float))
               : (vec 2))
              ((let mouseUV_34
                ((/
                  ((- ((* (u_mouse : (vec 2)) (2. : float)) : (vec 2))
                    (u_resolution : (vec 2)))
                   : (vec 2))
                  (res_min_32 : float))
                 : (vec 2))
                ((let ro_init_35
                  ((vec3 (0. : float) (0. : float) (-10. : float)) : (vec 3))
                  ((let rd_init_36
                    ((normalize
                      ((vec3 ((index (uv_33 : (vec 2)) 0) : float)
                        ((index (uv_33 : (vec 2)) 1) : float) (1. : float))
                       : (vec 3)))
                     : (vec 3))
                    ((let rotX_37
                      ((- (0. : float)
                        ((index (mouseUV_34 : (vec 2)) 1) : float))
                       : float)
                      ((let rotY_38
                        ((- (0. : float)
                          ((index (mouseUV_34 : (vec 2)) 0) : float))
                         : float)
                        ((let ro_yz_39
                          ((app
                            ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                              ((vec2 ((index (ro_init_35 : (vec 3)) 1) : float)
                                ((index (ro_init_35 : (vec 3)) 2) : float))
                               : (vec 2)))
                             : (float -> (vec 2)))
                            (rotX_37 : float))
                           : (vec 2))
                          ((let rd_yz_40
                            ((app
                              ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                                ((vec2 ((index (rd_init_36 : (vec 3)) 1) : float)
                                  ((index (rd_init_36 : (vec 3)) 2) : float))
                                 : (vec 2)))
                               : (float -> (vec 2)))
                              (rotX_37 : float))
                             : (vec 2))
                            ((let ro_41
                              ((vec3 ((index (ro_init_35 : (vec 3)) 0) : float)
                                ((index (ro_yz_39 : (vec 2)) 0) : float)
                                ((index (ro_yz_39 : (vec 2)) 1) : float))
                               : (vec 3))
                              ((let rd_42
                                ((vec3 ((index (rd_init_36 : (vec 3)) 0) : float)
                                  ((index (rd_yz_40 : (vec 2)) 0) : float)
                                  ((index (rd_yz_40 : (vec 2)) 1) : float))
                                 : (vec 3))
                                ((let ro_xz_43
                                  ((app
                                    ((app
                                      (rotate_0 :
                                       ((vec 2) -> (float -> (vec 2))))
                                      ((vec2
                                        ((index (ro_41 : (vec 3)) 0) : float)
                                        ((index (ro_41 : (vec 3)) 2) : float))
                                       : (vec 2)))
                                     : (float -> (vec 2)))
                                    (rotY_38 : float))
                                   : (vec 2))
                                  ((let rd_xz_44
                                    ((app
                                      ((app
                                        (rotate_0 :
                                         ((vec 2) -> (float -> (vec 2))))
                                        ((vec2
                                          ((index (rd_42 : (vec 3)) 0) : float)
                                          ((index (rd_42 : (vec 3)) 2) : float))
                                         : (vec 2)))
                                       : (float -> (vec 2)))
                                      (rotY_38 : float))
                                     : (vec 2))
                                    ((let ro_45
                                      ((vec3
                                        ((index (ro_xz_43 : (vec 2)) 0) : float)
                                        ((index (ro_41 : (vec 3)) 1) : float)
                                        ((index (ro_xz_43 : (vec 2)) 1) : float))
                                       : (vec 3))
                                      ((let rd_46
                                        ((vec3
                                          ((index (rd_xz_44 : (vec 2)) 0) :
                                           float)
                                          ((index (rd_42 : (vec 3)) 1) : float)
                                          ((index (rd_xz_44 : (vec 2)) 1) :
                                           float))
                                         : (vec 3))
                                        ((let t_47
                                          ((app
                                            ((app
                                              (march_24 :
                                               ((vec 3) -> ((vec 3) -> float)))
                                              (ro_45 : (vec 3)))
                                             : ((vec 3) -> float))
                                            (rd_46 : (vec 3)))
                                           : float)
                                          ((let col_48
                                            ((if
                                              ((> (t_47 : float) (100. : float))
                                               : bool)
                                              ((vec3 (0.2 : float) (0.2 : float)
                                                (0.2 : float))
                                               : (vec 3))
                                              ((app
                                                (palette_10 : (float -> (vec 3)))
                                                ((* (t_47 : float) (0.3 : float))
                                                 : float))
                                               : (vec 3)))
                                             : (vec 3))
                                            ((let glow_49
                                              ((/ (0.02 : float)
                                                ((length
                                                  ((- (uv_33 : (vec 2))
                                                    (mouseUV_34 : (vec 2)))
                                                   : (vec 2)))
                                                 : float))
                                               : float)
                                              ((+ (col_48 : (vec 3))
                                                (glow_49 : float))
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
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === monomorphize (raymarch.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Extern u_mouse) : (vec 2))
      ((Define Nonrec rotate_0
        ((lambda (p_1 (vec 2))
          ((lambda (angle_2 float)
            ((let s_3 ((sin (angle_2 : float)) : float)
              ((let c_4 ((cos (angle_2 : float)) : float)
                ((vec2
                  ((-
                    ((* ((index (p_1 : (vec 2)) 0) : float) (c_4 : float)) :
                     float)
                    ((* ((index (p_1 : (vec 2)) 1) : float) (s_3 : float)) :
                     float))
                   : float)
                  ((+
                    ((* ((index (p_1 : (vec 2)) 0) : float) (s_3 : float)) :
                     float)
                    ((* ((index (p_1 : (vec 2)) 1) : float) (c_4 : float)) :
                     float))
                   : float))
                 : (vec 2)))
               : (vec 2)))
             : (vec 2)))
           : (float -> (vec 2))))
         : ((vec 2) -> (float -> (vec 2)))))
       : ((vec 2) -> (float -> (vec 2))))
      ((Define Nonrec sMin_5
        ((lambda (a_6 float)
          ((lambda (b_7 float)
            ((let k_8 (0.1 : float)
              ((let h_9
                ((clamp
                  ((+ (0.5 : float)
                    ((/
                      ((* (0.5 : float)
                        ((- (b_7 : float) (a_6 : float)) : float))
                       : float)
                      (k_8 : float))
                     : float))
                   : float)
                  (0. : float) (1. : float))
                 : float)
                ((- ((mix (b_7 : float) (a_6 : float) (h_9 : float)) : float)
                  ((* ((* (k_8 : float) (h_9 : float)) : float)
                    ((- (1. : float) (h_9 : float)) : float))
                   : float))
                 : float))
               : float))
             : float))
           : (float -> float)))
         : (float -> (float -> float))))
       : (float -> (float -> float)))
      ((Define Nonrec palette_10
        ((lambda (t_11 float)
          ((let cfg_12
            ((vec3 (0.3 : float) (0.416 : float) (0.557 : float)) : (vec 3))
            ((+
              ((*
                ((cos
                  ((* ((+ (cfg_12 : (vec 3)) (t_11 : float)) : (vec 3))
                    (6.28318 : float))
                   : (vec 3)))
                 : (vec 3))
                (0.5 : float))
               : (vec 3))
              (0.5 : float))
             : (vec 3)))
           : (vec 3)))
         : (float -> (vec 3))))
       : (float -> (vec 3)))
      ((Define Nonrec sdTorus_13
        ((lambda (p_14 (vec 3))
          ((lambda (t_15 (vec 2))
            ((let q_16
              ((vec2
                ((-
                  ((length
                    ((vec2 ((index (p_14 : (vec 3)) 0) : float)
                      ((index (p_14 : (vec 3)) 2) : float))
                     : (vec 2)))
                   : float)
                  ((index (t_15 : (vec 2)) 0) : float))
                 : float)
                ((index (p_14 : (vec 3)) 1) : float))
               : (vec 2))
              ((- ((length (q_16 : (vec 2))) : float)
                ((index (t_15 : (vec 2)) 1) : float))
               : float))
             : float))
           : ((vec 2) -> float)))
         : ((vec 3) -> ((vec 2) -> float))))
       : ((vec 3) -> ((vec 2) -> float)))
      ((Define Nonrec map_17
        ((lambda (p_18 (vec 3))
          ((let angle_19 ((* (u_time : float) (2. : float)) : float)
            ((let p_xy_20
              ((app
                ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                  ((vec2 ((index (p_18 : (vec 3)) 0) : float)
                    ((index (p_18 : (vec 3)) 1) : float))
                   : (vec 2)))
                 : (float -> (vec 2)))
                (angle_19 : float))
               : (vec 2))
              ((let p_prime_21
                ((vec3 ((index (p_xy_20 : (vec 2)) 0) : float)
                  ((index (p_xy_20 : (vec 2)) 1) : float)
                  ((index (p_18 : (vec 3)) 2) : float))
                 : (vec 3))
                ((let p_yz_22
                  ((app
                    ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                      ((vec2 ((index (p_prime_21 : (vec 3)) 1) : float)
                        ((index (p_prime_21 : (vec 3)) 2) : float))
                       : (vec 2)))
                     : (float -> (vec 2)))
                    (angle_19 : float))
                   : (vec 2))
                  ((let p_prime_23
                    ((vec3 ((index (p_prime_21 : (vec 3)) 0) : float)
                      ((index (p_yz_22 : (vec 2)) 0) : float)
                      ((index (p_yz_22 : (vec 2)) 1) : float))
                     : (vec 3))
                    ((app
                      ((app (sMin_5 : (float -> (float -> float)))
                        ((app
                          ((app (sdTorus_13 : ((vec 3) -> ((vec 2) -> float)))
                            (p_prime_23 : (vec 3)))
                           : ((vec 2) -> float))
                          ((vec2 (1. : float) (0.3 : float)) : (vec 2)))
                         : float))
                       : (float -> float))
                      ((app
                        ((app (sdTorus_13 : ((vec 3) -> ((vec 2) -> float)))
                          (p_18 : (vec 3)))
                         : ((vec 2) -> float))
                        ((vec2 (2. : float) (0.5 : float)) : (vec 2)))
                       : float))
                     : float))
                   : float))
                 : float))
               : float))
             : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec march_24
        ((lambda (ro_25 (vec 3))
          ((lambda (rd_26 (vec 3))
            ((let (rec 1000 ((float -> (int -> float)))) march_27
              ((lambda (t_28 float)
                ((lambda (steps_29 int)
                  ((if ((> (steps_29 : int) (80 : int)) : bool) (t_28 : float)
                    ((let d_30
                      ((app (map_17 : ((vec 3) -> float))
                        ((+ (ro_25 : (vec 3))
                          ((* (rd_26 : (vec 3)) (t_28 : float)) : (vec 3)))
                         : (vec 3)))
                       : float)
                      ((if ((< (d_30 : float) (0.001 : float)) : bool)
                        (t_28 : float)
                        ((if ((> (t_28 : float) (100. : float)) : bool)
                          (100.1 : float)
                          ((app
                            ((app (march_27 : (float -> (int -> float)))
                              ((+ (t_28 : float) (d_30 : float)) : float))
                             : (int -> float))
                            ((+ (steps_29 : int) (1 : int)) : int))
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : (int -> float)))
               : (float -> (int -> float)))
              ((app
                ((app (march_27 : (float -> (int -> float))) (0. : float)) :
                 (int -> float))
                (0 : int))
               : float))
             : float))
           : ((vec 3) -> float)))
         : ((vec 3) -> ((vec 3) -> float))))
       : ((vec 3) -> ((vec 3) -> float)))
      ((Define Nonrec main
        ((lambda (coord_31 (vec 2))
          ((let res_min_32
            ((min ((index (u_resolution : (vec 2)) 0) : float)
              ((index (u_resolution : (vec 2)) 1) : float))
             : float)
            ((let uv_33
              ((/
                ((- ((* (coord_31 : (vec 2)) (2. : float)) : (vec 2))
                  (u_resolution : (vec 2)))
                 : (vec 2))
                (res_min_32 : float))
               : (vec 2))
              ((let mouseUV_34
                ((/
                  ((- ((* (u_mouse : (vec 2)) (2. : float)) : (vec 2))
                    (u_resolution : (vec 2)))
                   : (vec 2))
                  (res_min_32 : float))
                 : (vec 2))
                ((let ro_init_35
                  ((vec3 (0. : float) (0. : float) (-10. : float)) : (vec 3))
                  ((let rd_init_36
                    ((normalize
                      ((vec3 ((index (uv_33 : (vec 2)) 0) : float)
                        ((index (uv_33 : (vec 2)) 1) : float) (1. : float))
                       : (vec 3)))
                     : (vec 3))
                    ((let rotX_37
                      ((- (0. : float)
                        ((index (mouseUV_34 : (vec 2)) 1) : float))
                       : float)
                      ((let rotY_38
                        ((- (0. : float)
                          ((index (mouseUV_34 : (vec 2)) 0) : float))
                         : float)
                        ((let ro_yz_39
                          ((app
                            ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                              ((vec2 ((index (ro_init_35 : (vec 3)) 1) : float)
                                ((index (ro_init_35 : (vec 3)) 2) : float))
                               : (vec 2)))
                             : (float -> (vec 2)))
                            (rotX_37 : float))
                           : (vec 2))
                          ((let rd_yz_40
                            ((app
                              ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                                ((vec2 ((index (rd_init_36 : (vec 3)) 1) : float)
                                  ((index (rd_init_36 : (vec 3)) 2) : float))
                                 : (vec 2)))
                               : (float -> (vec 2)))
                              (rotX_37 : float))
                             : (vec 2))
                            ((let ro_41
                              ((vec3 ((index (ro_init_35 : (vec 3)) 0) : float)
                                ((index (ro_yz_39 : (vec 2)) 0) : float)
                                ((index (ro_yz_39 : (vec 2)) 1) : float))
                               : (vec 3))
                              ((let rd_42
                                ((vec3 ((index (rd_init_36 : (vec 3)) 0) : float)
                                  ((index (rd_yz_40 : (vec 2)) 0) : float)
                                  ((index (rd_yz_40 : (vec 2)) 1) : float))
                                 : (vec 3))
                                ((let ro_xz_43
                                  ((app
                                    ((app
                                      (rotate_0 :
                                       ((vec 2) -> (float -> (vec 2))))
                                      ((vec2
                                        ((index (ro_41 : (vec 3)) 0) : float)
                                        ((index (ro_41 : (vec 3)) 2) : float))
                                       : (vec 2)))
                                     : (float -> (vec 2)))
                                    (rotY_38 : float))
                                   : (vec 2))
                                  ((let rd_xz_44
                                    ((app
                                      ((app
                                        (rotate_0 :
                                         ((vec 2) -> (float -> (vec 2))))
                                        ((vec2
                                          ((index (rd_42 : (vec 3)) 0) : float)
                                          ((index (rd_42 : (vec 3)) 2) : float))
                                         : (vec 2)))
                                       : (float -> (vec 2)))
                                      (rotY_38 : float))
                                     : (vec 2))
                                    ((let ro_45
                                      ((vec3
                                        ((index (ro_xz_43 : (vec 2)) 0) : float)
                                        ((index (ro_41 : (vec 3)) 1) : float)
                                        ((index (ro_xz_43 : (vec 2)) 1) : float))
                                       : (vec 3))
                                      ((let rd_46
                                        ((vec3
                                          ((index (rd_xz_44 : (vec 2)) 0) :
                                           float)
                                          ((index (rd_42 : (vec 3)) 1) : float)
                                          ((index (rd_xz_44 : (vec 2)) 1) :
                                           float))
                                         : (vec 3))
                                        ((let t_47
                                          ((app
                                            ((app
                                              (march_24 :
                                               ((vec 3) -> ((vec 3) -> float)))
                                              (ro_45 : (vec 3)))
                                             : ((vec 3) -> float))
                                            (rd_46 : (vec 3)))
                                           : float)
                                          ((let col_48
                                            ((if
                                              ((> (t_47 : float) (100. : float))
                                               : bool)
                                              ((vec3 (0.2 : float) (0.2 : float)
                                                (0.2 : float))
                                               : (vec 3))
                                              ((app
                                                (palette_10 : (float -> (vec 3)))
                                                ((* (t_47 : float) (0.3 : float))
                                                 : float))
                                               : (vec 3)))
                                             : (vec 3))
                                            ((let glow_49
                                              ((/ (0.02 : float)
                                                ((length
                                                  ((- (uv_33 : (vec 2))
                                                    (mouseUV_34 : (vec 2)))
                                                   : (vec 2)))
                                                 : float))
                                               : float)
                                              ((+ (col_48 : (vec 3))
                                                (glow_49 : float))
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
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (raymarch.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Extern u_mouse) : (vec 2))
      ((Define Nonrec rotate_0
        (lambda ((p_1 (vec 2)) (angle_2 float))
         (let s_3 (sin angle_2)
          (let c_4 (cos angle_2)
           (vec2 (- (* (index p_1 0) c_4) (* (index p_1 1) s_3))
            (+ (* (index p_1 0) s_3) (* (index p_1 1) c_4)))))))
       : ((vec 2) -> (float -> (vec 2))))
      ((Define Nonrec sMin_5
        (lambda ((a_6 float) (b_7 float))
         (let k_8 0.1
          (let h_9 (clamp (+ 0.5 (/ (* 0.5 (- b_7 a_6)) k_8)) 0. 1.)
           (- (mix b_7 a_6 h_9) (* (* k_8 h_9) (- 1. h_9)))))))
       : (float -> (float -> float)))
      ((Define Nonrec palette_10
        (lambda ((t_11 float))
         (let cfg_12 (vec3 0.3 0.416 0.557)
          (+ (* (cos (* (+ cfg_12 t_11) 6.28318)) 0.5) 0.5))))
       : (float -> (vec 3)))
      ((Define Nonrec sdTorus_13
        (lambda ((p_14 (vec 3)) (t_15 (vec 2)))
         (let q_16
          (vec2 (- (length (vec2 (index p_14 0) (index p_14 2))) (index t_15 0))
           (index p_14 1))
          (- (length q_16) (index t_15 1)))))
       : ((vec 3) -> ((vec 2) -> float)))
      ((Define Nonrec map_17
        (lambda ((p_18 (vec 3)))
         (let angle_19 (* u_time 2.)
          (let p_xy_20
           (app rotate_0 (vec2 (index p_18 0) (index p_18 1)) angle_19)
           (let p_prime_21
            (vec3 (index p_xy_20 0) (index p_xy_20 1) (index p_18 2))
            (let p_yz_22
             (app rotate_0 (vec2 (index p_prime_21 1) (index p_prime_21 2))
              angle_19)
             (let p_prime_23
              (vec3 (index p_prime_21 0) (index p_yz_22 0) (index p_yz_22 1))
              (app sMin_5 (app sdTorus_13 p_prime_23 (vec2 1. 0.3))
               (app sdTorus_13 p_18 (vec2 2. 0.5))))))))))
       : ((vec 3) -> float))
      ((Define Nonrec march_24
        (lambda ((ro_25 (vec 3)) (rd_26 (vec 3)))
         (let (rec 1000 ((float -> (int -> float)))) march_27
          (lambda ((t_28 float) (steps_29 int))
           (if (> steps_29 80) t_28
            (let d_30 (app map_17 (+ ro_25 (* rd_26 t_28)))
             (if (< d_30 0.001) t_28
              (if (> t_28 100.) 100.1
               (app march_27 (+ t_28 d_30) (+ steps_29 1)))))))
          (app march_27 0. 0))))
       : ((vec 3) -> ((vec 3) -> float)))
      ((Define Nonrec main
        (lambda ((coord_31 (vec 2)))
         (let res_min_32 (min (index u_resolution 0) (index u_resolution 1))
          (let uv_33 (/ (- (* coord_31 2.) u_resolution) res_min_32)
           (let mouseUV_34 (/ (- (* u_mouse 2.) u_resolution) res_min_32)
            (let ro_init_35 (vec3 0. 0. -10.)
             (let rd_init_36
              (normalize (vec3 (index uv_33 0) (index uv_33 1) 1.))
              (let rotX_37 (- 0. (index mouseUV_34 1))
               (let rotY_38 (- 0. (index mouseUV_34 0))
                (let ro_yz_39
                 (app rotate_0 (vec2 (index ro_init_35 1) (index ro_init_35 2))
                  rotX_37)
                 (let rd_yz_40
                  (app rotate_0 (vec2 (index rd_init_36 1) (index rd_init_36 2))
                   rotX_37)
                  (let ro_41
                   (vec3 (index ro_init_35 0) (index ro_yz_39 0)
                    (index ro_yz_39 1))
                   (let rd_42
                    (vec3 (index rd_init_36 0) (index rd_yz_40 0)
                     (index rd_yz_40 1))
                    (let ro_xz_43
                     (app rotate_0 (vec2 (index ro_41 0) (index ro_41 2))
                      rotY_38)
                     (let rd_xz_44
                      (app rotate_0 (vec2 (index rd_42 0) (index rd_42 2))
                       rotY_38)
                      (let ro_45
                       (vec3 (index ro_xz_43 0) (index ro_41 1)
                        (index ro_xz_43 1))
                       (let rd_46
                        (vec3 (index rd_xz_44 0) (index rd_42 1)
                         (index rd_xz_44 1))
                        (let t_47 (app march_24 ro_45 rd_46)
                         (let col_48
                          (if (> t_47 100.) (vec3 0.2 0.2 0.2)
                           (app palette_10 (* t_47 0.3)))
                          (let glow_49 (/ 0.02 (length (- uv_33 mouseUV_34)))
                           (+ col_48 glow_49)))))))))))))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda_lift (raymarch.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define Nonrec (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (vec2 (- (* (index p_1 0) c_4) (* (index p_1 1) s_3))
           (+ (* (index p_1 0) s_3) (* (index p_1 1) c_4)))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define Nonrec (name sMin_5) (args ((a_6 float) (b_7 float)))
       (body
        (let k_8 0.1
         (let h_9 (clamp (+ 0.5 (/ (* 0.5 (- b_7 a_6)) k_8)) 0. 1.)
          (- (mix b_7 a_6 h_9) (* (* k_8 h_9) (- 1. h_9)))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name palette_10) (args ((t_11 float)))
       (body
        (let cfg_12 (vec3 0.3 0.416 0.557)
         (+ (* (cos (* (+ cfg_12 t_11) 6.28318)) 0.5) 0.5))))
      : (float -> (vec 3)))
     ((Define Nonrec (name sdTorus_13) (args ((p_14 (vec 3)) (t_15 (vec 2))))
       (body
        (let q_16
         (vec2 (- (length (vec2 (index p_14 0) (index p_14 2))) (index t_15 0))
          (index p_14 1))
         (- (length q_16) (index t_15 1)))))
      : ((vec 3) -> ((vec 2) -> float)))
     ((Define Nonrec (name map_17) (args ((p_18 (vec 3))))
       (body
        (let angle_19 (* u_time 2.)
         (let p_xy_20
          (app rotate_0 (vec2 (index p_18 0) (index p_18 1)) angle_19)
          (let p_prime_21
           (vec3 (index p_xy_20 0) (index p_xy_20 1) (index p_18 2))
           (let p_yz_22
            (app rotate_0 (vec2 (index p_prime_21 1) (index p_prime_21 2))
             angle_19)
            (let p_prime_23
             (vec3 (index p_prime_21 0) (index p_yz_22 0) (index p_yz_22 1))
             (app sMin_5 (app sdTorus_13 p_prime_23 (vec2 1. 0.3))
              (app sdTorus_13 p_18 (vec2 2. 0.5))))))))))
      : ((vec 3) -> float))
     ((Define (Rec 1000 ((float -> (int -> float)))) (name march_27_175)
       (args ((rd_26 (vec 3)) (ro_25 (vec 3)) (t_28 float) (steps_29 int)))
       (body
        (if (> steps_29 80) t_28
         (let d_30 (app map_17 (+ ro_25 (* rd_26 t_28)))
          (if (< d_30 0.001) t_28
           (if (> t_28 100.) 100.1
            (app march_27_175 rd_26 ro_25 (+ t_28 d_30) (+ steps_29 1))))))))
      : (float -> (int -> float)))
     ((Define Nonrec (name march_24) (args ((ro_25 (vec 3)) (rd_26 (vec 3))))
       (body (app march_27_175 rd_26 ro_25 0. 0)))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define Nonrec (name main) (args ((coord_31 (vec 2))))
       (body
        (let res_min_32 (min (index u_resolution 0) (index u_resolution 1))
         (let uv_33 (/ (- (* coord_31 2.) u_resolution) res_min_32)
          (let mouseUV_34 (/ (- (* u_mouse 2.) u_resolution) res_min_32)
           (let ro_init_35 (vec3 0. 0. -10.)
            (let rd_init_36 (normalize (vec3 (index uv_33 0) (index uv_33 1) 1.))
             (let rotX_37 (- 0. (index mouseUV_34 1))
              (let rotY_38 (- 0. (index mouseUV_34 0))
               (let ro_yz_39
                (app rotate_0 (vec2 (index ro_init_35 1) (index ro_init_35 2))
                 rotX_37)
                (let rd_yz_40
                 (app rotate_0 (vec2 (index rd_init_36 1) (index rd_init_36 2))
                  rotX_37)
                 (let ro_41
                  (vec3 (index ro_init_35 0) (index ro_yz_39 0)
                   (index ro_yz_39 1))
                  (let rd_42
                   (vec3 (index rd_init_36 0) (index rd_yz_40 0)
                    (index rd_yz_40 1))
                   (let ro_xz_43
                    (app rotate_0 (vec2 (index ro_41 0) (index ro_41 2)) rotY_38)
                    (let rd_xz_44
                     (app rotate_0 (vec2 (index rd_42 0) (index rd_42 2))
                      rotY_38)
                     (let ro_45
                      (vec3 (index ro_xz_43 0) (index ro_41 1)
                       (index ro_xz_43 1))
                      (let rd_46
                       (vec3 (index rd_xz_44 0) (index rd_42 1)
                        (index rd_xz_44 1))
                       (let t_47 (app march_24 ro_45 rd_46)
                        (let col_48
                         (if (> t_47 100.) (vec3 0.2 0.2 0.2)
                          (app palette_10 (* t_47 0.3)))
                         (let glow_49 (/ 0.02 (length (- uv_33 mouseUV_34)))
                          (+ col_48 glow_49)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === anf (raymarch.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define Nonrec (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_176 (index p_1 0)
           (let anf_177 (* anf_176 c_4)
            (let anf_178 (index p_1 1)
             (let anf_179 (* anf_178 s_3)
              (let anf_180 (- anf_177 anf_179)
               (let anf_181 (index p_1 0)
                (let anf_182 (* anf_181 s_3)
                 (let anf_183 (index p_1 1)
                  (let anf_184 (* anf_183 c_4)
                   (let anf_185 (+ anf_182 anf_184)
                    (return (vec2 anf_180 anf_185))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define Nonrec (name sMin_5) (args ((a_6 float) (b_7 float)))
       (body
        (let k_8 0.1
         (let anf_186 (- b_7 a_6)
          (let anf_187 (* 0.5 anf_186)
           (let anf_188 (/ anf_187 k_8)
            (let anf_189 (+ 0.5 anf_188)
             (let h_9 (clamp anf_189 0. 1.)
              (let anf_190 (mix b_7 a_6 h_9)
               (let anf_191 (* k_8 h_9)
                (let anf_192 (- 1. h_9)
                 (let anf_193 (* anf_191 anf_192) (return (- anf_190 anf_193))))))))))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name palette_10) (args ((t_11 float)))
       (body
        (let cfg_12 (vec3 0.3 0.416 0.557)
         (let anf_194 (+ cfg_12 t_11)
          (let anf_195 (* anf_194 6.28318)
           (let anf_196 (cos anf_195)
            (let anf_197 (* anf_196 0.5) (return (+ anf_197 0.5)))))))))
      : (float -> (vec 3)))
     ((Define Nonrec (name sdTorus_13) (args ((p_14 (vec 3)) (t_15 (vec 2))))
       (body
        (let anf_198 (index p_14 0)
         (let anf_199 (index p_14 2)
          (let anf_200 (vec2 anf_198 anf_199)
           (let anf_201 (length anf_200)
            (let anf_202 (index t_15 0)
             (let anf_203 (- anf_201 anf_202)
              (let anf_204 (index p_14 1)
               (let q_16 (vec2 anf_203 anf_204)
                (let anf_205 (length q_16)
                 (let anf_206 (index t_15 1) (return (- anf_205 anf_206))))))))))))))
      : ((vec 3) -> ((vec 2) -> float)))
     ((Define Nonrec (name map_17) (args ((p_18 (vec 3))))
       (body
        (let angle_19 (* u_time 2.)
         (let anf_207 (index p_18 0)
          (let anf_208 (index p_18 1)
           (let anf_209 (vec2 anf_207 anf_208)
            (let p_xy_20 (rotate_0 anf_209 angle_19)
             (let anf_210 (index p_xy_20 0)
              (let anf_211 (index p_xy_20 1)
               (let anf_212 (index p_18 2)
                (let p_prime_21 (vec3 anf_210 anf_211 anf_212)
                 (let anf_213 (index p_prime_21 1)
                  (let anf_214 (index p_prime_21 2)
                   (let anf_215 (vec2 anf_213 anf_214)
                    (let p_yz_22 (rotate_0 anf_215 angle_19)
                     (let anf_216 (index p_prime_21 0)
                      (let anf_217 (index p_yz_22 0)
                       (let anf_218 (index p_yz_22 1)
                        (let p_prime_23 (vec3 anf_216 anf_217 anf_218)
                         (let anf_219 (vec2 1. 0.3)
                          (let anf_220 (sdTorus_13 p_prime_23 anf_219)
                           (let anf_221 (vec2 2. 0.5)
                            (let anf_222 (sdTorus_13 p_18 anf_221)
                             (return (sMin_5 anf_220 anf_222)))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (Rec 1000 ((float -> (int -> float)))) (name march_27_175)
       (args ((rd_26 (vec 3)) (ro_25 (vec 3)) (t_28 float) (steps_29 int)))
       (body
        (let anf_223 (> steps_29 80)
         (return
          (if anf_223 (return t_28)
           (let anf_224 (* rd_26 t_28)
            (let anf_225 (+ ro_25 anf_224)
             (let d_30 (map_17 anf_225)
              (let anf_226 (< d_30 0.001)
               (return
                (if anf_226 (return t_28)
                 (let anf_227 (> t_28 100.)
                  (return
                   (if anf_227 (return 100.1)
                    (let anf_228 (+ t_28 d_30)
                     (let anf_229 (+ steps_29 1)
                      (return (march_27_175 rd_26 ro_25 anf_228 anf_229))))))))))))))))))
      : (float -> (int -> float)))
     ((Define Nonrec (name march_24) (args ((ro_25 (vec 3)) (rd_26 (vec 3))))
       (body (return (march_27_175 rd_26 ro_25 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define Nonrec (name main) (args ((coord_31 (vec 2))))
       (body
        (let anf_230 (index u_resolution 0)
         (let anf_231 (index u_resolution 1)
          (let res_min_32 (min anf_230 anf_231)
           (let anf_232 (* coord_31 2.)
            (let anf_233 (- anf_232 u_resolution)
             (let uv_33 (/ anf_233 res_min_32)
              (let anf_234 (* u_mouse 2.)
               (let anf_235 (- anf_234 u_resolution)
                (let mouseUV_34 (/ anf_235 res_min_32)
                 (let ro_init_35 (vec3 0. 0. -10.)
                  (let anf_236 (index uv_33 0)
                   (let anf_237 (index uv_33 1)
                    (let anf_238 (vec3 anf_236 anf_237 1.)
                     (let rd_init_36 (normalize anf_238)
                      (let anf_239 (index mouseUV_34 1)
                       (let rotX_37 (- 0. anf_239)
                        (let anf_240 (index mouseUV_34 0)
                         (let rotY_38 (- 0. anf_240)
                          (let anf_241 (index ro_init_35 1)
                           (let anf_242 (index ro_init_35 2)
                            (let anf_243 (vec2 anf_241 anf_242)
                             (let ro_yz_39 (rotate_0 anf_243 rotX_37)
                              (let anf_244 (index rd_init_36 1)
                               (let anf_245 (index rd_init_36 2)
                                (let anf_246 (vec2 anf_244 anf_245)
                                 (let rd_yz_40 (rotate_0 anf_246 rotX_37)
                                  (let anf_247 (index ro_init_35 0)
                                   (let anf_248 (index ro_yz_39 0)
                                    (let anf_249 (index ro_yz_39 1)
                                     (let ro_41 (vec3 anf_247 anf_248 anf_249)
                                      (let anf_250 (index rd_init_36 0)
                                       (let anf_251 (index rd_yz_40 0)
                                        (let anf_252 (index rd_yz_40 1)
                                         (let rd_42
                                          (vec3 anf_250 anf_251 anf_252)
                                          (let anf_253 (index ro_41 0)
                                           (let anf_254 (index ro_41 2)
                                            (let anf_255 (vec2 anf_253 anf_254)
                                             (let ro_xz_43
                                              (rotate_0 anf_255 rotY_38)
                                              (let anf_256 (index rd_42 0)
                                               (let anf_257 (index rd_42 2)
                                                (let anf_258
                                                 (vec2 anf_256 anf_257)
                                                 (let rd_xz_44
                                                  (rotate_0 anf_258 rotY_38)
                                                  (let anf_259 (index ro_xz_43 0)
                                                   (let anf_260 (index ro_41 1)
                                                    (let anf_261
                                                     (index ro_xz_43 1)
                                                     (let ro_45
                                                      (vec3 anf_259 anf_260
                                                       anf_261)
                                                      (let anf_262
                                                       (index rd_xz_44 0)
                                                       (let anf_263
                                                        (index rd_42 1)
                                                        (let anf_264
                                                         (index rd_xz_44 1)
                                                         (let rd_46
                                                          (vec3 anf_262 anf_263
                                                           anf_264)
                                                          (let t_47
                                                           (march_24 ro_45 rd_46)
                                                           (let anf_265
                                                            (> t_47 100.)
                                                            (let col_48
                                                             (if anf_265
                                                              (return
                                                               (vec3 0.2 0.2 0.2))
                                                              (let anf_266
                                                               (* t_47 0.3)
                                                               (return
                                                                (palette_10
                                                                 anf_266))))
                                                             (let anf_267
                                                              (- uv_33
                                                               mouseUV_34)
                                                              (let anf_268
                                                               (length anf_267)
                                                               (let glow_49
                                                                (/ 0.02 anf_268)
                                                                (return
                                                                 (+ col_48
                                                                  glow_49))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (raymarch.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_176 (index p_1 0)
           (let anf_177 (* anf_176 c_4)
            (let anf_178 (index p_1 1)
             (let anf_179 (* anf_178 s_3)
              (let anf_180 (- anf_177 anf_179)
               (let anf_181 (index p_1 0)
                (let anf_182 (* anf_181 s_3)
                 (let anf_183 (index p_1 1)
                  (let anf_184 (* anf_183 c_4)
                   (let anf_185 (+ anf_182 anf_184)
                    (return (vec2 anf_180 anf_185))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define (name sMin_5) (args ((a_6 float) (b_7 float)))
       (body
        (let k_8 0.1
         (let anf_186 (- b_7 a_6)
          (let anf_187 (* 0.5 anf_186)
           (let anf_188 (/ anf_187 k_8)
            (let anf_189 (+ 0.5 anf_188)
             (let h_9 (clamp anf_189 0. 1.)
              (let anf_190 (mix b_7 a_6 h_9)
               (let anf_191 (* k_8 h_9)
                (let anf_192 (- 1. h_9)
                 (let anf_193 (* anf_191 anf_192) (return (- anf_190 anf_193))))))))))))))
      : (float -> (float -> float)))
     ((Define (name palette_10) (args ((t_11 float)))
       (body
        (let cfg_12 (vec3 0.3 0.416 0.557)
         (let anf_194 (+ cfg_12 t_11)
          (let anf_195 (* anf_194 6.28318)
           (let anf_196 (cos anf_195)
            (let anf_197 (* anf_196 0.5) (return (+ anf_197 0.5)))))))))
      : (float -> (vec 3)))
     ((Define (name sdTorus_13) (args ((p_14 (vec 3)) (t_15 (vec 2))))
       (body
        (let anf_198 (index p_14 0)
         (let anf_199 (index p_14 2)
          (let anf_200 (vec2 anf_198 anf_199)
           (let anf_201 (length anf_200)
            (let anf_202 (index t_15 0)
             (let anf_203 (- anf_201 anf_202)
              (let anf_204 (index p_14 1)
               (let q_16 (vec2 anf_203 anf_204)
                (let anf_205 (length q_16)
                 (let anf_206 (index t_15 1) (return (- anf_205 anf_206))))))))))))))
      : ((vec 3) -> ((vec 2) -> float)))
     ((Define (name map_17) (args ((p_18 (vec 3))))
       (body
        (let angle_19 (* u_time 2.)
         (let anf_207 (index p_18 0)
          (let anf_208 (index p_18 1)
           (let anf_209 (vec2 anf_207 anf_208)
            (let p_xy_20 (rotate_0 anf_209 angle_19)
             (let anf_210 (index p_xy_20 0)
              (let anf_211 (index p_xy_20 1)
               (let anf_212 (index p_18 2)
                (let p_prime_21 (vec3 anf_210 anf_211 anf_212)
                 (let anf_213 (index p_prime_21 1)
                  (let anf_214 (index p_prime_21 2)
                   (let anf_215 (vec2 anf_213 anf_214)
                    (let p_yz_22 (rotate_0 anf_215 angle_19)
                     (let anf_216 (index p_prime_21 0)
                      (let anf_217 (index p_yz_22 0)
                       (let anf_218 (index p_yz_22 1)
                        (let p_prime_23 (vec3 anf_216 anf_217 anf_218)
                         (let anf_219 (vec2 1. 0.3)
                          (let anf_220 (sdTorus_13 p_prime_23 anf_219)
                           (let anf_221 (vec2 2. 0.5)
                            (let anf_222 (sdTorus_13 p_18 anf_221)
                             (return (sMin_5 anf_220 anf_222)))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name march_27_175)
       (args ((rd_26 (vec 3)) (ro_25 (vec 3)) (t_28 float) (steps_29 int)))
       (body
        (let _iter_269 0
         (while (< _iter_269 1000)
          (let anf_223 (> steps_29 80)
           (return
            (if anf_223 (return t_28)
             (let anf_224 (* rd_26 t_28)
              (let anf_225 (+ ro_25 anf_224)
               (let d_30 (map_17 anf_225)
                (let anf_226 (< d_30 0.001)
                 (return
                  (if anf_226 (return t_28)
                   (let anf_227 (> t_28 100.)
                    (return
                     (if anf_227 (return 100.1)
                      (let anf_228 (+ t_28 d_30)
                       (let anf_229 (+ steps_29 1)
                        (set rd_26 rd_26
                         (set ro_25 ro_25
                          (set t_28 anf_228
                           (set steps_29 anf_229
                            (let _iter_inc_270 (+ _iter_269 1)
                             (set _iter_269 _iter_inc_270 continue))))))))))))))))))))
          (return 0.)))))
      : (float -> (int -> float)))
     ((Define (name march_24) (args ((ro_25 (vec 3)) (rd_26 (vec 3))))
       (body (return (march_27_175 rd_26 ro_25 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define (name main) (args ((coord_31 (vec 2))))
       (body
        (let anf_230 (index u_resolution 0)
         (let anf_231 (index u_resolution 1)
          (let res_min_32 (min anf_230 anf_231)
           (let anf_232 (* coord_31 2.)
            (let anf_233 (- anf_232 u_resolution)
             (let uv_33 (/ anf_233 res_min_32)
              (let anf_234 (* u_mouse 2.)
               (let anf_235 (- anf_234 u_resolution)
                (let mouseUV_34 (/ anf_235 res_min_32)
                 (let ro_init_35 (vec3 0. 0. -10.)
                  (let anf_236 (index uv_33 0)
                   (let anf_237 (index uv_33 1)
                    (let anf_238 (vec3 anf_236 anf_237 1.)
                     (let rd_init_36 (normalize anf_238)
                      (let anf_239 (index mouseUV_34 1)
                       (let rotX_37 (- 0. anf_239)
                        (let anf_240 (index mouseUV_34 0)
                         (let rotY_38 (- 0. anf_240)
                          (let anf_241 (index ro_init_35 1)
                           (let anf_242 (index ro_init_35 2)
                            (let anf_243 (vec2 anf_241 anf_242)
                             (let ro_yz_39 (rotate_0 anf_243 rotX_37)
                              (let anf_244 (index rd_init_36 1)
                               (let anf_245 (index rd_init_36 2)
                                (let anf_246 (vec2 anf_244 anf_245)
                                 (let rd_yz_40 (rotate_0 anf_246 rotX_37)
                                  (let anf_247 (index ro_init_35 0)
                                   (let anf_248 (index ro_yz_39 0)
                                    (let anf_249 (index ro_yz_39 1)
                                     (let ro_41 (vec3 anf_247 anf_248 anf_249)
                                      (let anf_250 (index rd_init_36 0)
                                       (let anf_251 (index rd_yz_40 0)
                                        (let anf_252 (index rd_yz_40 1)
                                         (let rd_42
                                          (vec3 anf_250 anf_251 anf_252)
                                          (let anf_253 (index ro_41 0)
                                           (let anf_254 (index ro_41 2)
                                            (let anf_255 (vec2 anf_253 anf_254)
                                             (let ro_xz_43
                                              (rotate_0 anf_255 rotY_38)
                                              (let anf_256 (index rd_42 0)
                                               (let anf_257 (index rd_42 2)
                                                (let anf_258
                                                 (vec2 anf_256 anf_257)
                                                 (let rd_xz_44
                                                  (rotate_0 anf_258 rotY_38)
                                                  (let anf_259 (index ro_xz_43 0)
                                                   (let anf_260 (index ro_41 1)
                                                    (let anf_261
                                                     (index ro_xz_43 1)
                                                     (let ro_45
                                                      (vec3 anf_259 anf_260
                                                       anf_261)
                                                      (let anf_262
                                                       (index rd_xz_44 0)
                                                       (let anf_263
                                                        (index rd_42 1)
                                                        (let anf_264
                                                         (index rd_xz_44 1)
                                                         (let rd_46
                                                          (vec3 anf_262 anf_263
                                                           anf_264)
                                                          (let t_47
                                                           (march_24 ro_45 rd_46)
                                                           (let anf_265
                                                            (> t_47 100.)
                                                            (let col_48
                                                             (if anf_265
                                                              (return
                                                               (vec3 0.2 0.2 0.2))
                                                              (let anf_266
                                                               (* t_47 0.3)
                                                               (return
                                                                (palette_10
                                                                 anf_266))))
                                                             (let anf_267
                                                              (- uv_33
                                                               mouseUV_34)
                                                              (let anf_268
                                                               (length anf_267)
                                                               (let glow_49
                                                                (/ 0.02 anf_268)
                                                                (return
                                                                 (+ col_48
                                                                  glow_49))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (raymarch.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Global Uniform (TyVec 2) u_mouse)
      (Function (name rotate_0) (desc ())
       (params (((TyVec 2) p_1) (TyFloat angle_2))) (ret_type (TyVec 2))
       (body
        ((set () float s_3 (sin angle_2)) (set () float c_4 (cos angle_2))
         (set () float anf_176 (index p_1 0))
         (set () float anf_177 (* anf_176 c_4))
         (set () float anf_178 (index p_1 1))
         (set () float anf_179 (* anf_178 s_3))
         (set () float anf_180 (- anf_177 anf_179))
         (set () float anf_181 (index p_1 0))
         (set () float anf_182 (* anf_181 s_3))
         (set () float anf_183 (index p_1 1))
         (set () float anf_184 (* anf_183 c_4))
         (set () float anf_185 (+ anf_182 anf_184))
         (return (vec2 anf_180 anf_185)))))
      (Function (name sMin_5) (desc ()) (params ((TyFloat a_6) (TyFloat b_7)))
       (ret_type TyFloat)
       (body
        ((set () float k_8 0.1) (set () float anf_186 (- b_7 a_6))
         (set () float anf_187 (* 0.5 anf_186))
         (set () float anf_188 (/ anf_187 k_8))
         (set () float anf_189 (+ 0.5 anf_188))
         (set () float h_9 (clamp anf_189 0. 1.))
         (set () float anf_190 (mix b_7 a_6 h_9))
         (set () float anf_191 (* k_8 h_9)) (set () float anf_192 (- 1. h_9))
         (set () float anf_193 (* anf_191 anf_192)) (return (- anf_190 anf_193)))))
      (Function (name palette_10) (desc ()) (params ((TyFloat t_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec3 cfg_12 (vec3 0.3 0.416 0.557))
         (set () vec3 anf_194 (+ cfg_12 t_11))
         (set () vec3 anf_195 (* anf_194 6.28318))
         (set () vec3 anf_196 (cos anf_195))
         (set () vec3 anf_197 (* anf_196 0.5)) (return (+ anf_197 0.5)))))
      (Function (name sdTorus_13) (desc ())
       (params (((TyVec 3) p_14) ((TyVec 2) t_15))) (ret_type TyFloat)
       (body
        ((set () float anf_198 (index p_14 0))
         (set () float anf_199 (index p_14 2))
         (set () vec2 anf_200 (vec2 anf_198 anf_199))
         (set () float anf_201 (length anf_200))
         (set () float anf_202 (index t_15 0))
         (set () float anf_203 (- anf_201 anf_202))
         (set () float anf_204 (index p_14 1))
         (set () vec2 q_16 (vec2 anf_203 anf_204))
         (set () float anf_205 (length q_16))
         (set () float anf_206 (index t_15 1)) (return (- anf_205 anf_206)))))
      (Function (name map_17) (desc ()) (params (((TyVec 3) p_18)))
       (ret_type TyFloat)
       (body
        ((set () float angle_19 (* u_time 2.))
         (set () float anf_207 (index p_18 0))
         (set () float anf_208 (index p_18 1))
         (set () vec2 anf_209 (vec2 anf_207 anf_208))
         (set () vec2 p_xy_20 (rotate_0 anf_209 angle_19))
         (set () float anf_210 (index p_xy_20 0))
         (set () float anf_211 (index p_xy_20 1))
         (set () float anf_212 (index p_18 2))
         (set () vec3 p_prime_21 (vec3 anf_210 anf_211 anf_212))
         (set () float anf_213 (index p_prime_21 1))
         (set () float anf_214 (index p_prime_21 2))
         (set () vec2 anf_215 (vec2 anf_213 anf_214))
         (set () vec2 p_yz_22 (rotate_0 anf_215 angle_19))
         (set () float anf_216 (index p_prime_21 0))
         (set () float anf_217 (index p_yz_22 0))
         (set () float anf_218 (index p_yz_22 1))
         (set () vec3 p_prime_23 (vec3 anf_216 anf_217 anf_218))
         (set () vec2 anf_219 (vec2 1. 0.3))
         (set () float anf_220 (sdTorus_13 p_prime_23 anf_219))
         (set () vec2 anf_221 (vec2 2. 0.5))
         (set () float anf_222 (sdTorus_13 p_18 anf_221))
         (return (sMin_5 anf_220 anf_222)))))
      (Function (name march_27_175) (desc ())
       (params
        (((TyVec 3) rd_26) ((TyVec 3) ro_25) (TyFloat t_28) (TyInt steps_29)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_269 0)
         (while (< _iter_269 1000)
          (Block (set () bool anf_223 (> steps_29 80))
           (if anf_223 (Block (return t_28))
            (Block (set () vec3 anf_224 (* rd_26 t_28))
             (set () vec3 anf_225 (+ ro_25 anf_224))
             (set () float d_30 (map_17 anf_225))
             (set () bool anf_226 (< d_30 0.001))
             (if anf_226 (Block (return t_28))
              (Block (set () bool anf_227 (> t_28 100.))
               (if anf_227 (Block (return 100.1))
                (Block (set () float anf_228 (+ t_28 d_30))
                 (set () int anf_229 (+ steps_29 1)) (set rd_26 rd_26)
                 (set ro_25 ro_25) (set t_28 anf_228) (set steps_29 anf_229)
                 (set () int _iter_inc_270 (+ _iter_269 1))
                 (set _iter_269 _iter_inc_270) continue))))))))
         (return 0.))))
      (Function (name march_24) (desc ())
       (params (((TyVec 3) ro_25) ((TyVec 3) rd_26))) (ret_type TyFloat)
       (body ((return (march_27_175 rd_26 ro_25 0. 0)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_31)))
       (ret_type (TyVec 3))
       (body
        ((set () float anf_230 (index u_resolution 0))
         (set () float anf_231 (index u_resolution 1))
         (set () float res_min_32 (min anf_230 anf_231))
         (set () vec2 anf_232 (* coord_31 2.))
         (set () vec2 anf_233 (- anf_232 u_resolution))
         (set () vec2 uv_33 (/ anf_233 res_min_32))
         (set () vec2 anf_234 (* u_mouse 2.))
         (set () vec2 anf_235 (- anf_234 u_resolution))
         (set () vec2 mouseUV_34 (/ anf_235 res_min_32))
         (set () vec3 ro_init_35 (vec3 0. 0. -10.))
         (set () float anf_236 (index uv_33 0))
         (set () float anf_237 (index uv_33 1))
         (set () vec3 anf_238 (vec3 anf_236 anf_237 1.))
         (set () vec3 rd_init_36 (normalize anf_238))
         (set () float anf_239 (index mouseUV_34 1))
         (set () float rotX_37 (- 0. anf_239))
         (set () float anf_240 (index mouseUV_34 0))
         (set () float rotY_38 (- 0. anf_240))
         (set () float anf_241 (index ro_init_35 1))
         (set () float anf_242 (index ro_init_35 2))
         (set () vec2 anf_243 (vec2 anf_241 anf_242))
         (set () vec2 ro_yz_39 (rotate_0 anf_243 rotX_37))
         (set () float anf_244 (index rd_init_36 1))
         (set () float anf_245 (index rd_init_36 2))
         (set () vec2 anf_246 (vec2 anf_244 anf_245))
         (set () vec2 rd_yz_40 (rotate_0 anf_246 rotX_37))
         (set () float anf_247 (index ro_init_35 0))
         (set () float anf_248 (index ro_yz_39 0))
         (set () float anf_249 (index ro_yz_39 1))
         (set () vec3 ro_41 (vec3 anf_247 anf_248 anf_249))
         (set () float anf_250 (index rd_init_36 0))
         (set () float anf_251 (index rd_yz_40 0))
         (set () float anf_252 (index rd_yz_40 1))
         (set () vec3 rd_42 (vec3 anf_250 anf_251 anf_252))
         (set () float anf_253 (index ro_41 0))
         (set () float anf_254 (index ro_41 2))
         (set () vec2 anf_255 (vec2 anf_253 anf_254))
         (set () vec2 ro_xz_43 (rotate_0 anf_255 rotY_38))
         (set () float anf_256 (index rd_42 0))
         (set () float anf_257 (index rd_42 2))
         (set () vec2 anf_258 (vec2 anf_256 anf_257))
         (set () vec2 rd_xz_44 (rotate_0 anf_258 rotY_38))
         (set () float anf_259 (index ro_xz_43 0))
         (set () float anf_260 (index ro_41 1))
         (set () float anf_261 (index ro_xz_43 1))
         (set () vec3 ro_45 (vec3 anf_259 anf_260 anf_261))
         (set () float anf_262 (index rd_xz_44 0))
         (set () float anf_263 (index rd_42 1))
         (set () float anf_264 (index rd_xz_44 1))
         (set () vec3 rd_46 (vec3 anf_262 anf_263 anf_264))
         (set () float t_47 (march_24 ro_45 rd_46))
         (set () bool anf_265 (> t_47 100.)) (set () vec3 col_48 (vec3 0.))
         (if anf_265 (Block (set col_48 (vec3 0.2 0.2 0.2)))
          (Block (set () float anf_266 (* t_47 0.3))
           (set col_48 (palette_10 anf_266))))
         (set () vec2 anf_267 (- uv_33 mouseUV_34))
         (set () float anf_268 (length anf_267))
         (set () float glow_49 (/ 0.02 anf_268)) (return (+ col_48 glow_49)))))))

    === patch_main (raymarch.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time) (Global Uniform (TyVec 2) u_mouse)
      (Function (name rotate_0) (desc ())
       (params (((TyVec 2) p_1) (TyFloat angle_2))) (ret_type (TyVec 2))
       (body
        ((set () float s_3 (sin angle_2)) (set () float c_4 (cos angle_2))
         (set () float anf_176 (index p_1 0))
         (set () float anf_177 (* anf_176 c_4))
         (set () float anf_178 (index p_1 1))
         (set () float anf_179 (* anf_178 s_3))
         (set () float anf_180 (- anf_177 anf_179))
         (set () float anf_181 (index p_1 0))
         (set () float anf_182 (* anf_181 s_3))
         (set () float anf_183 (index p_1 1))
         (set () float anf_184 (* anf_183 c_4))
         (set () float anf_185 (+ anf_182 anf_184))
         (return (vec2 anf_180 anf_185)))))
      (Function (name sMin_5) (desc ()) (params ((TyFloat a_6) (TyFloat b_7)))
       (ret_type TyFloat)
       (body
        ((set () float k_8 0.1) (set () float anf_186 (- b_7 a_6))
         (set () float anf_187 (* 0.5 anf_186))
         (set () float anf_188 (/ anf_187 k_8))
         (set () float anf_189 (+ 0.5 anf_188))
         (set () float h_9 (clamp anf_189 0. 1.))
         (set () float anf_190 (mix b_7 a_6 h_9))
         (set () float anf_191 (* k_8 h_9)) (set () float anf_192 (- 1. h_9))
         (set () float anf_193 (* anf_191 anf_192)) (return (- anf_190 anf_193)))))
      (Function (name palette_10) (desc ()) (params ((TyFloat t_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec3 cfg_12 (vec3 0.3 0.416 0.557))
         (set () vec3 anf_194 (+ cfg_12 t_11))
         (set () vec3 anf_195 (* anf_194 6.28318))
         (set () vec3 anf_196 (cos anf_195))
         (set () vec3 anf_197 (* anf_196 0.5)) (return (+ anf_197 0.5)))))
      (Function (name sdTorus_13) (desc ())
       (params (((TyVec 3) p_14) ((TyVec 2) t_15))) (ret_type TyFloat)
       (body
        ((set () float anf_198 (index p_14 0))
         (set () float anf_199 (index p_14 2))
         (set () vec2 anf_200 (vec2 anf_198 anf_199))
         (set () float anf_201 (length anf_200))
         (set () float anf_202 (index t_15 0))
         (set () float anf_203 (- anf_201 anf_202))
         (set () float anf_204 (index p_14 1))
         (set () vec2 q_16 (vec2 anf_203 anf_204))
         (set () float anf_205 (length q_16))
         (set () float anf_206 (index t_15 1)) (return (- anf_205 anf_206)))))
      (Function (name map_17) (desc ()) (params (((TyVec 3) p_18)))
       (ret_type TyFloat)
       (body
        ((set () float angle_19 (* u_time 2.))
         (set () float anf_207 (index p_18 0))
         (set () float anf_208 (index p_18 1))
         (set () vec2 anf_209 (vec2 anf_207 anf_208))
         (set () vec2 p_xy_20 (rotate_0 anf_209 angle_19))
         (set () float anf_210 (index p_xy_20 0))
         (set () float anf_211 (index p_xy_20 1))
         (set () float anf_212 (index p_18 2))
         (set () vec3 p_prime_21 (vec3 anf_210 anf_211 anf_212))
         (set () float anf_213 (index p_prime_21 1))
         (set () float anf_214 (index p_prime_21 2))
         (set () vec2 anf_215 (vec2 anf_213 anf_214))
         (set () vec2 p_yz_22 (rotate_0 anf_215 angle_19))
         (set () float anf_216 (index p_prime_21 0))
         (set () float anf_217 (index p_yz_22 0))
         (set () float anf_218 (index p_yz_22 1))
         (set () vec3 p_prime_23 (vec3 anf_216 anf_217 anf_218))
         (set () vec2 anf_219 (vec2 1. 0.3))
         (set () float anf_220 (sdTorus_13 p_prime_23 anf_219))
         (set () vec2 anf_221 (vec2 2. 0.5))
         (set () float anf_222 (sdTorus_13 p_18 anf_221))
         (return (sMin_5 anf_220 anf_222)))))
      (Function (name march_27_175) (desc ())
       (params
        (((TyVec 3) rd_26) ((TyVec 3) ro_25) (TyFloat t_28) (TyInt steps_29)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_269 0)
         (while (< _iter_269 1000)
          (Block (set () bool anf_223 (> steps_29 80))
           (if anf_223 (Block (return t_28))
            (Block (set () vec3 anf_224 (* rd_26 t_28))
             (set () vec3 anf_225 (+ ro_25 anf_224))
             (set () float d_30 (map_17 anf_225))
             (set () bool anf_226 (< d_30 0.001))
             (if anf_226 (Block (return t_28))
              (Block (set () bool anf_227 (> t_28 100.))
               (if anf_227 (Block (return 100.1))
                (Block (set () float anf_228 (+ t_28 d_30))
                 (set () int anf_229 (+ steps_29 1)) (set rd_26 rd_26)
                 (set ro_25 ro_25) (set t_28 anf_228) (set steps_29 anf_229)
                 (set () int _iter_inc_270 (+ _iter_269 1))
                 (set _iter_269 _iter_inc_270) continue))))))))
         (return 0.))))
      (Function (name march_24) (desc ())
       (params (((TyVec 3) ro_25) ((TyVec 3) rd_26))) (ret_type TyFloat)
       (body ((return (march_27_175 rd_26 ro_25 0. 0)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_31)))
       (ret_type (TyVec 3))
       (body
        ((set () float anf_230 (index u_resolution 0))
         (set () float anf_231 (index u_resolution 1))
         (set () float res_min_32 (min anf_230 anf_231))
         (set () vec2 anf_232 (* coord_31 2.))
         (set () vec2 anf_233 (- anf_232 u_resolution))
         (set () vec2 uv_33 (/ anf_233 res_min_32))
         (set () vec2 anf_234 (* u_mouse 2.))
         (set () vec2 anf_235 (- anf_234 u_resolution))
         (set () vec2 mouseUV_34 (/ anf_235 res_min_32))
         (set () vec3 ro_init_35 (vec3 0. 0. -10.))
         (set () float anf_236 (index uv_33 0))
         (set () float anf_237 (index uv_33 1))
         (set () vec3 anf_238 (vec3 anf_236 anf_237 1.))
         (set () vec3 rd_init_36 (normalize anf_238))
         (set () float anf_239 (index mouseUV_34 1))
         (set () float rotX_37 (- 0. anf_239))
         (set () float anf_240 (index mouseUV_34 0))
         (set () float rotY_38 (- 0. anf_240))
         (set () float anf_241 (index ro_init_35 1))
         (set () float anf_242 (index ro_init_35 2))
         (set () vec2 anf_243 (vec2 anf_241 anf_242))
         (set () vec2 ro_yz_39 (rotate_0 anf_243 rotX_37))
         (set () float anf_244 (index rd_init_36 1))
         (set () float anf_245 (index rd_init_36 2))
         (set () vec2 anf_246 (vec2 anf_244 anf_245))
         (set () vec2 rd_yz_40 (rotate_0 anf_246 rotX_37))
         (set () float anf_247 (index ro_init_35 0))
         (set () float anf_248 (index ro_yz_39 0))
         (set () float anf_249 (index ro_yz_39 1))
         (set () vec3 ro_41 (vec3 anf_247 anf_248 anf_249))
         (set () float anf_250 (index rd_init_36 0))
         (set () float anf_251 (index rd_yz_40 0))
         (set () float anf_252 (index rd_yz_40 1))
         (set () vec3 rd_42 (vec3 anf_250 anf_251 anf_252))
         (set () float anf_253 (index ro_41 0))
         (set () float anf_254 (index ro_41 2))
         (set () vec2 anf_255 (vec2 anf_253 anf_254))
         (set () vec2 ro_xz_43 (rotate_0 anf_255 rotY_38))
         (set () float anf_256 (index rd_42 0))
         (set () float anf_257 (index rd_42 2))
         (set () vec2 anf_258 (vec2 anf_256 anf_257))
         (set () vec2 rd_xz_44 (rotate_0 anf_258 rotY_38))
         (set () float anf_259 (index ro_xz_43 0))
         (set () float anf_260 (index ro_41 1))
         (set () float anf_261 (index ro_xz_43 1))
         (set () vec3 ro_45 (vec3 anf_259 anf_260 anf_261))
         (set () float anf_262 (index rd_xz_44 0))
         (set () float anf_263 (index rd_42 1))
         (set () float anf_264 (index rd_xz_44 1))
         (set () vec3 rd_46 (vec3 anf_262 anf_263 anf_264))
         (set () float t_47 (march_24 ro_45 rd_46))
         (set () bool anf_265 (> t_47 100.)) (set () vec3 col_48 (vec3 0.))
         (if anf_265 (Block (set col_48 (vec3 0.2 0.2 0.2)))
          (Block (set () float anf_266 (* t_47 0.3))
           (set col_48 (palette_10 anf_266))))
         (set () vec2 anf_267 (- uv_33 mouseUV_34))
         (set () float anf_268 (length anf_267))
         (set () float glow_49 (/ 0.02 anf_268)) (return (+ col_48 glow_49)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE recursion.glml ======

    === stlc (recursion.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv
       (lambda (coord ((vec 2)))
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec rotate
       (lambda (angle (float))
        (let s (sin angle) (let c (cos angle) (mat2x2 c (* -1. s) s c)))))
      (Define (Rec 1000 ((float -> (float -> float)))) gcd
       (lambda (a (float))
        (lambda (b (float))
         (if (< a 0.05) b
          (if (< b 0.05) a
           (if (> a b) (app (app gcd (- a b)) b) (app (app gcd a) (- b a))))))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
        (let uv (app get_uv coord)
         (let uv (* (app rotate u_time) uv)
          (let x (abs (* (* (index uv 0) (sin (* u_time 2.))) 2.))
           (let y (abs (* (* (index uv 1) (sin (* u_time 2.))) 2.))
            (let res (app (app gcd x) y) (vec3 res (* res 0.5) (- 1. res)))))))))))

    === uniquify (recursion.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv_0
       (lambda (coord_1 ((vec 2)))
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define Nonrec rotate_4
       (lambda (angle_5 (float))
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5) (mat2x2 c_7 (* -1. s_6) s_6 c_7)))))
      (Define (Rec 1000 ((float -> (float -> float)))) gcd_8
       (lambda (a_9 (float))
        (lambda (b_10 (float))
         (if (< a_9 0.05) b_10
          (if (< b_10 0.05) a_9
           (if (> a_9 b_10) (app (app gcd_8 (- a_9 b_10)) b_10)
            (app (app gcd_8 a_9) (- b_10 a_9))))))))
      (Define Nonrec main
       (lambda (coord_11 ((vec 2)))
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
      ((Define (Rec 1000 ((float -> (float -> float)))) gcd_8
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

    === monomorphize (recursion.glml) ===
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
      ((Define (Rec 1000 ((float -> (float -> float)))) gcd_8
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
      ((Define (Rec 1000 ((float -> (float -> float)))) gcd_8
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
     ((Define (Rec 1000 ((float -> (float -> float)))) (name gcd_8)
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
        (let anf_55 (* 2. coord_1)
         (let top_2 (- anf_55 u_resolution)
          (let anf_56 (index u_resolution 0)
           (let anf_57 (index u_resolution 1)
            (let bot_3 (min anf_56 anf_57) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_58 (* -1. s_6) (return (mat2x2 c_7 anf_58 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (Rec 1000 ((float -> (float -> float)))) (name gcd_8)
       (args ((a_9 float) (b_10 float)))
       (body
        (let anf_59 (< a_9 0.05)
         (return
          (if anf_59 (return b_10)
           (let anf_60 (< b_10 0.05)
            (return
             (if anf_60 (return a_9)
              (let anf_61 (> a_9 b_10)
               (return
                (if anf_61 (let anf_62 (- a_9 b_10) (return (gcd_8 anf_62 b_10)))
                 (let anf_63 (- b_10 a_9) (return (gcd_8 a_9 anf_63))))))))))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_64 (rotate_4 u_time)
          (let uv_13 (* anf_64 uv_12)
           (let anf_65 (index uv_13 0)
            (let anf_66 (* u_time 2.)
             (let anf_67 (sin anf_66)
              (let anf_68 (* anf_65 anf_67)
               (let anf_69 (* anf_68 2.)
                (let x_14 (abs anf_69)
                 (let anf_70 (index uv_13 1)
                  (let anf_71 (* u_time 2.)
                   (let anf_72 (sin anf_71)
                    (let anf_73 (* anf_70 anf_72)
                     (let anf_74 (* anf_73 2.)
                      (let y_15 (abs anf_74)
                       (let res_16 (gcd_8 x_14 y_15)
                        (let anf_75 (* res_16 0.5)
                         (let anf_76 (- 1. res_16)
                          (return (vec3 res_16 anf_75 anf_76))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_55 (* 2. coord_1)
         (let top_2 (- anf_55 u_resolution)
          (let anf_56 (index u_resolution 0)
           (let anf_57 (index u_resolution 1)
            (let bot_3 (min anf_56 anf_57) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_58 (* -1. s_6) (return (mat2x2 c_7 anf_58 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (name gcd_8) (args ((a_9 float) (b_10 float)))
       (body
        (let _iter_77 0
         (while (< _iter_77 1000)
          (let anf_59 (< a_9 0.05)
           (return
            (if anf_59 (return b_10)
             (let anf_60 (< b_10 0.05)
              (return
               (if anf_60 (return a_9)
                (let anf_61 (> a_9 b_10)
                 (return
                  (if anf_61
                   (let anf_62 (- a_9 b_10)
                    (set a_9 anf_62
                     (set b_10 b_10
                      (let _iter_inc_78 (+ _iter_77 1)
                       (set _iter_77 _iter_inc_78 continue)))))
                   (let anf_63 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_63
                      (let _iter_inc_79 (+ _iter_77 1)
                       (set _iter_77 _iter_inc_79 continue))))))))))))))
          (return 0.)))))
      : (float -> (float -> float)))
     ((Define (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_64 (rotate_4 u_time)
          (let uv_13 (* anf_64 uv_12)
           (let anf_65 (index uv_13 0)
            (let anf_66 (* u_time 2.)
             (let anf_67 (sin anf_66)
              (let anf_68 (* anf_65 anf_67)
               (let anf_69 (* anf_68 2.)
                (let x_14 (abs anf_69)
                 (let anf_70 (index uv_13 1)
                  (let anf_71 (* u_time 2.)
                   (let anf_72 (sin anf_71)
                    (let anf_73 (* anf_70 anf_72)
                     (let anf_74 (* anf_73 2.)
                      (let y_15 (abs anf_74)
                       (let res_16 (gcd_8 x_14 y_15)
                        (let anf_75 (* res_16 0.5)
                         (let anf_76 (- 1. res_16)
                          (return (vec3 res_16 anf_75 anf_76))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (recursion.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_55 (* 2. coord_1))
         (set () vec2 top_2 (- anf_55 u_resolution))
         (set () float anf_56 (index u_resolution 0))
         (set () float anf_57 (index u_resolution 1))
         (set () float bot_3 (min anf_56 anf_57)) (return (/ top_2 bot_3)))))
      (Function (name rotate_4) (desc ()) (params ((TyFloat angle_5)))
       (ret_type (TyMat 2 2))
       (body
        ((set () float s_6 (sin angle_5)) (set () float c_7 (cos angle_5))
         (set () float anf_58 (* -1. s_6)) (return (mat2 c_7 anf_58 s_6 c_7)))))
      (Function (name gcd_8) (desc ()) (params ((TyFloat a_9) (TyFloat b_10)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_77 0)
         (while (< _iter_77 1000)
          (Block (set () bool anf_59 (< a_9 0.05))
           (if anf_59 (Block (return b_10))
            (Block (set () bool anf_60 (< b_10 0.05))
             (if anf_60 (Block (return a_9))
              (Block (set () bool anf_61 (> a_9 b_10))
               (if anf_61
                (Block (set () float anf_62 (- a_9 b_10)) (set a_9 anf_62)
                 (set b_10 b_10) (set () int _iter_inc_78 (+ _iter_77 1))
                 (set _iter_77 _iter_inc_78) continue)
                (Block (set () float anf_63 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_63) (set () int _iter_inc_79 (+ _iter_77 1))
                 (set _iter_77 _iter_inc_79) continue))))))))
         (return 0.))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_12 (get_uv_0 coord_11))
         (set () mat2 anf_64 (rotate_4 u_time))
         (set () vec2 uv_13 (* anf_64 uv_12))
         (set () float anf_65 (index uv_13 0))
         (set () float anf_66 (* u_time 2.)) (set () float anf_67 (sin anf_66))
         (set () float anf_68 (* anf_65 anf_67))
         (set () float anf_69 (* anf_68 2.)) (set () float x_14 (abs anf_69))
         (set () float anf_70 (index uv_13 1))
         (set () float anf_71 (* u_time 2.)) (set () float anf_72 (sin anf_71))
         (set () float anf_73 (* anf_70 anf_72))
         (set () float anf_74 (* anf_73 2.)) (set () float y_15 (abs anf_74))
         (set () float res_16 (gcd_8 x_14 y_15))
         (set () float anf_75 (* res_16 0.5)) (set () float anf_76 (- 1. res_16))
         (return (vec3 res_16 anf_75 anf_76)))))))

    === patch_main (recursion.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_55 (* 2. coord_1))
         (set () vec2 top_2 (- anf_55 u_resolution))
         (set () float anf_56 (index u_resolution 0))
         (set () float anf_57 (index u_resolution 1))
         (set () float bot_3 (min anf_56 anf_57)) (return (/ top_2 bot_3)))))
      (Function (name rotate_4) (desc ()) (params ((TyFloat angle_5)))
       (ret_type (TyMat 2 2))
       (body
        ((set () float s_6 (sin angle_5)) (set () float c_7 (cos angle_5))
         (set () float anf_58 (* -1. s_6)) (return (mat2 c_7 anf_58 s_6 c_7)))))
      (Function (name gcd_8) (desc ()) (params ((TyFloat a_9) (TyFloat b_10)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_77 0)
         (while (< _iter_77 1000)
          (Block (set () bool anf_59 (< a_9 0.05))
           (if anf_59 (Block (return b_10))
            (Block (set () bool anf_60 (< b_10 0.05))
             (if anf_60 (Block (return a_9))
              (Block (set () bool anf_61 (> a_9 b_10))
               (if anf_61
                (Block (set () float anf_62 (- a_9 b_10)) (set a_9 anf_62)
                 (set b_10 b_10) (set () int _iter_inc_78 (+ _iter_77 1))
                 (set _iter_77 _iter_inc_78) continue)
                (Block (set () float anf_63 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_63) (set () int _iter_inc_79 (+ _iter_77 1))
                 (set _iter_77 _iter_inc_79) continue))))))))
         (return 0.))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_12 (get_uv_0 coord_11))
         (set () mat2 anf_64 (rotate_4 u_time))
         (set () vec2 uv_13 (* anf_64 uv_12))
         (set () float anf_65 (index uv_13 0))
         (set () float anf_66 (* u_time 2.)) (set () float anf_67 (sin anf_66))
         (set () float anf_68 (* anf_65 anf_67))
         (set () float anf_69 (* anf_68 2.)) (set () float x_14 (abs anf_69))
         (set () float anf_70 (index uv_13 1))
         (set () float anf_71 (* u_time 2.)) (set () float anf_72 (sin anf_71))
         (set () float anf_73 (* anf_70 anf_72))
         (set () float anf_74 (* anf_73 2.)) (set () float y_15 (abs anf_74))
         (set () float res_16 (gcd_8 x_14 y_15))
         (set () float anf_75 (* res_16 0.5)) (set () float anf_76 (- 1. res_16))
         (return (vec3 res_16 anf_75 anf_76)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE warped_noise.glml ======

    === stlc (warped_noise.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec fract (lambda (v ((vec 4))) (- v (floor v))))
      (Define Nonrec smoothstep
       (lambda (edge0 (float))
        (lambda (edge1 (float))
         (lambda (x (float))
          (let t (clamp (/ (- x edge0) (- edge1 edge0)) 0. 1.)
           (* (* t t) (- 3. (* 2. t))))))))
      (Define Nonrec smoothNoise
       (lambda (p ((vec 2)))
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
       (lambda (p ((vec 2)))
        (+
         (+
          (+ (* (app smoothNoise p) 0.5333)
           (* (app smoothNoise (* p 2.)) 0.2667))
          (* (app smoothNoise (* p 4.)) 0.1333))
         (* (app smoothNoise (* p 8.)) 0.0667))))
      (Define Nonrec warpedNoise
       (lambda (p ((vec 2)))
        (let m (* (vec2 u_time (- 0. u_time)) 0.5)
         (let x (app fractalNoise (+ p m))
          (let y (app fractalNoise (+ (+ p (vec2 (index m 1) (index m 0))) x))
           (let z (app fractalNoise (+ (- (- p m) x) y))
            (let warp (+ (+ (vec2 x y) (vec2 y z)) (vec2 z x))
             (let mag (* (length (vec3 x y z)) 0.25)
              (app fractalNoise (+ (+ p warp) mag))))))))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
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
      (Define Nonrec fract_0 (lambda (v_1 ((vec 4))) (- v_1 (floor v_1))))
      (Define Nonrec smoothstep_2
       (lambda (edge0_3 (float))
        (lambda (edge1_4 (float))
         (lambda (x_5 (float))
          (let t_6 (clamp (/ (- x_5 edge0_3) (- edge1_4 edge0_3)) 0. 1.)
           (* (* t_6 t_6) (- 3. (* 2. t_6))))))))
      (Define Nonrec smoothNoise_7
       (lambda (p_8 ((vec 2)))
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
       (lambda (p_19 ((vec 2)))
        (+
         (+
          (+ (* (app smoothNoise_7 p_19) 0.5333)
           (* (app smoothNoise_7 (* p_19 2.)) 0.2667))
          (* (app smoothNoise_7 (* p_19 4.)) 0.1333))
         (* (app smoothNoise_7 (* p_19 8.)) 0.0667))))
      (Define Nonrec warpedNoise_20
       (lambda (p_21 ((vec 2)))
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
       (lambda (coord_28 ((vec 2)))
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

    === monomorphize (warped_noise.glml) ===
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
       (body (let anf_172 (floor v_1) (return (- v_1 anf_172)))))
      : ((vec 4) -> (vec 4)))
     ((Define Nonrec (name smoothstep_2)
       (args ((edge0_3 float) (edge1_4 float) (x_5 float)))
       (body
        (let anf_173 (- x_5 edge0_3)
         (let anf_174 (- edge1_4 edge0_3)
          (let anf_175 (/ anf_173 anf_174)
           (let t_6 (clamp anf_175 0. 1.)
            (let anf_176 (* t_6 t_6)
             (let anf_177 (* 2. t_6)
              (let anf_178 (- 3. anf_177) (return (* anf_176 anf_178)))))))))))
      : (float -> (float -> (float -> float))))
     ((Define Nonrec (name smoothNoise_7) (args ((p_8 (vec 2))))
       (body
        (let i_9 (floor p_8)
         (let pf_10 (- p_8 i_9)
          (let anf_179 (* pf_10 pf_10)
           (let anf_180 (* 2. pf_10)
            (let anf_181 (- 3. anf_180)
             (let inter_11 (* anf_179 anf_181)
              (let v4_12 (vec4 0. 1. 27. 28.)
               (let anf_182 (index i_9 0)
                (let anf_183 (+ v4_12 anf_182)
                 (let anf_184 (index i_9 1)
                  (let anf_185 (* anf_184 27.)
                   (let seed_13 (+ anf_183 anf_185)
                    (let anf_186 (% seed_13 6.2831853)
                     (let anf_187 (sin anf_186)
                      (let anf_188 (* anf_187 200000.)
                       (let hash_14 (fract_0 anf_188)
                        (let anf_189 (index hash_14 0)
                         (let anf_190 (index hash_14 1)
                          (let col0_15 (vec2 anf_189 anf_190)
                           (let anf_191 (index hash_14 2)
                            (let anf_192 (index hash_14 3)
                             (let col1_16 (vec2 anf_191 anf_192)
                              (let anf_193 (index inter_11 1)
                               (let anf_194 (- 1. anf_193)
                                (let anf_195 (* col0_15 anf_194)
                                 (let anf_196 (index inter_11 1)
                                  (let anf_197 (* col1_16 anf_196)
                                   (let res_v_17 (+ anf_195 anf_197)
                                    (let anf_198 (index inter_11 0)
                                     (let anf_199 (- 1. anf_198)
                                      (let anf_200 (index inter_11 0)
                                       (let anf_201 (vec2 anf_199 anf_200)
                                        (return (dot res_v_17 anf_201))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name fractalNoise_18) (args ((p_19 (vec 2))))
       (body
        (let anf_202 (smoothNoise_7 p_19)
         (let anf_203 (* anf_202 0.5333)
          (let anf_204 (* p_19 2.)
           (let anf_205 (smoothNoise_7 anf_204)
            (let anf_206 (* anf_205 0.2667)
             (let anf_207 (+ anf_203 anf_206)
              (let anf_208 (* p_19 4.)
               (let anf_209 (smoothNoise_7 anf_208)
                (let anf_210 (* anf_209 0.1333)
                 (let anf_211 (+ anf_207 anf_210)
                  (let anf_212 (* p_19 8.)
                   (let anf_213 (smoothNoise_7 anf_212)
                    (let anf_214 (* anf_213 0.0667) (return (+ anf_211 anf_214)))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name warpedNoise_20) (args ((p_21 (vec 2))))
       (body
        (let anf_215 (- 0. u_time)
         (let anf_216 (vec2 u_time anf_215)
          (let m_22 (* anf_216 0.5)
           (let anf_217 (+ p_21 m_22)
            (let x_23 (fractalNoise_18 anf_217)
             (let anf_218 (index m_22 1)
              (let anf_219 (index m_22 0)
               (let anf_220 (vec2 anf_218 anf_219)
                (let anf_221 (+ p_21 anf_220)
                 (let anf_222 (+ anf_221 x_23)
                  (let y_24 (fractalNoise_18 anf_222)
                   (let anf_223 (- p_21 m_22)
                    (let anf_224 (- anf_223 x_23)
                     (let anf_225 (+ anf_224 y_24)
                      (let z_25 (fractalNoise_18 anf_225)
                       (let anf_226 (vec2 x_23 y_24)
                        (let anf_227 (vec2 y_24 z_25)
                         (let anf_228 (+ anf_226 anf_227)
                          (let anf_229 (vec2 z_25 x_23)
                           (let warp_26 (+ anf_228 anf_229)
                            (let anf_230 (vec3 x_23 y_24 z_25)
                             (let anf_231 (length anf_230)
                              (let mag_27 (* anf_231 0.25)
                               (let anf_232 (+ p_21 warp_26)
                                (let anf_233 (+ anf_232 mag_27)
                                 (return (fractalNoise_18 anf_233)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name main) (args ((coord_28 (vec 2))))
       (body
        (let anf_234 (* u_resolution 0.5)
         (let anf_235 (- coord_28 anf_234)
          (let anf_236 (index u_resolution 1)
           (let uv_29 (/ anf_235 anf_236)
            (let anf_237 (* uv_29 6.)
             (let n_30 (warpedNoise_20 anf_237)
              (let anf_238 (* uv_29 6.)
               (let anf_239 (- anf_238 0.02)
                (let n2_31 (warpedNoise_20 anf_239)
                 (let anf_240 (- n2_31 n_30)
                  (let anf_241 (max anf_240 0.)
                   (let anf_242 (/ anf_241 0.02)
                    (let bump_32 (* anf_242 0.7071)
                     (let anf_243 (- n_30 n2_31)
                      (let anf_244 (max anf_243 0.)
                       (let anf_245 (/ anf_244 0.02)
                        (let bump2_33 (* anf_245 0.7071)
                         (let anf_246 (* bump_32 bump_32)
                          (let anf_247 (pow bump_32 4.)
                           (let anf_248 (* anf_247 0.5)
                            (let b1_34 (+ anf_246 anf_248)
                             (let anf_249 (* bump2_33 bump2_33)
                              (let anf_250 (pow bump2_33 4.)
                               (let anf_251 (* anf_250 0.5)
                                (let b2_35 (+ anf_249 anf_251)
                                 (let anf_252 (vec3 1. 0.7 0.6)
                                  (let anf_253 (+ b1_34 b2_35)
                                   (let anf_254 (* anf_253 0.4)
                                    (let anf_255 (vec3 b1_34 anf_254 b2_35)
                                     (let anf_256 (* anf_252 anf_255)
                                      (let anf_257 (* anf_256 0.3)
                                       (let base_col_36 (+ anf_257 0.5)
                                        (let anf_258 (* n_30 n_30)
                                         (let col_37 (* anf_258 base_col_36)
                                          (let anf_259 (- uv_29 0.65)
                                           (let spot1_dist_38 (length anf_259)
                                            (let anf_260 (+ uv_29 0.5)
                                             (let spot2_dist_39 (length anf_260)
                                              (let anf_261 (vec3 0.8 0.4 1.)
                                               (let a_40 (* anf_261 0.35)
                                                (let anf_262 (vec3 1. 0.5 0.2)
                                                 (let anf_263
                                                  (- 1. spot1_dist_38)
                                                  (let anf_264
                                                   (smoothstep_2 0. 1. anf_263)
                                                   (let b_41 (* anf_262 anf_264)
                                                    (let anf_265
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_266
                                                      (- 1. spot2_dist_39)
                                                      (let anf_267
                                                       (smoothstep_2 0. 1.
                                                        anf_266)
                                                       (let c_42
                                                        (* anf_265 anf_267)
                                                        (let anf_268
                                                         (+ b_41 c_42)
                                                         (let anf_269
                                                          (* anf_268 5.)
                                                          (let spot_logic_43
                                                           (+ a_40 anf_269)
                                                           (let final_col_44
                                                            (* col_37
                                                             spot_logic_43)
                                                            (let anf_270
                                                             (max final_col_44
                                                              0.)
                                                             (return
                                                              (sqrt anf_270)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail_call (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name fract_0) (args ((v_1 (vec 4))))
       (body (let anf_172 (floor v_1) (return (- v_1 anf_172)))))
      : ((vec 4) -> (vec 4)))
     ((Define (name smoothstep_2)
       (args ((edge0_3 float) (edge1_4 float) (x_5 float)))
       (body
        (let anf_173 (- x_5 edge0_3)
         (let anf_174 (- edge1_4 edge0_3)
          (let anf_175 (/ anf_173 anf_174)
           (let t_6 (clamp anf_175 0. 1.)
            (let anf_176 (* t_6 t_6)
             (let anf_177 (* 2. t_6)
              (let anf_178 (- 3. anf_177) (return (* anf_176 anf_178)))))))))))
      : (float -> (float -> (float -> float))))
     ((Define (name smoothNoise_7) (args ((p_8 (vec 2))))
       (body
        (let i_9 (floor p_8)
         (let pf_10 (- p_8 i_9)
          (let anf_179 (* pf_10 pf_10)
           (let anf_180 (* 2. pf_10)
            (let anf_181 (- 3. anf_180)
             (let inter_11 (* anf_179 anf_181)
              (let v4_12 (vec4 0. 1. 27. 28.)
               (let anf_182 (index i_9 0)
                (let anf_183 (+ v4_12 anf_182)
                 (let anf_184 (index i_9 1)
                  (let anf_185 (* anf_184 27.)
                   (let seed_13 (+ anf_183 anf_185)
                    (let anf_186 (% seed_13 6.2831853)
                     (let anf_187 (sin anf_186)
                      (let anf_188 (* anf_187 200000.)
                       (let hash_14 (fract_0 anf_188)
                        (let anf_189 (index hash_14 0)
                         (let anf_190 (index hash_14 1)
                          (let col0_15 (vec2 anf_189 anf_190)
                           (let anf_191 (index hash_14 2)
                            (let anf_192 (index hash_14 3)
                             (let col1_16 (vec2 anf_191 anf_192)
                              (let anf_193 (index inter_11 1)
                               (let anf_194 (- 1. anf_193)
                                (let anf_195 (* col0_15 anf_194)
                                 (let anf_196 (index inter_11 1)
                                  (let anf_197 (* col1_16 anf_196)
                                   (let res_v_17 (+ anf_195 anf_197)
                                    (let anf_198 (index inter_11 0)
                                     (let anf_199 (- 1. anf_198)
                                      (let anf_200 (index inter_11 0)
                                       (let anf_201 (vec2 anf_199 anf_200)
                                        (return (dot res_v_17 anf_201))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name fractalNoise_18) (args ((p_19 (vec 2))))
       (body
        (let anf_202 (smoothNoise_7 p_19)
         (let anf_203 (* anf_202 0.5333)
          (let anf_204 (* p_19 2.)
           (let anf_205 (smoothNoise_7 anf_204)
            (let anf_206 (* anf_205 0.2667)
             (let anf_207 (+ anf_203 anf_206)
              (let anf_208 (* p_19 4.)
               (let anf_209 (smoothNoise_7 anf_208)
                (let anf_210 (* anf_209 0.1333)
                 (let anf_211 (+ anf_207 anf_210)
                  (let anf_212 (* p_19 8.)
                   (let anf_213 (smoothNoise_7 anf_212)
                    (let anf_214 (* anf_213 0.0667) (return (+ anf_211 anf_214)))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name warpedNoise_20) (args ((p_21 (vec 2))))
       (body
        (let anf_215 (- 0. u_time)
         (let anf_216 (vec2 u_time anf_215)
          (let m_22 (* anf_216 0.5)
           (let anf_217 (+ p_21 m_22)
            (let x_23 (fractalNoise_18 anf_217)
             (let anf_218 (index m_22 1)
              (let anf_219 (index m_22 0)
               (let anf_220 (vec2 anf_218 anf_219)
                (let anf_221 (+ p_21 anf_220)
                 (let anf_222 (+ anf_221 x_23)
                  (let y_24 (fractalNoise_18 anf_222)
                   (let anf_223 (- p_21 m_22)
                    (let anf_224 (- anf_223 x_23)
                     (let anf_225 (+ anf_224 y_24)
                      (let z_25 (fractalNoise_18 anf_225)
                       (let anf_226 (vec2 x_23 y_24)
                        (let anf_227 (vec2 y_24 z_25)
                         (let anf_228 (+ anf_226 anf_227)
                          (let anf_229 (vec2 z_25 x_23)
                           (let warp_26 (+ anf_228 anf_229)
                            (let anf_230 (vec3 x_23 y_24 z_25)
                             (let anf_231 (length anf_230)
                              (let mag_27 (* anf_231 0.25)
                               (let anf_232 (+ p_21 warp_26)
                                (let anf_233 (+ anf_232 mag_27)
                                 (return (fractalNoise_18 anf_233)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name main) (args ((coord_28 (vec 2))))
       (body
        (let anf_234 (* u_resolution 0.5)
         (let anf_235 (- coord_28 anf_234)
          (let anf_236 (index u_resolution 1)
           (let uv_29 (/ anf_235 anf_236)
            (let anf_237 (* uv_29 6.)
             (let n_30 (warpedNoise_20 anf_237)
              (let anf_238 (* uv_29 6.)
               (let anf_239 (- anf_238 0.02)
                (let n2_31 (warpedNoise_20 anf_239)
                 (let anf_240 (- n2_31 n_30)
                  (let anf_241 (max anf_240 0.)
                   (let anf_242 (/ anf_241 0.02)
                    (let bump_32 (* anf_242 0.7071)
                     (let anf_243 (- n_30 n2_31)
                      (let anf_244 (max anf_243 0.)
                       (let anf_245 (/ anf_244 0.02)
                        (let bump2_33 (* anf_245 0.7071)
                         (let anf_246 (* bump_32 bump_32)
                          (let anf_247 (pow bump_32 4.)
                           (let anf_248 (* anf_247 0.5)
                            (let b1_34 (+ anf_246 anf_248)
                             (let anf_249 (* bump2_33 bump2_33)
                              (let anf_250 (pow bump2_33 4.)
                               (let anf_251 (* anf_250 0.5)
                                (let b2_35 (+ anf_249 anf_251)
                                 (let anf_252 (vec3 1. 0.7 0.6)
                                  (let anf_253 (+ b1_34 b2_35)
                                   (let anf_254 (* anf_253 0.4)
                                    (let anf_255 (vec3 b1_34 anf_254 b2_35)
                                     (let anf_256 (* anf_252 anf_255)
                                      (let anf_257 (* anf_256 0.3)
                                       (let base_col_36 (+ anf_257 0.5)
                                        (let anf_258 (* n_30 n_30)
                                         (let col_37 (* anf_258 base_col_36)
                                          (let anf_259 (- uv_29 0.65)
                                           (let spot1_dist_38 (length anf_259)
                                            (let anf_260 (+ uv_29 0.5)
                                             (let spot2_dist_39 (length anf_260)
                                              (let anf_261 (vec3 0.8 0.4 1.)
                                               (let a_40 (* anf_261 0.35)
                                                (let anf_262 (vec3 1. 0.5 0.2)
                                                 (let anf_263
                                                  (- 1. spot1_dist_38)
                                                  (let anf_264
                                                   (smoothstep_2 0. 1. anf_263)
                                                   (let b_41 (* anf_262 anf_264)
                                                    (let anf_265
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_266
                                                      (- 1. spot2_dist_39)
                                                      (let anf_267
                                                       (smoothstep_2 0. 1.
                                                        anf_266)
                                                       (let c_42
                                                        (* anf_265 anf_267)
                                                        (let anf_268
                                                         (+ b_41 c_42)
                                                         (let anf_269
                                                          (* anf_268 5.)
                                                          (let spot_logic_43
                                                           (+ a_40 anf_269)
                                                           (let final_col_44
                                                            (* col_37
                                                             spot_logic_43)
                                                            (let anf_270
                                                             (max final_col_44
                                                              0.)
                                                             (return
                                                              (sqrt anf_270)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (warped_noise.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution) (Global Uniform TyFloat u_time)
      (Function (name fract_0) (desc ()) (params (((TyVec 4) v_1)))
       (ret_type (TyVec 4))
       (body ((set () vec4 anf_172 (floor v_1)) (return (- v_1 anf_172)))))
      (Function (name smoothstep_2) (desc ())
       (params ((TyFloat edge0_3) (TyFloat edge1_4) (TyFloat x_5)))
       (ret_type TyFloat)
       (body
        ((set () float anf_173 (- x_5 edge0_3))
         (set () float anf_174 (- edge1_4 edge0_3))
         (set () float anf_175 (/ anf_173 anf_174))
         (set () float t_6 (clamp anf_175 0. 1.))
         (set () float anf_176 (* t_6 t_6)) (set () float anf_177 (* 2. t_6))
         (set () float anf_178 (- 3. anf_177)) (return (* anf_176 anf_178)))))
      (Function (name smoothNoise_7) (desc ()) (params (((TyVec 2) p_8)))
       (ret_type TyFloat)
       (body
        ((set () vec2 i_9 (floor p_8)) (set () vec2 pf_10 (- p_8 i_9))
         (set () vec2 anf_179 (* pf_10 pf_10)) (set () vec2 anf_180 (* 2. pf_10))
         (set () vec2 anf_181 (- 3. anf_180))
         (set () vec2 inter_11 (* anf_179 anf_181))
         (set () vec4 v4_12 (vec4 0. 1. 27. 28.))
         (set () float anf_182 (index i_9 0))
         (set () vec4 anf_183 (+ v4_12 anf_182))
         (set () float anf_184 (index i_9 1))
         (set () float anf_185 (* anf_184 27.))
         (set () vec4 seed_13 (+ anf_183 anf_185))
         (set () vec4 anf_186 (% seed_13 6.2831853))
         (set () vec4 anf_187 (sin anf_186))
         (set () vec4 anf_188 (* anf_187 200000.))
         (set () vec4 hash_14 (fract_0 anf_188))
         (set () float anf_189 (index hash_14 0))
         (set () float anf_190 (index hash_14 1))
         (set () vec2 col0_15 (vec2 anf_189 anf_190))
         (set () float anf_191 (index hash_14 2))
         (set () float anf_192 (index hash_14 3))
         (set () vec2 col1_16 (vec2 anf_191 anf_192))
         (set () float anf_193 (index inter_11 1))
         (set () float anf_194 (- 1. anf_193))
         (set () vec2 anf_195 (* col0_15 anf_194))
         (set () float anf_196 (index inter_11 1))
         (set () vec2 anf_197 (* col1_16 anf_196))
         (set () vec2 res_v_17 (+ anf_195 anf_197))
         (set () float anf_198 (index inter_11 0))
         (set () float anf_199 (- 1. anf_198))
         (set () float anf_200 (index inter_11 0))
         (set () vec2 anf_201 (vec2 anf_199 anf_200))
         (return (dot res_v_17 anf_201)))))
      (Function (name fractalNoise_18) (desc ()) (params (((TyVec 2) p_19)))
       (ret_type TyFloat)
       (body
        ((set () float anf_202 (smoothNoise_7 p_19))
         (set () float anf_203 (* anf_202 0.5333))
         (set () vec2 anf_204 (* p_19 2.))
         (set () float anf_205 (smoothNoise_7 anf_204))
         (set () float anf_206 (* anf_205 0.2667))
         (set () float anf_207 (+ anf_203 anf_206))
         (set () vec2 anf_208 (* p_19 4.))
         (set () float anf_209 (smoothNoise_7 anf_208))
         (set () float anf_210 (* anf_209 0.1333))
         (set () float anf_211 (+ anf_207 anf_210))
         (set () vec2 anf_212 (* p_19 8.))
         (set () float anf_213 (smoothNoise_7 anf_212))
         (set () float anf_214 (* anf_213 0.0667)) (return (+ anf_211 anf_214)))))
      (Function (name warpedNoise_20) (desc ()) (params (((TyVec 2) p_21)))
       (ret_type TyFloat)
       (body
        ((set () float anf_215 (- 0. u_time))
         (set () vec2 anf_216 (vec2 u_time anf_215))
         (set () vec2 m_22 (* anf_216 0.5)) (set () vec2 anf_217 (+ p_21 m_22))
         (set () float x_23 (fractalNoise_18 anf_217))
         (set () float anf_218 (index m_22 1))
         (set () float anf_219 (index m_22 0))
         (set () vec2 anf_220 (vec2 anf_218 anf_219))
         (set () vec2 anf_221 (+ p_21 anf_220))
         (set () vec2 anf_222 (+ anf_221 x_23))
         (set () float y_24 (fractalNoise_18 anf_222))
         (set () vec2 anf_223 (- p_21 m_22))
         (set () vec2 anf_224 (- anf_223 x_23))
         (set () vec2 anf_225 (+ anf_224 y_24))
         (set () float z_25 (fractalNoise_18 anf_225))
         (set () vec2 anf_226 (vec2 x_23 y_24))
         (set () vec2 anf_227 (vec2 y_24 z_25))
         (set () vec2 anf_228 (+ anf_226 anf_227))
         (set () vec2 anf_229 (vec2 z_25 x_23))
         (set () vec2 warp_26 (+ anf_228 anf_229))
         (set () vec3 anf_230 (vec3 x_23 y_24 z_25))
         (set () float anf_231 (length anf_230))
         (set () float mag_27 (* anf_231 0.25))
         (set () vec2 anf_232 (+ p_21 warp_26))
         (set () vec2 anf_233 (+ anf_232 mag_27))
         (return (fractalNoise_18 anf_233)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_28)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 anf_234 (* u_resolution 0.5))
         (set () vec2 anf_235 (- coord_28 anf_234))
         (set () float anf_236 (index u_resolution 1))
         (set () vec2 uv_29 (/ anf_235 anf_236))
         (set () vec2 anf_237 (* uv_29 6.))
         (set () float n_30 (warpedNoise_20 anf_237))
         (set () vec2 anf_238 (* uv_29 6.))
         (set () vec2 anf_239 (- anf_238 0.02))
         (set () float n2_31 (warpedNoise_20 anf_239))
         (set () float anf_240 (- n2_31 n_30))
         (set () float anf_241 (max anf_240 0.))
         (set () float anf_242 (/ anf_241 0.02))
         (set () float bump_32 (* anf_242 0.7071))
         (set () float anf_243 (- n_30 n2_31))
         (set () float anf_244 (max anf_243 0.))
         (set () float anf_245 (/ anf_244 0.02))
         (set () float bump2_33 (* anf_245 0.7071))
         (set () float anf_246 (* bump_32 bump_32))
         (set () float anf_247 (pow bump_32 4.))
         (set () float anf_248 (* anf_247 0.5))
         (set () float b1_34 (+ anf_246 anf_248))
         (set () float anf_249 (* bump2_33 bump2_33))
         (set () float anf_250 (pow bump2_33 4.))
         (set () float anf_251 (* anf_250 0.5))
         (set () float b2_35 (+ anf_249 anf_251))
         (set () vec3 anf_252 (vec3 1. 0.7 0.6))
         (set () float anf_253 (+ b1_34 b2_35))
         (set () float anf_254 (* anf_253 0.4))
         (set () vec3 anf_255 (vec3 b1_34 anf_254 b2_35))
         (set () vec3 anf_256 (* anf_252 anf_255))
         (set () vec3 anf_257 (* anf_256 0.3))
         (set () vec3 base_col_36 (+ anf_257 0.5))
         (set () float anf_258 (* n_30 n_30))
         (set () vec3 col_37 (* anf_258 base_col_36))
         (set () vec2 anf_259 (- uv_29 0.65))
         (set () float spot1_dist_38 (length anf_259))
         (set () vec2 anf_260 (+ uv_29 0.5))
         (set () float spot2_dist_39 (length anf_260))
         (set () vec3 anf_261 (vec3 0.8 0.4 1.))
         (set () vec3 a_40 (* anf_261 0.35))
         (set () vec3 anf_262 (vec3 1. 0.5 0.2))
         (set () float anf_263 (- 1. spot1_dist_38))
         (set () float anf_264 (smoothstep_2 0. 1. anf_263))
         (set () vec3 b_41 (* anf_262 anf_264))
         (set () vec3 anf_265 (vec3 0.2 0.4 1.))
         (set () float anf_266 (- 1. spot2_dist_39))
         (set () float anf_267 (smoothstep_2 0. 1. anf_266))
         (set () vec3 c_42 (* anf_265 anf_267))
         (set () vec3 anf_268 (+ b_41 c_42)) (set () vec3 anf_269 (* anf_268 5.))
         (set () vec3 spot_logic_43 (+ a_40 anf_269))
         (set () vec3 final_col_44 (* col_37 spot_logic_43))
         (set () vec3 anf_270 (max final_col_44 0.)) (return (sqrt anf_270)))))))

    === patch_main (warped_noise.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor) (Global Uniform (TyVec 2) u_resolution)
      (Global Uniform TyFloat u_time)
      (Function (name fract_0) (desc ()) (params (((TyVec 4) v_1)))
       (ret_type (TyVec 4))
       (body ((set () vec4 anf_172 (floor v_1)) (return (- v_1 anf_172)))))
      (Function (name smoothstep_2) (desc ())
       (params ((TyFloat edge0_3) (TyFloat edge1_4) (TyFloat x_5)))
       (ret_type TyFloat)
       (body
        ((set () float anf_173 (- x_5 edge0_3))
         (set () float anf_174 (- edge1_4 edge0_3))
         (set () float anf_175 (/ anf_173 anf_174))
         (set () float t_6 (clamp anf_175 0. 1.))
         (set () float anf_176 (* t_6 t_6)) (set () float anf_177 (* 2. t_6))
         (set () float anf_178 (- 3. anf_177)) (return (* anf_176 anf_178)))))
      (Function (name smoothNoise_7) (desc ()) (params (((TyVec 2) p_8)))
       (ret_type TyFloat)
       (body
        ((set () vec2 i_9 (floor p_8)) (set () vec2 pf_10 (- p_8 i_9))
         (set () vec2 anf_179 (* pf_10 pf_10)) (set () vec2 anf_180 (* 2. pf_10))
         (set () vec2 anf_181 (- 3. anf_180))
         (set () vec2 inter_11 (* anf_179 anf_181))
         (set () vec4 v4_12 (vec4 0. 1. 27. 28.))
         (set () float anf_182 (index i_9 0))
         (set () vec4 anf_183 (+ v4_12 anf_182))
         (set () float anf_184 (index i_9 1))
         (set () float anf_185 (* anf_184 27.))
         (set () vec4 seed_13 (+ anf_183 anf_185))
         (set () vec4 anf_186 (% seed_13 6.2831853))
         (set () vec4 anf_187 (sin anf_186))
         (set () vec4 anf_188 (* anf_187 200000.))
         (set () vec4 hash_14 (fract_0 anf_188))
         (set () float anf_189 (index hash_14 0))
         (set () float anf_190 (index hash_14 1))
         (set () vec2 col0_15 (vec2 anf_189 anf_190))
         (set () float anf_191 (index hash_14 2))
         (set () float anf_192 (index hash_14 3))
         (set () vec2 col1_16 (vec2 anf_191 anf_192))
         (set () float anf_193 (index inter_11 1))
         (set () float anf_194 (- 1. anf_193))
         (set () vec2 anf_195 (* col0_15 anf_194))
         (set () float anf_196 (index inter_11 1))
         (set () vec2 anf_197 (* col1_16 anf_196))
         (set () vec2 res_v_17 (+ anf_195 anf_197))
         (set () float anf_198 (index inter_11 0))
         (set () float anf_199 (- 1. anf_198))
         (set () float anf_200 (index inter_11 0))
         (set () vec2 anf_201 (vec2 anf_199 anf_200))
         (return (dot res_v_17 anf_201)))))
      (Function (name fractalNoise_18) (desc ()) (params (((TyVec 2) p_19)))
       (ret_type TyFloat)
       (body
        ((set () float anf_202 (smoothNoise_7 p_19))
         (set () float anf_203 (* anf_202 0.5333))
         (set () vec2 anf_204 (* p_19 2.))
         (set () float anf_205 (smoothNoise_7 anf_204))
         (set () float anf_206 (* anf_205 0.2667))
         (set () float anf_207 (+ anf_203 anf_206))
         (set () vec2 anf_208 (* p_19 4.))
         (set () float anf_209 (smoothNoise_7 anf_208))
         (set () float anf_210 (* anf_209 0.1333))
         (set () float anf_211 (+ anf_207 anf_210))
         (set () vec2 anf_212 (* p_19 8.))
         (set () float anf_213 (smoothNoise_7 anf_212))
         (set () float anf_214 (* anf_213 0.0667)) (return (+ anf_211 anf_214)))))
      (Function (name warpedNoise_20) (desc ()) (params (((TyVec 2) p_21)))
       (ret_type TyFloat)
       (body
        ((set () float anf_215 (- 0. u_time))
         (set () vec2 anf_216 (vec2 u_time anf_215))
         (set () vec2 m_22 (* anf_216 0.5)) (set () vec2 anf_217 (+ p_21 m_22))
         (set () float x_23 (fractalNoise_18 anf_217))
         (set () float anf_218 (index m_22 1))
         (set () float anf_219 (index m_22 0))
         (set () vec2 anf_220 (vec2 anf_218 anf_219))
         (set () vec2 anf_221 (+ p_21 anf_220))
         (set () vec2 anf_222 (+ anf_221 x_23))
         (set () float y_24 (fractalNoise_18 anf_222))
         (set () vec2 anf_223 (- p_21 m_22))
         (set () vec2 anf_224 (- anf_223 x_23))
         (set () vec2 anf_225 (+ anf_224 y_24))
         (set () float z_25 (fractalNoise_18 anf_225))
         (set () vec2 anf_226 (vec2 x_23 y_24))
         (set () vec2 anf_227 (vec2 y_24 z_25))
         (set () vec2 anf_228 (+ anf_226 anf_227))
         (set () vec2 anf_229 (vec2 z_25 x_23))
         (set () vec2 warp_26 (+ anf_228 anf_229))
         (set () vec3 anf_230 (vec3 x_23 y_24 z_25))
         (set () float anf_231 (length anf_230))
         (set () float mag_27 (* anf_231 0.25))
         (set () vec2 anf_232 (+ p_21 warp_26))
         (set () vec2 anf_233 (+ anf_232 mag_27))
         (return (fractalNoise_18 anf_233)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_28)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 anf_234 (* u_resolution 0.5))
         (set () vec2 anf_235 (- coord_28 anf_234))
         (set () float anf_236 (index u_resolution 1))
         (set () vec2 uv_29 (/ anf_235 anf_236))
         (set () vec2 anf_237 (* uv_29 6.))
         (set () float n_30 (warpedNoise_20 anf_237))
         (set () vec2 anf_238 (* uv_29 6.))
         (set () vec2 anf_239 (- anf_238 0.02))
         (set () float n2_31 (warpedNoise_20 anf_239))
         (set () float anf_240 (- n2_31 n_30))
         (set () float anf_241 (max anf_240 0.))
         (set () float anf_242 (/ anf_241 0.02))
         (set () float bump_32 (* anf_242 0.7071))
         (set () float anf_243 (- n_30 n2_31))
         (set () float anf_244 (max anf_243 0.))
         (set () float anf_245 (/ anf_244 0.02))
         (set () float bump2_33 (* anf_245 0.7071))
         (set () float anf_246 (* bump_32 bump_32))
         (set () float anf_247 (pow bump_32 4.))
         (set () float anf_248 (* anf_247 0.5))
         (set () float b1_34 (+ anf_246 anf_248))
         (set () float anf_249 (* bump2_33 bump2_33))
         (set () float anf_250 (pow bump2_33 4.))
         (set () float anf_251 (* anf_250 0.5))
         (set () float b2_35 (+ anf_249 anf_251))
         (set () vec3 anf_252 (vec3 1. 0.7 0.6))
         (set () float anf_253 (+ b1_34 b2_35))
         (set () float anf_254 (* anf_253 0.4))
         (set () vec3 anf_255 (vec3 b1_34 anf_254 b2_35))
         (set () vec3 anf_256 (* anf_252 anf_255))
         (set () vec3 anf_257 (* anf_256 0.3))
         (set () vec3 base_col_36 (+ anf_257 0.5))
         (set () float anf_258 (* n_30 n_30))
         (set () vec3 col_37 (* anf_258 base_col_36))
         (set () vec2 anf_259 (- uv_29 0.65))
         (set () float spot1_dist_38 (length anf_259))
         (set () vec2 anf_260 (+ uv_29 0.5))
         (set () float spot2_dist_39 (length anf_260))
         (set () vec3 anf_261 (vec3 0.8 0.4 1.))
         (set () vec3 a_40 (* anf_261 0.35))
         (set () vec3 anf_262 (vec3 1. 0.5 0.2))
         (set () float anf_263 (- 1. spot1_dist_38))
         (set () float anf_264 (smoothstep_2 0. 1. anf_263))
         (set () vec3 b_41 (* anf_262 anf_264))
         (set () vec3 anf_265 (vec3 0.2 0.4 1.))
         (set () float anf_266 (- 1. spot2_dist_39))
         (set () float anf_267 (smoothstep_2 0. 1. anf_266))
         (set () vec3 c_42 (* anf_265 anf_267))
         (set () vec3 anf_268 (+ b_41 c_42)) (set () vec3 anf_269 (* anf_268 5.))
         (set () vec3 spot_logic_43 (+ a_40 anf_269))
         (set () vec3 final_col_44 (* col_37 spot_logic_43))
         (set () vec3 anf_270 (max final_col_44 0.)) (return (sqrt anf_270)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))
    |}]
;;
