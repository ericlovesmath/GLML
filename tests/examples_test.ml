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

    === lambda lift (checkerboard.glml) ===
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

    === tail call (checkerboard.glml) ===
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

    === patch main (checkerboard.glml) ===
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

    === lambda lift (mouse_circle.glml) ===
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

    === tail call (mouse_circle.glml) ===
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

    === patch main (mouse_circle.glml) ===
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

    === lambda lift (rainbow.glml) ===
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

    === tail call (rainbow.glml) ===
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

    === patch main (rainbow.glml) ===
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

    === lambda lift (recursion.glml) ===
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

    === tail call (recursion.glml) ===
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
                      (let _iter_inc_41 (+ _iter_39 1)
                       (set _iter_39 _iter_inc_41 continue)))))
                   (let anf_25 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_25
                      (let _iter_inc_40 (+ _iter_39 1)
                       (set _iter_39 _iter_inc_40 continue))))))))))))))
          0.))))
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
                 (set b_10 b_10) (set () int _iter_inc_41 (+ _iter_39 1))
                 (set _iter_39 _iter_inc_41) continue)
                (Block (set () float anf_25 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_25) (set () int _iter_inc_40 (+ _iter_39 1))
                 (set _iter_39 _iter_inc_40) continue))))))))
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

    === patch main (recursion.glml) ===
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
                 (set b_10 b_10) (set () int _iter_inc_41 (+ _iter_39 1))
                 (set _iter_39 _iter_inc_41) continue)
                (Block (set () float anf_25 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_25) (set () int _iter_inc_40 (+ _iter_39 1))
                 (set _iter_39 _iter_inc_40) continue))))))))
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
    |}]
;;
