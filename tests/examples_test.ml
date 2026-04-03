open Core
open Glml_compiler

let examples_dir = "../examples"

let%expect_test "compile examples" =
  let glml_files = Stdlib.Sys.readdir examples_dir in
  Array.iter glml_files ~f:(fun file ->
    let source = In_channel.read_all (Filename.concat examples_dir file) in
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
    match compile ~dump source with
    | Ok _ -> ()
    | Error err -> print_endline (Compiler_error.to_string_hum ~source err));
  [%expect
    {|
    ====== COMPILING EXAMPLE 2d_sdf_variants.glml ======

    === stlc (2d_sdf_variants.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern (vec 2) u_mouse)
      (Extern float u_time)
      (TypeDef shape
       (VariantDecl () ((Circle (float)) (Rect (float float)) (Empty ()))))
      (Define Nonrec sdf
       (lambda (s (shape))
        (lambda (p ((vec 2)))
         (match s ((Circle r) (- (length p) r))
          ((Rect w h)
           (let d (vec2 (- (abs (index p 0)) w) (- (abs (index p 1)) h))
            (+ (length (max d (vec2 0. 0.)))
             (min (max (index d 0) (index d 1)) 0.))))
          ((Empty) 1.)))))
      (Define Nonrec scene
       (lambda (p ((vec 2)))
        (let circle (app (app sdf (Variant Circle 0.3)) p)
         (let rect (app (app sdf (Variant Rect 0.7 0.1)) p) (min circle rect)))))
      (Define Nonrec get_uv
       (lambda (coord ())
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
        (let p (app get_uv coord)
         (let m (app get_uv u_mouse)
          (let d (app scene p)
           (let col (if (> d 0.) (vec3 0.9 0.6 0.3) (vec3 0.65 0.85 1.))
            (let col
             (let darken (- 1. (exp (* -6. (abs d))))
              (let rings (+ 0.8 (* 0.2 (cos (* 150. d))))
               (* (* col darken) rings)))
             (let col
              (mix col (vec3 1. 1. 1.) (- 1. (smoothstep 0. 0.01 (abs d))))
              (let col
               (let d (abs (app scene m))
                (let dm (length (- p m))
                 (let d (min (- (abs (- dm d)) 0.0025) (- dm 0.015))
                  (mix col (vec3 1. 1. 0.) (- 1. (smoothstep 0. 0.005 d))))))
               col)))))))))))

    === uniquify (2d_sdf_variants.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern (vec 2) u_mouse)
      (Extern float u_time)
      (TypeDef shape
       (VariantDecl () ((Circle (float)) (Rect (float float)) (Empty ()))))
      (Define Nonrec sdf_0
       (lambda (s_1 (shape))
        (lambda (p_2 ((vec 2)))
         (match s_1 ((Circle r_3) (- (length p_2) r_3))
          ((Rect w_4 h_5)
           (let d_6
            (vec2 (- (abs (index p_2 0)) w_4) (- (abs (index p_2 1)) h_5))
            (+ (length (max d_6 (vec2 0. 0.)))
             (min (max (index d_6 0) (index d_6 1)) 0.))))
          ((Empty) 1.)))))
      (Define Nonrec scene_7
       (lambda (p_8 ((vec 2)))
        (let circle_9 (app (app sdf_0 (Variant Circle 0.3)) p_8)
         (let rect_10 (app (app sdf_0 (Variant Rect 0.7 0.1)) p_8)
          (min circle_9 rect_10)))))
      (Define Nonrec get_uv_11
       (lambda (coord_12 ())
        (let top_13 (- (* 2. coord_12) u_resolution)
         (let bot_14 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_13 bot_14)))))
      (Define Nonrec main
       (lambda (coord_15 ((vec 2)))
        (let p_16 (app get_uv_11 coord_15)
         (let m_17 (app get_uv_11 u_mouse)
          (let d_18 (app scene_7 p_16)
           (let col_19 (if (> d_18 0.) (vec3 0.9 0.6 0.3) (vec3 0.65 0.85 1.))
            (let col_20
             (let darken_21 (- 1. (exp (* -6. (abs d_18))))
              (let rings_22 (+ 0.8 (* 0.2 (cos (* 150. d_18))))
               (* (* col_19 darken_21) rings_22)))
             (let col_23
              (mix col_20 (vec3 1. 1. 1.) (- 1. (smoothstep 0. 0.01 (abs d_18))))
              (let col_24
               (let d_25 (abs (app scene_7 m_17))
                (let dm_26 (length (- p_16 m_17))
                 (let d_27 (min (- (abs (- dm_26 d_25)) 0.0025) (- dm_26 0.015))
                  (mix col_23 (vec3 1. 1. 0.) (- 1. (smoothstep 0. 0.005 d_27))))))
               col_24)))))))))))

    === typecheck (2d_sdf_variants.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
      ((Extern u_time) : float)
      ((TypeDef shape
        (VariantDecl () ((Circle (float)) (Rect (float float)) (Empty ()))))
       : shape)
      ((Define Nonrec sdf_0
        ((lambda s_1
          ((lambda p_2
            ((match (s_1 : shape)
              ((Circle r_3)
               ((- ((length (p_2 : (vec 2))) : float) (r_3 : float)) : float))
              ((Rect w_4 h_5)
               ((let d_6
                 ((vec2
                   ((- ((abs ((index (p_2 : (vec 2)) 0) : float)) : float)
                     (w_4 : float))
                    : float)
                   ((- ((abs ((index (p_2 : (vec 2)) 1) : float)) : float)
                     (h_5 : float))
                    : float))
                  : (vec 2))
                 ((+
                   ((length
                     ((max (d_6 : (vec 2))
                       ((vec2 (0. : float) (0. : float)) : (vec 2)))
                      : (vec 2)))
                    : float)
                   ((min
                     ((max ((index (d_6 : (vec 2)) 0) : float)
                       ((index (d_6 : (vec 2)) 1) : float))
                      : float)
                     (0. : float))
                    : float))
                  : float))
                : float))
              ((Empty) (1. : float)))
             : float))
           : ((vec 2) -> float)))
         : (shape -> ((vec 2) -> float))))
       : (shape -> ((vec 2) -> float)))
      ((Define Nonrec scene_7
        ((lambda p_8
          ((let circle_9
            ((app
              ((app (sdf_0 : (shape -> ((vec 2) -> float)))
                ((Variant shape Circle (0.3 : float)) : shape))
               : ((vec 2) -> float))
              (p_8 : (vec 2)))
             : float)
            ((let rect_10
              ((app
                ((app (sdf_0 : (shape -> ((vec 2) -> float)))
                  ((Variant shape Rect (0.7 : float) (0.1 : float)) : shape))
                 : ((vec 2) -> float))
                (p_8 : (vec 2)))
               : float)
              ((min (circle_9 : float) (rect_10 : float)) : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec get_uv_11
        ((lambda coord_12
          ((let top_13
            ((- ((* (2. : float) (coord_12 : 'v_57)) : 'v_58)
              (u_resolution : (vec 2)))
             : 'v_59)
            ((let bot_14
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_13 : 'v_59) (bot_14 : float)) : 'v_64))
             : 'v_64))
           : 'v_64))
         : ('v_57 -> 'v_64)))
       :
       (forall
        ((Broadcast 'v_58 (vec 2) 'v_59) (MulBroadcast float 'v_57 'v_58)
         (MulBroadcast 'v_59 float 'v_64))
        ('v_57 -> 'v_64)))
      ((Define Nonrec main
        ((lambda coord_15
          ((let p_16
            ((app (get_uv_11 : ((vec 2) -> (vec 2))) (coord_15 : (vec 2))) :
             (vec 2))
            ((let m_17
              ((app (get_uv_11 : ((vec 2) -> (vec 2))) (u_mouse : (vec 2))) :
               (vec 2))
              ((let d_18
                ((app (scene_7 : ((vec 2) -> float)) (p_16 : (vec 2))) : float)
                ((let col_19
                  ((if ((> (d_18 : float) (0. : float)) : bool)
                    ((vec3 (0.9 : float) (0.6 : float) (0.3 : float)) : (vec 3))
                    ((vec3 (0.65 : float) (0.85 : float) (1. : float)) : (vec 3)))
                   : (vec 3))
                  ((let col_20
                    ((let darken_21
                      ((- (1. : float)
                        ((exp
                          ((* (-6. : float) ((abs (d_18 : float)) : float)) :
                           float))
                         : float))
                       : float)
                      ((let rings_22
                        ((+ (0.8 : float)
                          ((* (0.2 : float)
                            ((cos ((* (150. : float) (d_18 : float)) : float)) :
                             float))
                           : float))
                         : float)
                        ((*
                          ((* (col_19 : (vec 3)) (darken_21 : float)) : (vec 3))
                          (rings_22 : float))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3))
                    ((let col_23
                      ((mix (col_20 : (vec 3))
                        ((vec3 (1. : float) (1. : float) (1. : float)) : (vec 3))
                        ((- (1. : float)
                          ((smoothstep (0. : float) (0.01 : float)
                            ((abs (d_18 : float)) : float))
                           : float))
                         : float))
                       : (vec 3))
                      ((let col_24
                        ((let d_25
                          ((abs
                            ((app (scene_7 : ((vec 2) -> float))
                              (m_17 : (vec 2)))
                             : float))
                           : float)
                          ((let dm_26
                            ((length
                              ((- (p_16 : (vec 2)) (m_17 : (vec 2))) : (vec 2)))
                             : float)
                            ((let d_27
                              ((min
                                ((-
                                  ((abs
                                    ((- (dm_26 : float) (d_25 : float)) : float))
                                   : float)
                                  (0.0025 : float))
                                 : float)
                                ((- (dm_26 : float) (0.015 : float)) : float))
                               : float)
                              ((mix (col_23 : (vec 3))
                                ((vec3 (1. : float) (1. : float) (0. : float)) :
                                 (vec 3))
                                ((- (1. : float)
                                  ((smoothstep (0. : float) (0.005 : float)
                                    (d_27 : float))
                                   : float))
                                 : float))
                               : (vec 3)))
                             : (vec 3)))
                           : (vec 3)))
                         : (vec 3))
                        (col_24 : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === specialize params (2d_sdf_variants.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
      ((Extern u_time) : float)
      ((TypeDef shape
        (VariantDecl () ((Circle (float)) (Rect (float float)) (Empty ()))))
       : shape)
      ((Define Nonrec sdf_0
        ((lambda s_1
          ((lambda p_2
            ((match (s_1 : shape)
              ((Circle r_3)
               ((- ((length (p_2 : (vec 2))) : float) (r_3 : float)) : float))
              ((Rect w_4 h_5)
               ((let d_6
                 ((vec2
                   ((- ((abs ((index (p_2 : (vec 2)) 0) : float)) : float)
                     (w_4 : float))
                    : float)
                   ((- ((abs ((index (p_2 : (vec 2)) 1) : float)) : float)
                     (h_5 : float))
                    : float))
                  : (vec 2))
                 ((+
                   ((length
                     ((max (d_6 : (vec 2))
                       ((vec2 (0. : float) (0. : float)) : (vec 2)))
                      : (vec 2)))
                    : float)
                   ((min
                     ((max ((index (d_6 : (vec 2)) 0) : float)
                       ((index (d_6 : (vec 2)) 1) : float))
                      : float)
                     (0. : float))
                    : float))
                  : float))
                : float))
              ((Empty) (1. : float)))
             : float))
           : ((vec 2) -> float)))
         : (shape -> ((vec 2) -> float))))
       : (shape -> ((vec 2) -> float)))
      ((Define Nonrec scene_7
        ((lambda p_8
          ((let circle_9
            ((app
              ((app (sdf_0 : (shape -> ((vec 2) -> float)))
                ((Variant shape Circle (0.3 : float)) : shape))
               : ((vec 2) -> float))
              (p_8 : (vec 2)))
             : float)
            ((let rect_10
              ((app
                ((app (sdf_0 : (shape -> ((vec 2) -> float)))
                  ((Variant shape Rect (0.7 : float) (0.1 : float)) : shape))
                 : ((vec 2) -> float))
                (p_8 : (vec 2)))
               : float)
              ((min (circle_9 : float) (rect_10 : float)) : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec get_uv_11
        ((lambda coord_12
          ((let top_13
            ((- ((* (2. : float) (coord_12 : 'v_57)) : 'v_58)
              (u_resolution : (vec 2)))
             : 'v_59)
            ((let bot_14
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_13 : 'v_59) (bot_14 : float)) : 'v_64))
             : 'v_64))
           : 'v_64))
         : ('v_57 -> 'v_64)))
       :
       (forall
        ((Broadcast 'v_58 (vec 2) 'v_59) (MulBroadcast float 'v_57 'v_58)
         (MulBroadcast 'v_59 float 'v_64))
        ('v_57 -> 'v_64)))
      ((Define Nonrec main
        ((lambda coord_15
          ((let p_16
            ((app (get_uv_11 : ((vec 2) -> (vec 2))) (coord_15 : (vec 2))) :
             (vec 2))
            ((let m_17
              ((app (get_uv_11 : ((vec 2) -> (vec 2))) (u_mouse : (vec 2))) :
               (vec 2))
              ((let d_18
                ((app (scene_7 : ((vec 2) -> float)) (p_16 : (vec 2))) : float)
                ((let col_19
                  ((if ((> (d_18 : float) (0. : float)) : bool)
                    ((vec3 (0.9 : float) (0.6 : float) (0.3 : float)) : (vec 3))
                    ((vec3 (0.65 : float) (0.85 : float) (1. : float)) : (vec 3)))
                   : (vec 3))
                  ((let col_20
                    ((let darken_21
                      ((- (1. : float)
                        ((exp
                          ((* (-6. : float) ((abs (d_18 : float)) : float)) :
                           float))
                         : float))
                       : float)
                      ((let rings_22
                        ((+ (0.8 : float)
                          ((* (0.2 : float)
                            ((cos ((* (150. : float) (d_18 : float)) : float)) :
                             float))
                           : float))
                         : float)
                        ((*
                          ((* (col_19 : (vec 3)) (darken_21 : float)) : (vec 3))
                          (rings_22 : float))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3))
                    ((let col_23
                      ((mix (col_20 : (vec 3))
                        ((vec3 (1. : float) (1. : float) (1. : float)) : (vec 3))
                        ((- (1. : float)
                          ((smoothstep (0. : float) (0.01 : float)
                            ((abs (d_18 : float)) : float))
                           : float))
                         : float))
                       : (vec 3))
                      ((let col_24
                        ((let d_25
                          ((abs
                            ((app (scene_7 : ((vec 2) -> float))
                              (m_17 : (vec 2)))
                             : float))
                           : float)
                          ((let dm_26
                            ((length
                              ((- (p_16 : (vec 2)) (m_17 : (vec 2))) : (vec 2)))
                             : float)
                            ((let d_27
                              ((min
                                ((-
                                  ((abs
                                    ((- (dm_26 : float) (d_25 : float)) : float))
                                   : float)
                                  (0.0025 : float))
                                 : float)
                                ((- (dm_26 : float) (0.015 : float)) : float))
                               : float)
                              ((mix (col_23 : (vec 3))
                                ((vec3 (1. : float) (1. : float) (0. : float)) :
                                 (vec 3))
                                ((- (1. : float)
                                  ((smoothstep (0. : float) (0.005 : float)
                                    (d_27 : float))
                                   : float))
                                 : float))
                               : (vec 3)))
                             : (vec 3)))
                           : (vec 3)))
                         : (vec 3))
                        (col_24 : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === monomorphize (2d_sdf_variants.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
      ((Extern u_time) : float)
      ((TypeDef shape
        (VariantDecl ((Circle (float)) (Rect (float float)) (Empty ()))))
       : shape)
      ((Define Nonrec sdf_0
        ((lambda s_1
          ((lambda p_2
            ((match (s_1 : shape)
              ((Circle r_3)
               ((- ((length (p_2 : (vec 2))) : float) (r_3 : float)) : float))
              ((Rect w_4 h_5)
               ((let d_6
                 ((vec2
                   ((- ((abs ((index (p_2 : (vec 2)) 0) : float)) : float)
                     (w_4 : float))
                    : float)
                   ((- ((abs ((index (p_2 : (vec 2)) 1) : float)) : float)
                     (h_5 : float))
                    : float))
                  : (vec 2))
                 ((+
                   ((length
                     ((max (d_6 : (vec 2))
                       ((vec2 (0. : float) (0. : float)) : (vec 2)))
                      : (vec 2)))
                    : float)
                   ((min
                     ((max ((index (d_6 : (vec 2)) 0) : float)
                       ((index (d_6 : (vec 2)) 1) : float))
                      : float)
                     (0. : float))
                    : float))
                  : float))
                : float))
              ((Empty) (1. : float)))
             : float))
           : ((vec 2) -> float)))
         : (shape -> ((vec 2) -> float))))
       : (shape -> ((vec 2) -> float)))
      ((Define Nonrec scene_7
        ((lambda p_8
          ((let circle_9
            ((app
              ((app (sdf_0 : (shape -> ((vec 2) -> float)))
                ((Variant shape Circle (0.3 : float)) : shape))
               : ((vec 2) -> float))
              (p_8 : (vec 2)))
             : float)
            ((let rect_10
              ((app
                ((app (sdf_0 : (shape -> ((vec 2) -> float)))
                  ((Variant shape Rect (0.7 : float) (0.1 : float)) : shape))
                 : ((vec 2) -> float))
                (p_8 : (vec 2)))
               : float)
              ((min (circle_9 : float) (rect_10 : float)) : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec get_uv_11_vec2_to_vec2_120
        ((lambda coord_12
          ((let top_13
            ((- ((* (2. : float) (coord_12 : (vec 2))) : (vec 2))
              (u_resolution : (vec 2)))
             : (vec 2))
            ((let bot_14
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_13 : (vec 2)) (bot_14 : float)) : (vec 2)))
             : (vec 2)))
           : (vec 2)))
         : ((vec 2) -> (vec 2))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        ((lambda coord_15
          ((let p_16
            ((app (get_uv_11_vec2_to_vec2_120 : ((vec 2) -> (vec 2)))
              (coord_15 : (vec 2)))
             : (vec 2))
            ((let m_17
              ((app (get_uv_11_vec2_to_vec2_120 : ((vec 2) -> (vec 2)))
                (u_mouse : (vec 2)))
               : (vec 2))
              ((let d_18
                ((app (scene_7 : ((vec 2) -> float)) (p_16 : (vec 2))) : float)
                ((let col_19
                  ((if ((> (d_18 : float) (0. : float)) : bool)
                    ((vec3 (0.9 : float) (0.6 : float) (0.3 : float)) : (vec 3))
                    ((vec3 (0.65 : float) (0.85 : float) (1. : float)) : (vec 3)))
                   : (vec 3))
                  ((let col_20
                    ((let darken_21
                      ((- (1. : float)
                        ((exp
                          ((* (-6. : float) ((abs (d_18 : float)) : float)) :
                           float))
                         : float))
                       : float)
                      ((let rings_22
                        ((+ (0.8 : float)
                          ((* (0.2 : float)
                            ((cos ((* (150. : float) (d_18 : float)) : float)) :
                             float))
                           : float))
                         : float)
                        ((*
                          ((* (col_19 : (vec 3)) (darken_21 : float)) : (vec 3))
                          (rings_22 : float))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3))
                    ((let col_23
                      ((mix (col_20 : (vec 3))
                        ((vec3 (1. : float) (1. : float) (1. : float)) : (vec 3))
                        ((- (1. : float)
                          ((smoothstep (0. : float) (0.01 : float)
                            ((abs (d_18 : float)) : float))
                           : float))
                         : float))
                       : (vec 3))
                      ((let col_24
                        ((let d_25
                          ((abs
                            ((app (scene_7 : ((vec 2) -> float))
                              (m_17 : (vec 2)))
                             : float))
                           : float)
                          ((let dm_26
                            ((length
                              ((- (p_16 : (vec 2)) (m_17 : (vec 2))) : (vec 2)))
                             : float)
                            ((let d_27
                              ((min
                                ((-
                                  ((abs
                                    ((- (dm_26 : float) (d_25 : float)) : float))
                                   : float)
                                  (0.0025 : float))
                                 : float)
                                ((- (dm_26 : float) (0.015 : float)) : float))
                               : float)
                              ((mix (col_23 : (vec 3))
                                ((vec3 (1. : float) (1. : float) (0. : float)) :
                                 (vec 3))
                                ((- (1. : float)
                                  ((smoothstep (0. : float) (0.005 : float)
                                    (d_27 : float))
                                   : float))
                                 : float))
                               : (vec 3)))
                             : (vec 3)))
                           : (vec 3)))
                         : (vec 3))
                        (col_24 : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (2d_sdf_variants.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
      ((Extern u_time) : float)
      ((TypeDef shape
        (VariantDecl ((Circle (float)) (Rect (float float)) (Empty ()))))
       : shape)
      ((Define Nonrec sdf_0
        (lambda ((s_1 shape) (p_2 (vec 2)))
         (match s_1 ((Circle r_3) (- (length p_2) r_3))
          ((Rect w_4 h_5)
           (let d_6
            (vec2 (- (abs (index p_2 0)) w_4) (- (abs (index p_2 1)) h_5))
            (+ (length (max d_6 (vec2 0. 0.)))
             (min (max (index d_6 0) (index d_6 1)) 0.))))
          ((Empty) 1.))))
       : (shape -> ((vec 2) -> float)))
      ((Define Nonrec scene_7
        (lambda ((p_8 (vec 2)))
         (let circle_9 (app sdf_0 (Variant shape Circle 0.3) p_8)
          (let rect_10 (app sdf_0 (Variant shape Rect 0.7 0.1) p_8)
           (min circle_9 rect_10)))))
       : ((vec 2) -> float))
      ((Define Nonrec get_uv_11_vec2_to_vec2_120
        (lambda ((coord_12 (vec 2)))
         (let top_13 (- (* 2. coord_12) u_resolution)
          (let bot_14 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_13 bot_14)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_15 (vec 2)))
         (let p_16 (app get_uv_11_vec2_to_vec2_120 coord_15)
          (let m_17 (app get_uv_11_vec2_to_vec2_120 u_mouse)
           (let d_18 (app scene_7 p_16)
            (let col_19 (if (> d_18 0.) (vec3 0.9 0.6 0.3) (vec3 0.65 0.85 1.))
             (let col_20
              (let darken_21 (- 1. (exp (* -6. (abs d_18))))
               (let rings_22 (+ 0.8 (* 0.2 (cos (* 150. d_18))))
                (* (* col_19 darken_21) rings_22)))
              (let col_23
               (mix col_20 (vec3 1. 1. 1.)
                (- 1. (smoothstep 0. 0.01 (abs d_18))))
               (let col_24
                (let d_25 (abs (app scene_7 m_17))
                 (let dm_26 (length (- p_16 m_17))
                  (let d_27 (min (- (abs (- dm_26 d_25)) 0.0025) (- dm_26 0.015))
                   (mix col_23 (vec3 1. 1. 0.) (- 1. (smoothstep 0. 0.005 d_27))))))
                col_24)))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (2d_sdf_variants.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((TypeDef shape
       (VariantDecl ((Circle (float)) (Rect (float float)) (Empty ()))))
      : shape)
     ((Define Nonrec (name sdf_0) (args ((s_1 shape) (p_2 (vec 2))))
       (body
        (match s_1 ((Circle r_3) (- (length p_2) r_3))
         ((Rect w_4 h_5)
          (let d_6 (vec2 (- (abs (index p_2 0)) w_4) (- (abs (index p_2 1)) h_5))
           (+ (length (max d_6 (vec2 0. 0.)))
            (min (max (index d_6 0) (index d_6 1)) 0.))))
         ((Empty) 1.))))
      : (shape -> ((vec 2) -> float)))
     ((Define Nonrec (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let circle_9 (app sdf_0 (Variant shape Circle 0.3) p_8)
         (let rect_10 (app sdf_0 (Variant shape Rect 0.7 0.1) p_8)
          (min circle_9 rect_10)))))
      : ((vec 2) -> float))
     ((Define Nonrec (name get_uv_11_vec2_to_vec2_120)
       (args ((coord_12 (vec 2))))
       (body
        (let top_13 (- (* 2. coord_12) u_resolution)
         (let bot_14 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_13 bot_14)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (app get_uv_11_vec2_to_vec2_120 coord_15)
         (let m_17 (app get_uv_11_vec2_to_vec2_120 u_mouse)
          (let d_18 (app scene_7 p_16)
           (let col_19 (if (> d_18 0.) (vec3 0.9 0.6 0.3) (vec3 0.65 0.85 1.))
            (let col_20
             (let darken_21 (- 1. (exp (* -6. (abs d_18))))
              (let rings_22 (+ 0.8 (* 0.2 (cos (* 150. d_18))))
               (* (* col_19 darken_21) rings_22)))
             (let col_23
              (mix col_20 (vec3 1. 1. 1.) (- 1. (smoothstep 0. 0.01 (abs d_18))))
              (let col_24
               (let d_25 (abs (app scene_7 m_17))
                (let dm_26 (length (- p_16 m_17))
                 (let d_27 (min (- (abs (- dm_26 d_25)) 0.0025) (- dm_26 0.015))
                  (mix col_23 (vec3 1. 1. 0.) (- 1. (smoothstep 0. 0.005 d_27))))))
               col_24)))))))))
      : ((vec 2) -> (vec 3))))

    === anf (2d_sdf_variants.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((TypeDef shape
       (VariantDecl ((Circle (float)) (Rect (float float)) (Empty ()))))
      : shape)
     ((Define Nonrec (name sdf_0) (args ((s_1 shape) (p_2 (vec 2))))
       (body
        (return
         (match s_1
          ((Circle r_3) (let anf_121 (length p_2) (return (- anf_121 r_3))))
          ((Rect w_4 h_5)
           (let anf_122 (index p_2 0)
            (let anf_123 (abs anf_122)
             (let anf_124 (- anf_123 w_4)
              (let anf_125 (index p_2 1)
               (let anf_126 (abs anf_125)
                (let anf_127 (- anf_126 h_5)
                 (let d_6 (vec2 anf_124 anf_127)
                  (let anf_128 (vec2 0. 0.)
                   (let anf_129 (max d_6 anf_128)
                    (let anf_130 (length anf_129)
                     (let anf_131 (index d_6 0)
                      (let anf_132 (index d_6 1)
                       (let anf_133 (max anf_131 anf_132)
                        (let anf_134 (min anf_133 0.)
                         (return (+ anf_130 anf_134)))))))))))))))))
          ((Empty) (return 1.))))))
      : (shape -> ((vec 2) -> float)))
     ((Define Nonrec (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let anf_135 (Variant shape Circle 0.3)
         (let circle_9 (sdf_0 anf_135 p_8)
          (let anf_136 (Variant shape Rect 0.7 0.1)
           (let rect_10 (sdf_0 anf_136 p_8) (return (min circle_9 rect_10))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name get_uv_11_vec2_to_vec2_120)
       (args ((coord_12 (vec 2))))
       (body
        (let anf_137 (* 2. coord_12)
         (let top_13 (- anf_137 u_resolution)
          (let anf_138 (index u_resolution 0)
           (let anf_139 (index u_resolution 1)
            (let bot_14 (min anf_138 anf_139) (return (/ top_13 bot_14)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (get_uv_11_vec2_to_vec2_120 coord_15)
         (let m_17 (get_uv_11_vec2_to_vec2_120 u_mouse)
          (let d_18 (scene_7 p_16)
           (let anf_140 (> d_18 0.)
            (let col_19
             (if anf_140 (return (vec3 0.9 0.6 0.3))
              (return (vec3 0.65 0.85 1.)))
             (let anf_141 (abs d_18)
              (let anf_142 (* -6. anf_141)
               (let anf_143 (exp anf_142)
                (let darken_21 (- 1. anf_143)
                 (let anf_144 (* 150. d_18)
                  (let anf_145 (cos anf_144)
                   (let anf_146 (* 0.2 anf_145)
                    (let rings_22 (+ 0.8 anf_146)
                     (let anf_147 (* col_19 darken_21)
                      (let col_20 (* anf_147 rings_22)
                       (let anf_148 (vec3 1. 1. 1.)
                        (let anf_149 (abs d_18)
                         (let anf_150 (smoothstep 0. 0.01 anf_149)
                          (let anf_151 (- 1. anf_150)
                           (let col_23 (mix col_20 anf_148 anf_151)
                            (let anf_152 (scene_7 m_17)
                             (let d_25 (abs anf_152)
                              (let anf_153 (- p_16 m_17)
                               (let dm_26 (length anf_153)
                                (let anf_154 (- dm_26 d_25)
                                 (let anf_155 (abs anf_154)
                                  (let anf_156 (- anf_155 0.0025)
                                   (let anf_157 (- dm_26 0.015)
                                    (let d_27 (min anf_156 anf_157)
                                     (let anf_158 (vec3 1. 1. 0.)
                                      (let anf_159 (smoothstep 0. 0.005 d_27)
                                       (let anf_160 (- 1. anf_159)
                                        (let col_24 (mix col_23 anf_158 anf_160)
                                         (return col_24))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (2d_sdf_variants.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((TypeDef shape
       (VariantDecl ((Circle (float)) (Rect (float float)) (Empty ()))))
      : shape)
     ((Define (name sdf_0) (args ((s_1 shape) (p_2 (vec 2))))
       (body
        (return
         (match s_1
          ((Circle r_3) (let anf_121 (length p_2) (return (- anf_121 r_3))))
          ((Rect w_4 h_5)
           (let anf_122 (index p_2 0)
            (let anf_123 (abs anf_122)
             (let anf_124 (- anf_123 w_4)
              (let anf_125 (index p_2 1)
               (let anf_126 (abs anf_125)
                (let anf_127 (- anf_126 h_5)
                 (let d_6 (vec2 anf_124 anf_127)
                  (let anf_128 (vec2 0. 0.)
                   (let anf_129 (max d_6 anf_128)
                    (let anf_130 (length anf_129)
                     (let anf_131 (index d_6 0)
                      (let anf_132 (index d_6 1)
                       (let anf_133 (max anf_131 anf_132)
                        (let anf_134 (min anf_133 0.)
                         (return (+ anf_130 anf_134)))))))))))))))))
          ((Empty) (return 1.))))))
      : (shape -> ((vec 2) -> float)))
     ((Define (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let anf_135 (Variant shape Circle 0.3)
         (let circle_9 (sdf_0 anf_135 p_8)
          (let anf_136 (Variant shape Rect 0.7 0.1)
           (let rect_10 (sdf_0 anf_136 p_8) (return (min circle_9 rect_10))))))))
      : ((vec 2) -> float))
     ((Define (name get_uv_11_vec2_to_vec2_120) (args ((coord_12 (vec 2))))
       (body
        (let anf_137 (* 2. coord_12)
         (let top_13 (- anf_137 u_resolution)
          (let anf_138 (index u_resolution 0)
           (let anf_139 (index u_resolution 1)
            (let bot_14 (min anf_138 anf_139) (return (/ top_13 bot_14)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (get_uv_11_vec2_to_vec2_120 coord_15)
         (let m_17 (get_uv_11_vec2_to_vec2_120 u_mouse)
          (let d_18 (scene_7 p_16)
           (let anf_140 (> d_18 0.)
            (let col_19
             (if anf_140 (return (vec3 0.9 0.6 0.3))
              (return (vec3 0.65 0.85 1.)))
             (let anf_141 (abs d_18)
              (let anf_142 (* -6. anf_141)
               (let anf_143 (exp anf_142)
                (let darken_21 (- 1. anf_143)
                 (let anf_144 (* 150. d_18)
                  (let anf_145 (cos anf_144)
                   (let anf_146 (* 0.2 anf_145)
                    (let rings_22 (+ 0.8 anf_146)
                     (let anf_147 (* col_19 darken_21)
                      (let col_20 (* anf_147 rings_22)
                       (let anf_148 (vec3 1. 1. 1.)
                        (let anf_149 (abs d_18)
                         (let anf_150 (smoothstep 0. 0.01 anf_149)
                          (let anf_151 (- 1. anf_150)
                           (let col_23 (mix col_20 anf_148 anf_151)
                            (let anf_152 (scene_7 m_17)
                             (let d_25 (abs anf_152)
                              (let anf_153 (- p_16 m_17)
                               (let dm_26 (length anf_153)
                                (let anf_154 (- dm_26 d_25)
                                 (let anf_155 (abs anf_154)
                                  (let anf_156 (- anf_155 0.0025)
                                   (let anf_157 (- dm_26 0.015)
                                    (let d_27 (min anf_156 anf_157)
                                     (let anf_158 (vec3 1. 1. 0.)
                                      (let anf_159 (smoothstep 0. 0.005 d_27)
                                       (let anf_160 (- 1. anf_159)
                                        (let col_24 (mix col_23 anf_158 anf_160)
                                         (return col_24))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (2d_sdf_variants.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((TypeDef shape
       (RecordDecl ((tag int) (Circle_0 float) (Rect_0 float) (Rect_1 float))))
      : shape)
     ((Define (name sdf_0) (args ((s_1 shape) (p_2 (vec 2))))
       (body
        (let _lv_tag_161 (. s_1 tag)
         (return
          (switch _lv_tag_161
           (0
            (let r_3 (. s_1 Circle_0)
             (let anf_121 (length p_2) (return (- anf_121 r_3)))))
           (1
            (let w_4 (. s_1 Rect_0)
             (let h_5 (. s_1 Rect_1)
              (let anf_122 (index p_2 0)
               (let anf_123 (abs anf_122)
                (let anf_124 (- anf_123 w_4)
                 (let anf_125 (index p_2 1)
                  (let anf_126 (abs anf_125)
                   (let anf_127 (- anf_126 h_5)
                    (let d_6 (vec2 anf_124 anf_127)
                     (let anf_128 (vec2 0. 0.)
                      (let anf_129 (max d_6 anf_128)
                       (let anf_130 (length anf_129)
                        (let anf_131 (index d_6 0)
                         (let anf_132 (index d_6 1)
                          (let anf_133 (max anf_131 anf_132)
                           (let anf_134 (min anf_133 0.)
                            (return (+ anf_130 anf_134)))))))))))))))))))
           (default (return 1.)))))))
      : (shape -> ((vec 2) -> float)))
     ((Define (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let anf_135 (shape 0 0.3 0. 0.)
         (let circle_9 (sdf_0 anf_135 p_8)
          (let anf_136 (shape 1 0. 0.7 0.1)
           (let rect_10 (sdf_0 anf_136 p_8) (return (min circle_9 rect_10))))))))
      : ((vec 2) -> float))
     ((Define (name get_uv_11_vec2_to_vec2_120) (args ((coord_12 (vec 2))))
       (body
        (let anf_137 (* 2. coord_12)
         (let top_13 (- anf_137 u_resolution)
          (let anf_138 (index u_resolution 0)
           (let anf_139 (index u_resolution 1)
            (let bot_14 (min anf_138 anf_139) (return (/ top_13 bot_14)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (get_uv_11_vec2_to_vec2_120 coord_15)
         (let m_17 (get_uv_11_vec2_to_vec2_120 u_mouse)
          (let d_18 (scene_7 p_16)
           (let anf_140 (> d_18 0.)
            (let col_19
             (if anf_140 (return (vec3 0.9 0.6 0.3))
              (return (vec3 0.65 0.85 1.)))
             (let anf_141 (abs d_18)
              (let anf_142 (* -6. anf_141)
               (let anf_143 (exp anf_142)
                (let darken_21 (- 1. anf_143)
                 (let anf_144 (* 150. d_18)
                  (let anf_145 (cos anf_144)
                   (let anf_146 (* 0.2 anf_145)
                    (let rings_22 (+ 0.8 anf_146)
                     (let anf_147 (* col_19 darken_21)
                      (let col_20 (* anf_147 rings_22)
                       (let anf_148 (vec3 1. 1. 1.)
                        (let anf_149 (abs d_18)
                         (let anf_150 (smoothstep 0. 0.01 anf_149)
                          (let anf_151 (- 1. anf_150)
                           (let col_23 (mix col_20 anf_148 anf_151)
                            (let anf_152 (scene_7 m_17)
                             (let d_25 (abs anf_152)
                              (let anf_153 (- p_16 m_17)
                               (let dm_26 (length anf_153)
                                (let anf_154 (- dm_26 d_25)
                                 (let anf_155 (abs anf_154)
                                  (let anf_156 (- anf_155 0.0025)
                                   (let anf_157 (- dm_26 0.015)
                                    (let d_27 (min anf_156 anf_157)
                                     (let anf_158 (vec3 1. 1. 0.)
                                      (let anf_159 (smoothstep 0. 0.005 d_27)
                                       (let anf_160 (- 1. anf_159)
                                        (let col_24 (mix col_23 anf_158 anf_160)
                                         (return col_24))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (2d_sdf_variants.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((TypeDef shape
       (RecordDecl ((tag int) (Circle_0 float) (Rect_0 float) (Rect_1 float))))
      : shape)
     ((Define (name sdf_0) (args ((s_1 shape) (p_2 (vec 2))))
       (body
        (let _lv_tag_161 (. s_1 tag)
         (return
          (switch _lv_tag_161
           (0
            (let r_3 (. s_1 Circle_0)
             (let anf_121 (length p_2) (return (- anf_121 r_3)))))
           (1
            (let w_4 (. s_1 Rect_0)
             (let h_5 (. s_1 Rect_1)
              (let anf_122 (index p_2 0)
               (let anf_123 (abs anf_122)
                (let anf_124 (- anf_123 w_4)
                 (let anf_125 (index p_2 1)
                  (let anf_126 (abs anf_125)
                   (let anf_127 (- anf_126 h_5)
                    (let d_6 (vec2 anf_124 anf_127)
                     (let anf_128 (vec2 0. 0.)
                      (let anf_129 (max d_6 anf_128)
                       (let anf_130 (length anf_129)
                        (let anf_131 (index d_6 0)
                         (let anf_132 (index d_6 1)
                          (let anf_133 (max anf_131 anf_132)
                           (let anf_134 (min anf_133 0.)
                            (return (+ anf_130 anf_134)))))))))))))))))))
           (default (return 1.)))))))
      : (shape -> ((vec 2) -> float)))
     ((Define (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let anf_135 (shape 0 0.3 0. 0.)
         (let circle_9 (sdf_0 anf_135 p_8)
          (let anf_136 (shape 1 0. 0.7 0.1)
           (let rect_10 (sdf_0 anf_136 p_8) (return (min circle_9 rect_10))))))))
      : ((vec 2) -> float))
     ((Define (name get_uv_11_vec2_to_vec2_120) (args ((coord_12 (vec 2))))
       (body
        (let anf_137 (* 2. coord_12)
         (let top_13 (- anf_137 u_resolution)
          (let anf_138 (index u_resolution 0)
           (let anf_139 (index u_resolution 1)
            (let bot_14 (min anf_138 anf_139) (return (/ top_13 bot_14)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (get_uv_11_vec2_to_vec2_120 coord_15)
         (let m_17 (get_uv_11_vec2_to_vec2_120 u_mouse)
          (let d_18 (scene_7 p_16)
           (let anf_140 (> d_18 0.)
            (let col_19
             (if anf_140 (return (vec3 0.9 0.6 0.3))
              (return (vec3 0.65 0.85 1.)))
             (let anf_141 (abs d_18)
              (let anf_142 (* -6. anf_141)
               (let anf_143 (exp anf_142)
                (let darken_21 (- 1. anf_143)
                 (let anf_144 (* 150. d_18)
                  (let anf_145 (cos anf_144)
                   (let anf_146 (* 0.2 anf_145)
                    (let rings_22 (+ 0.8 anf_146)
                     (let anf_147 (* col_19 darken_21)
                      (let col_20 (* anf_147 rings_22)
                       (let anf_148 (vec3 1. 1. 1.)
                        (let anf_149 (abs d_18)
                         (let anf_150 (smoothstep 0. 0.01 anf_149)
                          (let anf_151 (- 1. anf_150)
                           (let col_23 (mix col_20 anf_148 anf_151)
                            (let anf_152 (scene_7 m_17)
                             (let d_25 (abs anf_152)
                              (let anf_153 (- p_16 m_17)
                               (let dm_26 (length anf_153)
                                (let anf_154 (- dm_26 d_25)
                                 (let anf_155 (abs anf_154)
                                  (let anf_156 (- anf_155 0.0025)
                                   (let anf_157 (- dm_26 0.015)
                                    (let d_27 (min anf_156 anf_157)
                                     (let anf_158 (vec3 1. 1. 0.)
                                      (let anf_159 (smoothstep 0. 0.005 d_27)
                                       (let anf_160 (- 1. anf_159)
                                        (let col_24 (mix col_23 anf_158 anf_160)
                                         (return col_24))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (2d_sdf_variants.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform (TyVec 2) u_mouse ()) (Global Uniform TyFloat u_time ())
      (Struct shape
       ((TyInt tag) (TyFloat Circle_0) (TyFloat Rect_0) (TyFloat Rect_1)))
      (Function (name sdf_0) (desc ())
       (params (((TyStruct shape) s_1) ((TyVec 2) p_2))) (ret_type TyFloat)
       (body
        ((set () int _lv_tag_161 (. s_1 tag))
         (switch _lv_tag_161
          (0 (set () float r_3 (. s_1 Circle_0))
           (set () float anf_121 (length p_2)) (return (- anf_121 r_3)) break)
          (1 (set () float w_4 (. s_1 Rect_0)) (set () float h_5 (. s_1 Rect_1))
           (set () float anf_122 (index p_2 0))
           (set () float anf_123 (abs anf_122))
           (set () float anf_124 (- anf_123 w_4))
           (set () float anf_125 (index p_2 1))
           (set () float anf_126 (abs anf_125))
           (set () float anf_127 (- anf_126 h_5))
           (set () vec2 d_6 (vec2 anf_124 anf_127))
           (set () vec2 anf_128 (vec2 0. 0.))
           (set () vec2 anf_129 (max d_6 anf_128))
           (set () float anf_130 (length anf_129))
           (set () float anf_131 (index d_6 0))
           (set () float anf_132 (index d_6 1))
           (set () float anf_133 (max anf_131 anf_132))
           (set () float anf_134 (min anf_133 0.)) (return (+ anf_130 anf_134))
           break)
          (default (return 1.) break)))))
      (Function (name scene_7) (desc ()) (params (((TyVec 2) p_8)))
       (ret_type TyFloat)
       (body
        ((set () shape anf_135 (shape 0 0.3 0. 0.))
         (set () float circle_9 (sdf_0 anf_135 p_8))
         (set () shape anf_136 (shape 1 0. 0.7 0.1))
         (set () float rect_10 (sdf_0 anf_136 p_8))
         (return (min circle_9 rect_10)))))
      (Function (name get_uv_11_vec2_to_vec2_120) (desc ())
       (params (((TyVec 2) coord_12))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_137 (* 2. coord_12))
         (set () vec2 top_13 (- anf_137 u_resolution))
         (set () float anf_138 (index u_resolution 0))
         (set () float anf_139 (index u_resolution 1))
         (set () float bot_14 (min anf_138 anf_139)) (return (/ top_13 bot_14)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_15)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 p_16 (get_uv_11_vec2_to_vec2_120 coord_15))
         (set () vec2 m_17 (get_uv_11_vec2_to_vec2_120 u_mouse))
         (set () float d_18 (scene_7 p_16)) (set () bool anf_140 (> d_18 0.))
         (set () vec3 col_19 (vec3 0.))
         (if anf_140 (Block (set col_19 (vec3 0.9 0.6 0.3)))
          (Block (set col_19 (vec3 0.65 0.85 1.))))
         (set () float anf_141 (abs d_18)) (set () float anf_142 (* -6. anf_141))
         (set () float anf_143 (exp anf_142))
         (set () float darken_21 (- 1. anf_143))
         (set () float anf_144 (* 150. d_18))
         (set () float anf_145 (cos anf_144))
         (set () float anf_146 (* 0.2 anf_145))
         (set () float rings_22 (+ 0.8 anf_146))
         (set () vec3 anf_147 (* col_19 darken_21))
         (set () vec3 col_20 (* anf_147 rings_22))
         (set () vec3 anf_148 (vec3 1. 1. 1.)) (set () float anf_149 (abs d_18))
         (set () float anf_150 (smoothstep 0. 0.01 anf_149))
         (set () float anf_151 (- 1. anf_150))
         (set () vec3 col_23 (mix col_20 anf_148 anf_151))
         (set () float anf_152 (scene_7 m_17)) (set () float d_25 (abs anf_152))
         (set () vec2 anf_153 (- p_16 m_17))
         (set () float dm_26 (length anf_153))
         (set () float anf_154 (- dm_26 d_25))
         (set () float anf_155 (abs anf_154))
         (set () float anf_156 (- anf_155 0.0025))
         (set () float anf_157 (- dm_26 0.015))
         (set () float d_27 (min anf_156 anf_157))
         (set () vec3 anf_158 (vec3 1. 1. 0.))
         (set () float anf_159 (smoothstep 0. 0.005 d_27))
         (set () float anf_160 (- 1. anf_159))
         (set () vec3 col_24 (mix col_23 anf_158 anf_160)) (return col_24))))))

    === patch main (2d_sdf_variants.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform (TyVec 2) u_mouse ()) (Global Uniform TyFloat u_time ())
      (Struct shape
       ((TyInt tag) (TyFloat Circle_0) (TyFloat Rect_0) (TyFloat Rect_1)))
      (Function (name sdf_0) (desc ())
       (params (((TyStruct shape) s_1) ((TyVec 2) p_2))) (ret_type TyFloat)
       (body
        ((set () int _lv_tag_161 (. s_1 tag))
         (switch _lv_tag_161
          (0 (set () float r_3 (. s_1 Circle_0))
           (set () float anf_121 (length p_2)) (return (- anf_121 r_3)) break)
          (1 (set () float w_4 (. s_1 Rect_0)) (set () float h_5 (. s_1 Rect_1))
           (set () float anf_122 (index p_2 0))
           (set () float anf_123 (abs anf_122))
           (set () float anf_124 (- anf_123 w_4))
           (set () float anf_125 (index p_2 1))
           (set () float anf_126 (abs anf_125))
           (set () float anf_127 (- anf_126 h_5))
           (set () vec2 d_6 (vec2 anf_124 anf_127))
           (set () vec2 anf_128 (vec2 0. 0.))
           (set () vec2 anf_129 (max d_6 anf_128))
           (set () float anf_130 (length anf_129))
           (set () float anf_131 (index d_6 0))
           (set () float anf_132 (index d_6 1))
           (set () float anf_133 (max anf_131 anf_132))
           (set () float anf_134 (min anf_133 0.)) (return (+ anf_130 anf_134))
           break)
          (default (return 1.) break)))))
      (Function (name scene_7) (desc ()) (params (((TyVec 2) p_8)))
       (ret_type TyFloat)
       (body
        ((set () shape anf_135 (shape 0 0.3 0. 0.))
         (set () float circle_9 (sdf_0 anf_135 p_8))
         (set () shape anf_136 (shape 1 0. 0.7 0.1))
         (set () float rect_10 (sdf_0 anf_136 p_8))
         (return (min circle_9 rect_10)))))
      (Function (name get_uv_11_vec2_to_vec2_120) (desc ())
       (params (((TyVec 2) coord_12))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_137 (* 2. coord_12))
         (set () vec2 top_13 (- anf_137 u_resolution))
         (set () float anf_138 (index u_resolution 0))
         (set () float anf_139 (index u_resolution 1))
         (set () float bot_14 (min anf_138 anf_139)) (return (/ top_13 bot_14)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_15)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 p_16 (get_uv_11_vec2_to_vec2_120 coord_15))
         (set () vec2 m_17 (get_uv_11_vec2_to_vec2_120 u_mouse))
         (set () float d_18 (scene_7 p_16)) (set () bool anf_140 (> d_18 0.))
         (set () vec3 col_19 (vec3 0.))
         (if anf_140 (Block (set col_19 (vec3 0.9 0.6 0.3)))
          (Block (set col_19 (vec3 0.65 0.85 1.))))
         (set () float anf_141 (abs d_18)) (set () float anf_142 (* -6. anf_141))
         (set () float anf_143 (exp anf_142))
         (set () float darken_21 (- 1. anf_143))
         (set () float anf_144 (* 150. d_18))
         (set () float anf_145 (cos anf_144))
         (set () float anf_146 (* 0.2 anf_145))
         (set () float rings_22 (+ 0.8 anf_146))
         (set () vec3 anf_147 (* col_19 darken_21))
         (set () vec3 col_20 (* anf_147 rings_22))
         (set () vec3 anf_148 (vec3 1. 1. 1.)) (set () float anf_149 (abs d_18))
         (set () float anf_150 (smoothstep 0. 0.01 anf_149))
         (set () float anf_151 (- 1. anf_150))
         (set () vec3 col_23 (mix col_20 anf_148 anf_151))
         (set () float anf_152 (scene_7 m_17)) (set () float d_25 (abs anf_152))
         (set () vec2 anf_153 (- p_16 m_17))
         (set () float dm_26 (length anf_153))
         (set () float anf_154 (- dm_26 d_25))
         (set () float anf_155 (abs anf_154))
         (set () float anf_156 (- anf_155 0.0025))
         (set () float anf_157 (- dm_26 0.015))
         (set () float d_27 (min anf_156 anf_157))
         (set () vec3 anf_158 (vec3 1. 1. 0.))
         (set () float anf_159 (smoothstep 0. 0.005 d_27))
         (set () float anf_160 (- 1. anf_159))
         (set () vec3 col_24 (mix col_23 anf_158 anf_160)) (return col_24))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE checkerboard.glml ======

    === stlc (checkerboard.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv
       (lambda (coord ())
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
       (lambda (coord_1 ())
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
        ((lambda coord_1
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_11)) : 'v_12)
              (u_resolution : (vec 2)))
             : 'v_13)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_13) (bot_3 : float)) : 'v_18))
             : 'v_18))
           : 'v_18))
         : ('v_11 -> 'v_18)))
       :
       (forall
        ((Broadcast 'v_12 (vec 2) 'v_13) (MulBroadcast float 'v_11 'v_12)
         (MulBroadcast 'v_13 float 'v_18))
        ('v_11 -> 'v_18)))
      ((Define Nonrec main
        ((lambda coord_4
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

    === specialize params (checkerboard.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda coord_1
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_11)) : 'v_12)
              (u_resolution : (vec 2)))
             : 'v_13)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_13) (bot_3 : float)) : 'v_18))
             : 'v_18))
           : 'v_18))
         : ('v_11 -> 'v_18)))
       :
       (forall
        ((Broadcast 'v_12 (vec 2) 'v_13) (MulBroadcast float 'v_11 'v_12)
         (MulBroadcast 'v_13 float 'v_18))
        ('v_11 -> 'v_18)))
      ((Define Nonrec main
        ((lambda coord_4
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_42
        ((lambda coord_1
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
        ((lambda coord_4
          ((let uv_5
            ((app (get_uv_0_vec2_to_vec2_42 : ((vec 2) -> (vec 2)))
              (coord_4 : (vec 2)))
             : (vec 2))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_42
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0_vec2_to_vec2_42 coord_4)
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
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0_vec2_to_vec2_42 coord_4)
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
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let anf_43 (* 2. coord_1)
         (let top_2 (- anf_43 u_resolution)
          (let anf_44 (index u_resolution 0)
           (let anf_45 (index u_resolution 1)
            (let bot_3 (min anf_44 anf_45) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_42 coord_4)
         (let size_6 5.
          (let anf_46 (index uv_5 0)
           (let anf_47 (* anf_46 size_6)
            (let anf_48 (* u_time 2.)
             (let anf_49 (+ anf_47 anf_48)
              (let cx_7 (floor anf_49)
               (let anf_50 (index uv_5 1)
                (let anf_51 (* anf_50 size_6)
                 (let cy_8 (floor anf_51)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_52 (/ checker_sum_9 2.)
                    (let anf_53 (floor anf_52)
                     (let anf_54 (* anf_53 2.)
                      (let is_even_10 (- checker_sum_9 anf_54)
                       (let anf_55 (< is_even_10 0.5)
                        (return
                         (if anf_55 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let anf_43 (* 2. coord_1)
         (let top_2 (- anf_43 u_resolution)
          (let anf_44 (index u_resolution 0)
           (let anf_45 (index u_resolution 1)
            (let bot_3 (min anf_44 anf_45) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_42 coord_4)
         (let size_6 5.
          (let anf_46 (index uv_5 0)
           (let anf_47 (* anf_46 size_6)
            (let anf_48 (* u_time 2.)
             (let anf_49 (+ anf_47 anf_48)
              (let cx_7 (floor anf_49)
               (let anf_50 (index uv_5 1)
                (let anf_51 (* anf_50 size_6)
                 (let cy_8 (floor anf_51)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_52 (/ checker_sum_9 2.)
                    (let anf_53 (floor anf_52)
                     (let anf_54 (* anf_53 2.)
                      (let is_even_10 (- checker_sum_9 anf_54)
                       (let anf_55 (< is_even_10 0.5)
                        (return
                         (if anf_55 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let anf_43 (* 2. coord_1)
         (let top_2 (- anf_43 u_resolution)
          (let anf_44 (index u_resolution 0)
           (let anf_45 (index u_resolution 1)
            (let bot_3 (min anf_44 anf_45) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_42 coord_4)
         (let size_6 5.
          (let anf_46 (index uv_5 0)
           (let anf_47 (* anf_46 size_6)
            (let anf_48 (* u_time 2.)
             (let anf_49 (+ anf_47 anf_48)
              (let cx_7 (floor anf_49)
               (let anf_50 (index uv_5 1)
                (let anf_51 (* anf_50 size_6)
                 (let cy_8 (floor anf_51)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_52 (/ checker_sum_9 2.)
                    (let anf_53 (floor anf_52)
                     (let anf_54 (* anf_53 2.)
                      (let is_even_10 (- checker_sum_9 anf_54)
                       (let anf_55 (< is_even_10 0.5)
                        (return
                         (if anf_55 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let anf_43 (* 2. coord_1)
         (let top_2 (- anf_43 u_resolution)
          (let anf_44 (index u_resolution 0)
           (let anf_45 (index u_resolution 1)
            (let bot_3 (min anf_44 anf_45) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_42 coord_4)
         (let size_6 5.
          (let anf_46 (index uv_5 0)
           (let anf_47 (* anf_46 size_6)
            (let anf_48 (* u_time 2.)
             (let anf_49 (+ anf_47 anf_48)
              (let cx_7 (floor anf_49)
               (let anf_50 (index uv_5 1)
                (let anf_51 (* anf_50 size_6)
                 (let cy_8 (floor anf_51)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_52 (/ checker_sum_9 2.)
                    (let anf_53 (floor anf_52)
                     (let anf_54 (* anf_53 2.)
                      (let is_even_10 (- checker_sum_9 anf_54)
                       (let anf_55 (< is_even_10 0.5)
                        (return
                         (if anf_55 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (checkerboard.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_42) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_43 (* 2. coord_1))
         (set () vec2 top_2 (- anf_43 u_resolution))
         (set () float anf_44 (index u_resolution 0))
         (set () float anf_45 (index u_resolution 1))
         (set () float bot_3 (min anf_44 anf_45)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_42 coord_4))
         (set () float size_6 5.) (set () float anf_46 (index uv_5 0))
         (set () float anf_47 (* anf_46 size_6))
         (set () float anf_48 (* u_time 2.))
         (set () float anf_49 (+ anf_47 anf_48))
         (set () float cx_7 (floor anf_49)) (set () float anf_50 (index uv_5 1))
         (set () float anf_51 (* anf_50 size_6))
         (set () float cy_8 (floor anf_51))
         (set () float checker_sum_9 (+ cx_7 cy_8))
         (set () float anf_52 (/ checker_sum_9 2.))
         (set () float anf_53 (floor anf_52)) (set () float anf_54 (* anf_53 2.))
         (set () float is_even_10 (- checker_sum_9 anf_54))
         (set () bool anf_55 (< is_even_10 0.5))
         (if anf_55 (Block (return (vec3 0.2 0.2 0.2)))
          (Block (return (vec3 0.8 0.8 0.8)))))))))

    === patch main (checkerboard.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_42) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_43 (* 2. coord_1))
         (set () vec2 top_2 (- anf_43 u_resolution))
         (set () float anf_44 (index u_resolution 0))
         (set () float anf_45 (index u_resolution 1))
         (set () float bot_3 (min anf_44 anf_45)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_42 coord_4))
         (set () float size_6 5.) (set () float anf_46 (index uv_5 0))
         (set () float anf_47 (* anf_46 size_6))
         (set () float anf_48 (* u_time 2.))
         (set () float anf_49 (+ anf_47 anf_48))
         (set () float cx_7 (floor anf_49)) (set () float anf_50 (index uv_5 1))
         (set () float anf_51 (* anf_50 size_6))
         (set () float cy_8 (floor anf_51))
         (set () float checker_sum_9 (+ cx_7 cy_8))
         (set () float anf_52 (/ checker_sum_9 2.))
         (set () float anf_53 (floor anf_52)) (set () float anf_54 (* anf_53 2.))
         (set () float is_even_10 (- checker_sum_9 anf_54))
         (set () bool anf_55 (< is_even_10 0.5))
         (if anf_55 (Block (return (vec3 0.2 0.2 0.2)))
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
       (lambda (coord ())
        (let top (- (* 2. coord) u_resolution)
         (let bot (min (index u_resolution 0) (index u_resolution 1))
          (/ top bot)))))
      (Define (Rec 1000) mandel
       (lambda (zx ())
        (lambda (zy ())
         (lambda (cx ())
          (lambda (cy ())
           (lambda (i ())
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
               (+ (* (sin (+ (* n (vec3 10. 20. 30.)) u_time)) 0.5) 0.5))))))))))))

    === uniquify (mandelbrot.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv_0
       (lambda (coord_1 ())
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      (Define (Rec 1000) mandel_4
       (lambda (zx_5 ())
        (lambda (zy_6 ())
         (lambda (cx_7 ())
          (lambda (cy_8 ())
           (lambda (i_9 ())
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
               (+ (* (sin (+ (* n_18 (vec3 10. 20. 30.)) u_time)) 0.5) 0.5))))))))))))

    === typecheck (mandelbrot.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda coord_1
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_19)) : 'v_20)
              (u_resolution : (vec 2)))
             : 'v_21)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_21) (bot_3 : float)) : 'v_26))
             : 'v_26))
           : 'v_26))
         : ('v_19 -> 'v_26)))
       :
       (forall
        ((Broadcast 'v_20 (vec 2) 'v_21) (MulBroadcast float 'v_19 'v_20)
         (MulBroadcast 'v_21 float 'v_26))
        ('v_19 -> 'v_26)))
      ((Define (Rec 1000) mandel_4
        ((lambda zx_5
          ((lambda zy_6
            ((lambda cx_7
              ((lambda cy_8
                ((lambda i_9
                  ((if
                    ((||
                      ((>
                        ((length
                          ((vec2 (zx_5 : 'v_43) (zy_6 : 'v_46)) : (vec 2)))
                         : float)
                        (2. : float))
                       : bool)
                      ((> (i_9 : 'v_51) (150. : float)) : bool))
                     : bool)
                    (i_9 : 'v_51)
                    ((let next_zx_10
                      ((+
                        ((- ((* (zx_5 : 'v_43) (zx_5 : 'v_43)) : 'v_43)
                          ((* (zy_6 : 'v_46) (zy_6 : 'v_46)) : 'v_46))
                         : 'v_42)
                        (cx_7 : 'v_30))
                       : 'v_43)
                      ((let next_zy_11
                        ((+
                          ((* ((* (2. : float) (zx_5 : 'v_43)) : 'v_44)
                            (zy_6 : 'v_46))
                           : 'v_45)
                          (cy_8 : 'v_31))
                         : 'v_46)
                        ((app
                          ((app
                            ((app
                              ((app
                                ((app
                                  (mandel_4 :
                                   ('v_43 ->
                                    ('v_46 ->
                                     ('v_30 -> ('v_31 -> ('v_51 -> 'v_51))))))
                                  (next_zx_10 : 'v_43))
                                 :
                                 ('v_46 ->
                                  ('v_30 -> ('v_31 -> ('v_51 -> 'v_51)))))
                                (next_zy_11 : 'v_46))
                               : ('v_30 -> ('v_31 -> ('v_51 -> 'v_51))))
                              (cx_7 : 'v_30))
                             : ('v_31 -> ('v_51 -> 'v_51)))
                            (cy_8 : 'v_31))
                           : ('v_51 -> 'v_51))
                          ((+ (i_9 : 'v_51) (1. : float)) : 'v_51))
                         : 'v_51))
                       : 'v_51))
                     : 'v_51))
                   : 'v_51))
                 : ('v_51 -> 'v_51)))
               : ('v_31 -> ('v_51 -> 'v_51))))
             : ('v_30 -> ('v_31 -> ('v_51 -> 'v_51)))))
           : ('v_46 -> ('v_30 -> ('v_31 -> ('v_51 -> 'v_51))))))
         : ('v_43 -> ('v_46 -> ('v_30 -> ('v_31 -> ('v_51 -> 'v_51)))))))
       :
       (forall
        ((Comparable 'v_46) (Comparable 'v_43) (Broadcast 'v_51 float 'v_38)
         (Comparable 'v_38) (Broadcast 'v_42 'v_30 'v_43)
         (Broadcast 'v_43 'v_46 'v_42) (MulBroadcast 'v_43 'v_43 'v_43)
         (MulBroadcast 'v_46 'v_46 'v_46) (Broadcast 'v_45 'v_31 'v_46)
         (MulBroadcast 'v_44 'v_46 'v_45) (MulBroadcast float 'v_43 'v_44)
         (Broadcast 'v_51 float 'v_51))
        ('v_43 -> ('v_46 -> ('v_30 -> ('v_31 -> ('v_51 -> 'v_51)))))))
      ((Define Nonrec main
        ((lambda coord_12
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
                        ((+
                          ((*
                            ((sin
                              ((+
                                ((* (n_18 : float)
                                  ((vec3 (10. : float) (20. : float)
                                    (30. : float))
                                   : (vec 3)))
                                 : (vec 3))
                                (u_time : float))
                               : (vec 3)))
                             : (vec 3))
                            (0.5 : float))
                           : (vec 3))
                          (0.5 : float))
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

    === specialize params (mandelbrot.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda coord_1
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_19)) : 'v_20)
              (u_resolution : (vec 2)))
             : 'v_21)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_21) (bot_3 : float)) : 'v_26))
             : 'v_26))
           : 'v_26))
         : ('v_19 -> 'v_26)))
       :
       (forall
        ((Broadcast 'v_20 (vec 2) 'v_21) (MulBroadcast float 'v_19 'v_20)
         (MulBroadcast 'v_21 float 'v_26))
        ('v_19 -> 'v_26)))
      ((Define (Rec 1000) mandel_4
        ((lambda zx_5
          ((lambda zy_6
            ((lambda cx_7
              ((lambda cy_8
                ((lambda i_9
                  ((if
                    ((||
                      ((>
                        ((length
                          ((vec2 (zx_5 : 'v_43) (zy_6 : 'v_46)) : (vec 2)))
                         : float)
                        (2. : float))
                       : bool)
                      ((> (i_9 : 'v_51) (150. : float)) : bool))
                     : bool)
                    (i_9 : 'v_51)
                    ((let next_zx_10
                      ((+
                        ((- ((* (zx_5 : 'v_43) (zx_5 : 'v_43)) : 'v_43)
                          ((* (zy_6 : 'v_46) (zy_6 : 'v_46)) : 'v_46))
                         : 'v_42)
                        (cx_7 : 'v_30))
                       : 'v_43)
                      ((let next_zy_11
                        ((+
                          ((* ((* (2. : float) (zx_5 : 'v_43)) : 'v_44)
                            (zy_6 : 'v_46))
                           : 'v_45)
                          (cy_8 : 'v_31))
                         : 'v_46)
                        ((app
                          ((app
                            ((app
                              ((app
                                ((app
                                  (mandel_4 :
                                   ('v_43 ->
                                    ('v_46 ->
                                     ('v_30 -> ('v_31 -> ('v_51 -> 'v_51))))))
                                  (next_zx_10 : 'v_43))
                                 :
                                 ('v_46 ->
                                  ('v_30 -> ('v_31 -> ('v_51 -> 'v_51)))))
                                (next_zy_11 : 'v_46))
                               : ('v_30 -> ('v_31 -> ('v_51 -> 'v_51))))
                              (cx_7 : 'v_30))
                             : ('v_31 -> ('v_51 -> 'v_51)))
                            (cy_8 : 'v_31))
                           : ('v_51 -> 'v_51))
                          ((+ (i_9 : 'v_51) (1. : float)) : 'v_51))
                         : 'v_51))
                       : 'v_51))
                     : 'v_51))
                   : 'v_51))
                 : ('v_51 -> 'v_51)))
               : ('v_31 -> ('v_51 -> 'v_51))))
             : ('v_30 -> ('v_31 -> ('v_51 -> 'v_51)))))
           : ('v_46 -> ('v_30 -> ('v_31 -> ('v_51 -> 'v_51))))))
         : ('v_43 -> ('v_46 -> ('v_30 -> ('v_31 -> ('v_51 -> 'v_51)))))))
       :
       (forall
        ((Comparable 'v_46) (Comparable 'v_43) (Broadcast 'v_51 float 'v_38)
         (Comparable 'v_38) (Broadcast 'v_42 'v_30 'v_43)
         (Broadcast 'v_43 'v_46 'v_42) (MulBroadcast 'v_43 'v_43 'v_43)
         (MulBroadcast 'v_46 'v_46 'v_46) (Broadcast 'v_45 'v_31 'v_46)
         (MulBroadcast 'v_44 'v_46 'v_45) (MulBroadcast float 'v_43 'v_44)
         (Broadcast 'v_51 float 'v_51))
        ('v_43 -> ('v_46 -> ('v_30 -> ('v_31 -> ('v_51 -> 'v_51)))))))
      ((Define Nonrec main
        ((lambda coord_12
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
                        ((+
                          ((*
                            ((sin
                              ((+
                                ((* (n_18 : float)
                                  ((vec3 (10. : float) (20. : float)
                                    (30. : float))
                                   : (vec 3)))
                                 : (vec 3))
                                (u_time : float))
                               : (vec 3)))
                             : (vec 3))
                            (0.5 : float))
                           : (vec 3))
                          (0.5 : float))
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
      ((Define (Rec 1000)
        mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
        ((lambda zx_5
          ((lambda zy_6
            ((lambda cx_7
              ((lambda cy_8
                ((lambda i_9
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
                                  (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
                                   :
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_95
        ((lambda coord_1
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
        ((lambda coord_12
          ((let uv_13
            ((app (get_uv_0_vec2_to_vec2_95 : ((vec 2) -> (vec 2)))
              (coord_12 : (vec 2)))
             : (vec 2))
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
                              (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
                               :
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
                        ((+
                          ((*
                            ((sin
                              ((+
                                ((* (n_18 : float)
                                  ((vec3 (10. : float) (20. : float)
                                    (30. : float))
                                   : (vec 3)))
                                 : (vec 3))
                                (u_time : float))
                               : (vec 3)))
                             : (vec 3))
                            (0.5 : float))
                           : (vec 3))
                          (0.5 : float))
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
      ((Define (Rec 1000)
        mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
        (lambda ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float))
         (if (|| (> (length (vec2 zx_5 zy_6)) 2.) (> i_9 150.)) i_9
          (let next_zx_10 (+ (- (* zx_5 zx_5) (* zy_6 zy_6)) cx_7)
           (let next_zy_11 (+ (* (* 2. zx_5) zy_6) cy_8)
            (app mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
             next_zx_10 next_zy_11 cx_7 cy_8 (+ i_9 1.)))))))
       : (float -> (float -> (float -> (float -> (float -> float))))))
      ((Define Nonrec get_uv_0_vec2_to_vec2_95
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_12 (vec 2)))
         (let uv_13 (app get_uv_0_vec2_to_vec2_95 coord_12)
          (let zoom_14 (exp (+ (* (sin (* u_time 0.4)) 4.5) 3.5))
           (let cx_15 (+ -0.7453 (/ (index uv_13 0) zoom_14))
            (let cy_16 (+ 0.1127 (/ (index uv_13 1) zoom_14))
             (let iter_17
              (app mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
               0. 0. cx_15 cy_16 0.)
              (if (> iter_17 149.) (vec3 0. 0. 0.)
               (let n_18 (/ iter_17 150.)
                (+ (* (sin (+ (* n_18 (vec3 10. 20. 30.)) u_time)) 0.5) 0.5))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (Rec 1000)
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_94)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (if (|| (> (length (vec2 zx_5 zy_6)) 2.) (> i_9 150.)) i_9
         (let next_zx_10 (+ (- (* zx_5 zx_5) (* zy_6 zy_6)) cx_7)
          (let next_zy_11 (+ (* (* 2. zx_5) zy_6) cy_8)
           (app mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
            next_zx_10 next_zy_11 cx_7 cy_8 (+ i_9 1.)))))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_95) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (app get_uv_0_vec2_to_vec2_95 coord_12)
         (let zoom_14 (exp (+ (* (sin (* u_time 0.4)) 4.5) 3.5))
          (let cx_15 (+ -0.7453 (/ (index uv_13 0) zoom_14))
           (let cy_16 (+ 0.1127 (/ (index uv_13 1) zoom_14))
            (let iter_17
             (app mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
              0. 0. cx_15 cy_16 0.)
             (if (> iter_17 149.) (vec3 0. 0. 0.)
              (let n_18 (/ iter_17 150.)
               (+ (* (sin (+ (* n_18 (vec3 10. 20. 30.)) u_time)) 0.5) 0.5))))))))))
      : ((vec 2) -> (vec 3))))

    === anf (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (Rec 1000)
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_94)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let anf_96 (vec2 zx_5 zy_6)
         (let anf_97 (length anf_96)
          (let anf_98 (> anf_97 2.)
           (let anf_99 (> i_9 150.)
            (let anf_100 (|| anf_98 anf_99)
             (return
              (if anf_100 (return i_9)
               (let anf_101 (* zx_5 zx_5)
                (let anf_102 (* zy_6 zy_6)
                 (let anf_103 (- anf_101 anf_102)
                  (let next_zx_10 (+ anf_103 cx_7)
                   (let anf_104 (* 2. zx_5)
                    (let anf_105 (* anf_104 zy_6)
                     (let next_zy_11 (+ anf_105 cy_8)
                      (let anf_106 (+ i_9 1.)
                       (return
                        (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
                         next_zx_10 next_zy_11 cx_7 cy_8 anf_106)))))))))))))))))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_95) (args ((coord_1 (vec 2))))
       (body
        (let anf_107 (* 2. coord_1)
         (let top_2 (- anf_107 u_resolution)
          (let anf_108 (index u_resolution 0)
           (let anf_109 (index u_resolution 1)
            (let bot_3 (min anf_108 anf_109) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0_vec2_to_vec2_95 coord_12)
         (let anf_110 (* u_time 0.4)
          (let anf_111 (sin anf_110)
           (let anf_112 (* anf_111 4.5)
            (let anf_113 (+ anf_112 3.5)
             (let zoom_14 (exp anf_113)
              (let anf_114 (index uv_13 0)
               (let anf_115 (/ anf_114 zoom_14)
                (let cx_15 (+ -0.7453 anf_115)
                 (let anf_116 (index uv_13 1)
                  (let anf_117 (/ anf_116 zoom_14)
                   (let cy_16 (+ 0.1127 anf_117)
                    (let iter_17
                     (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
                      0. 0. cx_15 cy_16 0.)
                     (let anf_118 (> iter_17 149.)
                      (return
                       (if anf_118 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_119 (vec3 10. 20. 30.)
                          (let anf_120 (* n_18 anf_119)
                           (let anf_121 (+ anf_120 u_time)
                            (let anf_122 (sin anf_121)
                             (let anf_123 (* anf_122 0.5)
                              (return (+ anf_123 0.5))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_94)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let _iter_124 0
         (while (< _iter_124 1000)
          (let anf_96 (vec2 zx_5 zy_6)
           (let anf_97 (length anf_96)
            (let anf_98 (> anf_97 2.)
             (let anf_99 (> i_9 150.)
              (let anf_100 (|| anf_98 anf_99)
               (return
                (if anf_100 (return i_9)
                 (let anf_101 (* zx_5 zx_5)
                  (let anf_102 (* zy_6 zy_6)
                   (let anf_103 (- anf_101 anf_102)
                    (let next_zx_10 (+ anf_103 cx_7)
                     (let anf_104 (* 2. zx_5)
                      (let anf_105 (* anf_104 zy_6)
                       (let next_zy_11 (+ anf_105 cy_8)
                        (let anf_106 (+ i_9 1.)
                         (set zx_5 next_zx_10
                          (set zy_6 next_zy_11
                           (set cx_7 cx_7
                            (set cy_8 cy_8
                             (set i_9 anf_106
                              (let _iter_inc_125 (+ _iter_124 1)
                               (set _iter_124 _iter_inc_125 continue))))))))))))))))))))))
          (return 0.)))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define (name get_uv_0_vec2_to_vec2_95) (args ((coord_1 (vec 2))))
       (body
        (let anf_107 (* 2. coord_1)
         (let top_2 (- anf_107 u_resolution)
          (let anf_108 (index u_resolution 0)
           (let anf_109 (index u_resolution 1)
            (let bot_3 (min anf_108 anf_109) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0_vec2_to_vec2_95 coord_12)
         (let anf_110 (* u_time 0.4)
          (let anf_111 (sin anf_110)
           (let anf_112 (* anf_111 4.5)
            (let anf_113 (+ anf_112 3.5)
             (let zoom_14 (exp anf_113)
              (let anf_114 (index uv_13 0)
               (let anf_115 (/ anf_114 zoom_14)
                (let cx_15 (+ -0.7453 anf_115)
                 (let anf_116 (index uv_13 1)
                  (let anf_117 (/ anf_116 zoom_14)
                   (let cy_16 (+ 0.1127 anf_117)
                    (let iter_17
                     (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
                      0. 0. cx_15 cy_16 0.)
                     (let anf_118 (> iter_17 149.)
                      (return
                       (if anf_118 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_119 (vec3 10. 20. 30.)
                          (let anf_120 (* n_18 anf_119)
                           (let anf_121 (+ anf_120 u_time)
                            (let anf_122 (sin anf_121)
                             (let anf_123 (* anf_122 0.5)
                              (return (+ anf_123 0.5))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_94)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let _iter_124 0
         (while (< _iter_124 1000)
          (let anf_96 (vec2 zx_5 zy_6)
           (let anf_97 (length anf_96)
            (let anf_98 (> anf_97 2.)
             (let anf_99 (> i_9 150.)
              (let anf_100 (|| anf_98 anf_99)
               (return
                (if anf_100 (return i_9)
                 (let anf_101 (* zx_5 zx_5)
                  (let anf_102 (* zy_6 zy_6)
                   (let anf_103 (- anf_101 anf_102)
                    (let next_zx_10 (+ anf_103 cx_7)
                     (let anf_104 (* 2. zx_5)
                      (let anf_105 (* anf_104 zy_6)
                       (let next_zy_11 (+ anf_105 cy_8)
                        (let anf_106 (+ i_9 1.)
                         (set zx_5 next_zx_10
                          (set zy_6 next_zy_11
                           (set cx_7 cx_7
                            (set cy_8 cy_8
                             (set i_9 anf_106
                              (let _iter_inc_125 (+ _iter_124 1)
                               (set _iter_124 _iter_inc_125 continue))))))))))))))))))))))
          (return 0.)))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define (name get_uv_0_vec2_to_vec2_95) (args ((coord_1 (vec 2))))
       (body
        (let anf_107 (* 2. coord_1)
         (let top_2 (- anf_107 u_resolution)
          (let anf_108 (index u_resolution 0)
           (let anf_109 (index u_resolution 1)
            (let bot_3 (min anf_108 anf_109) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0_vec2_to_vec2_95 coord_12)
         (let anf_110 (* u_time 0.4)
          (let anf_111 (sin anf_110)
           (let anf_112 (* anf_111 4.5)
            (let anf_113 (+ anf_112 3.5)
             (let zoom_14 (exp anf_113)
              (let anf_114 (index uv_13 0)
               (let anf_115 (/ anf_114 zoom_14)
                (let cx_15 (+ -0.7453 anf_115)
                 (let anf_116 (index uv_13 1)
                  (let anf_117 (/ anf_116 zoom_14)
                   (let cy_16 (+ 0.1127 anf_117)
                    (let iter_17
                     (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
                      0. 0. cx_15 cy_16 0.)
                     (let anf_118 (> iter_17 149.)
                      (return
                       (if anf_118 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_119 (vec3 10. 20. 30.)
                          (let anf_120 (* n_18 anf_119)
                           (let anf_121 (+ anf_120 u_time)
                            (let anf_122 (sin anf_121)
                             (let anf_123 (* anf_122 0.5)
                              (return (+ anf_123 0.5))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_94)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let _iter_124 0
         (while (< _iter_124 1000)
          (let anf_96 (vec2 zx_5 zy_6)
           (let anf_97 (length anf_96)
            (let anf_98 (> anf_97 2.)
             (let anf_99 (> i_9 150.)
              (let anf_100 (|| anf_98 anf_99)
               (return
                (if anf_100 (return i_9)
                 (let anf_101 (* zx_5 zx_5)
                  (let anf_102 (* zy_6 zy_6)
                   (let anf_103 (- anf_101 anf_102)
                    (let next_zx_10 (+ anf_103 cx_7)
                     (let anf_104 (* 2. zx_5)
                      (let anf_105 (* anf_104 zy_6)
                       (let next_zy_11 (+ anf_105 cy_8)
                        (let anf_106 (+ i_9 1.)
                         (set zx_5 next_zx_10
                          (set zy_6 next_zy_11
                           (set cx_7 cx_7
                            (set cy_8 cy_8
                             (set i_9 anf_106
                              (let _iter_inc_125 (+ _iter_124 1)
                               (set _iter_124 _iter_inc_125 continue))))))))))))))))))))))
          (return 0.)))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define (name get_uv_0_vec2_to_vec2_95) (args ((coord_1 (vec 2))))
       (body
        (let anf_107 (* 2. coord_1)
         (let top_2 (- anf_107 u_resolution)
          (let anf_108 (index u_resolution 0)
           (let anf_109 (index u_resolution 1)
            (let bot_3 (min anf_108 anf_109) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0_vec2_to_vec2_95 coord_12)
         (let anf_110 (* u_time 0.4)
          (let anf_111 (sin anf_110)
           (let anf_112 (* anf_111 4.5)
            (let anf_113 (+ anf_112 3.5)
             (let zoom_14 (exp anf_113)
              (let anf_114 (index uv_13 0)
               (let anf_115 (/ anf_114 zoom_14)
                (let cx_15 (+ -0.7453 anf_115)
                 (let anf_116 (index uv_13 1)
                  (let anf_117 (/ anf_116 zoom_14)
                   (let cy_16 (+ 0.1127 anf_117)
                    (let iter_17
                     (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94
                      0. 0. cx_15 cy_16 0.)
                     (let anf_118 (> iter_17 149.)
                      (return
                       (if anf_118 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_119 (vec3 10. 20. 30.)
                          (let anf_120 (* n_18 anf_119)
                           (let anf_121 (+ anf_120 u_time)
                            (let anf_122 (sin anf_121)
                             (let anf_123 (* anf_122 0.5)
                              (return (+ anf_123 0.5))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (mandelbrot.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_94)
       (desc ())
       (params
        ((TyFloat zx_5) (TyFloat zy_6) (TyFloat cx_7) (TyFloat cy_8)
         (TyFloat i_9)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_124 0)
         (while (< _iter_124 1000)
          (Block (set () vec2 anf_96 (vec2 zx_5 zy_6))
           (set () float anf_97 (length anf_96))
           (set () bool anf_98 (> anf_97 2.)) (set () bool anf_99 (> i_9 150.))
           (set () bool anf_100 (|| anf_98 anf_99))
           (if anf_100 (Block (return i_9))
            (Block (set () float anf_101 (* zx_5 zx_5))
             (set () float anf_102 (* zy_6 zy_6))
             (set () float anf_103 (- anf_101 anf_102))
             (set () float next_zx_10 (+ anf_103 cx_7))
             (set () float anf_104 (* 2. zx_5))
             (set () float anf_105 (* anf_104 zy_6))
             (set () float next_zy_11 (+ anf_105 cy_8))
             (set () float anf_106 (+ i_9 1.)) (set zx_5 next_zx_10)
             (set zy_6 next_zy_11) (set cx_7 cx_7) (set cy_8 cy_8)
             (set i_9 anf_106) (set () int _iter_inc_125 (+ _iter_124 1))
             (set _iter_124 _iter_inc_125) continue))))
         (return 0.))))
      (Function (name get_uv_0_vec2_to_vec2_95) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_107 (* 2. coord_1))
         (set () vec2 top_2 (- anf_107 u_resolution))
         (set () float anf_108 (index u_resolution 0))
         (set () float anf_109 (index u_resolution 1))
         (set () float bot_3 (min anf_108 anf_109)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_12)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_13 (get_uv_0_vec2_to_vec2_95 coord_12))
         (set () float anf_110 (* u_time 0.4))
         (set () float anf_111 (sin anf_110))
         (set () float anf_112 (* anf_111 4.5))
         (set () float anf_113 (+ anf_112 3.5))
         (set () float zoom_14 (exp anf_113))
         (set () float anf_114 (index uv_13 0))
         (set () float anf_115 (/ anf_114 zoom_14))
         (set () float cx_15 (+ -0.7453 anf_115))
         (set () float anf_116 (index uv_13 1))
         (set () float anf_117 (/ anf_116 zoom_14))
         (set () float cy_16 (+ 0.1127 anf_117))
         (set () float iter_17
          (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94 0. 0.
           cx_15 cy_16 0.))
         (set () bool anf_118 (> iter_17 149.))
         (if anf_118 (Block (return (vec3 0. 0. 0.)))
          (Block (set () float n_18 (/ iter_17 150.))
           (set () vec3 anf_119 (vec3 10. 20. 30.))
           (set () vec3 anf_120 (* n_18 anf_119))
           (set () vec3 anf_121 (+ anf_120 u_time))
           (set () vec3 anf_122 (sin anf_121))
           (set () vec3 anf_123 (* anf_122 0.5)) (return (+ anf_123 0.5)))))))))

    === patch main (mandelbrot.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_94)
       (desc ())
       (params
        ((TyFloat zx_5) (TyFloat zy_6) (TyFloat cx_7) (TyFloat cy_8)
         (TyFloat i_9)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_124 0)
         (while (< _iter_124 1000)
          (Block (set () vec2 anf_96 (vec2 zx_5 zy_6))
           (set () float anf_97 (length anf_96))
           (set () bool anf_98 (> anf_97 2.)) (set () bool anf_99 (> i_9 150.))
           (set () bool anf_100 (|| anf_98 anf_99))
           (if anf_100 (Block (return i_9))
            (Block (set () float anf_101 (* zx_5 zx_5))
             (set () float anf_102 (* zy_6 zy_6))
             (set () float anf_103 (- anf_101 anf_102))
             (set () float next_zx_10 (+ anf_103 cx_7))
             (set () float anf_104 (* 2. zx_5))
             (set () float anf_105 (* anf_104 zy_6))
             (set () float next_zy_11 (+ anf_105 cy_8))
             (set () float anf_106 (+ i_9 1.)) (set zx_5 next_zx_10)
             (set zy_6 next_zy_11) (set cx_7 cx_7) (set cy_8 cy_8)
             (set i_9 anf_106) (set () int _iter_inc_125 (+ _iter_124 1))
             (set _iter_124 _iter_inc_125) continue))))
         (return 0.))))
      (Function (name get_uv_0_vec2_to_vec2_95) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_107 (* 2. coord_1))
         (set () vec2 top_2 (- anf_107 u_resolution))
         (set () float anf_108 (index u_resolution 0))
         (set () float anf_109 (index u_resolution 1))
         (set () float bot_3 (min anf_108 anf_109)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_12)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_13 (get_uv_0_vec2_to_vec2_95 coord_12))
         (set () float anf_110 (* u_time 0.4))
         (set () float anf_111 (sin anf_110))
         (set () float anf_112 (* anf_111 4.5))
         (set () float anf_113 (+ anf_112 3.5))
         (set () float zoom_14 (exp anf_113))
         (set () float anf_114 (index uv_13 0))
         (set () float anf_115 (/ anf_114 zoom_14))
         (set () float cx_15 (+ -0.7453 anf_115))
         (set () float anf_116 (index uv_13 1))
         (set () float anf_117 (/ anf_116 zoom_14))
         (set () float cy_16 (+ 0.1127 anf_117))
         (set () float iter_17
          (mandel_4_float_to_float_to_float_to_float_to_float_to_float_94 0. 0.
           cx_15 cy_16 0.))
         (set () bool anf_118 (> iter_17 149.))
         (if anf_118 (Block (return (vec3 0. 0. 0.)))
          (Block (set () float n_18 (/ iter_17 150.))
           (set () vec3 anf_119 (vec3 10. 20. 30.))
           (set () vec3 anf_120 (* n_18 anf_119))
           (set () vec3 anf_121 (+ anf_120 u_time))
           (set () vec3 anf_122 (sin anf_121))
           (set () vec3 anf_123 (* anf_122 0.5)) (return (+ anf_123 0.5)))))))
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
       (lambda (coord ())
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
       (lambda (coord_1 ())
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
        ((lambda coord_1
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_8)) : 'v_9)
              (u_resolution : (vec 2)))
             : 'v_10)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_10) (bot_3 : float)) : 'v_15))
             : 'v_15))
           : 'v_15))
         : ('v_8 -> 'v_15)))
       :
       (forall
        ((Broadcast 'v_9 (vec 2) 'v_10) (MulBroadcast float 'v_8 'v_9)
         (MulBroadcast 'v_10 float 'v_15))
        ('v_8 -> 'v_15)))
      ((Define Nonrec main
        ((lambda coord_4
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

    === specialize params (mouse_circle.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
      ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda coord_1
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_8)) : 'v_9)
              (u_resolution : (vec 2)))
             : 'v_10)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_10) (bot_3 : float)) : 'v_15))
             : 'v_15))
           : 'v_15))
         : ('v_8 -> 'v_15)))
       :
       (forall
        ((Broadcast 'v_9 (vec 2) 'v_10) (MulBroadcast float 'v_8 'v_9)
         (MulBroadcast 'v_10 float 'v_15))
        ('v_8 -> 'v_15)))
      ((Define Nonrec main
        ((lambda coord_4
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_33
        ((lambda coord_1
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
        ((lambda coord_4
          ((let uv_5
            ((app (get_uv_0_vec2_to_vec2_33 : ((vec 2) -> (vec 2)))
              (coord_4 : (vec 2)))
             : (vec 2))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_33
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0_vec2_to_vec2_33 coord_4)
          (let mouseUV_6
           (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
           (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
            (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
             (vec3 0.5 0.5 1.)))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_33) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0_vec2_to_vec2_33 coord_4)
         (let mouseUV_6
          (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
          (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
           (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
            (vec3 0.5 0.5 1.)))))))
      : ((vec 2) -> (vec 3))))

    === anf (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_33) (args ((coord_1 (vec 2))))
       (body
        (let anf_34 (* 2. coord_1)
         (let top_2 (- anf_34 u_resolution)
          (let anf_35 (index u_resolution 0)
           (let anf_36 (index u_resolution 1)
            (let bot_3 (min anf_35 anf_36) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_33 coord_4)
         (let anf_37 (* 2. u_mouse)
          (let anf_38 (- anf_37 u_resolution)
           (let anf_39 (index u_resolution 1)
            (let mouseUV_6 (/ anf_38 anf_39)
             (let anf_40 (* u_time 2.)
              (let anf_41 (sin anf_40)
               (let anf_42 (* anf_41 0.1)
                (let radius_7 (+ anf_42 0.15)
                 (let anf_43 (distance uv_5 mouseUV_6)
                  (let anf_44 (< anf_43 radius_7)
                   (return
                    (if anf_44 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_33) (args ((coord_1 (vec 2))))
       (body
        (let anf_34 (* 2. coord_1)
         (let top_2 (- anf_34 u_resolution)
          (let anf_35 (index u_resolution 0)
           (let anf_36 (index u_resolution 1)
            (let bot_3 (min anf_35 anf_36) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_33 coord_4)
         (let anf_37 (* 2. u_mouse)
          (let anf_38 (- anf_37 u_resolution)
           (let anf_39 (index u_resolution 1)
            (let mouseUV_6 (/ anf_38 anf_39)
             (let anf_40 (* u_time 2.)
              (let anf_41 (sin anf_40)
               (let anf_42 (* anf_41 0.1)
                (let radius_7 (+ anf_42 0.15)
                 (let anf_43 (distance uv_5 mouseUV_6)
                  (let anf_44 (< anf_43 radius_7)
                   (return
                    (if anf_44 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_33) (args ((coord_1 (vec 2))))
       (body
        (let anf_34 (* 2. coord_1)
         (let top_2 (- anf_34 u_resolution)
          (let anf_35 (index u_resolution 0)
           (let anf_36 (index u_resolution 1)
            (let bot_3 (min anf_35 anf_36) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_33 coord_4)
         (let anf_37 (* 2. u_mouse)
          (let anf_38 (- anf_37 u_resolution)
           (let anf_39 (index u_resolution 1)
            (let mouseUV_6 (/ anf_38 anf_39)
             (let anf_40 (* u_time 2.)
              (let anf_41 (sin anf_40)
               (let anf_42 (* anf_41 0.1)
                (let radius_7 (+ anf_42 0.15)
                 (let anf_43 (distance uv_5 mouseUV_6)
                  (let anf_44 (< anf_43 radius_7)
                   (return
                    (if anf_44 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_33) (args ((coord_1 (vec 2))))
       (body
        (let anf_34 (* 2. coord_1)
         (let top_2 (- anf_34 u_resolution)
          (let anf_35 (index u_resolution 0)
           (let anf_36 (index u_resolution 1)
            (let bot_3 (min anf_35 anf_36) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_33 coord_4)
         (let anf_37 (* 2. u_mouse)
          (let anf_38 (- anf_37 u_resolution)
           (let anf_39 (index u_resolution 1)
            (let mouseUV_6 (/ anf_38 anf_39)
             (let anf_40 (* u_time 2.)
              (let anf_41 (sin anf_40)
               (let anf_42 (* anf_41 0.1)
                (let radius_7 (+ anf_42 0.15)
                 (let anf_43 (distance uv_5 mouseUV_6)
                  (let anf_44 (< anf_43 radius_7)
                   (return
                    (if anf_44 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (mouse_circle.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform (TyVec 2) u_mouse ()) (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_33) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_34 (* 2. coord_1))
         (set () vec2 top_2 (- anf_34 u_resolution))
         (set () float anf_35 (index u_resolution 0))
         (set () float anf_36 (index u_resolution 1))
         (set () float bot_3 (min anf_35 anf_36)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_33 coord_4))
         (set () vec2 anf_37 (* 2. u_mouse))
         (set () vec2 anf_38 (- anf_37 u_resolution))
         (set () float anf_39 (index u_resolution 1))
         (set () vec2 mouseUV_6 (/ anf_38 anf_39))
         (set () float anf_40 (* u_time 2.)) (set () float anf_41 (sin anf_40))
         (set () float anf_42 (* anf_41 0.1))
         (set () float radius_7 (+ anf_42 0.15))
         (set () float anf_43 (distance uv_5 mouseUV_6))
         (set () bool anf_44 (< anf_43 radius_7))
         (if anf_44 (Block (return (vec3 0. 0. 0.5)))
          (Block (return (vec3 0.5 0.5 1.)))))))))

    === patch main (mouse_circle.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform (TyVec 2) u_mouse ()) (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_33) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_34 (* 2. coord_1))
         (set () vec2 top_2 (- anf_34 u_resolution))
         (set () float anf_35 (index u_resolution 0))
         (set () float anf_36 (index u_resolution 1))
         (set () float bot_3 (min anf_35 anf_36)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_33 coord_4))
         (set () vec2 anf_37 (* 2. u_mouse))
         (set () vec2 anf_38 (- anf_37 u_resolution))
         (set () float anf_39 (index u_resolution 1))
         (set () vec2 mouseUV_6 (/ anf_38 anf_39))
         (set () float anf_40 (* u_time 2.)) (set () float anf_41 (sin anf_40))
         (set () float anf_42 (* anf_41 0.1))
         (set () float radius_7 (+ anf_42 0.15))
         (set () float anf_43 (distance uv_5 mouseUV_6))
         (set () bool anf_44 (< anf_43 radius_7))
         (if anf_44 (Block (return (vec3 0. 0. 0.5)))
          (Block (return (vec3 0.5 0.5 1.)))))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE planet.glml ======

    === stlc (planet.glml) ===
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
      (Define Nonrec noise3d
       (lambda (p ((vec 3)))
        (let i (floor p)
         (let f (fract p)
          (let u (* (* f f) (- 3. (* 2. f)))
           (let hash
            (lambda (p ((vec 3)))
             (let d (dot p (vec3 127.1 311.7 74.7))
              (fract (* (sin d) 43758.5453))))
            (let a (app hash i)
             (let b (app hash (+ i (vec3 1 0 0)))
              (let c (app hash (+ i (vec3 0 1 0)))
               (let d (app hash (+ i (vec3 1 1 0)))
                (let e (app hash (+ i (vec3 0 0 1)))
                 (let f (app hash (+ i (vec3 1 0 1)))
                  (let g (app hash (+ i (vec3 0 1 1)))
                   (let h (app hash (+ i (vec3 1 1 1)))
                    (let ab (mix a b (index u 0))
                     (let cd (mix c d (index u 0))
                      (let ef (mix e f (index u 0))
                       (let gh (mix g h (index u 0))
                        (let abcd (mix ab cd (index u 1))
                         (let efgh (mix ef gh (index u 1))
                          (mix abcd efgh (index u 2))))))))))))))))))))))
      (Define Nonrec fbm
       (lambda (p ((vec 3)))
        (+
         (+ (+ (* (app noise3d (* p 1.)) 0.5) (* (app noise3d (* p 2.)) 0.25))
          (* (app noise3d (* p 4.)) 0.125))
         (* (app noise3d (* p 8.)) 0.0625))))
      (Define Nonrec sdPlanet
       (lambda (p ((vec 3)))
        (lambda (radius (float))
         (let len (length p)
          (let dir (/ p len)
           (let terrain (* (app fbm (* dir 3.)) 0.4) (- (- len radius) terrain)))))))
      (Define Nonrec map (lambda (p ((vec 3))) (app (app sdPlanet p) 1.5)))
      (Define Nonrec getNormal
       (lambda (p ((vec 3)))
        (let e 0.002
         (let e_x (vec3 e 0 0)
          (let e_y (vec3 0 e 0)
           (let e_z (vec3 0 0 e)
            (let dx (- (app map (+ p e_x)) (app map (- p e_x)))
             (let dy (- (app map (+ p e_y)) (app map (- p e_y)))
              (let dz (- (app map (+ p e_z)) (app map (- p e_z)))
               (normalize (vec3 dx dy dz)))))))))))
      (Define Nonrec march
       (lambda (ro ((vec 3)))
        (lambda (rd ((vec 3)))
         (let (rec 1000) march
          (lambda (t (float))
           (lambda (steps (int))
            (if (> steps 120) t
             (let d (app map (+ ro (* rd t)))
              (if (< d 0.0005) t
               (if (> t 50.) 50.1 (app (app march (+ t (* d 0.8))) (+ steps 1))))))))
          (app (app march 0.) 0)))))
      (Define Nonrec main
       (lambda (coord ((vec 2)))
        (let res_min (min (index u_resolution 0) (index u_resolution 1))
         (let uv (/ (- (* coord 2.) u_resolution) res_min)
          (let mouseUV (/ (- (* u_mouse 2.) u_resolution) res_min)
           (let rotate_by_mouse
            (lambda (ray ())
             (let rotX (* (* -1 (index mouseUV 1)) 1.5)
              (let ro_yz
               (app (app rotate (vec2 (index ray 1) (index ray 2))) rotX)
               (let rotY (* (* -1 (index mouseUV 0)) 1.5)
                (let ro_xz
                 (app (app rotate (vec2 (index ray 0) (index ro_yz 1))) rotY)
                 (vec3 (index ro_xz 0) (index ro_yz 0) (index ro_xz 1)))))))
            (let ro (app rotate_by_mouse (vec3 0 0 -4))
             (let rd
              (app rotate_by_mouse
               (normalize (vec3 (index uv 0) (index uv 1) 1.5)))
              (let t (app (app march ro) rd)
               (if (> t 50.) (vec3 0 0 0)
                (let hitPos (+ ro (* rd t))
                 (let n (app getNormal hitPos)
                  (let lightDir (normalize (vec3 1. 0.8 -0.5))
                   (let diff (max (dot n lightDir) 0)
                    (let ambient 0.08
                     (let dir (/ hitPos (length hitPos))
                      (let rawHeight (app fbm (* dir 3.))
                       (let seaLevel 0.35
                        (let h_norm
                         (clamp (/ (- rawHeight seaLevel) (- 1. seaLevel)) 0. 1.)
                         (let deepColor (vec3 0.02 0.05 0.2)
                          (let landColor (vec3 0.15 0.35 0.1)
                           (let mountColor (vec3 0.4 0.3 0.2)
                            (let snowColor (vec3 0.85 0.85 0.9)
                             (let baseColor
                              (if (< h_norm 0.3)
                               (mix deepColor landColor (/ h_norm 0.3))
                               (if (< h_norm 0.6)
                                (mix landColor mountColor (/ (- h_norm 0.3) 0.3))
                                (mix mountColor snowColor (/ (- h_norm 0.6) 0.4))))
                              (let fresnel (- 1. (max (dot n (* rd -1.)) 0))
                               (let rim (* (* (* fresnel fresnel) fresnel) 0.4)
                                (let atmoColor (vec3 0.3 0.5 1.)
                                 (+ (* baseColor (+ (* diff 0.9) ambient))
                                  (* atmoColor rim)))))))))))))))))))))))))))))))

    === uniquify (planet.glml) ===
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
      (Define Nonrec noise3d_5
       (lambda (p_6 ((vec 3)))
        (let i_7 (floor p_6)
         (let f_8 (fract p_6)
          (let u_9 (* (* f_8 f_8) (- 3. (* 2. f_8)))
           (let hash_10
            (lambda (p_11 ((vec 3)))
             (let d_12 (dot p_11 (vec3 127.1 311.7 74.7))
              (fract (* (sin d_12) 43758.5453))))
            (let a_13 (app hash_10 i_7)
             (let b_14 (app hash_10 (+ i_7 (vec3 1 0 0)))
              (let c_15 (app hash_10 (+ i_7 (vec3 0 1 0)))
               (let d_16 (app hash_10 (+ i_7 (vec3 1 1 0)))
                (let e_17 (app hash_10 (+ i_7 (vec3 0 0 1)))
                 (let f_18 (app hash_10 (+ i_7 (vec3 1 0 1)))
                  (let g_19 (app hash_10 (+ i_7 (vec3 0 1 1)))
                   (let h_20 (app hash_10 (+ i_7 (vec3 1 1 1)))
                    (let ab_21 (mix a_13 b_14 (index u_9 0))
                     (let cd_22 (mix c_15 d_16 (index u_9 0))
                      (let ef_23 (mix e_17 f_18 (index u_9 0))
                       (let gh_24 (mix g_19 h_20 (index u_9 0))
                        (let abcd_25 (mix ab_21 cd_22 (index u_9 1))
                         (let efgh_26 (mix ef_23 gh_24 (index u_9 1))
                          (mix abcd_25 efgh_26 (index u_9 2))))))))))))))))))))))
      (Define Nonrec fbm_27
       (lambda (p_28 ((vec 3)))
        (+
         (+
          (+ (* (app noise3d_5 (* p_28 1.)) 0.5)
           (* (app noise3d_5 (* p_28 2.)) 0.25))
          (* (app noise3d_5 (* p_28 4.)) 0.125))
         (* (app noise3d_5 (* p_28 8.)) 0.0625))))
      (Define Nonrec sdPlanet_29
       (lambda (p_30 ((vec 3)))
        (lambda (radius_31 (float))
         (let len_32 (length p_30)
          (let dir_33 (/ p_30 len_32)
           (let terrain_34 (* (app fbm_27 (* dir_33 3.)) 0.4)
            (- (- len_32 radius_31) terrain_34)))))))
      (Define Nonrec map_35
       (lambda (p_36 ((vec 3))) (app (app sdPlanet_29 p_36) 1.5)))
      (Define Nonrec getNormal_37
       (lambda (p_38 ((vec 3)))
        (let e_39 0.002
         (let e_x_40 (vec3 e_39 0 0)
          (let e_y_41 (vec3 0 e_39 0)
           (let e_z_42 (vec3 0 0 e_39)
            (let dx_43
             (- (app map_35 (+ p_38 e_x_40)) (app map_35 (- p_38 e_x_40)))
             (let dy_44
              (- (app map_35 (+ p_38 e_y_41)) (app map_35 (- p_38 e_y_41)))
              (let dz_45
               (- (app map_35 (+ p_38 e_z_42)) (app map_35 (- p_38 e_z_42)))
               (normalize (vec3 dx_43 dy_44 dz_45)))))))))))
      (Define Nonrec march_46
       (lambda (ro_47 ((vec 3)))
        (lambda (rd_48 ((vec 3)))
         (let (rec 1000) march_49
          (lambda (t_50 (float))
           (lambda (steps_51 (int))
            (if (> steps_51 120) t_50
             (let d_52 (app map_35 (+ ro_47 (* rd_48 t_50)))
              (if (< d_52 0.0005) t_50
               (if (> t_50 50.) 50.1
                (app (app march_49 (+ t_50 (* d_52 0.8))) (+ steps_51 1))))))))
          (app (app march_49 0.) 0)))))
      (Define Nonrec main
       (lambda (coord_53 ((vec 2)))
        (let res_min_54 (min (index u_resolution 0) (index u_resolution 1))
         (let uv_55 (/ (- (* coord_53 2.) u_resolution) res_min_54)
          (let mouseUV_56 (/ (- (* u_mouse 2.) u_resolution) res_min_54)
           (let rotate_by_mouse_57
            (lambda (ray_58 ())
             (let rotX_59 (* (* -1 (index mouseUV_56 1)) 1.5)
              (let ro_yz_60
               (app (app rotate_0 (vec2 (index ray_58 1) (index ray_58 2)))
                rotX_59)
               (let rotY_61 (* (* -1 (index mouseUV_56 0)) 1.5)
                (let ro_xz_62
                 (app (app rotate_0 (vec2 (index ray_58 0) (index ro_yz_60 1)))
                  rotY_61)
                 (vec3 (index ro_xz_62 0) (index ro_yz_60 0) (index ro_xz_62 1)))))))
            (let ro_63 (app rotate_by_mouse_57 (vec3 0 0 -4))
             (let rd_64
              (app rotate_by_mouse_57
               (normalize (vec3 (index uv_55 0) (index uv_55 1) 1.5)))
              (let t_65 (app (app march_46 ro_63) rd_64)
               (if (> t_65 50.) (vec3 0 0 0)
                (let hitPos_66 (+ ro_63 (* rd_64 t_65))
                 (let n_67 (app getNormal_37 hitPos_66)
                  (let lightDir_68 (normalize (vec3 1. 0.8 -0.5))
                   (let diff_69 (max (dot n_67 lightDir_68) 0)
                    (let ambient_70 0.08
                     (let dir_71 (/ hitPos_66 (length hitPos_66))
                      (let rawHeight_72 (app fbm_27 (* dir_71 3.))
                       (let seaLevel_73 0.35
                        (let h_norm_74
                         (clamp
                          (/ (- rawHeight_72 seaLevel_73) (- 1. seaLevel_73)) 0.
                          1.)
                         (let deepColor_75 (vec3 0.02 0.05 0.2)
                          (let landColor_76 (vec3 0.15 0.35 0.1)
                           (let mountColor_77 (vec3 0.4 0.3 0.2)
                            (let snowColor_78 (vec3 0.85 0.85 0.9)
                             (let baseColor_79
                              (if (< h_norm_74 0.3)
                               (mix deepColor_75 landColor_76 (/ h_norm_74 0.3))
                               (if (< h_norm_74 0.6)
                                (mix landColor_76 mountColor_77
                                 (/ (- h_norm_74 0.3) 0.3))
                                (mix mountColor_77 snowColor_78
                                 (/ (- h_norm_74 0.6) 0.4))))
                              (let fresnel_80
                               (- 1. (max (dot n_67 (* rd_64 -1.)) 0))
                               (let rim_81
                                (* (* (* fresnel_80 fresnel_80) fresnel_80) 0.4)
                                (let atmoColor_82 (vec3 0.3 0.5 1.)
                                 (+
                                  (* baseColor_79 (+ (* diff_69 0.9) ambient_70))
                                  (* atmoColor_82 rim_81)))))))))))))))))))))))))))))))

    === typecheck (planet.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Extern u_mouse) : (vec 2))
      ((Define Nonrec rotate_0
        ((lambda p_1
          ((lambda angle_2
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
      ((Define Nonrec noise3d_5
        ((lambda p_6
          ((let i_7 ((floor (p_6 : (vec 3))) : (vec 3))
            ((let f_8 ((fract (p_6 : (vec 3))) : (vec 3))
              ((let u_9
                ((* ((* (f_8 : (vec 3)) (f_8 : (vec 3))) : (vec 3))
                  ((- (3. : float) ((* (2. : float) (f_8 : (vec 3))) : (vec 3)))
                   : (vec 3)))
                 : (vec 3))
                ((let hash_10
                  ((lambda p_11
                    ((let d_12
                      ((dot (p_11 : (vec 3))
                        ((vec3 (127.1 : float) (311.7 : float) (74.7 : float)) :
                         (vec 3)))
                       : float)
                      ((fract
                        ((* ((sin (d_12 : float)) : float) (43758.5453 : float))
                         : float))
                       : float))
                     : float))
                   : ((vec 3) -> float))
                  ((let a_13
                    ((app (hash_10 : ((vec 3) -> float)) (i_7 : (vec 3))) :
                     float)
                    ((let b_14
                      ((app (hash_10 : ((vec 3) -> float))
                        ((+ (i_7 : (vec 3))
                          ((vec3 (1 : int) (0 : int) (0 : int)) : (vec 3)))
                         : (vec 3)))
                       : float)
                      ((let c_15
                        ((app (hash_10 : ((vec 3) -> float))
                          ((+ (i_7 : (vec 3))
                            ((vec3 (0 : int) (1 : int) (0 : int)) : (vec 3)))
                           : (vec 3)))
                         : float)
                        ((let d_16
                          ((app (hash_10 : ((vec 3) -> float))
                            ((+ (i_7 : (vec 3))
                              ((vec3 (1 : int) (1 : int) (0 : int)) : (vec 3)))
                             : (vec 3)))
                           : float)
                          ((let e_17
                            ((app (hash_10 : ((vec 3) -> float))
                              ((+ (i_7 : (vec 3))
                                ((vec3 (0 : int) (0 : int) (1 : int)) : (vec 3)))
                               : (vec 3)))
                             : float)
                            ((let f_18
                              ((app (hash_10 : ((vec 3) -> float))
                                ((+ (i_7 : (vec 3))
                                  ((vec3 (1 : int) (0 : int) (1 : int)) :
                                   (vec 3)))
                                 : (vec 3)))
                               : float)
                              ((let g_19
                                ((app (hash_10 : ((vec 3) -> float))
                                  ((+ (i_7 : (vec 3))
                                    ((vec3 (0 : int) (1 : int) (1 : int)) :
                                     (vec 3)))
                                   : (vec 3)))
                                 : float)
                                ((let h_20
                                  ((app (hash_10 : ((vec 3) -> float))
                                    ((+ (i_7 : (vec 3))
                                      ((vec3 (1 : int) (1 : int) (1 : int)) :
                                       (vec 3)))
                                     : (vec 3)))
                                   : float)
                                  ((let ab_21
                                    ((mix (a_13 : float) (b_14 : float)
                                      ((index (u_9 : (vec 3)) 0) : float))
                                     : float)
                                    ((let cd_22
                                      ((mix (c_15 : float) (d_16 : float)
                                        ((index (u_9 : (vec 3)) 0) : float))
                                       : float)
                                      ((let ef_23
                                        ((mix (e_17 : float) (f_18 : float)
                                          ((index (u_9 : (vec 3)) 0) : float))
                                         : float)
                                        ((let gh_24
                                          ((mix (g_19 : float) (h_20 : float)
                                            ((index (u_9 : (vec 3)) 0) : float))
                                           : float)
                                          ((let abcd_25
                                            ((mix (ab_21 : float) (cd_22 : float)
                                              ((index (u_9 : (vec 3)) 1) : float))
                                             : float)
                                            ((let efgh_26
                                              ((mix (ef_23 : float)
                                                (gh_24 : float)
                                                ((index (u_9 : (vec 3)) 1) :
                                                 float))
                                               : float)
                                              ((mix (abcd_25 : float)
                                                (efgh_26 : float)
                                                ((index (u_9 : (vec 3)) 2) :
                                                 float))
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
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : float))
               : float))
             : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec fbm_27
        ((lambda p_28
          ((+
            ((+
              ((+
                ((*
                  ((app (noise3d_5 : ((vec 3) -> float))
                    ((* (p_28 : (vec 3)) (1. : float)) : (vec 3)))
                   : float)
                  (0.5 : float))
                 : float)
                ((*
                  ((app (noise3d_5 : ((vec 3) -> float))
                    ((* (p_28 : (vec 3)) (2. : float)) : (vec 3)))
                   : float)
                  (0.25 : float))
                 : float))
               : float)
              ((*
                ((app (noise3d_5 : ((vec 3) -> float))
                  ((* (p_28 : (vec 3)) (4. : float)) : (vec 3)))
                 : float)
                (0.125 : float))
               : float))
             : float)
            ((*
              ((app (noise3d_5 : ((vec 3) -> float))
                ((* (p_28 : (vec 3)) (8. : float)) : (vec 3)))
               : float)
              (0.0625 : float))
             : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec sdPlanet_29
        ((lambda p_30
          ((lambda radius_31
            ((let len_32 ((length (p_30 : (vec 3))) : float)
              ((let dir_33 ((/ (p_30 : (vec 3)) (len_32 : float)) : (vec 3))
                ((let terrain_34
                  ((*
                    ((app (fbm_27 : ((vec 3) -> float))
                      ((* (dir_33 : (vec 3)) (3. : float)) : (vec 3)))
                     : float)
                    (0.4 : float))
                   : float)
                  ((- ((- (len_32 : float) (radius_31 : float)) : float)
                    (terrain_34 : float))
                   : float))
                 : float))
               : float))
             : float))
           : (float -> float)))
         : ((vec 3) -> (float -> float))))
       : ((vec 3) -> (float -> float)))
      ((Define Nonrec map_35
        ((lambda p_36
          ((app
            ((app (sdPlanet_29 : ((vec 3) -> (float -> float))) (p_36 : (vec 3)))
             : (float -> float))
            (1.5 : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec getNormal_37
        ((lambda p_38
          ((let e_39 (0.002 : float)
            ((let e_x_40 ((vec3 (e_39 : float) (0 : int) (0 : int)) : (vec 3))
              ((let e_y_41 ((vec3 (0 : int) (e_39 : float) (0 : int)) : (vec 3))
                ((let e_z_42
                  ((vec3 (0 : int) (0 : int) (e_39 : float)) : (vec 3))
                  ((let dx_43
                    ((-
                      ((app (map_35 : ((vec 3) -> float))
                        ((+ (p_38 : (vec 3)) (e_x_40 : (vec 3))) : (vec 3)))
                       : float)
                      ((app (map_35 : ((vec 3) -> float))
                        ((- (p_38 : (vec 3)) (e_x_40 : (vec 3))) : (vec 3)))
                       : float))
                     : float)
                    ((let dy_44
                      ((-
                        ((app (map_35 : ((vec 3) -> float))
                          ((+ (p_38 : (vec 3)) (e_y_41 : (vec 3))) : (vec 3)))
                         : float)
                        ((app (map_35 : ((vec 3) -> float))
                          ((- (p_38 : (vec 3)) (e_y_41 : (vec 3))) : (vec 3)))
                         : float))
                       : float)
                      ((let dz_45
                        ((-
                          ((app (map_35 : ((vec 3) -> float))
                            ((+ (p_38 : (vec 3)) (e_z_42 : (vec 3))) : (vec 3)))
                           : float)
                          ((app (map_35 : ((vec 3) -> float))
                            ((- (p_38 : (vec 3)) (e_z_42 : (vec 3))) : (vec 3)))
                           : float))
                         : float)
                        ((normalize
                          ((vec3 (dx_43 : float) (dy_44 : float) (dz_45 : float))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 3) -> (vec 3))))
       : ((vec 3) -> (vec 3)))
      ((Define Nonrec march_46
        ((lambda ro_47
          ((lambda rd_48
            ((let (rec 1000) march_49
              ((lambda t_50
                ((lambda steps_51
                  ((if ((> (steps_51 : int) (120 : int)) : bool) (t_50 : float)
                    ((let d_52
                      ((app (map_35 : ((vec 3) -> float))
                        ((+ (ro_47 : (vec 3))
                          ((* (rd_48 : (vec 3)) (t_50 : float)) : (vec 3)))
                         : (vec 3)))
                       : float)
                      ((if ((< (d_52 : float) (0.0005 : float)) : bool)
                        (t_50 : float)
                        ((if ((> (t_50 : float) (50. : float)) : bool)
                          (50.1 : float)
                          ((app
                            ((app (march_49 : (float -> (int -> float)))
                              ((+ (t_50 : float)
                                ((* (d_52 : float) (0.8 : float)) : float))
                               : float))
                             : (int -> float))
                            ((+ (steps_51 : int) (1 : int)) : int))
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : (int -> float)))
               : (float -> (int -> float)))
              ((app
                ((app (march_49 : (float -> (int -> float))) (0. : float)) :
                 (int -> float))
                (0 : int))
               : float))
             : float))
           : ((vec 3) -> float)))
         : ((vec 3) -> ((vec 3) -> float))))
       : ((vec 3) -> ((vec 3) -> float)))
      ((Define Nonrec main
        ((lambda coord_53
          ((let res_min_54
            ((min ((index (u_resolution : (vec 2)) 0) : float)
              ((index (u_resolution : (vec 2)) 1) : float))
             : float)
            ((let uv_55
              ((/
                ((- ((* (coord_53 : (vec 2)) (2. : float)) : (vec 2))
                  (u_resolution : (vec 2)))
                 : (vec 2))
                (res_min_54 : float))
               : (vec 2))
              ((let mouseUV_56
                ((/
                  ((- ((* (u_mouse : (vec 2)) (2. : float)) : (vec 2))
                    (u_resolution : (vec 2)))
                   : (vec 2))
                  (res_min_54 : float))
                 : (vec 2))
                ((let rotate_by_mouse_57
                  ((lambda ray_58
                    ((let rotX_59
                      ((*
                        ((* (-1 : int)
                          ((index (mouseUV_56 : (vec 2)) 1) : float))
                         : float)
                        (1.5 : float))
                       : float)
                      ((let ro_yz_60
                        ((app
                          ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                            ((vec2 ((index (ray_58 : 'v_223) 1) : 'v_227)
                              ((index (ray_58 : 'v_223) 2) : 'v_228))
                             : (vec 2)))
                           : (float -> (vec 2)))
                          (rotX_59 : float))
                         : (vec 2))
                        ((let rotY_61
                          ((*
                            ((* (-1 : int)
                              ((index (mouseUV_56 : (vec 2)) 0) : float))
                             : float)
                            (1.5 : float))
                           : float)
                          ((let ro_xz_62
                            ((app
                              ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                                ((vec2 ((index (ray_58 : 'v_223) 0) : 'v_234)
                                  ((index (ro_yz_60 : (vec 2)) 1) : float))
                                 : (vec 2)))
                               : (float -> (vec 2)))
                              (rotY_61 : float))
                             : (vec 2))
                            ((vec3 ((index (ro_xz_62 : (vec 2)) 0) : float)
                              ((index (ro_yz_60 : (vec 2)) 0) : float)
                              ((index (ro_xz_62 : (vec 2)) 1) : float))
                             : (vec 3)))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   :
                   (forall
                    ((Comparable 'v_228) (IndexAccess 'v_223 2 'v_228)
                     (Comparable 'v_227) (IndexAccess 'v_223 1 'v_227)
                     (Comparable 'v_234) (IndexAccess 'v_223 0 'v_234))
                    ('v_223 -> (vec 3))))
                  ((let ro_63
                    ((app (rotate_by_mouse_57 : ((vec 3) -> (vec 3)))
                      ((vec3 (0 : int) (0 : int) (-4 : int)) : (vec 3)))
                     : (vec 3))
                    ((let rd_64
                      ((app (rotate_by_mouse_57 : ((vec 3) -> (vec 3)))
                        ((normalize
                          ((vec3 ((index (uv_55 : (vec 2)) 0) : float)
                            ((index (uv_55 : (vec 2)) 1) : float) (1.5 : float))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3))
                      ((let t_65
                        ((app
                          ((app (march_46 : ((vec 3) -> ((vec 3) -> float)))
                            (ro_63 : (vec 3)))
                           : ((vec 3) -> float))
                          (rd_64 : (vec 3)))
                         : float)
                        ((if ((> (t_65 : float) (50. : float)) : bool)
                          ((vec3 (0 : int) (0 : int) (0 : int)) : (vec 3))
                          ((let hitPos_66
                            ((+ (ro_63 : (vec 3))
                              ((* (rd_64 : (vec 3)) (t_65 : float)) : (vec 3)))
                             : (vec 3))
                            ((let n_67
                              ((app (getNormal_37 : ((vec 3) -> (vec 3)))
                                (hitPos_66 : (vec 3)))
                               : (vec 3))
                              ((let lightDir_68
                                ((normalize
                                  ((vec3 (1. : float) (0.8 : float)
                                    (-0.5 : float))
                                   : (vec 3)))
                                 : (vec 3))
                                ((let diff_69
                                  ((max
                                    ((dot (n_67 : (vec 3))
                                      (lightDir_68 : (vec 3)))
                                     : float)
                                    (0 : int))
                                   : float)
                                  ((let ambient_70 (0.08 : float)
                                    ((let dir_71
                                      ((/ (hitPos_66 : (vec 3))
                                        ((length (hitPos_66 : (vec 3))) : float))
                                       : (vec 3))
                                      ((let rawHeight_72
                                        ((app (fbm_27 : ((vec 3) -> float))
                                          ((* (dir_71 : (vec 3)) (3. : float)) :
                                           (vec 3)))
                                         : float)
                                        ((let seaLevel_73 (0.35 : float)
                                          ((let h_norm_74
                                            ((clamp
                                              ((/
                                                ((- (rawHeight_72 : float)
                                                  (seaLevel_73 : float))
                                                 : float)
                                                ((- (1. : float)
                                                  (seaLevel_73 : float))
                                                 : float))
                                               : float)
                                              (0. : float) (1. : float))
                                             : float)
                                            ((let deepColor_75
                                              ((vec3 (0.02 : float)
                                                (0.05 : float) (0.2 : float))
                                               : (vec 3))
                                              ((let landColor_76
                                                ((vec3 (0.15 : float)
                                                  (0.35 : float) (0.1 : float))
                                                 : (vec 3))
                                                ((let mountColor_77
                                                  ((vec3 (0.4 : float)
                                                    (0.3 : float) (0.2 : float))
                                                   : (vec 3))
                                                  ((let snowColor_78
                                                    ((vec3 (0.85 : float)
                                                      (0.85 : float)
                                                      (0.9 : float))
                                                     : (vec 3))
                                                    ((let baseColor_79
                                                      ((if
                                                        ((< (h_norm_74 : float)
                                                          (0.3 : float))
                                                         : bool)
                                                        ((mix
                                                          (deepColor_75 :
                                                           (vec 3))
                                                          (landColor_76 :
                                                           (vec 3))
                                                          ((/ (h_norm_74 : float)
                                                            (0.3 : float))
                                                           : float))
                                                         : (vec 3))
                                                        ((if
                                                          ((< (h_norm_74 : float)
                                                            (0.6 : float))
                                                           : bool)
                                                          ((mix
                                                            (landColor_76 :
                                                             (vec 3))
                                                            (mountColor_77 :
                                                             (vec 3))
                                                            ((/
                                                              ((-
                                                                (h_norm_74 :
                                                                 float)
                                                                (0.3 : float))
                                                               : float)
                                                              (0.3 : float))
                                                             : float))
                                                           : (vec 3))
                                                          ((mix
                                                            (mountColor_77 :
                                                             (vec 3))
                                                            (snowColor_78 :
                                                             (vec 3))
                                                            ((/
                                                              ((-
                                                                (h_norm_74 :
                                                                 float)
                                                                (0.6 : float))
                                                               : float)
                                                              (0.4 : float))
                                                             : float))
                                                           : (vec 3)))
                                                         : (vec 3)))
                                                       : (vec 3))
                                                      ((let fresnel_80
                                                        ((- (1. : float)
                                                          ((max
                                                            ((dot
                                                              (n_67 : (vec 3))
                                                              ((*
                                                                (rd_64 : (vec 3))
                                                                (-1. : float))
                                                               : (vec 3)))
                                                             : float)
                                                            (0 : int))
                                                           : float))
                                                         : float)
                                                        ((let rim_81
                                                          ((*
                                                            ((*
                                                              ((*
                                                                (fresnel_80 :
                                                                 float)
                                                                (fresnel_80 :
                                                                 float))
                                                               : float)
                                                              (fresnel_80 :
                                                               float))
                                                             : float)
                                                            (0.4 : float))
                                                           : float)
                                                          ((let atmoColor_82
                                                            ((vec3 (0.3 : float)
                                                              (0.5 : float)
                                                              (1. : float))
                                                             : (vec 3))
                                                            ((+
                                                              ((*
                                                                (baseColor_79 :
                                                                 (vec 3))
                                                                ((+
                                                                  ((*
                                                                    (diff_69 :
                                                                     float)
                                                                    (0.9 : float))
                                                                   : float)
                                                                  (ambient_70 :
                                                                   float))
                                                                 : float))
                                                               : (vec 3))
                                                              ((*
                                                                (atmoColor_82 :
                                                                 (vec 3))
                                                                (rim_81 : float))
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

    === specialize params (planet.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Extern u_mouse) : (vec 2))
      ((Define Nonrec rotate_0
        ((lambda p_1
          ((lambda angle_2
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
      ((Define Nonrec noise3d_5
        ((lambda p_6
          ((let i_7 ((floor (p_6 : (vec 3))) : (vec 3))
            ((let f_8 ((fract (p_6 : (vec 3))) : (vec 3))
              ((let u_9
                ((* ((* (f_8 : (vec 3)) (f_8 : (vec 3))) : (vec 3))
                  ((- (3. : float) ((* (2. : float) (f_8 : (vec 3))) : (vec 3)))
                   : (vec 3)))
                 : (vec 3))
                ((let hash_10
                  ((lambda p_11
                    ((let d_12
                      ((dot (p_11 : (vec 3))
                        ((vec3 (127.1 : float) (311.7 : float) (74.7 : float)) :
                         (vec 3)))
                       : float)
                      ((fract
                        ((* ((sin (d_12 : float)) : float) (43758.5453 : float))
                         : float))
                       : float))
                     : float))
                   : ((vec 3) -> float))
                  ((let a_13
                    ((app (hash_10 : ((vec 3) -> float)) (i_7 : (vec 3))) :
                     float)
                    ((let b_14
                      ((app (hash_10 : ((vec 3) -> float))
                        ((+ (i_7 : (vec 3))
                          ((vec3 (1 : int) (0 : int) (0 : int)) : (vec 3)))
                         : (vec 3)))
                       : float)
                      ((let c_15
                        ((app (hash_10 : ((vec 3) -> float))
                          ((+ (i_7 : (vec 3))
                            ((vec3 (0 : int) (1 : int) (0 : int)) : (vec 3)))
                           : (vec 3)))
                         : float)
                        ((let d_16
                          ((app (hash_10 : ((vec 3) -> float))
                            ((+ (i_7 : (vec 3))
                              ((vec3 (1 : int) (1 : int) (0 : int)) : (vec 3)))
                             : (vec 3)))
                           : float)
                          ((let e_17
                            ((app (hash_10 : ((vec 3) -> float))
                              ((+ (i_7 : (vec 3))
                                ((vec3 (0 : int) (0 : int) (1 : int)) : (vec 3)))
                               : (vec 3)))
                             : float)
                            ((let f_18
                              ((app (hash_10 : ((vec 3) -> float))
                                ((+ (i_7 : (vec 3))
                                  ((vec3 (1 : int) (0 : int) (1 : int)) :
                                   (vec 3)))
                                 : (vec 3)))
                               : float)
                              ((let g_19
                                ((app (hash_10 : ((vec 3) -> float))
                                  ((+ (i_7 : (vec 3))
                                    ((vec3 (0 : int) (1 : int) (1 : int)) :
                                     (vec 3)))
                                   : (vec 3)))
                                 : float)
                                ((let h_20
                                  ((app (hash_10 : ((vec 3) -> float))
                                    ((+ (i_7 : (vec 3))
                                      ((vec3 (1 : int) (1 : int) (1 : int)) :
                                       (vec 3)))
                                     : (vec 3)))
                                   : float)
                                  ((let ab_21
                                    ((mix (a_13 : float) (b_14 : float)
                                      ((index (u_9 : (vec 3)) 0) : float))
                                     : float)
                                    ((let cd_22
                                      ((mix (c_15 : float) (d_16 : float)
                                        ((index (u_9 : (vec 3)) 0) : float))
                                       : float)
                                      ((let ef_23
                                        ((mix (e_17 : float) (f_18 : float)
                                          ((index (u_9 : (vec 3)) 0) : float))
                                         : float)
                                        ((let gh_24
                                          ((mix (g_19 : float) (h_20 : float)
                                            ((index (u_9 : (vec 3)) 0) : float))
                                           : float)
                                          ((let abcd_25
                                            ((mix (ab_21 : float) (cd_22 : float)
                                              ((index (u_9 : (vec 3)) 1) : float))
                                             : float)
                                            ((let efgh_26
                                              ((mix (ef_23 : float)
                                                (gh_24 : float)
                                                ((index (u_9 : (vec 3)) 1) :
                                                 float))
                                               : float)
                                              ((mix (abcd_25 : float)
                                                (efgh_26 : float)
                                                ((index (u_9 : (vec 3)) 2) :
                                                 float))
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
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : float))
               : float))
             : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec fbm_27
        ((lambda p_28
          ((+
            ((+
              ((+
                ((*
                  ((app (noise3d_5 : ((vec 3) -> float))
                    ((* (p_28 : (vec 3)) (1. : float)) : (vec 3)))
                   : float)
                  (0.5 : float))
                 : float)
                ((*
                  ((app (noise3d_5 : ((vec 3) -> float))
                    ((* (p_28 : (vec 3)) (2. : float)) : (vec 3)))
                   : float)
                  (0.25 : float))
                 : float))
               : float)
              ((*
                ((app (noise3d_5 : ((vec 3) -> float))
                  ((* (p_28 : (vec 3)) (4. : float)) : (vec 3)))
                 : float)
                (0.125 : float))
               : float))
             : float)
            ((*
              ((app (noise3d_5 : ((vec 3) -> float))
                ((* (p_28 : (vec 3)) (8. : float)) : (vec 3)))
               : float)
              (0.0625 : float))
             : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec sdPlanet_29
        ((lambda p_30
          ((lambda radius_31
            ((let len_32 ((length (p_30 : (vec 3))) : float)
              ((let dir_33 ((/ (p_30 : (vec 3)) (len_32 : float)) : (vec 3))
                ((let terrain_34
                  ((*
                    ((app (fbm_27 : ((vec 3) -> float))
                      ((* (dir_33 : (vec 3)) (3. : float)) : (vec 3)))
                     : float)
                    (0.4 : float))
                   : float)
                  ((- ((- (len_32 : float) (radius_31 : float)) : float)
                    (terrain_34 : float))
                   : float))
                 : float))
               : float))
             : float))
           : (float -> float)))
         : ((vec 3) -> (float -> float))))
       : ((vec 3) -> (float -> float)))
      ((Define Nonrec map_35
        ((lambda p_36
          ((app
            ((app (sdPlanet_29 : ((vec 3) -> (float -> float))) (p_36 : (vec 3)))
             : (float -> float))
            (1.5 : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec getNormal_37
        ((lambda p_38
          ((let e_39 (0.002 : float)
            ((let e_x_40 ((vec3 (e_39 : float) (0 : int) (0 : int)) : (vec 3))
              ((let e_y_41 ((vec3 (0 : int) (e_39 : float) (0 : int)) : (vec 3))
                ((let e_z_42
                  ((vec3 (0 : int) (0 : int) (e_39 : float)) : (vec 3))
                  ((let dx_43
                    ((-
                      ((app (map_35 : ((vec 3) -> float))
                        ((+ (p_38 : (vec 3)) (e_x_40 : (vec 3))) : (vec 3)))
                       : float)
                      ((app (map_35 : ((vec 3) -> float))
                        ((- (p_38 : (vec 3)) (e_x_40 : (vec 3))) : (vec 3)))
                       : float))
                     : float)
                    ((let dy_44
                      ((-
                        ((app (map_35 : ((vec 3) -> float))
                          ((+ (p_38 : (vec 3)) (e_y_41 : (vec 3))) : (vec 3)))
                         : float)
                        ((app (map_35 : ((vec 3) -> float))
                          ((- (p_38 : (vec 3)) (e_y_41 : (vec 3))) : (vec 3)))
                         : float))
                       : float)
                      ((let dz_45
                        ((-
                          ((app (map_35 : ((vec 3) -> float))
                            ((+ (p_38 : (vec 3)) (e_z_42 : (vec 3))) : (vec 3)))
                           : float)
                          ((app (map_35 : ((vec 3) -> float))
                            ((- (p_38 : (vec 3)) (e_z_42 : (vec 3))) : (vec 3)))
                           : float))
                         : float)
                        ((normalize
                          ((vec3 (dx_43 : float) (dy_44 : float) (dz_45 : float))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 3) -> (vec 3))))
       : ((vec 3) -> (vec 3)))
      ((Define Nonrec march_46
        ((lambda ro_47
          ((lambda rd_48
            ((let (rec 1000) march_49
              ((lambda t_50
                ((lambda steps_51
                  ((if ((> (steps_51 : int) (120 : int)) : bool) (t_50 : float)
                    ((let d_52
                      ((app (map_35 : ((vec 3) -> float))
                        ((+ (ro_47 : (vec 3))
                          ((* (rd_48 : (vec 3)) (t_50 : float)) : (vec 3)))
                         : (vec 3)))
                       : float)
                      ((if ((< (d_52 : float) (0.0005 : float)) : bool)
                        (t_50 : float)
                        ((if ((> (t_50 : float) (50. : float)) : bool)
                          (50.1 : float)
                          ((app
                            ((app (march_49 : (float -> (int -> float)))
                              ((+ (t_50 : float)
                                ((* (d_52 : float) (0.8 : float)) : float))
                               : float))
                             : (int -> float))
                            ((+ (steps_51 : int) (1 : int)) : int))
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : (int -> float)))
               : (float -> (int -> float)))
              ((app
                ((app (march_49 : (float -> (int -> float))) (0. : float)) :
                 (int -> float))
                (0 : int))
               : float))
             : float))
           : ((vec 3) -> float)))
         : ((vec 3) -> ((vec 3) -> float))))
       : ((vec 3) -> ((vec 3) -> float)))
      ((Define Nonrec main
        ((lambda coord_53
          ((let res_min_54
            ((min ((index (u_resolution : (vec 2)) 0) : float)
              ((index (u_resolution : (vec 2)) 1) : float))
             : float)
            ((let uv_55
              ((/
                ((- ((* (coord_53 : (vec 2)) (2. : float)) : (vec 2))
                  (u_resolution : (vec 2)))
                 : (vec 2))
                (res_min_54 : float))
               : (vec 2))
              ((let mouseUV_56
                ((/
                  ((- ((* (u_mouse : (vec 2)) (2. : float)) : (vec 2))
                    (u_resolution : (vec 2)))
                   : (vec 2))
                  (res_min_54 : float))
                 : (vec 2))
                ((let rotate_by_mouse_57
                  ((lambda ray_58
                    ((let rotX_59
                      ((*
                        ((* (-1 : int)
                          ((index (mouseUV_56 : (vec 2)) 1) : float))
                         : float)
                        (1.5 : float))
                       : float)
                      ((let ro_yz_60
                        ((app
                          ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                            ((vec2 ((index (ray_58 : 'v_223) 1) : 'v_227)
                              ((index (ray_58 : 'v_223) 2) : 'v_228))
                             : (vec 2)))
                           : (float -> (vec 2)))
                          (rotX_59 : float))
                         : (vec 2))
                        ((let rotY_61
                          ((*
                            ((* (-1 : int)
                              ((index (mouseUV_56 : (vec 2)) 0) : float))
                             : float)
                            (1.5 : float))
                           : float)
                          ((let ro_xz_62
                            ((app
                              ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                                ((vec2 ((index (ray_58 : 'v_223) 0) : 'v_234)
                                  ((index (ro_yz_60 : (vec 2)) 1) : float))
                                 : (vec 2)))
                               : (float -> (vec 2)))
                              (rotY_61 : float))
                             : (vec 2))
                            ((vec3 ((index (ro_xz_62 : (vec 2)) 0) : float)
                              ((index (ro_yz_60 : (vec 2)) 0) : float)
                              ((index (ro_xz_62 : (vec 2)) 1) : float))
                             : (vec 3)))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   :
                   (forall
                    ((Comparable 'v_228) (IndexAccess 'v_223 2 'v_228)
                     (Comparable 'v_227) (IndexAccess 'v_223 1 'v_227)
                     (Comparable 'v_234) (IndexAccess 'v_223 0 'v_234))
                    ('v_223 -> (vec 3))))
                  ((let ro_63
                    ((app (rotate_by_mouse_57 : ((vec 3) -> (vec 3)))
                      ((vec3 (0 : int) (0 : int) (-4 : int)) : (vec 3)))
                     : (vec 3))
                    ((let rd_64
                      ((app (rotate_by_mouse_57 : ((vec 3) -> (vec 3)))
                        ((normalize
                          ((vec3 ((index (uv_55 : (vec 2)) 0) : float)
                            ((index (uv_55 : (vec 2)) 1) : float) (1.5 : float))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3))
                      ((let t_65
                        ((app
                          ((app (march_46 : ((vec 3) -> ((vec 3) -> float)))
                            (ro_63 : (vec 3)))
                           : ((vec 3) -> float))
                          (rd_64 : (vec 3)))
                         : float)
                        ((if ((> (t_65 : float) (50. : float)) : bool)
                          ((vec3 (0 : int) (0 : int) (0 : int)) : (vec 3))
                          ((let hitPos_66
                            ((+ (ro_63 : (vec 3))
                              ((* (rd_64 : (vec 3)) (t_65 : float)) : (vec 3)))
                             : (vec 3))
                            ((let n_67
                              ((app (getNormal_37 : ((vec 3) -> (vec 3)))
                                (hitPos_66 : (vec 3)))
                               : (vec 3))
                              ((let lightDir_68
                                ((normalize
                                  ((vec3 (1. : float) (0.8 : float)
                                    (-0.5 : float))
                                   : (vec 3)))
                                 : (vec 3))
                                ((let diff_69
                                  ((max
                                    ((dot (n_67 : (vec 3))
                                      (lightDir_68 : (vec 3)))
                                     : float)
                                    (0 : int))
                                   : float)
                                  ((let ambient_70 (0.08 : float)
                                    ((let dir_71
                                      ((/ (hitPos_66 : (vec 3))
                                        ((length (hitPos_66 : (vec 3))) : float))
                                       : (vec 3))
                                      ((let rawHeight_72
                                        ((app (fbm_27 : ((vec 3) -> float))
                                          ((* (dir_71 : (vec 3)) (3. : float)) :
                                           (vec 3)))
                                         : float)
                                        ((let seaLevel_73 (0.35 : float)
                                          ((let h_norm_74
                                            ((clamp
                                              ((/
                                                ((- (rawHeight_72 : float)
                                                  (seaLevel_73 : float))
                                                 : float)
                                                ((- (1. : float)
                                                  (seaLevel_73 : float))
                                                 : float))
                                               : float)
                                              (0. : float) (1. : float))
                                             : float)
                                            ((let deepColor_75
                                              ((vec3 (0.02 : float)
                                                (0.05 : float) (0.2 : float))
                                               : (vec 3))
                                              ((let landColor_76
                                                ((vec3 (0.15 : float)
                                                  (0.35 : float) (0.1 : float))
                                                 : (vec 3))
                                                ((let mountColor_77
                                                  ((vec3 (0.4 : float)
                                                    (0.3 : float) (0.2 : float))
                                                   : (vec 3))
                                                  ((let snowColor_78
                                                    ((vec3 (0.85 : float)
                                                      (0.85 : float)
                                                      (0.9 : float))
                                                     : (vec 3))
                                                    ((let baseColor_79
                                                      ((if
                                                        ((< (h_norm_74 : float)
                                                          (0.3 : float))
                                                         : bool)
                                                        ((mix
                                                          (deepColor_75 :
                                                           (vec 3))
                                                          (landColor_76 :
                                                           (vec 3))
                                                          ((/ (h_norm_74 : float)
                                                            (0.3 : float))
                                                           : float))
                                                         : (vec 3))
                                                        ((if
                                                          ((< (h_norm_74 : float)
                                                            (0.6 : float))
                                                           : bool)
                                                          ((mix
                                                            (landColor_76 :
                                                             (vec 3))
                                                            (mountColor_77 :
                                                             (vec 3))
                                                            ((/
                                                              ((-
                                                                (h_norm_74 :
                                                                 float)
                                                                (0.3 : float))
                                                               : float)
                                                              (0.3 : float))
                                                             : float))
                                                           : (vec 3))
                                                          ((mix
                                                            (mountColor_77 :
                                                             (vec 3))
                                                            (snowColor_78 :
                                                             (vec 3))
                                                            ((/
                                                              ((-
                                                                (h_norm_74 :
                                                                 float)
                                                                (0.6 : float))
                                                               : float)
                                                              (0.4 : float))
                                                             : float))
                                                           : (vec 3)))
                                                         : (vec 3)))
                                                       : (vec 3))
                                                      ((let fresnel_80
                                                        ((- (1. : float)
                                                          ((max
                                                            ((dot
                                                              (n_67 : (vec 3))
                                                              ((*
                                                                (rd_64 : (vec 3))
                                                                (-1. : float))
                                                               : (vec 3)))
                                                             : float)
                                                            (0 : int))
                                                           : float))
                                                         : float)
                                                        ((let rim_81
                                                          ((*
                                                            ((*
                                                              ((*
                                                                (fresnel_80 :
                                                                 float)
                                                                (fresnel_80 :
                                                                 float))
                                                               : float)
                                                              (fresnel_80 :
                                                               float))
                                                             : float)
                                                            (0.4 : float))
                                                           : float)
                                                          ((let atmoColor_82
                                                            ((vec3 (0.3 : float)
                                                              (0.5 : float)
                                                              (1. : float))
                                                             : (vec 3))
                                                            ((+
                                                              ((*
                                                                (baseColor_79 :
                                                                 (vec 3))
                                                                ((+
                                                                  ((*
                                                                    (diff_69 :
                                                                     float)
                                                                    (0.9 : float))
                                                                   : float)
                                                                  (ambient_70 :
                                                                   float))
                                                                 : float))
                                                               : (vec 3))
                                                              ((*
                                                                (atmoColor_82 :
                                                                 (vec 3))
                                                                (rim_81 : float))
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

    === monomorphize (planet.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Extern u_mouse) : (vec 2))
      ((Define Nonrec rotate_0
        ((lambda p_1
          ((lambda angle_2
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
      ((Define Nonrec noise3d_5
        ((lambda p_6
          ((let i_7 ((floor (p_6 : (vec 3))) : (vec 3))
            ((let f_8 ((fract (p_6 : (vec 3))) : (vec 3))
              ((let u_9
                ((* ((* (f_8 : (vec 3)) (f_8 : (vec 3))) : (vec 3))
                  ((- (3. : float) ((* (2. : float) (f_8 : (vec 3))) : (vec 3)))
                   : (vec 3)))
                 : (vec 3))
                ((let hash_10
                  ((lambda p_11
                    ((let d_12
                      ((dot (p_11 : (vec 3))
                        ((vec3 (127.1 : float) (311.7 : float) (74.7 : float)) :
                         (vec 3)))
                       : float)
                      ((fract
                        ((* ((sin (d_12 : float)) : float) (43758.5453 : float))
                         : float))
                       : float))
                     : float))
                   : ((vec 3) -> float))
                  ((let a_13
                    ((app (hash_10 : ((vec 3) -> float)) (i_7 : (vec 3))) :
                     float)
                    ((let b_14
                      ((app (hash_10 : ((vec 3) -> float))
                        ((+ (i_7 : (vec 3))
                          ((vec3 (1 : int) (0 : int) (0 : int)) : (vec 3)))
                         : (vec 3)))
                       : float)
                      ((let c_15
                        ((app (hash_10 : ((vec 3) -> float))
                          ((+ (i_7 : (vec 3))
                            ((vec3 (0 : int) (1 : int) (0 : int)) : (vec 3)))
                           : (vec 3)))
                         : float)
                        ((let d_16
                          ((app (hash_10 : ((vec 3) -> float))
                            ((+ (i_7 : (vec 3))
                              ((vec3 (1 : int) (1 : int) (0 : int)) : (vec 3)))
                             : (vec 3)))
                           : float)
                          ((let e_17
                            ((app (hash_10 : ((vec 3) -> float))
                              ((+ (i_7 : (vec 3))
                                ((vec3 (0 : int) (0 : int) (1 : int)) : (vec 3)))
                               : (vec 3)))
                             : float)
                            ((let f_18
                              ((app (hash_10 : ((vec 3) -> float))
                                ((+ (i_7 : (vec 3))
                                  ((vec3 (1 : int) (0 : int) (1 : int)) :
                                   (vec 3)))
                                 : (vec 3)))
                               : float)
                              ((let g_19
                                ((app (hash_10 : ((vec 3) -> float))
                                  ((+ (i_7 : (vec 3))
                                    ((vec3 (0 : int) (1 : int) (1 : int)) :
                                     (vec 3)))
                                   : (vec 3)))
                                 : float)
                                ((let h_20
                                  ((app (hash_10 : ((vec 3) -> float))
                                    ((+ (i_7 : (vec 3))
                                      ((vec3 (1 : int) (1 : int) (1 : int)) :
                                       (vec 3)))
                                     : (vec 3)))
                                   : float)
                                  ((let ab_21
                                    ((mix (a_13 : float) (b_14 : float)
                                      ((index (u_9 : (vec 3)) 0) : float))
                                     : float)
                                    ((let cd_22
                                      ((mix (c_15 : float) (d_16 : float)
                                        ((index (u_9 : (vec 3)) 0) : float))
                                       : float)
                                      ((let ef_23
                                        ((mix (e_17 : float) (f_18 : float)
                                          ((index (u_9 : (vec 3)) 0) : float))
                                         : float)
                                        ((let gh_24
                                          ((mix (g_19 : float) (h_20 : float)
                                            ((index (u_9 : (vec 3)) 0) : float))
                                           : float)
                                          ((let abcd_25
                                            ((mix (ab_21 : float) (cd_22 : float)
                                              ((index (u_9 : (vec 3)) 1) : float))
                                             : float)
                                            ((let efgh_26
                                              ((mix (ef_23 : float)
                                                (gh_24 : float)
                                                ((index (u_9 : (vec 3)) 1) :
                                                 float))
                                               : float)
                                              ((mix (abcd_25 : float)
                                                (efgh_26 : float)
                                                ((index (u_9 : (vec 3)) 2) :
                                                 float))
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
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : float))
               : float))
             : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec fbm_27
        ((lambda p_28
          ((+
            ((+
              ((+
                ((*
                  ((app (noise3d_5 : ((vec 3) -> float))
                    ((* (p_28 : (vec 3)) (1. : float)) : (vec 3)))
                   : float)
                  (0.5 : float))
                 : float)
                ((*
                  ((app (noise3d_5 : ((vec 3) -> float))
                    ((* (p_28 : (vec 3)) (2. : float)) : (vec 3)))
                   : float)
                  (0.25 : float))
                 : float))
               : float)
              ((*
                ((app (noise3d_5 : ((vec 3) -> float))
                  ((* (p_28 : (vec 3)) (4. : float)) : (vec 3)))
                 : float)
                (0.125 : float))
               : float))
             : float)
            ((*
              ((app (noise3d_5 : ((vec 3) -> float))
                ((* (p_28 : (vec 3)) (8. : float)) : (vec 3)))
               : float)
              (0.0625 : float))
             : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec sdPlanet_29
        ((lambda p_30
          ((lambda radius_31
            ((let len_32 ((length (p_30 : (vec 3))) : float)
              ((let dir_33 ((/ (p_30 : (vec 3)) (len_32 : float)) : (vec 3))
                ((let terrain_34
                  ((*
                    ((app (fbm_27 : ((vec 3) -> float))
                      ((* (dir_33 : (vec 3)) (3. : float)) : (vec 3)))
                     : float)
                    (0.4 : float))
                   : float)
                  ((- ((- (len_32 : float) (radius_31 : float)) : float)
                    (terrain_34 : float))
                   : float))
                 : float))
               : float))
             : float))
           : (float -> float)))
         : ((vec 3) -> (float -> float))))
       : ((vec 3) -> (float -> float)))
      ((Define Nonrec map_35
        ((lambda p_36
          ((app
            ((app (sdPlanet_29 : ((vec 3) -> (float -> float))) (p_36 : (vec 3)))
             : (float -> float))
            (1.5 : float))
           : float))
         : ((vec 3) -> float)))
       : ((vec 3) -> float))
      ((Define Nonrec getNormal_37
        ((lambda p_38
          ((let e_39 (0.002 : float)
            ((let e_x_40 ((vec3 (e_39 : float) (0 : int) (0 : int)) : (vec 3))
              ((let e_y_41 ((vec3 (0 : int) (e_39 : float) (0 : int)) : (vec 3))
                ((let e_z_42
                  ((vec3 (0 : int) (0 : int) (e_39 : float)) : (vec 3))
                  ((let dx_43
                    ((-
                      ((app (map_35 : ((vec 3) -> float))
                        ((+ (p_38 : (vec 3)) (e_x_40 : (vec 3))) : (vec 3)))
                       : float)
                      ((app (map_35 : ((vec 3) -> float))
                        ((- (p_38 : (vec 3)) (e_x_40 : (vec 3))) : (vec 3)))
                       : float))
                     : float)
                    ((let dy_44
                      ((-
                        ((app (map_35 : ((vec 3) -> float))
                          ((+ (p_38 : (vec 3)) (e_y_41 : (vec 3))) : (vec 3)))
                         : float)
                        ((app (map_35 : ((vec 3) -> float))
                          ((- (p_38 : (vec 3)) (e_y_41 : (vec 3))) : (vec 3)))
                         : float))
                       : float)
                      ((let dz_45
                        ((-
                          ((app (map_35 : ((vec 3) -> float))
                            ((+ (p_38 : (vec 3)) (e_z_42 : (vec 3))) : (vec 3)))
                           : float)
                          ((app (map_35 : ((vec 3) -> float))
                            ((- (p_38 : (vec 3)) (e_z_42 : (vec 3))) : (vec 3)))
                           : float))
                         : float)
                        ((normalize
                          ((vec3 (dx_43 : float) (dy_44 : float) (dz_45 : float))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : (vec 3)))
                 : (vec 3)))
               : (vec 3)))
             : (vec 3)))
           : (vec 3)))
         : ((vec 3) -> (vec 3))))
       : ((vec 3) -> (vec 3)))
      ((Define Nonrec march_46
        ((lambda ro_47
          ((lambda rd_48
            ((let (rec 1000) march_49
              ((lambda t_50
                ((lambda steps_51
                  ((if ((> (steps_51 : int) (120 : int)) : bool) (t_50 : float)
                    ((let d_52
                      ((app (map_35 : ((vec 3) -> float))
                        ((+ (ro_47 : (vec 3))
                          ((* (rd_48 : (vec 3)) (t_50 : float)) : (vec 3)))
                         : (vec 3)))
                       : float)
                      ((if ((< (d_52 : float) (0.0005 : float)) : bool)
                        (t_50 : float)
                        ((if ((> (t_50 : float) (50. : float)) : bool)
                          (50.1 : float)
                          ((app
                            ((app (march_49 : (float -> (int -> float)))
                              ((+ (t_50 : float)
                                ((* (d_52 : float) (0.8 : float)) : float))
                               : float))
                             : (int -> float))
                            ((+ (steps_51 : int) (1 : int)) : int))
                           : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : (int -> float)))
               : (float -> (int -> float)))
              ((app
                ((app (march_49 : (float -> (int -> float))) (0. : float)) :
                 (int -> float))
                (0 : int))
               : float))
             : float))
           : ((vec 3) -> float)))
         : ((vec 3) -> ((vec 3) -> float))))
       : ((vec 3) -> ((vec 3) -> float)))
      ((Define Nonrec main
        ((lambda coord_53
          ((let res_min_54
            ((min ((index (u_resolution : (vec 2)) 0) : float)
              ((index (u_resolution : (vec 2)) 1) : float))
             : float)
            ((let uv_55
              ((/
                ((- ((* (coord_53 : (vec 2)) (2. : float)) : (vec 2))
                  (u_resolution : (vec 2)))
                 : (vec 2))
                (res_min_54 : float))
               : (vec 2))
              ((let mouseUV_56
                ((/
                  ((- ((* (u_mouse : (vec 2)) (2. : float)) : (vec 2))
                    (u_resolution : (vec 2)))
                   : (vec 2))
                  (res_min_54 : float))
                 : (vec 2))
                ((let rotate_by_mouse_57_vec3_to_vec3_309
                  ((lambda ray_58
                    ((let rotX_59
                      ((*
                        ((* (-1 : int)
                          ((index (mouseUV_56 : (vec 2)) 1) : float))
                         : float)
                        (1.5 : float))
                       : float)
                      ((let ro_yz_60
                        ((app
                          ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                            ((vec2 ((index (ray_58 : (vec 3)) 1) : float)
                              ((index (ray_58 : (vec 3)) 2) : float))
                             : (vec 2)))
                           : (float -> (vec 2)))
                          (rotX_59 : float))
                         : (vec 2))
                        ((let rotY_61
                          ((*
                            ((* (-1 : int)
                              ((index (mouseUV_56 : (vec 2)) 0) : float))
                             : float)
                            (1.5 : float))
                           : float)
                          ((let ro_xz_62
                            ((app
                              ((app (rotate_0 : ((vec 2) -> (float -> (vec 2))))
                                ((vec2 ((index (ray_58 : (vec 3)) 0) : float)
                                  ((index (ro_yz_60 : (vec 2)) 1) : float))
                                 : (vec 2)))
                               : (float -> (vec 2)))
                              (rotY_61 : float))
                             : (vec 2))
                            ((vec3 ((index (ro_xz_62 : (vec 2)) 0) : float)
                              ((index (ro_yz_60 : (vec 2)) 0) : float)
                              ((index (ro_xz_62 : (vec 2)) 1) : float))
                             : (vec 3)))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3)))
                     : (vec 3)))
                   : ((vec 3) -> (vec 3)))
                  ((let ro_63
                    ((app
                      (rotate_by_mouse_57_vec3_to_vec3_309 :
                       ((vec 3) -> (vec 3)))
                      ((vec3 (0 : int) (0 : int) (-4 : int)) : (vec 3)))
                     : (vec 3))
                    ((let rd_64
                      ((app
                        (rotate_by_mouse_57_vec3_to_vec3_309 :
                         ((vec 3) -> (vec 3)))
                        ((normalize
                          ((vec3 ((index (uv_55 : (vec 2)) 0) : float)
                            ((index (uv_55 : (vec 2)) 1) : float) (1.5 : float))
                           : (vec 3)))
                         : (vec 3)))
                       : (vec 3))
                      ((let t_65
                        ((app
                          ((app (march_46 : ((vec 3) -> ((vec 3) -> float)))
                            (ro_63 : (vec 3)))
                           : ((vec 3) -> float))
                          (rd_64 : (vec 3)))
                         : float)
                        ((if ((> (t_65 : float) (50. : float)) : bool)
                          ((vec3 (0 : int) (0 : int) (0 : int)) : (vec 3))
                          ((let hitPos_66
                            ((+ (ro_63 : (vec 3))
                              ((* (rd_64 : (vec 3)) (t_65 : float)) : (vec 3)))
                             : (vec 3))
                            ((let n_67
                              ((app (getNormal_37 : ((vec 3) -> (vec 3)))
                                (hitPos_66 : (vec 3)))
                               : (vec 3))
                              ((let lightDir_68
                                ((normalize
                                  ((vec3 (1. : float) (0.8 : float)
                                    (-0.5 : float))
                                   : (vec 3)))
                                 : (vec 3))
                                ((let diff_69
                                  ((max
                                    ((dot (n_67 : (vec 3))
                                      (lightDir_68 : (vec 3)))
                                     : float)
                                    (0 : int))
                                   : float)
                                  ((let ambient_70 (0.08 : float)
                                    ((let dir_71
                                      ((/ (hitPos_66 : (vec 3))
                                        ((length (hitPos_66 : (vec 3))) : float))
                                       : (vec 3))
                                      ((let rawHeight_72
                                        ((app (fbm_27 : ((vec 3) -> float))
                                          ((* (dir_71 : (vec 3)) (3. : float)) :
                                           (vec 3)))
                                         : float)
                                        ((let seaLevel_73 (0.35 : float)
                                          ((let h_norm_74
                                            ((clamp
                                              ((/
                                                ((- (rawHeight_72 : float)
                                                  (seaLevel_73 : float))
                                                 : float)
                                                ((- (1. : float)
                                                  (seaLevel_73 : float))
                                                 : float))
                                               : float)
                                              (0. : float) (1. : float))
                                             : float)
                                            ((let deepColor_75
                                              ((vec3 (0.02 : float)
                                                (0.05 : float) (0.2 : float))
                                               : (vec 3))
                                              ((let landColor_76
                                                ((vec3 (0.15 : float)
                                                  (0.35 : float) (0.1 : float))
                                                 : (vec 3))
                                                ((let mountColor_77
                                                  ((vec3 (0.4 : float)
                                                    (0.3 : float) (0.2 : float))
                                                   : (vec 3))
                                                  ((let snowColor_78
                                                    ((vec3 (0.85 : float)
                                                      (0.85 : float)
                                                      (0.9 : float))
                                                     : (vec 3))
                                                    ((let baseColor_79
                                                      ((if
                                                        ((< (h_norm_74 : float)
                                                          (0.3 : float))
                                                         : bool)
                                                        ((mix
                                                          (deepColor_75 :
                                                           (vec 3))
                                                          (landColor_76 :
                                                           (vec 3))
                                                          ((/ (h_norm_74 : float)
                                                            (0.3 : float))
                                                           : float))
                                                         : (vec 3))
                                                        ((if
                                                          ((< (h_norm_74 : float)
                                                            (0.6 : float))
                                                           : bool)
                                                          ((mix
                                                            (landColor_76 :
                                                             (vec 3))
                                                            (mountColor_77 :
                                                             (vec 3))
                                                            ((/
                                                              ((-
                                                                (h_norm_74 :
                                                                 float)
                                                                (0.3 : float))
                                                               : float)
                                                              (0.3 : float))
                                                             : float))
                                                           : (vec 3))
                                                          ((mix
                                                            (mountColor_77 :
                                                             (vec 3))
                                                            (snowColor_78 :
                                                             (vec 3))
                                                            ((/
                                                              ((-
                                                                (h_norm_74 :
                                                                 float)
                                                                (0.6 : float))
                                                               : float)
                                                              (0.4 : float))
                                                             : float))
                                                           : (vec 3)))
                                                         : (vec 3)))
                                                       : (vec 3))
                                                      ((let fresnel_80
                                                        ((- (1. : float)
                                                          ((max
                                                            ((dot
                                                              (n_67 : (vec 3))
                                                              ((*
                                                                (rd_64 : (vec 3))
                                                                (-1. : float))
                                                               : (vec 3)))
                                                             : float)
                                                            (0 : int))
                                                           : float))
                                                         : float)
                                                        ((let rim_81
                                                          ((*
                                                            ((*
                                                              ((*
                                                                (fresnel_80 :
                                                                 float)
                                                                (fresnel_80 :
                                                                 float))
                                                               : float)
                                                              (fresnel_80 :
                                                               float))
                                                             : float)
                                                            (0.4 : float))
                                                           : float)
                                                          ((let atmoColor_82
                                                            ((vec3 (0.3 : float)
                                                              (0.5 : float)
                                                              (1. : float))
                                                             : (vec 3))
                                                            ((+
                                                              ((*
                                                                (baseColor_79 :
                                                                 (vec 3))
                                                                ((+
                                                                  ((*
                                                                    (diff_69 :
                                                                     float)
                                                                    (0.9 : float))
                                                                   : float)
                                                                  (ambient_70 :
                                                                   float))
                                                                 : float))
                                                               : (vec 3))
                                                              ((*
                                                                (atmoColor_82 :
                                                                 (vec 3))
                                                                (rim_81 : float))
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

    === uncurry (planet.glml) ===
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
      ((Define Nonrec noise3d_5
        (lambda ((p_6 (vec 3)))
         (let i_7 (floor p_6)
          (let f_8 (fract p_6)
           (let u_9 (* (* f_8 f_8) (- 3. (* 2. f_8)))
            (let hash_10
             (lambda ((p_11 (vec 3)))
              (let d_12 (dot p_11 (vec3 127.1 311.7 74.7))
               (fract (* (sin d_12) 43758.5453))))
             (let a_13 (app hash_10 i_7)
              (let b_14 (app hash_10 (+ i_7 (vec3 1 0 0)))
               (let c_15 (app hash_10 (+ i_7 (vec3 0 1 0)))
                (let d_16 (app hash_10 (+ i_7 (vec3 1 1 0)))
                 (let e_17 (app hash_10 (+ i_7 (vec3 0 0 1)))
                  (let f_18 (app hash_10 (+ i_7 (vec3 1 0 1)))
                   (let g_19 (app hash_10 (+ i_7 (vec3 0 1 1)))
                    (let h_20 (app hash_10 (+ i_7 (vec3 1 1 1)))
                     (let ab_21 (mix a_13 b_14 (index u_9 0))
                      (let cd_22 (mix c_15 d_16 (index u_9 0))
                       (let ef_23 (mix e_17 f_18 (index u_9 0))
                        (let gh_24 (mix g_19 h_20 (index u_9 0))
                         (let abcd_25 (mix ab_21 cd_22 (index u_9 1))
                          (let efgh_26 (mix ef_23 gh_24 (index u_9 1))
                           (mix abcd_25 efgh_26 (index u_9 2))))))))))))))))))))))
       : ((vec 3) -> float))
      ((Define Nonrec fbm_27
        (lambda ((p_28 (vec 3)))
         (+
          (+
           (+ (* (app noise3d_5 (* p_28 1.)) 0.5)
            (* (app noise3d_5 (* p_28 2.)) 0.25))
           (* (app noise3d_5 (* p_28 4.)) 0.125))
          (* (app noise3d_5 (* p_28 8.)) 0.0625))))
       : ((vec 3) -> float))
      ((Define Nonrec sdPlanet_29
        (lambda ((p_30 (vec 3)) (radius_31 float))
         (let len_32 (length p_30)
          (let dir_33 (/ p_30 len_32)
           (let terrain_34 (* (app fbm_27 (* dir_33 3.)) 0.4)
            (- (- len_32 radius_31) terrain_34))))))
       : ((vec 3) -> (float -> float)))
      ((Define Nonrec map_35
        (lambda ((p_36 (vec 3))) (app sdPlanet_29 p_36 1.5)))
       : ((vec 3) -> float))
      ((Define Nonrec getNormal_37
        (lambda ((p_38 (vec 3)))
         (let e_39 0.002
          (let e_x_40 (vec3 e_39 0 0)
           (let e_y_41 (vec3 0 e_39 0)
            (let e_z_42 (vec3 0 0 e_39)
             (let dx_43
              (- (app map_35 (+ p_38 e_x_40)) (app map_35 (- p_38 e_x_40)))
              (let dy_44
               (- (app map_35 (+ p_38 e_y_41)) (app map_35 (- p_38 e_y_41)))
               (let dz_45
                (- (app map_35 (+ p_38 e_z_42)) (app map_35 (- p_38 e_z_42)))
                (normalize (vec3 dx_43 dy_44 dz_45)))))))))))
       : ((vec 3) -> (vec 3)))
      ((Define Nonrec march_46
        (lambda ((ro_47 (vec 3)) (rd_48 (vec 3)))
         (let (rec 1000) march_49
          (lambda ((t_50 float) (steps_51 int))
           (if (> steps_51 120) t_50
            (let d_52 (app map_35 (+ ro_47 (* rd_48 t_50)))
             (if (< d_52 0.0005) t_50
              (if (> t_50 50.) 50.1
               (app march_49 (+ t_50 (* d_52 0.8)) (+ steps_51 1)))))))
          (app march_49 0. 0))))
       : ((vec 3) -> ((vec 3) -> float)))
      ((Define Nonrec main
        (lambda ((coord_53 (vec 2)))
         (let res_min_54 (min (index u_resolution 0) (index u_resolution 1))
          (let uv_55 (/ (- (* coord_53 2.) u_resolution) res_min_54)
           (let mouseUV_56 (/ (- (* u_mouse 2.) u_resolution) res_min_54)
            (let rotate_by_mouse_57_vec3_to_vec3_309
             (lambda ((ray_58 (vec 3)))
              (let rotX_59 (* (* -1 (index mouseUV_56 1)) 1.5)
               (let ro_yz_60
                (app rotate_0 (vec2 (index ray_58 1) (index ray_58 2)) rotX_59)
                (let rotY_61 (* (* -1 (index mouseUV_56 0)) 1.5)
                 (let ro_xz_62
                  (app rotate_0 (vec2 (index ray_58 0) (index ro_yz_60 1))
                   rotY_61)
                  (vec3 (index ro_xz_62 0) (index ro_yz_60 0) (index ro_xz_62 1)))))))
             (let ro_63 (app rotate_by_mouse_57_vec3_to_vec3_309 (vec3 0 0 -4))
              (let rd_64
               (app rotate_by_mouse_57_vec3_to_vec3_309
                (normalize (vec3 (index uv_55 0) (index uv_55 1) 1.5)))
               (let t_65 (app march_46 ro_63 rd_64)
                (if (> t_65 50.) (vec3 0 0 0)
                 (let hitPos_66 (+ ro_63 (* rd_64 t_65))
                  (let n_67 (app getNormal_37 hitPos_66)
                   (let lightDir_68 (normalize (vec3 1. 0.8 -0.5))
                    (let diff_69 (max (dot n_67 lightDir_68) 0)
                     (let ambient_70 0.08
                      (let dir_71 (/ hitPos_66 (length hitPos_66))
                       (let rawHeight_72 (app fbm_27 (* dir_71 3.))
                        (let seaLevel_73 0.35
                         (let h_norm_74
                          (clamp
                           (/ (- rawHeight_72 seaLevel_73) (- 1. seaLevel_73)) 0.
                           1.)
                          (let deepColor_75 (vec3 0.02 0.05 0.2)
                           (let landColor_76 (vec3 0.15 0.35 0.1)
                            (let mountColor_77 (vec3 0.4 0.3 0.2)
                             (let snowColor_78 (vec3 0.85 0.85 0.9)
                              (let baseColor_79
                               (if (< h_norm_74 0.3)
                                (mix deepColor_75 landColor_76 (/ h_norm_74 0.3))
                                (if (< h_norm_74 0.6)
                                 (mix landColor_76 mountColor_77
                                  (/ (- h_norm_74 0.3) 0.3))
                                 (mix mountColor_77 snowColor_78
                                  (/ (- h_norm_74 0.6) 0.4))))
                               (let fresnel_80
                                (- 1. (max (dot n_67 (* rd_64 -1.)) 0))
                                (let rim_81
                                 (* (* (* fresnel_80 fresnel_80) fresnel_80) 0.4)
                                 (let atmoColor_82 (vec3 0.3 0.5 1.)
                                  (+
                                   (* baseColor_79
                                    (+ (* diff_69 0.9) ambient_70))
                                   (* atmoColor_82 rim_81)))))))))))))))))))))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (planet.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define Nonrec (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (vec2 (- (* (index p_1 0) c_4) (* (index p_1 1) s_3))
           (+ (* (index p_1 0) s_3) (* (index p_1 1) c_4)))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define Nonrec (name hash_10_310) (args ((p_11 (vec 3))))
       (body
        (let d_12 (dot p_11 (vec3 127.1 311.7 74.7))
         (fract (* (sin d_12) 43758.5453)))))
      : ((vec 3) -> float))
     ((Define Nonrec (name noise3d_5) (args ((p_6 (vec 3))))
       (body
        (let i_7 (floor p_6)
         (let f_8 (fract p_6)
          (let u_9 (* (* f_8 f_8) (- 3. (* 2. f_8)))
           (let a_13 (app hash_10_310 i_7)
            (let b_14 (app hash_10_310 (+ i_7 (vec3 1 0 0)))
             (let c_15 (app hash_10_310 (+ i_7 (vec3 0 1 0)))
              (let d_16 (app hash_10_310 (+ i_7 (vec3 1 1 0)))
               (let e_17 (app hash_10_310 (+ i_7 (vec3 0 0 1)))
                (let f_18 (app hash_10_310 (+ i_7 (vec3 1 0 1)))
                 (let g_19 (app hash_10_310 (+ i_7 (vec3 0 1 1)))
                  (let h_20 (app hash_10_310 (+ i_7 (vec3 1 1 1)))
                   (let ab_21 (mix a_13 b_14 (index u_9 0))
                    (let cd_22 (mix c_15 d_16 (index u_9 0))
                     (let ef_23 (mix e_17 f_18 (index u_9 0))
                      (let gh_24 (mix g_19 h_20 (index u_9 0))
                       (let abcd_25 (mix ab_21 cd_22 (index u_9 1))
                        (let efgh_26 (mix ef_23 gh_24 (index u_9 1))
                         (mix abcd_25 efgh_26 (index u_9 2)))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define Nonrec (name fbm_27) (args ((p_28 (vec 3))))
       (body
        (+
         (+
          (+ (* (app noise3d_5 (* p_28 1.)) 0.5)
           (* (app noise3d_5 (* p_28 2.)) 0.25))
          (* (app noise3d_5 (* p_28 4.)) 0.125))
         (* (app noise3d_5 (* p_28 8.)) 0.0625))))
      : ((vec 3) -> float))
     ((Define Nonrec (name sdPlanet_29) (args ((p_30 (vec 3)) (radius_31 float)))
       (body
        (let len_32 (length p_30)
         (let dir_33 (/ p_30 len_32)
          (let terrain_34 (* (app fbm_27 (* dir_33 3.)) 0.4)
           (- (- len_32 radius_31) terrain_34))))))
      : ((vec 3) -> (float -> float)))
     ((Define Nonrec (name map_35) (args ((p_36 (vec 3))))
       (body (app sdPlanet_29 p_36 1.5)))
      : ((vec 3) -> float))
     ((Define Nonrec (name getNormal_37) (args ((p_38 (vec 3))))
       (body
        (let e_39 0.002
         (let e_x_40 (vec3 e_39 0 0)
          (let e_y_41 (vec3 0 e_39 0)
           (let e_z_42 (vec3 0 0 e_39)
            (let dx_43
             (- (app map_35 (+ p_38 e_x_40)) (app map_35 (- p_38 e_x_40)))
             (let dy_44
              (- (app map_35 (+ p_38 e_y_41)) (app map_35 (- p_38 e_y_41)))
              (let dz_45
               (- (app map_35 (+ p_38 e_z_42)) (app map_35 (- p_38 e_z_42)))
               (normalize (vec3 dx_43 dy_44 dz_45)))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define (Rec 1000) (name march_49_311)
       (args ((rd_48 (vec 3)) (ro_47 (vec 3)) (t_50 float) (steps_51 int)))
       (body
        (if (> steps_51 120) t_50
         (let d_52 (app map_35 (+ ro_47 (* rd_48 t_50)))
          (if (< d_52 0.0005) t_50
           (if (> t_50 50.) 50.1
            (app march_49_311 rd_48 ro_47 (+ t_50 (* d_52 0.8)) (+ steps_51 1))))))))
      : (float -> (int -> float)))
     ((Define Nonrec (name march_46) (args ((ro_47 (vec 3)) (rd_48 (vec 3))))
       (body (app march_49_311 rd_48 ro_47 0. 0)))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define Nonrec (name rotate_by_mouse_57_vec3_to_vec3_309_312)
       (args ((mouseUV_56 (vec 2)) (ray_58 (vec 3))))
       (body
        (let rotX_59 (* (* -1 (index mouseUV_56 1)) 1.5)
         (let ro_yz_60
          (app rotate_0 (vec2 (index ray_58 1) (index ray_58 2)) rotX_59)
          (let rotY_61 (* (* -1 (index mouseUV_56 0)) 1.5)
           (let ro_xz_62
            (app rotate_0 (vec2 (index ray_58 0) (index ro_yz_60 1)) rotY_61)
            (vec3 (index ro_xz_62 0) (index ro_yz_60 0) (index ro_xz_62 1))))))))
      : ((vec 3) -> (vec 3)))
     ((Define Nonrec (name main) (args ((coord_53 (vec 2))))
       (body
        (let res_min_54 (min (index u_resolution 0) (index u_resolution 1))
         (let uv_55 (/ (- (* coord_53 2.) u_resolution) res_min_54)
          (let mouseUV_56 (/ (- (* u_mouse 2.) u_resolution) res_min_54)
           (let ro_63
            (app rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56
             (vec3 0 0 -4))
            (let rd_64
             (app rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56
              (normalize (vec3 (index uv_55 0) (index uv_55 1) 1.5)))
             (let t_65 (app march_46 ro_63 rd_64)
              (if (> t_65 50.) (vec3 0 0 0)
               (let hitPos_66 (+ ro_63 (* rd_64 t_65))
                (let n_67 (app getNormal_37 hitPos_66)
                 (let lightDir_68 (normalize (vec3 1. 0.8 -0.5))
                  (let diff_69 (max (dot n_67 lightDir_68) 0)
                   (let ambient_70 0.08
                    (let dir_71 (/ hitPos_66 (length hitPos_66))
                     (let rawHeight_72 (app fbm_27 (* dir_71 3.))
                      (let seaLevel_73 0.35
                       (let h_norm_74
                        (clamp
                         (/ (- rawHeight_72 seaLevel_73) (- 1. seaLevel_73)) 0.
                         1.)
                        (let deepColor_75 (vec3 0.02 0.05 0.2)
                         (let landColor_76 (vec3 0.15 0.35 0.1)
                          (let mountColor_77 (vec3 0.4 0.3 0.2)
                           (let snowColor_78 (vec3 0.85 0.85 0.9)
                            (let baseColor_79
                             (if (< h_norm_74 0.3)
                              (mix deepColor_75 landColor_76 (/ h_norm_74 0.3))
                              (if (< h_norm_74 0.6)
                               (mix landColor_76 mountColor_77
                                (/ (- h_norm_74 0.3) 0.3))
                               (mix mountColor_77 snowColor_78
                                (/ (- h_norm_74 0.6) 0.4))))
                             (let fresnel_80
                              (- 1. (max (dot n_67 (* rd_64 -1.)) 0))
                              (let rim_81
                               (* (* (* fresnel_80 fresnel_80) fresnel_80) 0.4)
                               (let atmoColor_82 (vec3 0.3 0.5 1.)
                                (+
                                 (* baseColor_79 (+ (* diff_69 0.9) ambient_70))
                                 (* atmoColor_82 rim_81))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === anf (planet.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define Nonrec (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_313 (index p_1 0)
           (let anf_314 (* anf_313 c_4)
            (let anf_315 (index p_1 1)
             (let anf_316 (* anf_315 s_3)
              (let anf_317 (- anf_314 anf_316)
               (let anf_318 (index p_1 0)
                (let anf_319 (* anf_318 s_3)
                 (let anf_320 (index p_1 1)
                  (let anf_321 (* anf_320 c_4)
                   (let anf_322 (+ anf_319 anf_321)
                    (return (vec2 anf_317 anf_322))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define Nonrec (name hash_10_310) (args ((p_11 (vec 3))))
       (body
        (let anf_323 (vec3 127.1 311.7 74.7)
         (let d_12 (dot p_11 anf_323)
          (let anf_324 (sin d_12)
           (let anf_325 (* anf_324 43758.5453) (return (fract anf_325))))))))
      : ((vec 3) -> float))
     ((Define Nonrec (name noise3d_5) (args ((p_6 (vec 3))))
       (body
        (let i_7 (floor p_6)
         (let f_8 (fract p_6)
          (let anf_326 (* f_8 f_8)
           (let anf_327 (* 2. f_8)
            (let anf_328 (- 3. anf_327)
             (let u_9 (* anf_326 anf_328)
              (let a_13 (hash_10_310 i_7)
               (let anf_329 (vec3 1 0 0)
                (let anf_330 (+ i_7 anf_329)
                 (let b_14 (hash_10_310 anf_330)
                  (let anf_331 (vec3 0 1 0)
                   (let anf_332 (+ i_7 anf_331)
                    (let c_15 (hash_10_310 anf_332)
                     (let anf_333 (vec3 1 1 0)
                      (let anf_334 (+ i_7 anf_333)
                       (let d_16 (hash_10_310 anf_334)
                        (let anf_335 (vec3 0 0 1)
                         (let anf_336 (+ i_7 anf_335)
                          (let e_17 (hash_10_310 anf_336)
                           (let anf_337 (vec3 1 0 1)
                            (let anf_338 (+ i_7 anf_337)
                             (let f_18 (hash_10_310 anf_338)
                              (let anf_339 (vec3 0 1 1)
                               (let anf_340 (+ i_7 anf_339)
                                (let g_19 (hash_10_310 anf_340)
                                 (let anf_341 (vec3 1 1 1)
                                  (let anf_342 (+ i_7 anf_341)
                                   (let h_20 (hash_10_310 anf_342)
                                    (let anf_343 (index u_9 0)
                                     (let ab_21 (mix a_13 b_14 anf_343)
                                      (let anf_344 (index u_9 0)
                                       (let cd_22 (mix c_15 d_16 anf_344)
                                        (let anf_345 (index u_9 0)
                                         (let ef_23 (mix e_17 f_18 anf_345)
                                          (let anf_346 (index u_9 0)
                                           (let gh_24 (mix g_19 h_20 anf_346)
                                            (let anf_347 (index u_9 1)
                                             (let abcd_25
                                              (mix ab_21 cd_22 anf_347)
                                              (let anf_348 (index u_9 1)
                                               (let efgh_26
                                                (mix ef_23 gh_24 anf_348)
                                                (let anf_349 (index u_9 2)
                                                 (return
                                                  (mix abcd_25 efgh_26 anf_349)))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define Nonrec (name fbm_27) (args ((p_28 (vec 3))))
       (body
        (let anf_350 (* p_28 1.)
         (let anf_351 (noise3d_5 anf_350)
          (let anf_352 (* anf_351 0.5)
           (let anf_353 (* p_28 2.)
            (let anf_354 (noise3d_5 anf_353)
             (let anf_355 (* anf_354 0.25)
              (let anf_356 (+ anf_352 anf_355)
               (let anf_357 (* p_28 4.)
                (let anf_358 (noise3d_5 anf_357)
                 (let anf_359 (* anf_358 0.125)
                  (let anf_360 (+ anf_356 anf_359)
                   (let anf_361 (* p_28 8.)
                    (let anf_362 (noise3d_5 anf_361)
                     (let anf_363 (* anf_362 0.0625)
                      (return (+ anf_360 anf_363))))))))))))))))))
      : ((vec 3) -> float))
     ((Define Nonrec (name sdPlanet_29) (args ((p_30 (vec 3)) (radius_31 float)))
       (body
        (let len_32 (length p_30)
         (let dir_33 (/ p_30 len_32)
          (let anf_364 (* dir_33 3.)
           (let anf_365 (fbm_27 anf_364)
            (let terrain_34 (* anf_365 0.4)
             (let anf_366 (- len_32 radius_31) (return (- anf_366 terrain_34))))))))))
      : ((vec 3) -> (float -> float)))
     ((Define Nonrec (name map_35) (args ((p_36 (vec 3))))
       (body (return (sdPlanet_29 p_36 1.5))))
      : ((vec 3) -> float))
     ((Define Nonrec (name getNormal_37) (args ((p_38 (vec 3))))
       (body
        (let e_39 0.002
         (let e_x_40 (vec3 e_39 0 0)
          (let e_y_41 (vec3 0 e_39 0)
           (let e_z_42 (vec3 0 0 e_39)
            (let anf_367 (+ p_38 e_x_40)
             (let anf_368 (map_35 anf_367)
              (let anf_369 (- p_38 e_x_40)
               (let anf_370 (map_35 anf_369)
                (let dx_43 (- anf_368 anf_370)
                 (let anf_371 (+ p_38 e_y_41)
                  (let anf_372 (map_35 anf_371)
                   (let anf_373 (- p_38 e_y_41)
                    (let anf_374 (map_35 anf_373)
                     (let dy_44 (- anf_372 anf_374)
                      (let anf_375 (+ p_38 e_z_42)
                       (let anf_376 (map_35 anf_375)
                        (let anf_377 (- p_38 e_z_42)
                         (let anf_378 (map_35 anf_377)
                          (let dz_45 (- anf_376 anf_378)
                           (let anf_379 (vec3 dx_43 dy_44 dz_45)
                            (return (normalize anf_379))))))))))))))))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define (Rec 1000) (name march_49_311)
       (args ((rd_48 (vec 3)) (ro_47 (vec 3)) (t_50 float) (steps_51 int)))
       (body
        (let anf_380 (> steps_51 120)
         (return
          (if anf_380 (return t_50)
           (let anf_381 (* rd_48 t_50)
            (let anf_382 (+ ro_47 anf_381)
             (let d_52 (map_35 anf_382)
              (let anf_383 (< d_52 0.0005)
               (return
                (if anf_383 (return t_50)
                 (let anf_384 (> t_50 50.)
                  (return
                   (if anf_384 (return 50.1)
                    (let anf_385 (* d_52 0.8)
                     (let anf_386 (+ t_50 anf_385)
                      (let anf_387 (+ steps_51 1)
                       (return (march_49_311 rd_48 ro_47 anf_386 anf_387)))))))))))))))))))
      : (float -> (int -> float)))
     ((Define Nonrec (name march_46) (args ((ro_47 (vec 3)) (rd_48 (vec 3))))
       (body (return (march_49_311 rd_48 ro_47 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define Nonrec (name rotate_by_mouse_57_vec3_to_vec3_309_312)
       (args ((mouseUV_56 (vec 2)) (ray_58 (vec 3))))
       (body
        (let anf_388 (index mouseUV_56 1)
         (let anf_389 (* -1 anf_388)
          (let rotX_59 (* anf_389 1.5)
           (let anf_390 (index ray_58 1)
            (let anf_391 (index ray_58 2)
             (let anf_392 (vec2 anf_390 anf_391)
              (let ro_yz_60 (rotate_0 anf_392 rotX_59)
               (let anf_393 (index mouseUV_56 0)
                (let anf_394 (* -1 anf_393)
                 (let rotY_61 (* anf_394 1.5)
                  (let anf_395 (index ray_58 0)
                   (let anf_396 (index ro_yz_60 1)
                    (let anf_397 (vec2 anf_395 anf_396)
                     (let ro_xz_62 (rotate_0 anf_397 rotY_61)
                      (let anf_398 (index ro_xz_62 0)
                       (let anf_399 (index ro_yz_60 0)
                        (let anf_400 (index ro_xz_62 1)
                         (return (vec3 anf_398 anf_399 anf_400)))))))))))))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define Nonrec (name main) (args ((coord_53 (vec 2))))
       (body
        (let anf_401 (index u_resolution 0)
         (let anf_402 (index u_resolution 1)
          (let res_min_54 (min anf_401 anf_402)
           (let anf_403 (* coord_53 2.)
            (let anf_404 (- anf_403 u_resolution)
             (let uv_55 (/ anf_404 res_min_54)
              (let anf_405 (* u_mouse 2.)
               (let anf_406 (- anf_405 u_resolution)
                (let mouseUV_56 (/ anf_406 res_min_54)
                 (let anf_407 (vec3 0 0 -4)
                  (let ro_63
                   (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56 anf_407)
                   (let anf_408 (index uv_55 0)
                    (let anf_409 (index uv_55 1)
                     (let anf_410 (vec3 anf_408 anf_409 1.5)
                      (let anf_411 (normalize anf_410)
                       (let rd_64
                        (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56
                         anf_411)
                        (let t_65 (march_46 ro_63 rd_64)
                         (let anf_412 (> t_65 50.)
                          (return
                           (if anf_412 (return (vec3 0 0 0))
                            (let anf_413 (* rd_64 t_65)
                             (let hitPos_66 (+ ro_63 anf_413)
                              (let n_67 (getNormal_37 hitPos_66)
                               (let anf_414 (vec3 1. 0.8 -0.5)
                                (let lightDir_68 (normalize anf_414)
                                 (let anf_415 (dot n_67 lightDir_68)
                                  (let diff_69 (max anf_415 0)
                                   (let ambient_70 0.08
                                    (let anf_416 (length hitPos_66)
                                     (let dir_71 (/ hitPos_66 anf_416)
                                      (let anf_417 (* dir_71 3.)
                                       (let rawHeight_72 (fbm_27 anf_417)
                                        (let seaLevel_73 0.35
                                         (let anf_418
                                          (- rawHeight_72 seaLevel_73)
                                          (let anf_419 (- 1. seaLevel_73)
                                           (let anf_420 (/ anf_418 anf_419)
                                            (let h_norm_74 (clamp anf_420 0. 1.)
                                             (let deepColor_75
                                              (vec3 0.02 0.05 0.2)
                                              (let landColor_76
                                               (vec3 0.15 0.35 0.1)
                                               (let mountColor_77
                                                (vec3 0.4 0.3 0.2)
                                                (let snowColor_78
                                                 (vec3 0.85 0.85 0.9)
                                                 (let anf_421 (< h_norm_74 0.3)
                                                  (let baseColor_79
                                                   (if anf_421
                                                    (let anf_422
                                                     (/ h_norm_74 0.3)
                                                     (return
                                                      (mix deepColor_75
                                                       landColor_76 anf_422)))
                                                    (let anf_423
                                                     (< h_norm_74 0.6)
                                                     (return
                                                      (if anf_423
                                                       (let anf_424
                                                        (- h_norm_74 0.3)
                                                        (let anf_425
                                                         (/ anf_424 0.3)
                                                         (return
                                                          (mix landColor_76
                                                           mountColor_77 anf_425))))
                                                       (let anf_426
                                                        (- h_norm_74 0.6)
                                                        (let anf_427
                                                         (/ anf_426 0.4)
                                                         (return
                                                          (mix mountColor_77
                                                           snowColor_78 anf_427))))))))
                                                   (let anf_428 (* rd_64 -1.)
                                                    (let anf_429
                                                     (dot n_67 anf_428)
                                                     (let anf_430 (max anf_429 0)
                                                      (let fresnel_80
                                                       (- 1. anf_430)
                                                       (let anf_431
                                                        (* fresnel_80 fresnel_80)
                                                        (let anf_432
                                                         (* anf_431 fresnel_80)
                                                         (let rim_81
                                                          (* anf_432 0.4)
                                                          (let atmoColor_82
                                                           (vec3 0.3 0.5 1.)
                                                           (let anf_433
                                                            (* diff_69 0.9)
                                                            (let anf_434
                                                             (+ anf_433
                                                              ambient_70)
                                                             (let anf_435
                                                              (* baseColor_79
                                                               anf_434)
                                                              (let anf_436
                                                               (* atmoColor_82
                                                                rim_81)
                                                               (return
                                                                (+ anf_435
                                                                 anf_436)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (planet.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_313 (index p_1 0)
           (let anf_314 (* anf_313 c_4)
            (let anf_315 (index p_1 1)
             (let anf_316 (* anf_315 s_3)
              (let anf_317 (- anf_314 anf_316)
               (let anf_318 (index p_1 0)
                (let anf_319 (* anf_318 s_3)
                 (let anf_320 (index p_1 1)
                  (let anf_321 (* anf_320 c_4)
                   (let anf_322 (+ anf_319 anf_321)
                    (return (vec2 anf_317 anf_322))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define (name hash_10_310) (args ((p_11 (vec 3))))
       (body
        (let anf_323 (vec3 127.1 311.7 74.7)
         (let d_12 (dot p_11 anf_323)
          (let anf_324 (sin d_12)
           (let anf_325 (* anf_324 43758.5453) (return (fract anf_325))))))))
      : ((vec 3) -> float))
     ((Define (name noise3d_5) (args ((p_6 (vec 3))))
       (body
        (let i_7 (floor p_6)
         (let f_8 (fract p_6)
          (let anf_326 (* f_8 f_8)
           (let anf_327 (* 2. f_8)
            (let anf_328 (- 3. anf_327)
             (let u_9 (* anf_326 anf_328)
              (let a_13 (hash_10_310 i_7)
               (let anf_329 (vec3 1 0 0)
                (let anf_330 (+ i_7 anf_329)
                 (let b_14 (hash_10_310 anf_330)
                  (let anf_331 (vec3 0 1 0)
                   (let anf_332 (+ i_7 anf_331)
                    (let c_15 (hash_10_310 anf_332)
                     (let anf_333 (vec3 1 1 0)
                      (let anf_334 (+ i_7 anf_333)
                       (let d_16 (hash_10_310 anf_334)
                        (let anf_335 (vec3 0 0 1)
                         (let anf_336 (+ i_7 anf_335)
                          (let e_17 (hash_10_310 anf_336)
                           (let anf_337 (vec3 1 0 1)
                            (let anf_338 (+ i_7 anf_337)
                             (let f_18 (hash_10_310 anf_338)
                              (let anf_339 (vec3 0 1 1)
                               (let anf_340 (+ i_7 anf_339)
                                (let g_19 (hash_10_310 anf_340)
                                 (let anf_341 (vec3 1 1 1)
                                  (let anf_342 (+ i_7 anf_341)
                                   (let h_20 (hash_10_310 anf_342)
                                    (let anf_343 (index u_9 0)
                                     (let ab_21 (mix a_13 b_14 anf_343)
                                      (let anf_344 (index u_9 0)
                                       (let cd_22 (mix c_15 d_16 anf_344)
                                        (let anf_345 (index u_9 0)
                                         (let ef_23 (mix e_17 f_18 anf_345)
                                          (let anf_346 (index u_9 0)
                                           (let gh_24 (mix g_19 h_20 anf_346)
                                            (let anf_347 (index u_9 1)
                                             (let abcd_25
                                              (mix ab_21 cd_22 anf_347)
                                              (let anf_348 (index u_9 1)
                                               (let efgh_26
                                                (mix ef_23 gh_24 anf_348)
                                                (let anf_349 (index u_9 2)
                                                 (return
                                                  (mix abcd_25 efgh_26 anf_349)))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name fbm_27) (args ((p_28 (vec 3))))
       (body
        (let anf_350 (* p_28 1.)
         (let anf_351 (noise3d_5 anf_350)
          (let anf_352 (* anf_351 0.5)
           (let anf_353 (* p_28 2.)
            (let anf_354 (noise3d_5 anf_353)
             (let anf_355 (* anf_354 0.25)
              (let anf_356 (+ anf_352 anf_355)
               (let anf_357 (* p_28 4.)
                (let anf_358 (noise3d_5 anf_357)
                 (let anf_359 (* anf_358 0.125)
                  (let anf_360 (+ anf_356 anf_359)
                   (let anf_361 (* p_28 8.)
                    (let anf_362 (noise3d_5 anf_361)
                     (let anf_363 (* anf_362 0.0625)
                      (return (+ anf_360 anf_363))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name sdPlanet_29) (args ((p_30 (vec 3)) (radius_31 float)))
       (body
        (let len_32 (length p_30)
         (let dir_33 (/ p_30 len_32)
          (let anf_364 (* dir_33 3.)
           (let anf_365 (fbm_27 anf_364)
            (let terrain_34 (* anf_365 0.4)
             (let anf_366 (- len_32 radius_31) (return (- anf_366 terrain_34))))))))))
      : ((vec 3) -> (float -> float)))
     ((Define (name map_35) (args ((p_36 (vec 3))))
       (body (return (sdPlanet_29 p_36 1.5))))
      : ((vec 3) -> float))
     ((Define (name getNormal_37) (args ((p_38 (vec 3))))
       (body
        (let e_39 0.002
         (let e_x_40 (vec3 e_39 0 0)
          (let e_y_41 (vec3 0 e_39 0)
           (let e_z_42 (vec3 0 0 e_39)
            (let anf_367 (+ p_38 e_x_40)
             (let anf_368 (map_35 anf_367)
              (let anf_369 (- p_38 e_x_40)
               (let anf_370 (map_35 anf_369)
                (let dx_43 (- anf_368 anf_370)
                 (let anf_371 (+ p_38 e_y_41)
                  (let anf_372 (map_35 anf_371)
                   (let anf_373 (- p_38 e_y_41)
                    (let anf_374 (map_35 anf_373)
                     (let dy_44 (- anf_372 anf_374)
                      (let anf_375 (+ p_38 e_z_42)
                       (let anf_376 (map_35 anf_375)
                        (let anf_377 (- p_38 e_z_42)
                         (let anf_378 (map_35 anf_377)
                          (let dz_45 (- anf_376 anf_378)
                           (let anf_379 (vec3 dx_43 dy_44 dz_45)
                            (return (normalize anf_379))))))))))))))))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define (name march_49_311)
       (args ((rd_48 (vec 3)) (ro_47 (vec 3)) (t_50 float) (steps_51 int)))
       (body
        (let _iter_437 0
         (while (< _iter_437 1000)
          (let anf_380 (> steps_51 120)
           (return
            (if anf_380 (return t_50)
             (let anf_381 (* rd_48 t_50)
              (let anf_382 (+ ro_47 anf_381)
               (let d_52 (map_35 anf_382)
                (let anf_383 (< d_52 0.0005)
                 (return
                  (if anf_383 (return t_50)
                   (let anf_384 (> t_50 50.)
                    (return
                     (if anf_384 (return 50.1)
                      (let anf_385 (* d_52 0.8)
                       (let anf_386 (+ t_50 anf_385)
                        (let anf_387 (+ steps_51 1)
                         (set rd_48 rd_48
                          (set ro_47 ro_47
                           (set t_50 anf_386
                            (set steps_51 anf_387
                             (let _iter_inc_438 (+ _iter_437 1)
                              (set _iter_437 _iter_inc_438 continue)))))))))))))))))))))
          (return 0.)))))
      : (float -> (int -> float)))
     ((Define (name march_46) (args ((ro_47 (vec 3)) (rd_48 (vec 3))))
       (body (return (march_49_311 rd_48 ro_47 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define (name rotate_by_mouse_57_vec3_to_vec3_309_312)
       (args ((mouseUV_56 (vec 2)) (ray_58 (vec 3))))
       (body
        (let anf_388 (index mouseUV_56 1)
         (let anf_389 (* -1 anf_388)
          (let rotX_59 (* anf_389 1.5)
           (let anf_390 (index ray_58 1)
            (let anf_391 (index ray_58 2)
             (let anf_392 (vec2 anf_390 anf_391)
              (let ro_yz_60 (rotate_0 anf_392 rotX_59)
               (let anf_393 (index mouseUV_56 0)
                (let anf_394 (* -1 anf_393)
                 (let rotY_61 (* anf_394 1.5)
                  (let anf_395 (index ray_58 0)
                   (let anf_396 (index ro_yz_60 1)
                    (let anf_397 (vec2 anf_395 anf_396)
                     (let ro_xz_62 (rotate_0 anf_397 rotY_61)
                      (let anf_398 (index ro_xz_62 0)
                       (let anf_399 (index ro_yz_60 0)
                        (let anf_400 (index ro_xz_62 1)
                         (return (vec3 anf_398 anf_399 anf_400)))))))))))))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define (name main) (args ((coord_53 (vec 2))))
       (body
        (let anf_401 (index u_resolution 0)
         (let anf_402 (index u_resolution 1)
          (let res_min_54 (min anf_401 anf_402)
           (let anf_403 (* coord_53 2.)
            (let anf_404 (- anf_403 u_resolution)
             (let uv_55 (/ anf_404 res_min_54)
              (let anf_405 (* u_mouse 2.)
               (let anf_406 (- anf_405 u_resolution)
                (let mouseUV_56 (/ anf_406 res_min_54)
                 (let anf_407 (vec3 0 0 -4)
                  (let ro_63
                   (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56 anf_407)
                   (let anf_408 (index uv_55 0)
                    (let anf_409 (index uv_55 1)
                     (let anf_410 (vec3 anf_408 anf_409 1.5)
                      (let anf_411 (normalize anf_410)
                       (let rd_64
                        (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56
                         anf_411)
                        (let t_65 (march_46 ro_63 rd_64)
                         (let anf_412 (> t_65 50.)
                          (return
                           (if anf_412 (return (vec3 0 0 0))
                            (let anf_413 (* rd_64 t_65)
                             (let hitPos_66 (+ ro_63 anf_413)
                              (let n_67 (getNormal_37 hitPos_66)
                               (let anf_414 (vec3 1. 0.8 -0.5)
                                (let lightDir_68 (normalize anf_414)
                                 (let anf_415 (dot n_67 lightDir_68)
                                  (let diff_69 (max anf_415 0)
                                   (let ambient_70 0.08
                                    (let anf_416 (length hitPos_66)
                                     (let dir_71 (/ hitPos_66 anf_416)
                                      (let anf_417 (* dir_71 3.)
                                       (let rawHeight_72 (fbm_27 anf_417)
                                        (let seaLevel_73 0.35
                                         (let anf_418
                                          (- rawHeight_72 seaLevel_73)
                                          (let anf_419 (- 1. seaLevel_73)
                                           (let anf_420 (/ anf_418 anf_419)
                                            (let h_norm_74 (clamp anf_420 0. 1.)
                                             (let deepColor_75
                                              (vec3 0.02 0.05 0.2)
                                              (let landColor_76
                                               (vec3 0.15 0.35 0.1)
                                               (let mountColor_77
                                                (vec3 0.4 0.3 0.2)
                                                (let snowColor_78
                                                 (vec3 0.85 0.85 0.9)
                                                 (let anf_421 (< h_norm_74 0.3)
                                                  (let baseColor_79
                                                   (if anf_421
                                                    (let anf_422
                                                     (/ h_norm_74 0.3)
                                                     (return
                                                      (mix deepColor_75
                                                       landColor_76 anf_422)))
                                                    (let anf_423
                                                     (< h_norm_74 0.6)
                                                     (return
                                                      (if anf_423
                                                       (let anf_424
                                                        (- h_norm_74 0.3)
                                                        (let anf_425
                                                         (/ anf_424 0.3)
                                                         (return
                                                          (mix landColor_76
                                                           mountColor_77 anf_425))))
                                                       (let anf_426
                                                        (- h_norm_74 0.6)
                                                        (let anf_427
                                                         (/ anf_426 0.4)
                                                         (return
                                                          (mix mountColor_77
                                                           snowColor_78 anf_427))))))))
                                                   (let anf_428 (* rd_64 -1.)
                                                    (let anf_429
                                                     (dot n_67 anf_428)
                                                     (let anf_430 (max anf_429 0)
                                                      (let fresnel_80
                                                       (- 1. anf_430)
                                                       (let anf_431
                                                        (* fresnel_80 fresnel_80)
                                                        (let anf_432
                                                         (* anf_431 fresnel_80)
                                                         (let rim_81
                                                          (* anf_432 0.4)
                                                          (let atmoColor_82
                                                           (vec3 0.3 0.5 1.)
                                                           (let anf_433
                                                            (* diff_69 0.9)
                                                            (let anf_434
                                                             (+ anf_433
                                                              ambient_70)
                                                             (let anf_435
                                                              (* baseColor_79
                                                               anf_434)
                                                              (let anf_436
                                                               (* atmoColor_82
                                                                rim_81)
                                                               (return
                                                                (+ anf_435
                                                                 anf_436)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (planet.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_313 (index p_1 0)
           (let anf_314 (* anf_313 c_4)
            (let anf_315 (index p_1 1)
             (let anf_316 (* anf_315 s_3)
              (let anf_317 (- anf_314 anf_316)
               (let anf_318 (index p_1 0)
                (let anf_319 (* anf_318 s_3)
                 (let anf_320 (index p_1 1)
                  (let anf_321 (* anf_320 c_4)
                   (let anf_322 (+ anf_319 anf_321)
                    (return (vec2 anf_317 anf_322))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define (name hash_10_310) (args ((p_11 (vec 3))))
       (body
        (let anf_323 (vec3 127.1 311.7 74.7)
         (let d_12 (dot p_11 anf_323)
          (let anf_324 (sin d_12)
           (let anf_325 (* anf_324 43758.5453) (return (fract anf_325))))))))
      : ((vec 3) -> float))
     ((Define (name noise3d_5) (args ((p_6 (vec 3))))
       (body
        (let i_7 (floor p_6)
         (let f_8 (fract p_6)
          (let anf_326 (* f_8 f_8)
           (let anf_327 (* 2. f_8)
            (let anf_328 (- 3. anf_327)
             (let u_9 (* anf_326 anf_328)
              (let a_13 (hash_10_310 i_7)
               (let anf_329 (vec3 1 0 0)
                (let anf_330 (+ i_7 anf_329)
                 (let b_14 (hash_10_310 anf_330)
                  (let anf_331 (vec3 0 1 0)
                   (let anf_332 (+ i_7 anf_331)
                    (let c_15 (hash_10_310 anf_332)
                     (let anf_333 (vec3 1 1 0)
                      (let anf_334 (+ i_7 anf_333)
                       (let d_16 (hash_10_310 anf_334)
                        (let anf_335 (vec3 0 0 1)
                         (let anf_336 (+ i_7 anf_335)
                          (let e_17 (hash_10_310 anf_336)
                           (let anf_337 (vec3 1 0 1)
                            (let anf_338 (+ i_7 anf_337)
                             (let f_18 (hash_10_310 anf_338)
                              (let anf_339 (vec3 0 1 1)
                               (let anf_340 (+ i_7 anf_339)
                                (let g_19 (hash_10_310 anf_340)
                                 (let anf_341 (vec3 1 1 1)
                                  (let anf_342 (+ i_7 anf_341)
                                   (let h_20 (hash_10_310 anf_342)
                                    (let anf_343 (index u_9 0)
                                     (let ab_21 (mix a_13 b_14 anf_343)
                                      (let anf_344 (index u_9 0)
                                       (let cd_22 (mix c_15 d_16 anf_344)
                                        (let anf_345 (index u_9 0)
                                         (let ef_23 (mix e_17 f_18 anf_345)
                                          (let anf_346 (index u_9 0)
                                           (let gh_24 (mix g_19 h_20 anf_346)
                                            (let anf_347 (index u_9 1)
                                             (let abcd_25
                                              (mix ab_21 cd_22 anf_347)
                                              (let anf_348 (index u_9 1)
                                               (let efgh_26
                                                (mix ef_23 gh_24 anf_348)
                                                (let anf_349 (index u_9 2)
                                                 (return
                                                  (mix abcd_25 efgh_26 anf_349)))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name fbm_27) (args ((p_28 (vec 3))))
       (body
        (let anf_350 (* p_28 1.)
         (let anf_351 (noise3d_5 anf_350)
          (let anf_352 (* anf_351 0.5)
           (let anf_353 (* p_28 2.)
            (let anf_354 (noise3d_5 anf_353)
             (let anf_355 (* anf_354 0.25)
              (let anf_356 (+ anf_352 anf_355)
               (let anf_357 (* p_28 4.)
                (let anf_358 (noise3d_5 anf_357)
                 (let anf_359 (* anf_358 0.125)
                  (let anf_360 (+ anf_356 anf_359)
                   (let anf_361 (* p_28 8.)
                    (let anf_362 (noise3d_5 anf_361)
                     (let anf_363 (* anf_362 0.0625)
                      (return (+ anf_360 anf_363))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name sdPlanet_29) (args ((p_30 (vec 3)) (radius_31 float)))
       (body
        (let len_32 (length p_30)
         (let dir_33 (/ p_30 len_32)
          (let anf_364 (* dir_33 3.)
           (let anf_365 (fbm_27 anf_364)
            (let terrain_34 (* anf_365 0.4)
             (let anf_366 (- len_32 radius_31) (return (- anf_366 terrain_34))))))))))
      : ((vec 3) -> (float -> float)))
     ((Define (name map_35) (args ((p_36 (vec 3))))
       (body (return (sdPlanet_29 p_36 1.5))))
      : ((vec 3) -> float))
     ((Define (name getNormal_37) (args ((p_38 (vec 3))))
       (body
        (let e_39 0.002
         (let e_x_40 (vec3 e_39 0 0)
          (let e_y_41 (vec3 0 e_39 0)
           (let e_z_42 (vec3 0 0 e_39)
            (let anf_367 (+ p_38 e_x_40)
             (let anf_368 (map_35 anf_367)
              (let anf_369 (- p_38 e_x_40)
               (let anf_370 (map_35 anf_369)
                (let dx_43 (- anf_368 anf_370)
                 (let anf_371 (+ p_38 e_y_41)
                  (let anf_372 (map_35 anf_371)
                   (let anf_373 (- p_38 e_y_41)
                    (let anf_374 (map_35 anf_373)
                     (let dy_44 (- anf_372 anf_374)
                      (let anf_375 (+ p_38 e_z_42)
                       (let anf_376 (map_35 anf_375)
                        (let anf_377 (- p_38 e_z_42)
                         (let anf_378 (map_35 anf_377)
                          (let dz_45 (- anf_376 anf_378)
                           (let anf_379 (vec3 dx_43 dy_44 dz_45)
                            (return (normalize anf_379))))))))))))))))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define (name march_49_311)
       (args ((rd_48 (vec 3)) (ro_47 (vec 3)) (t_50 float) (steps_51 int)))
       (body
        (let _iter_437 0
         (while (< _iter_437 1000)
          (let anf_380 (> steps_51 120)
           (return
            (if anf_380 (return t_50)
             (let anf_381 (* rd_48 t_50)
              (let anf_382 (+ ro_47 anf_381)
               (let d_52 (map_35 anf_382)
                (let anf_383 (< d_52 0.0005)
                 (return
                  (if anf_383 (return t_50)
                   (let anf_384 (> t_50 50.)
                    (return
                     (if anf_384 (return 50.1)
                      (let anf_385 (* d_52 0.8)
                       (let anf_386 (+ t_50 anf_385)
                        (let anf_387 (+ steps_51 1)
                         (set rd_48 rd_48
                          (set ro_47 ro_47
                           (set t_50 anf_386
                            (set steps_51 anf_387
                             (let _iter_inc_438 (+ _iter_437 1)
                              (set _iter_437 _iter_inc_438 continue)))))))))))))))))))))
          (return 0.)))))
      : (float -> (int -> float)))
     ((Define (name march_46) (args ((ro_47 (vec 3)) (rd_48 (vec 3))))
       (body (return (march_49_311 rd_48 ro_47 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define (name rotate_by_mouse_57_vec3_to_vec3_309_312)
       (args ((mouseUV_56 (vec 2)) (ray_58 (vec 3))))
       (body
        (let anf_388 (index mouseUV_56 1)
         (let anf_389 (* -1 anf_388)
          (let rotX_59 (* anf_389 1.5)
           (let anf_390 (index ray_58 1)
            (let anf_391 (index ray_58 2)
             (let anf_392 (vec2 anf_390 anf_391)
              (let ro_yz_60 (rotate_0 anf_392 rotX_59)
               (let anf_393 (index mouseUV_56 0)
                (let anf_394 (* -1 anf_393)
                 (let rotY_61 (* anf_394 1.5)
                  (let anf_395 (index ray_58 0)
                   (let anf_396 (index ro_yz_60 1)
                    (let anf_397 (vec2 anf_395 anf_396)
                     (let ro_xz_62 (rotate_0 anf_397 rotY_61)
                      (let anf_398 (index ro_xz_62 0)
                       (let anf_399 (index ro_yz_60 0)
                        (let anf_400 (index ro_xz_62 1)
                         (return (vec3 anf_398 anf_399 anf_400)))))))))))))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define (name main) (args ((coord_53 (vec 2))))
       (body
        (let anf_401 (index u_resolution 0)
         (let anf_402 (index u_resolution 1)
          (let res_min_54 (min anf_401 anf_402)
           (let anf_403 (* coord_53 2.)
            (let anf_404 (- anf_403 u_resolution)
             (let uv_55 (/ anf_404 res_min_54)
              (let anf_405 (* u_mouse 2.)
               (let anf_406 (- anf_405 u_resolution)
                (let mouseUV_56 (/ anf_406 res_min_54)
                 (let anf_407 (vec3 0 0 -4)
                  (let ro_63
                   (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56 anf_407)
                   (let anf_408 (index uv_55 0)
                    (let anf_409 (index uv_55 1)
                     (let anf_410 (vec3 anf_408 anf_409 1.5)
                      (let anf_411 (normalize anf_410)
                       (let rd_64
                        (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56
                         anf_411)
                        (let t_65 (march_46 ro_63 rd_64)
                         (let anf_412 (> t_65 50.)
                          (return
                           (if anf_412 (return (vec3 0 0 0))
                            (let anf_413 (* rd_64 t_65)
                             (let hitPos_66 (+ ro_63 anf_413)
                              (let n_67 (getNormal_37 hitPos_66)
                               (let anf_414 (vec3 1. 0.8 -0.5)
                                (let lightDir_68 (normalize anf_414)
                                 (let anf_415 (dot n_67 lightDir_68)
                                  (let diff_69 (max anf_415 0)
                                   (let ambient_70 0.08
                                    (let anf_416 (length hitPos_66)
                                     (let dir_71 (/ hitPos_66 anf_416)
                                      (let anf_417 (* dir_71 3.)
                                       (let rawHeight_72 (fbm_27 anf_417)
                                        (let seaLevel_73 0.35
                                         (let anf_418
                                          (- rawHeight_72 seaLevel_73)
                                          (let anf_419 (- 1. seaLevel_73)
                                           (let anf_420 (/ anf_418 anf_419)
                                            (let h_norm_74 (clamp anf_420 0. 1.)
                                             (let deepColor_75
                                              (vec3 0.02 0.05 0.2)
                                              (let landColor_76
                                               (vec3 0.15 0.35 0.1)
                                               (let mountColor_77
                                                (vec3 0.4 0.3 0.2)
                                                (let snowColor_78
                                                 (vec3 0.85 0.85 0.9)
                                                 (let anf_421 (< h_norm_74 0.3)
                                                  (let baseColor_79
                                                   (if anf_421
                                                    (let anf_422
                                                     (/ h_norm_74 0.3)
                                                     (return
                                                      (mix deepColor_75
                                                       landColor_76 anf_422)))
                                                    (let anf_423
                                                     (< h_norm_74 0.6)
                                                     (return
                                                      (if anf_423
                                                       (let anf_424
                                                        (- h_norm_74 0.3)
                                                        (let anf_425
                                                         (/ anf_424 0.3)
                                                         (return
                                                          (mix landColor_76
                                                           mountColor_77 anf_425))))
                                                       (let anf_426
                                                        (- h_norm_74 0.6)
                                                        (let anf_427
                                                         (/ anf_426 0.4)
                                                         (return
                                                          (mix mountColor_77
                                                           snowColor_78 anf_427))))))))
                                                   (let anf_428 (* rd_64 -1.)
                                                    (let anf_429
                                                     (dot n_67 anf_428)
                                                     (let anf_430 (max anf_429 0)
                                                      (let fresnel_80
                                                       (- 1. anf_430)
                                                       (let anf_431
                                                        (* fresnel_80 fresnel_80)
                                                        (let anf_432
                                                         (* anf_431 fresnel_80)
                                                         (let rim_81
                                                          (* anf_432 0.4)
                                                          (let atmoColor_82
                                                           (vec3 0.3 0.5 1.)
                                                           (let anf_433
                                                            (* diff_69 0.9)
                                                            (let anf_434
                                                             (+ anf_433
                                                              ambient_70)
                                                             (let anf_435
                                                              (* baseColor_79
                                                               anf_434)
                                                              (let anf_436
                                                               (* atmoColor_82
                                                                rim_81)
                                                               (return
                                                                (+ anf_435
                                                                 anf_436)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (planet.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_313 (index p_1 0)
           (let anf_314 (* anf_313 c_4)
            (let anf_315 (index p_1 1)
             (let anf_316 (* anf_315 s_3)
              (let anf_317 (- anf_314 anf_316)
               (let anf_318 (index p_1 0)
                (let anf_319 (* anf_318 s_3)
                 (let anf_320 (index p_1 1)
                  (let anf_321 (* anf_320 c_4)
                   (let anf_322 (+ anf_319 anf_321)
                    (return (vec2 anf_317 anf_322))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define (name hash_10_310) (args ((p_11 (vec 3))))
       (body
        (let anf_323 (vec3 127.1 311.7 74.7)
         (let d_12 (dot p_11 anf_323)
          (let anf_324 (sin d_12)
           (let anf_325 (* anf_324 43758.5453) (return (fract anf_325))))))))
      : ((vec 3) -> float))
     ((Define (name noise3d_5) (args ((p_6 (vec 3))))
       (body
        (let i_7 (floor p_6)
         (let f_8 (fract p_6)
          (let anf_326 (* f_8 f_8)
           (let anf_327 (* 2. f_8)
            (let anf_328 (- 3. anf_327)
             (let u_9 (* anf_326 anf_328)
              (let a_13 (hash_10_310 i_7)
               (let anf_329 (vec3 1. 0. 0.)
                (let anf_330 (+ i_7 anf_329)
                 (let b_14 (hash_10_310 anf_330)
                  (let anf_331 (vec3 0. 1. 0.)
                   (let anf_332 (+ i_7 anf_331)
                    (let c_15 (hash_10_310 anf_332)
                     (let anf_333 (vec3 1. 1. 0.)
                      (let anf_334 (+ i_7 anf_333)
                       (let d_16 (hash_10_310 anf_334)
                        (let anf_335 (vec3 0. 0. 1.)
                         (let anf_336 (+ i_7 anf_335)
                          (let e_17 (hash_10_310 anf_336)
                           (let anf_337 (vec3 1. 0. 1.)
                            (let anf_338 (+ i_7 anf_337)
                             (let f_18 (hash_10_310 anf_338)
                              (let anf_339 (vec3 0. 1. 1.)
                               (let anf_340 (+ i_7 anf_339)
                                (let g_19 (hash_10_310 anf_340)
                                 (let anf_341 (vec3 1. 1. 1.)
                                  (let anf_342 (+ i_7 anf_341)
                                   (let h_20 (hash_10_310 anf_342)
                                    (let anf_343 (index u_9 0)
                                     (let ab_21 (mix a_13 b_14 anf_343)
                                      (let anf_344 (index u_9 0)
                                       (let cd_22 (mix c_15 d_16 anf_344)
                                        (let anf_345 (index u_9 0)
                                         (let ef_23 (mix e_17 f_18 anf_345)
                                          (let anf_346 (index u_9 0)
                                           (let gh_24 (mix g_19 h_20 anf_346)
                                            (let anf_347 (index u_9 1)
                                             (let abcd_25
                                              (mix ab_21 cd_22 anf_347)
                                              (let anf_348 (index u_9 1)
                                               (let efgh_26
                                                (mix ef_23 gh_24 anf_348)
                                                (let anf_349 (index u_9 2)
                                                 (return
                                                  (mix abcd_25 efgh_26 anf_349)))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name fbm_27) (args ((p_28 (vec 3))))
       (body
        (let anf_350 (* p_28 1.)
         (let anf_351 (noise3d_5 anf_350)
          (let anf_352 (* anf_351 0.5)
           (let anf_353 (* p_28 2.)
            (let anf_354 (noise3d_5 anf_353)
             (let anf_355 (* anf_354 0.25)
              (let anf_356 (+ anf_352 anf_355)
               (let anf_357 (* p_28 4.)
                (let anf_358 (noise3d_5 anf_357)
                 (let anf_359 (* anf_358 0.125)
                  (let anf_360 (+ anf_356 anf_359)
                   (let anf_361 (* p_28 8.)
                    (let anf_362 (noise3d_5 anf_361)
                     (let anf_363 (* anf_362 0.0625)
                      (return (+ anf_360 anf_363))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name sdPlanet_29) (args ((p_30 (vec 3)) (radius_31 float)))
       (body
        (let len_32 (length p_30)
         (let dir_33 (/ p_30 len_32)
          (let anf_364 (* dir_33 3.)
           (let anf_365 (fbm_27 anf_364)
            (let terrain_34 (* anf_365 0.4)
             (let anf_366 (- len_32 radius_31) (return (- anf_366 terrain_34))))))))))
      : ((vec 3) -> (float -> float)))
     ((Define (name map_35) (args ((p_36 (vec 3))))
       (body (return (sdPlanet_29 p_36 1.5))))
      : ((vec 3) -> float))
     ((Define (name getNormal_37) (args ((p_38 (vec 3))))
       (body
        (let e_39 0.002
         (let e_x_40 (vec3 e_39 0. 0.)
          (let e_y_41 (vec3 0. e_39 0.)
           (let e_z_42 (vec3 0. 0. e_39)
            (let anf_367 (+ p_38 e_x_40)
             (let anf_368 (map_35 anf_367)
              (let anf_369 (- p_38 e_x_40)
               (let anf_370 (map_35 anf_369)
                (let dx_43 (- anf_368 anf_370)
                 (let anf_371 (+ p_38 e_y_41)
                  (let anf_372 (map_35 anf_371)
                   (let anf_373 (- p_38 e_y_41)
                    (let anf_374 (map_35 anf_373)
                     (let dy_44 (- anf_372 anf_374)
                      (let anf_375 (+ p_38 e_z_42)
                       (let anf_376 (map_35 anf_375)
                        (let anf_377 (- p_38 e_z_42)
                         (let anf_378 (map_35 anf_377)
                          (let dz_45 (- anf_376 anf_378)
                           (let anf_379 (vec3 dx_43 dy_44 dz_45)
                            (return (normalize anf_379))))))))))))))))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define (name march_49_311)
       (args ((rd_48 (vec 3)) (ro_47 (vec 3)) (t_50 float) (steps_51 int)))
       (body
        (let _iter_437 0
         (while (< _iter_437 1000)
          (let anf_380 (> steps_51 120)
           (return
            (if anf_380 (return t_50)
             (let anf_381 (* rd_48 t_50)
              (let anf_382 (+ ro_47 anf_381)
               (let d_52 (map_35 anf_382)
                (let anf_383 (< d_52 0.0005)
                 (return
                  (if anf_383 (return t_50)
                   (let anf_384 (> t_50 50.)
                    (return
                     (if anf_384 (return 50.1)
                      (let anf_385 (* d_52 0.8)
                       (let anf_386 (+ t_50 anf_385)
                        (let anf_387 (+ steps_51 1)
                         (set rd_48 rd_48
                          (set ro_47 ro_47
                           (set t_50 anf_386
                            (set steps_51 anf_387
                             (let _iter_inc_438 (+ _iter_437 1)
                              (set _iter_437 _iter_inc_438 continue)))))))))))))))))))))
          (return 0.)))))
      : (float -> (int -> float)))
     ((Define (name march_46) (args ((ro_47 (vec 3)) (rd_48 (vec 3))))
       (body (return (march_49_311 rd_48 ro_47 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define (name rotate_by_mouse_57_vec3_to_vec3_309_312)
       (args ((mouseUV_56 (vec 2)) (ray_58 (vec 3))))
       (body
        (let anf_388 (index mouseUV_56 1)
         (let anf_389 (* -1. anf_388)
          (let rotX_59 (* anf_389 1.5)
           (let anf_390 (index ray_58 1)
            (let anf_391 (index ray_58 2)
             (let anf_392 (vec2 anf_390 anf_391)
              (let ro_yz_60 (rotate_0 anf_392 rotX_59)
               (let anf_393 (index mouseUV_56 0)
                (let anf_394 (* -1. anf_393)
                 (let rotY_61 (* anf_394 1.5)
                  (let anf_395 (index ray_58 0)
                   (let anf_396 (index ro_yz_60 1)
                    (let anf_397 (vec2 anf_395 anf_396)
                     (let ro_xz_62 (rotate_0 anf_397 rotY_61)
                      (let anf_398 (index ro_xz_62 0)
                       (let anf_399 (index ro_yz_60 0)
                        (let anf_400 (index ro_xz_62 1)
                         (return (vec3 anf_398 anf_399 anf_400)))))))))))))))))))))
      : ((vec 3) -> (vec 3)))
     ((Define (name main) (args ((coord_53 (vec 2))))
       (body
        (let anf_401 (index u_resolution 0)
         (let anf_402 (index u_resolution 1)
          (let res_min_54 (min anf_401 anf_402)
           (let anf_403 (* coord_53 2.)
            (let anf_404 (- anf_403 u_resolution)
             (let uv_55 (/ anf_404 res_min_54)
              (let anf_405 (* u_mouse 2.)
               (let anf_406 (- anf_405 u_resolution)
                (let mouseUV_56 (/ anf_406 res_min_54)
                 (let anf_407 (vec3 0. 0. -4.)
                  (let ro_63
                   (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56 anf_407)
                   (let anf_408 (index uv_55 0)
                    (let anf_409 (index uv_55 1)
                     (let anf_410 (vec3 anf_408 anf_409 1.5)
                      (let anf_411 (normalize anf_410)
                       (let rd_64
                        (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56
                         anf_411)
                        (let t_65 (march_46 ro_63 rd_64)
                         (let anf_412 (> t_65 50.)
                          (return
                           (if anf_412 (return (vec3 0. 0. 0.))
                            (let anf_413 (* rd_64 t_65)
                             (let hitPos_66 (+ ro_63 anf_413)
                              (let n_67 (getNormal_37 hitPos_66)
                               (let anf_414 (vec3 1. 0.8 -0.5)
                                (let lightDir_68 (normalize anf_414)
                                 (let anf_415 (dot n_67 lightDir_68)
                                  (let diff_69 (max anf_415 0.)
                                   (let ambient_70 0.08
                                    (let anf_416 (length hitPos_66)
                                     (let dir_71 (/ hitPos_66 anf_416)
                                      (let anf_417 (* dir_71 3.)
                                       (let rawHeight_72 (fbm_27 anf_417)
                                        (let seaLevel_73 0.35
                                         (let anf_418
                                          (- rawHeight_72 seaLevel_73)
                                          (let anf_419 (- 1. seaLevel_73)
                                           (let anf_420 (/ anf_418 anf_419)
                                            (let h_norm_74 (clamp anf_420 0. 1.)
                                             (let deepColor_75
                                              (vec3 0.02 0.05 0.2)
                                              (let landColor_76
                                               (vec3 0.15 0.35 0.1)
                                               (let mountColor_77
                                                (vec3 0.4 0.3 0.2)
                                                (let snowColor_78
                                                 (vec3 0.85 0.85 0.9)
                                                 (let anf_421 (< h_norm_74 0.3)
                                                  (let baseColor_79
                                                   (if anf_421
                                                    (let anf_422
                                                     (/ h_norm_74 0.3)
                                                     (return
                                                      (mix deepColor_75
                                                       landColor_76 anf_422)))
                                                    (let anf_423
                                                     (< h_norm_74 0.6)
                                                     (return
                                                      (if anf_423
                                                       (let anf_424
                                                        (- h_norm_74 0.3)
                                                        (let anf_425
                                                         (/ anf_424 0.3)
                                                         (return
                                                          (mix landColor_76
                                                           mountColor_77 anf_425))))
                                                       (let anf_426
                                                        (- h_norm_74 0.6)
                                                        (let anf_427
                                                         (/ anf_426 0.4)
                                                         (return
                                                          (mix mountColor_77
                                                           snowColor_78 anf_427))))))))
                                                   (let anf_428 (* rd_64 -1.)
                                                    (let anf_429
                                                     (dot n_67 anf_428)
                                                     (let anf_430
                                                      (max anf_429 0.)
                                                      (let fresnel_80
                                                       (- 1. anf_430)
                                                       (let anf_431
                                                        (* fresnel_80 fresnel_80)
                                                        (let anf_432
                                                         (* anf_431 fresnel_80)
                                                         (let rim_81
                                                          (* anf_432 0.4)
                                                          (let atmoColor_82
                                                           (vec3 0.3 0.5 1.)
                                                           (let anf_433
                                                            (* diff_69 0.9)
                                                            (let anf_434
                                                             (+ anf_433
                                                              ambient_70)
                                                             (let anf_435
                                                              (* baseColor_79
                                                               anf_434)
                                                              (let anf_436
                                                               (* atmoColor_82
                                                                rim_81)
                                                               (return
                                                                (+ anf_435
                                                                 anf_436)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (planet.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ()) (Global Uniform (TyVec 2) u_mouse ())
      (Function (name rotate_0) (desc ())
       (params (((TyVec 2) p_1) (TyFloat angle_2))) (ret_type (TyVec 2))
       (body
        ((set () float s_3 (sin angle_2)) (set () float c_4 (cos angle_2))
         (set () float anf_313 (index p_1 0))
         (set () float anf_314 (* anf_313 c_4))
         (set () float anf_315 (index p_1 1))
         (set () float anf_316 (* anf_315 s_3))
         (set () float anf_317 (- anf_314 anf_316))
         (set () float anf_318 (index p_1 0))
         (set () float anf_319 (* anf_318 s_3))
         (set () float anf_320 (index p_1 1))
         (set () float anf_321 (* anf_320 c_4))
         (set () float anf_322 (+ anf_319 anf_321))
         (return (vec2 anf_317 anf_322)))))
      (Function (name hash_10_310) (desc ()) (params (((TyVec 3) p_11)))
       (ret_type TyFloat)
       (body
        ((set () vec3 anf_323 (vec3 127.1 311.7 74.7))
         (set () float d_12 (dot p_11 anf_323)) (set () float anf_324 (sin d_12))
         (set () float anf_325 (* anf_324 43758.5453)) (return (fract anf_325)))))
      (Function (name noise3d_5) (desc ()) (params (((TyVec 3) p_6)))
       (ret_type TyFloat)
       (body
        ((set () vec3 i_7 (floor p_6)) (set () vec3 f_8 (fract p_6))
         (set () vec3 anf_326 (* f_8 f_8)) (set () vec3 anf_327 (* 2. f_8))
         (set () vec3 anf_328 (- 3. anf_327))
         (set () vec3 u_9 (* anf_326 anf_328))
         (set () float a_13 (hash_10_310 i_7))
         (set () vec3 anf_329 (vec3 1. 0. 0.))
         (set () vec3 anf_330 (+ i_7 anf_329))
         (set () float b_14 (hash_10_310 anf_330))
         (set () vec3 anf_331 (vec3 0. 1. 0.))
         (set () vec3 anf_332 (+ i_7 anf_331))
         (set () float c_15 (hash_10_310 anf_332))
         (set () vec3 anf_333 (vec3 1. 1. 0.))
         (set () vec3 anf_334 (+ i_7 anf_333))
         (set () float d_16 (hash_10_310 anf_334))
         (set () vec3 anf_335 (vec3 0. 0. 1.))
         (set () vec3 anf_336 (+ i_7 anf_335))
         (set () float e_17 (hash_10_310 anf_336))
         (set () vec3 anf_337 (vec3 1. 0. 1.))
         (set () vec3 anf_338 (+ i_7 anf_337))
         (set () float f_18 (hash_10_310 anf_338))
         (set () vec3 anf_339 (vec3 0. 1. 1.))
         (set () vec3 anf_340 (+ i_7 anf_339))
         (set () float g_19 (hash_10_310 anf_340))
         (set () vec3 anf_341 (vec3 1. 1. 1.))
         (set () vec3 anf_342 (+ i_7 anf_341))
         (set () float h_20 (hash_10_310 anf_342))
         (set () float anf_343 (index u_9 0))
         (set () float ab_21 (mix a_13 b_14 anf_343))
         (set () float anf_344 (index u_9 0))
         (set () float cd_22 (mix c_15 d_16 anf_344))
         (set () float anf_345 (index u_9 0))
         (set () float ef_23 (mix e_17 f_18 anf_345))
         (set () float anf_346 (index u_9 0))
         (set () float gh_24 (mix g_19 h_20 anf_346))
         (set () float anf_347 (index u_9 1))
         (set () float abcd_25 (mix ab_21 cd_22 anf_347))
         (set () float anf_348 (index u_9 1))
         (set () float efgh_26 (mix ef_23 gh_24 anf_348))
         (set () float anf_349 (index u_9 2))
         (return (mix abcd_25 efgh_26 anf_349)))))
      (Function (name fbm_27) (desc ()) (params (((TyVec 3) p_28)))
       (ret_type TyFloat)
       (body
        ((set () vec3 anf_350 (* p_28 1.))
         (set () float anf_351 (noise3d_5 anf_350))
         (set () float anf_352 (* anf_351 0.5)) (set () vec3 anf_353 (* p_28 2.))
         (set () float anf_354 (noise3d_5 anf_353))
         (set () float anf_355 (* anf_354 0.25))
         (set () float anf_356 (+ anf_352 anf_355))
         (set () vec3 anf_357 (* p_28 4.))
         (set () float anf_358 (noise3d_5 anf_357))
         (set () float anf_359 (* anf_358 0.125))
         (set () float anf_360 (+ anf_356 anf_359))
         (set () vec3 anf_361 (* p_28 8.))
         (set () float anf_362 (noise3d_5 anf_361))
         (set () float anf_363 (* anf_362 0.0625)) (return (+ anf_360 anf_363)))))
      (Function (name sdPlanet_29) (desc ())
       (params (((TyVec 3) p_30) (TyFloat radius_31))) (ret_type TyFloat)
       (body
        ((set () float len_32 (length p_30)) (set () vec3 dir_33 (/ p_30 len_32))
         (set () vec3 anf_364 (* dir_33 3.))
         (set () float anf_365 (fbm_27 anf_364))
         (set () float terrain_34 (* anf_365 0.4))
         (set () float anf_366 (- len_32 radius_31))
         (return (- anf_366 terrain_34)))))
      (Function (name map_35) (desc ()) (params (((TyVec 3) p_36)))
       (ret_type TyFloat) (body ((return (sdPlanet_29 p_36 1.5)))))
      (Function (name getNormal_37) (desc ()) (params (((TyVec 3) p_38)))
       (ret_type (TyVec 3))
       (body
        ((set () float e_39 0.002) (set () vec3 e_x_40 (vec3 e_39 0. 0.))
         (set () vec3 e_y_41 (vec3 0. e_39 0.))
         (set () vec3 e_z_42 (vec3 0. 0. e_39))
         (set () vec3 anf_367 (+ p_38 e_x_40))
         (set () float anf_368 (map_35 anf_367))
         (set () vec3 anf_369 (- p_38 e_x_40))
         (set () float anf_370 (map_35 anf_369))
         (set () float dx_43 (- anf_368 anf_370))
         (set () vec3 anf_371 (+ p_38 e_y_41))
         (set () float anf_372 (map_35 anf_371))
         (set () vec3 anf_373 (- p_38 e_y_41))
         (set () float anf_374 (map_35 anf_373))
         (set () float dy_44 (- anf_372 anf_374))
         (set () vec3 anf_375 (+ p_38 e_z_42))
         (set () float anf_376 (map_35 anf_375))
         (set () vec3 anf_377 (- p_38 e_z_42))
         (set () float anf_378 (map_35 anf_377))
         (set () float dz_45 (- anf_376 anf_378))
         (set () vec3 anf_379 (vec3 dx_43 dy_44 dz_45))
         (return (normalize anf_379)))))
      (Function (name march_49_311) (desc ())
       (params
        (((TyVec 3) rd_48) ((TyVec 3) ro_47) (TyFloat t_50) (TyInt steps_51)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_437 0)
         (while (< _iter_437 1000)
          (Block (set () bool anf_380 (> steps_51 120))
           (if anf_380 (Block (return t_50))
            (Block (set () vec3 anf_381 (* rd_48 t_50))
             (set () vec3 anf_382 (+ ro_47 anf_381))
             (set () float d_52 (map_35 anf_382))
             (set () bool anf_383 (< d_52 0.0005))
             (if anf_383 (Block (return t_50))
              (Block (set () bool anf_384 (> t_50 50.))
               (if anf_384 (Block (return 50.1))
                (Block (set () float anf_385 (* d_52 0.8))
                 (set () float anf_386 (+ t_50 anf_385))
                 (set () int anf_387 (+ steps_51 1)) (set rd_48 rd_48)
                 (set ro_47 ro_47) (set t_50 anf_386) (set steps_51 anf_387)
                 (set () int _iter_inc_438 (+ _iter_437 1))
                 (set _iter_437 _iter_inc_438) continue))))))))
         (return 0.))))
      (Function (name march_46) (desc ())
       (params (((TyVec 3) ro_47) ((TyVec 3) rd_48))) (ret_type TyFloat)
       (body ((return (march_49_311 rd_48 ro_47 0. 0)))))
      (Function (name rotate_by_mouse_57_vec3_to_vec3_309_312) (desc ())
       (params (((TyVec 2) mouseUV_56) ((TyVec 3) ray_58))) (ret_type (TyVec 3))
       (body
        ((set () float anf_388 (index mouseUV_56 1))
         (set () float anf_389 (* -1. anf_388))
         (set () float rotX_59 (* anf_389 1.5))
         (set () float anf_390 (index ray_58 1))
         (set () float anf_391 (index ray_58 2))
         (set () vec2 anf_392 (vec2 anf_390 anf_391))
         (set () vec2 ro_yz_60 (rotate_0 anf_392 rotX_59))
         (set () float anf_393 (index mouseUV_56 0))
         (set () float anf_394 (* -1. anf_393))
         (set () float rotY_61 (* anf_394 1.5))
         (set () float anf_395 (index ray_58 0))
         (set () float anf_396 (index ro_yz_60 1))
         (set () vec2 anf_397 (vec2 anf_395 anf_396))
         (set () vec2 ro_xz_62 (rotate_0 anf_397 rotY_61))
         (set () float anf_398 (index ro_xz_62 0))
         (set () float anf_399 (index ro_yz_60 0))
         (set () float anf_400 (index ro_xz_62 1))
         (return (vec3 anf_398 anf_399 anf_400)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_53)))
       (ret_type (TyVec 3))
       (body
        ((set () float anf_401 (index u_resolution 0))
         (set () float anf_402 (index u_resolution 1))
         (set () float res_min_54 (min anf_401 anf_402))
         (set () vec2 anf_403 (* coord_53 2.))
         (set () vec2 anf_404 (- anf_403 u_resolution))
         (set () vec2 uv_55 (/ anf_404 res_min_54))
         (set () vec2 anf_405 (* u_mouse 2.))
         (set () vec2 anf_406 (- anf_405 u_resolution))
         (set () vec2 mouseUV_56 (/ anf_406 res_min_54))
         (set () vec3 anf_407 (vec3 0. 0. -4.))
         (set () vec3 ro_63
          (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56 anf_407))
         (set () float anf_408 (index uv_55 0))
         (set () float anf_409 (index uv_55 1))
         (set () vec3 anf_410 (vec3 anf_408 anf_409 1.5))
         (set () vec3 anf_411 (normalize anf_410))
         (set () vec3 rd_64
          (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56 anf_411))
         (set () float t_65 (march_46 ro_63 rd_64))
         (set () bool anf_412 (> t_65 50.))
         (if anf_412 (Block (return (vec3 0. 0. 0.)))
          (Block (set () vec3 anf_413 (* rd_64 t_65))
           (set () vec3 hitPos_66 (+ ro_63 anf_413))
           (set () vec3 n_67 (getNormal_37 hitPos_66))
           (set () vec3 anf_414 (vec3 1. 0.8 -0.5))
           (set () vec3 lightDir_68 (normalize anf_414))
           (set () float anf_415 (dot n_67 lightDir_68))
           (set () float diff_69 (max anf_415 0.)) (set () float ambient_70 0.08)
           (set () float anf_416 (length hitPos_66))
           (set () vec3 dir_71 (/ hitPos_66 anf_416))
           (set () vec3 anf_417 (* dir_71 3.))
           (set () float rawHeight_72 (fbm_27 anf_417))
           (set () float seaLevel_73 0.35)
           (set () float anf_418 (- rawHeight_72 seaLevel_73))
           (set () float anf_419 (- 1. seaLevel_73))
           (set () float anf_420 (/ anf_418 anf_419))
           (set () float h_norm_74 (clamp anf_420 0. 1.))
           (set () vec3 deepColor_75 (vec3 0.02 0.05 0.2))
           (set () vec3 landColor_76 (vec3 0.15 0.35 0.1))
           (set () vec3 mountColor_77 (vec3 0.4 0.3 0.2))
           (set () vec3 snowColor_78 (vec3 0.85 0.85 0.9))
           (set () bool anf_421 (< h_norm_74 0.3))
           (set () vec3 baseColor_79 (vec3 0.))
           (if anf_421
            (Block (set () float anf_422 (/ h_norm_74 0.3))
             (set baseColor_79 (mix deepColor_75 landColor_76 anf_422)))
            (Block (set () bool anf_423 (< h_norm_74 0.6))
             (if anf_423
              (Block (set () float anf_424 (- h_norm_74 0.3))
               (set () float anf_425 (/ anf_424 0.3))
               (set baseColor_79 (mix landColor_76 mountColor_77 anf_425)))
              (Block (set () float anf_426 (- h_norm_74 0.6))
               (set () float anf_427 (/ anf_426 0.4))
               (set baseColor_79 (mix mountColor_77 snowColor_78 anf_427))))))
           (set () vec3 anf_428 (* rd_64 -1.))
           (set () float anf_429 (dot n_67 anf_428))
           (set () float anf_430 (max anf_429 0.))
           (set () float fresnel_80 (- 1. anf_430))
           (set () float anf_431 (* fresnel_80 fresnel_80))
           (set () float anf_432 (* anf_431 fresnel_80))
           (set () float rim_81 (* anf_432 0.4))
           (set () vec3 atmoColor_82 (vec3 0.3 0.5 1.))
           (set () float anf_433 (* diff_69 0.9))
           (set () float anf_434 (+ anf_433 ambient_70))
           (set () vec3 anf_435 (* baseColor_79 anf_434))
           (set () vec3 anf_436 (* atmoColor_82 rim_81))
           (return (+ anf_435 anf_436)))))))))

    === patch main (planet.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ()) (Global Uniform (TyVec 2) u_mouse ())
      (Function (name rotate_0) (desc ())
       (params (((TyVec 2) p_1) (TyFloat angle_2))) (ret_type (TyVec 2))
       (body
        ((set () float s_3 (sin angle_2)) (set () float c_4 (cos angle_2))
         (set () float anf_313 (index p_1 0))
         (set () float anf_314 (* anf_313 c_4))
         (set () float anf_315 (index p_1 1))
         (set () float anf_316 (* anf_315 s_3))
         (set () float anf_317 (- anf_314 anf_316))
         (set () float anf_318 (index p_1 0))
         (set () float anf_319 (* anf_318 s_3))
         (set () float anf_320 (index p_1 1))
         (set () float anf_321 (* anf_320 c_4))
         (set () float anf_322 (+ anf_319 anf_321))
         (return (vec2 anf_317 anf_322)))))
      (Function (name hash_10_310) (desc ()) (params (((TyVec 3) p_11)))
       (ret_type TyFloat)
       (body
        ((set () vec3 anf_323 (vec3 127.1 311.7 74.7))
         (set () float d_12 (dot p_11 anf_323)) (set () float anf_324 (sin d_12))
         (set () float anf_325 (* anf_324 43758.5453)) (return (fract anf_325)))))
      (Function (name noise3d_5) (desc ()) (params (((TyVec 3) p_6)))
       (ret_type TyFloat)
       (body
        ((set () vec3 i_7 (floor p_6)) (set () vec3 f_8 (fract p_6))
         (set () vec3 anf_326 (* f_8 f_8)) (set () vec3 anf_327 (* 2. f_8))
         (set () vec3 anf_328 (- 3. anf_327))
         (set () vec3 u_9 (* anf_326 anf_328))
         (set () float a_13 (hash_10_310 i_7))
         (set () vec3 anf_329 (vec3 1. 0. 0.))
         (set () vec3 anf_330 (+ i_7 anf_329))
         (set () float b_14 (hash_10_310 anf_330))
         (set () vec3 anf_331 (vec3 0. 1. 0.))
         (set () vec3 anf_332 (+ i_7 anf_331))
         (set () float c_15 (hash_10_310 anf_332))
         (set () vec3 anf_333 (vec3 1. 1. 0.))
         (set () vec3 anf_334 (+ i_7 anf_333))
         (set () float d_16 (hash_10_310 anf_334))
         (set () vec3 anf_335 (vec3 0. 0. 1.))
         (set () vec3 anf_336 (+ i_7 anf_335))
         (set () float e_17 (hash_10_310 anf_336))
         (set () vec3 anf_337 (vec3 1. 0. 1.))
         (set () vec3 anf_338 (+ i_7 anf_337))
         (set () float f_18 (hash_10_310 anf_338))
         (set () vec3 anf_339 (vec3 0. 1. 1.))
         (set () vec3 anf_340 (+ i_7 anf_339))
         (set () float g_19 (hash_10_310 anf_340))
         (set () vec3 anf_341 (vec3 1. 1. 1.))
         (set () vec3 anf_342 (+ i_7 anf_341))
         (set () float h_20 (hash_10_310 anf_342))
         (set () float anf_343 (index u_9 0))
         (set () float ab_21 (mix a_13 b_14 anf_343))
         (set () float anf_344 (index u_9 0))
         (set () float cd_22 (mix c_15 d_16 anf_344))
         (set () float anf_345 (index u_9 0))
         (set () float ef_23 (mix e_17 f_18 anf_345))
         (set () float anf_346 (index u_9 0))
         (set () float gh_24 (mix g_19 h_20 anf_346))
         (set () float anf_347 (index u_9 1))
         (set () float abcd_25 (mix ab_21 cd_22 anf_347))
         (set () float anf_348 (index u_9 1))
         (set () float efgh_26 (mix ef_23 gh_24 anf_348))
         (set () float anf_349 (index u_9 2))
         (return (mix abcd_25 efgh_26 anf_349)))))
      (Function (name fbm_27) (desc ()) (params (((TyVec 3) p_28)))
       (ret_type TyFloat)
       (body
        ((set () vec3 anf_350 (* p_28 1.))
         (set () float anf_351 (noise3d_5 anf_350))
         (set () float anf_352 (* anf_351 0.5)) (set () vec3 anf_353 (* p_28 2.))
         (set () float anf_354 (noise3d_5 anf_353))
         (set () float anf_355 (* anf_354 0.25))
         (set () float anf_356 (+ anf_352 anf_355))
         (set () vec3 anf_357 (* p_28 4.))
         (set () float anf_358 (noise3d_5 anf_357))
         (set () float anf_359 (* anf_358 0.125))
         (set () float anf_360 (+ anf_356 anf_359))
         (set () vec3 anf_361 (* p_28 8.))
         (set () float anf_362 (noise3d_5 anf_361))
         (set () float anf_363 (* anf_362 0.0625)) (return (+ anf_360 anf_363)))))
      (Function (name sdPlanet_29) (desc ())
       (params (((TyVec 3) p_30) (TyFloat radius_31))) (ret_type TyFloat)
       (body
        ((set () float len_32 (length p_30)) (set () vec3 dir_33 (/ p_30 len_32))
         (set () vec3 anf_364 (* dir_33 3.))
         (set () float anf_365 (fbm_27 anf_364))
         (set () float terrain_34 (* anf_365 0.4))
         (set () float anf_366 (- len_32 radius_31))
         (return (- anf_366 terrain_34)))))
      (Function (name map_35) (desc ()) (params (((TyVec 3) p_36)))
       (ret_type TyFloat) (body ((return (sdPlanet_29 p_36 1.5)))))
      (Function (name getNormal_37) (desc ()) (params (((TyVec 3) p_38)))
       (ret_type (TyVec 3))
       (body
        ((set () float e_39 0.002) (set () vec3 e_x_40 (vec3 e_39 0. 0.))
         (set () vec3 e_y_41 (vec3 0. e_39 0.))
         (set () vec3 e_z_42 (vec3 0. 0. e_39))
         (set () vec3 anf_367 (+ p_38 e_x_40))
         (set () float anf_368 (map_35 anf_367))
         (set () vec3 anf_369 (- p_38 e_x_40))
         (set () float anf_370 (map_35 anf_369))
         (set () float dx_43 (- anf_368 anf_370))
         (set () vec3 anf_371 (+ p_38 e_y_41))
         (set () float anf_372 (map_35 anf_371))
         (set () vec3 anf_373 (- p_38 e_y_41))
         (set () float anf_374 (map_35 anf_373))
         (set () float dy_44 (- anf_372 anf_374))
         (set () vec3 anf_375 (+ p_38 e_z_42))
         (set () float anf_376 (map_35 anf_375))
         (set () vec3 anf_377 (- p_38 e_z_42))
         (set () float anf_378 (map_35 anf_377))
         (set () float dz_45 (- anf_376 anf_378))
         (set () vec3 anf_379 (vec3 dx_43 dy_44 dz_45))
         (return (normalize anf_379)))))
      (Function (name march_49_311) (desc ())
       (params
        (((TyVec 3) rd_48) ((TyVec 3) ro_47) (TyFloat t_50) (TyInt steps_51)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_437 0)
         (while (< _iter_437 1000)
          (Block (set () bool anf_380 (> steps_51 120))
           (if anf_380 (Block (return t_50))
            (Block (set () vec3 anf_381 (* rd_48 t_50))
             (set () vec3 anf_382 (+ ro_47 anf_381))
             (set () float d_52 (map_35 anf_382))
             (set () bool anf_383 (< d_52 0.0005))
             (if anf_383 (Block (return t_50))
              (Block (set () bool anf_384 (> t_50 50.))
               (if anf_384 (Block (return 50.1))
                (Block (set () float anf_385 (* d_52 0.8))
                 (set () float anf_386 (+ t_50 anf_385))
                 (set () int anf_387 (+ steps_51 1)) (set rd_48 rd_48)
                 (set ro_47 ro_47) (set t_50 anf_386) (set steps_51 anf_387)
                 (set () int _iter_inc_438 (+ _iter_437 1))
                 (set _iter_437 _iter_inc_438) continue))))))))
         (return 0.))))
      (Function (name march_46) (desc ())
       (params (((TyVec 3) ro_47) ((TyVec 3) rd_48))) (ret_type TyFloat)
       (body ((return (march_49_311 rd_48 ro_47 0. 0)))))
      (Function (name rotate_by_mouse_57_vec3_to_vec3_309_312) (desc ())
       (params (((TyVec 2) mouseUV_56) ((TyVec 3) ray_58))) (ret_type (TyVec 3))
       (body
        ((set () float anf_388 (index mouseUV_56 1))
         (set () float anf_389 (* -1. anf_388))
         (set () float rotX_59 (* anf_389 1.5))
         (set () float anf_390 (index ray_58 1))
         (set () float anf_391 (index ray_58 2))
         (set () vec2 anf_392 (vec2 anf_390 anf_391))
         (set () vec2 ro_yz_60 (rotate_0 anf_392 rotX_59))
         (set () float anf_393 (index mouseUV_56 0))
         (set () float anf_394 (* -1. anf_393))
         (set () float rotY_61 (* anf_394 1.5))
         (set () float anf_395 (index ray_58 0))
         (set () float anf_396 (index ro_yz_60 1))
         (set () vec2 anf_397 (vec2 anf_395 anf_396))
         (set () vec2 ro_xz_62 (rotate_0 anf_397 rotY_61))
         (set () float anf_398 (index ro_xz_62 0))
         (set () float anf_399 (index ro_yz_60 0))
         (set () float anf_400 (index ro_xz_62 1))
         (return (vec3 anf_398 anf_399 anf_400)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_53)))
       (ret_type (TyVec 3))
       (body
        ((set () float anf_401 (index u_resolution 0))
         (set () float anf_402 (index u_resolution 1))
         (set () float res_min_54 (min anf_401 anf_402))
         (set () vec2 anf_403 (* coord_53 2.))
         (set () vec2 anf_404 (- anf_403 u_resolution))
         (set () vec2 uv_55 (/ anf_404 res_min_54))
         (set () vec2 anf_405 (* u_mouse 2.))
         (set () vec2 anf_406 (- anf_405 u_resolution))
         (set () vec2 mouseUV_56 (/ anf_406 res_min_54))
         (set () vec3 anf_407 (vec3 0. 0. -4.))
         (set () vec3 ro_63
          (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56 anf_407))
         (set () float anf_408 (index uv_55 0))
         (set () float anf_409 (index uv_55 1))
         (set () vec3 anf_410 (vec3 anf_408 anf_409 1.5))
         (set () vec3 anf_411 (normalize anf_410))
         (set () vec3 rd_64
          (rotate_by_mouse_57_vec3_to_vec3_309_312 mouseUV_56 anf_411))
         (set () float t_65 (march_46 ro_63 rd_64))
         (set () bool anf_412 (> t_65 50.))
         (if anf_412 (Block (return (vec3 0. 0. 0.)))
          (Block (set () vec3 anf_413 (* rd_64 t_65))
           (set () vec3 hitPos_66 (+ ro_63 anf_413))
           (set () vec3 n_67 (getNormal_37 hitPos_66))
           (set () vec3 anf_414 (vec3 1. 0.8 -0.5))
           (set () vec3 lightDir_68 (normalize anf_414))
           (set () float anf_415 (dot n_67 lightDir_68))
           (set () float diff_69 (max anf_415 0.)) (set () float ambient_70 0.08)
           (set () float anf_416 (length hitPos_66))
           (set () vec3 dir_71 (/ hitPos_66 anf_416))
           (set () vec3 anf_417 (* dir_71 3.))
           (set () float rawHeight_72 (fbm_27 anf_417))
           (set () float seaLevel_73 0.35)
           (set () float anf_418 (- rawHeight_72 seaLevel_73))
           (set () float anf_419 (- 1. seaLevel_73))
           (set () float anf_420 (/ anf_418 anf_419))
           (set () float h_norm_74 (clamp anf_420 0. 1.))
           (set () vec3 deepColor_75 (vec3 0.02 0.05 0.2))
           (set () vec3 landColor_76 (vec3 0.15 0.35 0.1))
           (set () vec3 mountColor_77 (vec3 0.4 0.3 0.2))
           (set () vec3 snowColor_78 (vec3 0.85 0.85 0.9))
           (set () bool anf_421 (< h_norm_74 0.3))
           (set () vec3 baseColor_79 (vec3 0.))
           (if anf_421
            (Block (set () float anf_422 (/ h_norm_74 0.3))
             (set baseColor_79 (mix deepColor_75 landColor_76 anf_422)))
            (Block (set () bool anf_423 (< h_norm_74 0.6))
             (if anf_423
              (Block (set () float anf_424 (- h_norm_74 0.3))
               (set () float anf_425 (/ anf_424 0.3))
               (set baseColor_79 (mix landColor_76 mountColor_77 anf_425)))
              (Block (set () float anf_426 (- h_norm_74 0.6))
               (set () float anf_427 (/ anf_426 0.4))
               (set baseColor_79 (mix mountColor_77 snowColor_78 anf_427))))))
           (set () vec3 anf_428 (* rd_64 -1.))
           (set () float anf_429 (dot n_67 anf_428))
           (set () float anf_430 (max anf_429 0.))
           (set () float fresnel_80 (- 1. anf_430))
           (set () float anf_431 (* fresnel_80 fresnel_80))
           (set () float anf_432 (* anf_431 fresnel_80))
           (set () float rim_81 (* anf_432 0.4))
           (set () vec3 atmoColor_82 (vec3 0.3 0.5 1.))
           (set () float anf_433 (* diff_69 0.9))
           (set () float anf_434 (+ anf_433 ambient_70))
           (set () vec3 anf_435 (* baseColor_79 anf_434))
           (set () vec3 anf_436 (* atmoColor_82 rim_81))
           (return (+ anf_435 anf_436)))))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE rainbow.glml ======

    === stlc (rainbow.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec get_uv
       (lambda (coord ())
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
       (lambda (coord_1 ())
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
        ((lambda coord_1
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_10)) : 'v_11)
              (u_resolution : (vec 2)))
             : 'v_12)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_12) (bot_3 : float)) : 'v_17))
             : 'v_17))
           : 'v_17))
         : ('v_10 -> 'v_17)))
       :
       (forall
        ((Broadcast 'v_11 (vec 2) 'v_12) (MulBroadcast float 'v_10 'v_11)
         (MulBroadcast 'v_12 float 'v_17))
        ('v_10 -> 'v_17)))
      ((Define Nonrec main
        ((lambda coord_4
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

    === specialize params (rainbow.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda coord_1
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_10)) : 'v_11)
              (u_resolution : (vec 2)))
             : 'v_12)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_12) (bot_3 : float)) : 'v_17))
             : 'v_17))
           : 'v_17))
         : ('v_10 -> 'v_17)))
       :
       (forall
        ((Broadcast 'v_11 (vec 2) 'v_12) (MulBroadcast float 'v_10 'v_11)
         (MulBroadcast 'v_12 float 'v_17))
        ('v_10 -> 'v_17)))
      ((Define Nonrec main
        ((lambda coord_4
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_42
        ((lambda coord_1
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
        ((lambda coord_4
          ((let uv_5
            ((app (get_uv_0_vec2_to_vec2_42 : ((vec 2) -> (vec 2)))
              (coord_4 : (vec 2)))
             : (vec 2))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_42
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0_vec2_to_vec2_42 coord_4)
          (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
           (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
            (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
             (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0_vec2_to_vec2_42 coord_4)
         (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
          (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
           (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
            (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))
      : ((vec 2) -> (vec 3))))

    === anf (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let anf_43 (* 2. coord_1)
         (let top_2 (- anf_43 u_resolution)
          (let anf_44 (index u_resolution 0)
           (let anf_45 (index u_resolution 1)
            (let bot_3 (min anf_44 anf_45) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_42 coord_4)
         (let anf_46 (index uv_5 0)
          (let anf_47 (index uv_5 1)
           (let anf_48 (+ anf_46 anf_47)
            (let anf_49 (* 5. anf_48)
             (let wave_6 (+ anf_49 u_time)
              (let anf_50 (sin wave_6)
               (let anf_51 (* anf_50 0.3)
                (let r_7 (+ anf_51 0.7)
                 (let anf_52 (+ wave_6 2.)
                  (let anf_53 (sin anf_52)
                   (let anf_54 (* anf_53 0.3)
                    (let g_8 (+ anf_54 0.7)
                     (let anf_55 (+ wave_6 4.)
                      (let anf_56 (sin anf_55)
                       (let anf_57 (* anf_56 0.3)
                        (let b_9 (+ anf_57 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let anf_43 (* 2. coord_1)
         (let top_2 (- anf_43 u_resolution)
          (let anf_44 (index u_resolution 0)
           (let anf_45 (index u_resolution 1)
            (let bot_3 (min anf_44 anf_45) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_42 coord_4)
         (let anf_46 (index uv_5 0)
          (let anf_47 (index uv_5 1)
           (let anf_48 (+ anf_46 anf_47)
            (let anf_49 (* 5. anf_48)
             (let wave_6 (+ anf_49 u_time)
              (let anf_50 (sin wave_6)
               (let anf_51 (* anf_50 0.3)
                (let r_7 (+ anf_51 0.7)
                 (let anf_52 (+ wave_6 2.)
                  (let anf_53 (sin anf_52)
                   (let anf_54 (* anf_53 0.3)
                    (let g_8 (+ anf_54 0.7)
                     (let anf_55 (+ wave_6 4.)
                      (let anf_56 (sin anf_55)
                       (let anf_57 (* anf_56 0.3)
                        (let b_9 (+ anf_57 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let anf_43 (* 2. coord_1)
         (let top_2 (- anf_43 u_resolution)
          (let anf_44 (index u_resolution 0)
           (let anf_45 (index u_resolution 1)
            (let bot_3 (min anf_44 anf_45) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_42 coord_4)
         (let anf_46 (index uv_5 0)
          (let anf_47 (index uv_5 1)
           (let anf_48 (+ anf_46 anf_47)
            (let anf_49 (* 5. anf_48)
             (let wave_6 (+ anf_49 u_time)
              (let anf_50 (sin wave_6)
               (let anf_51 (* anf_50 0.3)
                (let r_7 (+ anf_51 0.7)
                 (let anf_52 (+ wave_6 2.)
                  (let anf_53 (sin anf_52)
                   (let anf_54 (* anf_53 0.3)
                    (let g_8 (+ anf_54 0.7)
                     (let anf_55 (+ wave_6 4.)
                      (let anf_56 (sin anf_55)
                       (let anf_57 (* anf_56 0.3)
                        (let b_9 (+ anf_57 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_42) (args ((coord_1 (vec 2))))
       (body
        (let anf_43 (* 2. coord_1)
         (let top_2 (- anf_43 u_resolution)
          (let anf_44 (index u_resolution 0)
           (let anf_45 (index u_resolution 1)
            (let bot_3 (min anf_44 anf_45) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_42 coord_4)
         (let anf_46 (index uv_5 0)
          (let anf_47 (index uv_5 1)
           (let anf_48 (+ anf_46 anf_47)
            (let anf_49 (* 5. anf_48)
             (let wave_6 (+ anf_49 u_time)
              (let anf_50 (sin wave_6)
               (let anf_51 (* anf_50 0.3)
                (let r_7 (+ anf_51 0.7)
                 (let anf_52 (+ wave_6 2.)
                  (let anf_53 (sin anf_52)
                   (let anf_54 (* anf_53 0.3)
                    (let g_8 (+ anf_54 0.7)
                     (let anf_55 (+ wave_6 4.)
                      (let anf_56 (sin anf_55)
                       (let anf_57 (* anf_56 0.3)
                        (let b_9 (+ anf_57 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (rainbow.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_42) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_43 (* 2. coord_1))
         (set () vec2 top_2 (- anf_43 u_resolution))
         (set () float anf_44 (index u_resolution 0))
         (set () float anf_45 (index u_resolution 1))
         (set () float bot_3 (min anf_44 anf_45)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_42 coord_4))
         (set () float anf_46 (index uv_5 0))
         (set () float anf_47 (index uv_5 1))
         (set () float anf_48 (+ anf_46 anf_47))
         (set () float anf_49 (* 5. anf_48))
         (set () float wave_6 (+ anf_49 u_time))
         (set () float anf_50 (sin wave_6)) (set () float anf_51 (* anf_50 0.3))
         (set () float r_7 (+ anf_51 0.7)) (set () float anf_52 (+ wave_6 2.))
         (set () float anf_53 (sin anf_52)) (set () float anf_54 (* anf_53 0.3))
         (set () float g_8 (+ anf_54 0.7)) (set () float anf_55 (+ wave_6 4.))
         (set () float anf_56 (sin anf_55)) (set () float anf_57 (* anf_56 0.3))
         (set () float b_9 (+ anf_57 0.7)) (return (vec3 r_7 g_8 b_9)))))))

    === patch main (rainbow.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_42) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_43 (* 2. coord_1))
         (set () vec2 top_2 (- anf_43 u_resolution))
         (set () float anf_44 (index u_resolution 0))
         (set () float anf_45 (index u_resolution 1))
         (set () float bot_3 (min anf_44 anf_45)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_42 coord_4))
         (set () float anf_46 (index uv_5 0))
         (set () float anf_47 (index uv_5 1))
         (set () float anf_48 (+ anf_46 anf_47))
         (set () float anf_49 (* 5. anf_48))
         (set () float wave_6 (+ anf_49 u_time))
         (set () float anf_50 (sin wave_6)) (set () float anf_51 (* anf_50 0.3))
         (set () float r_7 (+ anf_51 0.7)) (set () float anf_52 (+ wave_6 2.))
         (set () float anf_53 (sin anf_52)) (set () float anf_54 (* anf_53 0.3))
         (set () float g_8 (+ anf_54 0.7)) (set () float anf_55 (+ wave_6 4.))
         (set () float anf_56 (sin anf_55)) (set () float anf_57 (* anf_56 0.3))
         (set () float b_9 (+ anf_57 0.7)) (return (vec3 r_7 g_8 b_9)))))
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
         (let (rec 1000) march
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
         (let (rec 1000) march_27
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
        ((lambda p_1
          ((lambda angle_2
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
        ((lambda a_6
          ((lambda b_7
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
        ((lambda t_11
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
        ((lambda p_14
          ((lambda t_15
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
        ((lambda p_18
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
        ((lambda ro_25
          ((lambda rd_26
            ((let (rec 1000) march_27
              ((lambda t_28
                ((lambda steps_29
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
        ((lambda coord_31
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

    === specialize params (raymarch.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Extern u_mouse) : (vec 2))
      ((Define Nonrec rotate_0
        ((lambda p_1
          ((lambda angle_2
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
        ((lambda a_6
          ((lambda b_7
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
        ((lambda t_11
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
        ((lambda p_14
          ((lambda t_15
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
        ((lambda p_18
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
        ((lambda ro_25
          ((lambda rd_26
            ((let (rec 1000) march_27
              ((lambda t_28
                ((lambda steps_29
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
        ((lambda coord_31
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
        ((lambda p_1
          ((lambda angle_2
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
        ((lambda a_6
          ((lambda b_7
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
        ((lambda t_11
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
        ((lambda p_14
          ((lambda t_15
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
        ((lambda p_18
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
        ((lambda ro_25
          ((lambda rd_26
            ((let (rec 1000) march_27
              ((lambda t_28
                ((lambda steps_29
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
        ((lambda coord_31
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
         (let (rec 1000) march_27
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

    === lambda lift (raymarch.glml) ===
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
     ((Define (Rec 1000) (name march_27_189)
       (args ((rd_26 (vec 3)) (ro_25 (vec 3)) (t_28 float) (steps_29 int)))
       (body
        (if (> steps_29 80) t_28
         (let d_30 (app map_17 (+ ro_25 (* rd_26 t_28)))
          (if (< d_30 0.001) t_28
           (if (> t_28 100.) 100.1
            (app march_27_189 rd_26 ro_25 (+ t_28 d_30) (+ steps_29 1))))))))
      : (float -> (int -> float)))
     ((Define Nonrec (name march_24) (args ((ro_25 (vec 3)) (rd_26 (vec 3))))
       (body (app march_27_189 rd_26 ro_25 0. 0)))
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
          (let anf_190 (index p_1 0)
           (let anf_191 (* anf_190 c_4)
            (let anf_192 (index p_1 1)
             (let anf_193 (* anf_192 s_3)
              (let anf_194 (- anf_191 anf_193)
               (let anf_195 (index p_1 0)
                (let anf_196 (* anf_195 s_3)
                 (let anf_197 (index p_1 1)
                  (let anf_198 (* anf_197 c_4)
                   (let anf_199 (+ anf_196 anf_198)
                    (return (vec2 anf_194 anf_199))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define Nonrec (name sMin_5) (args ((a_6 float) (b_7 float)))
       (body
        (let k_8 0.1
         (let anf_200 (- b_7 a_6)
          (let anf_201 (* 0.5 anf_200)
           (let anf_202 (/ anf_201 k_8)
            (let anf_203 (+ 0.5 anf_202)
             (let h_9 (clamp anf_203 0. 1.)
              (let anf_204 (mix b_7 a_6 h_9)
               (let anf_205 (* k_8 h_9)
                (let anf_206 (- 1. h_9)
                 (let anf_207 (* anf_205 anf_206) (return (- anf_204 anf_207))))))))))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name palette_10) (args ((t_11 float)))
       (body
        (let cfg_12 (vec3 0.3 0.416 0.557)
         (let anf_208 (+ cfg_12 t_11)
          (let anf_209 (* anf_208 6.28318)
           (let anf_210 (cos anf_209)
            (let anf_211 (* anf_210 0.5) (return (+ anf_211 0.5)))))))))
      : (float -> (vec 3)))
     ((Define Nonrec (name sdTorus_13) (args ((p_14 (vec 3)) (t_15 (vec 2))))
       (body
        (let anf_212 (index p_14 0)
         (let anf_213 (index p_14 2)
          (let anf_214 (vec2 anf_212 anf_213)
           (let anf_215 (length anf_214)
            (let anf_216 (index t_15 0)
             (let anf_217 (- anf_215 anf_216)
              (let anf_218 (index p_14 1)
               (let q_16 (vec2 anf_217 anf_218)
                (let anf_219 (length q_16)
                 (let anf_220 (index t_15 1) (return (- anf_219 anf_220))))))))))))))
      : ((vec 3) -> ((vec 2) -> float)))
     ((Define Nonrec (name map_17) (args ((p_18 (vec 3))))
       (body
        (let angle_19 (* u_time 2.)
         (let anf_221 (index p_18 0)
          (let anf_222 (index p_18 1)
           (let anf_223 (vec2 anf_221 anf_222)
            (let p_xy_20 (rotate_0 anf_223 angle_19)
             (let anf_224 (index p_xy_20 0)
              (let anf_225 (index p_xy_20 1)
               (let anf_226 (index p_18 2)
                (let p_prime_21 (vec3 anf_224 anf_225 anf_226)
                 (let anf_227 (index p_prime_21 1)
                  (let anf_228 (index p_prime_21 2)
                   (let anf_229 (vec2 anf_227 anf_228)
                    (let p_yz_22 (rotate_0 anf_229 angle_19)
                     (let anf_230 (index p_prime_21 0)
                      (let anf_231 (index p_yz_22 0)
                       (let anf_232 (index p_yz_22 1)
                        (let p_prime_23 (vec3 anf_230 anf_231 anf_232)
                         (let anf_233 (vec2 1. 0.3)
                          (let anf_234 (sdTorus_13 p_prime_23 anf_233)
                           (let anf_235 (vec2 2. 0.5)
                            (let anf_236 (sdTorus_13 p_18 anf_235)
                             (return (sMin_5 anf_234 anf_236)))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (Rec 1000) (name march_27_189)
       (args ((rd_26 (vec 3)) (ro_25 (vec 3)) (t_28 float) (steps_29 int)))
       (body
        (let anf_237 (> steps_29 80)
         (return
          (if anf_237 (return t_28)
           (let anf_238 (* rd_26 t_28)
            (let anf_239 (+ ro_25 anf_238)
             (let d_30 (map_17 anf_239)
              (let anf_240 (< d_30 0.001)
               (return
                (if anf_240 (return t_28)
                 (let anf_241 (> t_28 100.)
                  (return
                   (if anf_241 (return 100.1)
                    (let anf_242 (+ t_28 d_30)
                     (let anf_243 (+ steps_29 1)
                      (return (march_27_189 rd_26 ro_25 anf_242 anf_243))))))))))))))))))
      : (float -> (int -> float)))
     ((Define Nonrec (name march_24) (args ((ro_25 (vec 3)) (rd_26 (vec 3))))
       (body (return (march_27_189 rd_26 ro_25 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define Nonrec (name main) (args ((coord_31 (vec 2))))
       (body
        (let anf_244 (index u_resolution 0)
         (let anf_245 (index u_resolution 1)
          (let res_min_32 (min anf_244 anf_245)
           (let anf_246 (* coord_31 2.)
            (let anf_247 (- anf_246 u_resolution)
             (let uv_33 (/ anf_247 res_min_32)
              (let anf_248 (* u_mouse 2.)
               (let anf_249 (- anf_248 u_resolution)
                (let mouseUV_34 (/ anf_249 res_min_32)
                 (let ro_init_35 (vec3 0. 0. -10.)
                  (let anf_250 (index uv_33 0)
                   (let anf_251 (index uv_33 1)
                    (let anf_252 (vec3 anf_250 anf_251 1.)
                     (let rd_init_36 (normalize anf_252)
                      (let anf_253 (index mouseUV_34 1)
                       (let rotX_37 (- 0. anf_253)
                        (let anf_254 (index mouseUV_34 0)
                         (let rotY_38 (- 0. anf_254)
                          (let anf_255 (index ro_init_35 1)
                           (let anf_256 (index ro_init_35 2)
                            (let anf_257 (vec2 anf_255 anf_256)
                             (let ro_yz_39 (rotate_0 anf_257 rotX_37)
                              (let anf_258 (index rd_init_36 1)
                               (let anf_259 (index rd_init_36 2)
                                (let anf_260 (vec2 anf_258 anf_259)
                                 (let rd_yz_40 (rotate_0 anf_260 rotX_37)
                                  (let anf_261 (index ro_init_35 0)
                                   (let anf_262 (index ro_yz_39 0)
                                    (let anf_263 (index ro_yz_39 1)
                                     (let ro_41 (vec3 anf_261 anf_262 anf_263)
                                      (let anf_264 (index rd_init_36 0)
                                       (let anf_265 (index rd_yz_40 0)
                                        (let anf_266 (index rd_yz_40 1)
                                         (let rd_42
                                          (vec3 anf_264 anf_265 anf_266)
                                          (let anf_267 (index ro_41 0)
                                           (let anf_268 (index ro_41 2)
                                            (let anf_269 (vec2 anf_267 anf_268)
                                             (let ro_xz_43
                                              (rotate_0 anf_269 rotY_38)
                                              (let anf_270 (index rd_42 0)
                                               (let anf_271 (index rd_42 2)
                                                (let anf_272
                                                 (vec2 anf_270 anf_271)
                                                 (let rd_xz_44
                                                  (rotate_0 anf_272 rotY_38)
                                                  (let anf_273 (index ro_xz_43 0)
                                                   (let anf_274 (index ro_41 1)
                                                    (let anf_275
                                                     (index ro_xz_43 1)
                                                     (let ro_45
                                                      (vec3 anf_273 anf_274
                                                       anf_275)
                                                      (let anf_276
                                                       (index rd_xz_44 0)
                                                       (let anf_277
                                                        (index rd_42 1)
                                                        (let anf_278
                                                         (index rd_xz_44 1)
                                                         (let rd_46
                                                          (vec3 anf_276 anf_277
                                                           anf_278)
                                                          (let t_47
                                                           (march_24 ro_45 rd_46)
                                                           (let anf_279
                                                            (> t_47 100.)
                                                            (let col_48
                                                             (if anf_279
                                                              (return
                                                               (vec3 0.2 0.2 0.2))
                                                              (let anf_280
                                                               (* t_47 0.3)
                                                               (return
                                                                (palette_10
                                                                 anf_280))))
                                                             (let anf_281
                                                              (- uv_33
                                                               mouseUV_34)
                                                              (let anf_282
                                                               (length anf_281)
                                                               (let glow_49
                                                                (/ 0.02 anf_282)
                                                                (return
                                                                 (+ col_48
                                                                  glow_49))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (raymarch.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_190 (index p_1 0)
           (let anf_191 (* anf_190 c_4)
            (let anf_192 (index p_1 1)
             (let anf_193 (* anf_192 s_3)
              (let anf_194 (- anf_191 anf_193)
               (let anf_195 (index p_1 0)
                (let anf_196 (* anf_195 s_3)
                 (let anf_197 (index p_1 1)
                  (let anf_198 (* anf_197 c_4)
                   (let anf_199 (+ anf_196 anf_198)
                    (return (vec2 anf_194 anf_199))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define (name sMin_5) (args ((a_6 float) (b_7 float)))
       (body
        (let k_8 0.1
         (let anf_200 (- b_7 a_6)
          (let anf_201 (* 0.5 anf_200)
           (let anf_202 (/ anf_201 k_8)
            (let anf_203 (+ 0.5 anf_202)
             (let h_9 (clamp anf_203 0. 1.)
              (let anf_204 (mix b_7 a_6 h_9)
               (let anf_205 (* k_8 h_9)
                (let anf_206 (- 1. h_9)
                 (let anf_207 (* anf_205 anf_206) (return (- anf_204 anf_207))))))))))))))
      : (float -> (float -> float)))
     ((Define (name palette_10) (args ((t_11 float)))
       (body
        (let cfg_12 (vec3 0.3 0.416 0.557)
         (let anf_208 (+ cfg_12 t_11)
          (let anf_209 (* anf_208 6.28318)
           (let anf_210 (cos anf_209)
            (let anf_211 (* anf_210 0.5) (return (+ anf_211 0.5)))))))))
      : (float -> (vec 3)))
     ((Define (name sdTorus_13) (args ((p_14 (vec 3)) (t_15 (vec 2))))
       (body
        (let anf_212 (index p_14 0)
         (let anf_213 (index p_14 2)
          (let anf_214 (vec2 anf_212 anf_213)
           (let anf_215 (length anf_214)
            (let anf_216 (index t_15 0)
             (let anf_217 (- anf_215 anf_216)
              (let anf_218 (index p_14 1)
               (let q_16 (vec2 anf_217 anf_218)
                (let anf_219 (length q_16)
                 (let anf_220 (index t_15 1) (return (- anf_219 anf_220))))))))))))))
      : ((vec 3) -> ((vec 2) -> float)))
     ((Define (name map_17) (args ((p_18 (vec 3))))
       (body
        (let angle_19 (* u_time 2.)
         (let anf_221 (index p_18 0)
          (let anf_222 (index p_18 1)
           (let anf_223 (vec2 anf_221 anf_222)
            (let p_xy_20 (rotate_0 anf_223 angle_19)
             (let anf_224 (index p_xy_20 0)
              (let anf_225 (index p_xy_20 1)
               (let anf_226 (index p_18 2)
                (let p_prime_21 (vec3 anf_224 anf_225 anf_226)
                 (let anf_227 (index p_prime_21 1)
                  (let anf_228 (index p_prime_21 2)
                   (let anf_229 (vec2 anf_227 anf_228)
                    (let p_yz_22 (rotate_0 anf_229 angle_19)
                     (let anf_230 (index p_prime_21 0)
                      (let anf_231 (index p_yz_22 0)
                       (let anf_232 (index p_yz_22 1)
                        (let p_prime_23 (vec3 anf_230 anf_231 anf_232)
                         (let anf_233 (vec2 1. 0.3)
                          (let anf_234 (sdTorus_13 p_prime_23 anf_233)
                           (let anf_235 (vec2 2. 0.5)
                            (let anf_236 (sdTorus_13 p_18 anf_235)
                             (return (sMin_5 anf_234 anf_236)))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name march_27_189)
       (args ((rd_26 (vec 3)) (ro_25 (vec 3)) (t_28 float) (steps_29 int)))
       (body
        (let _iter_283 0
         (while (< _iter_283 1000)
          (let anf_237 (> steps_29 80)
           (return
            (if anf_237 (return t_28)
             (let anf_238 (* rd_26 t_28)
              (let anf_239 (+ ro_25 anf_238)
               (let d_30 (map_17 anf_239)
                (let anf_240 (< d_30 0.001)
                 (return
                  (if anf_240 (return t_28)
                   (let anf_241 (> t_28 100.)
                    (return
                     (if anf_241 (return 100.1)
                      (let anf_242 (+ t_28 d_30)
                       (let anf_243 (+ steps_29 1)
                        (set rd_26 rd_26
                         (set ro_25 ro_25
                          (set t_28 anf_242
                           (set steps_29 anf_243
                            (let _iter_inc_284 (+ _iter_283 1)
                             (set _iter_283 _iter_inc_284 continue))))))))))))))))))))
          (return 0.)))))
      : (float -> (int -> float)))
     ((Define (name march_24) (args ((ro_25 (vec 3)) (rd_26 (vec 3))))
       (body (return (march_27_189 rd_26 ro_25 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define (name main) (args ((coord_31 (vec 2))))
       (body
        (let anf_244 (index u_resolution 0)
         (let anf_245 (index u_resolution 1)
          (let res_min_32 (min anf_244 anf_245)
           (let anf_246 (* coord_31 2.)
            (let anf_247 (- anf_246 u_resolution)
             (let uv_33 (/ anf_247 res_min_32)
              (let anf_248 (* u_mouse 2.)
               (let anf_249 (- anf_248 u_resolution)
                (let mouseUV_34 (/ anf_249 res_min_32)
                 (let ro_init_35 (vec3 0. 0. -10.)
                  (let anf_250 (index uv_33 0)
                   (let anf_251 (index uv_33 1)
                    (let anf_252 (vec3 anf_250 anf_251 1.)
                     (let rd_init_36 (normalize anf_252)
                      (let anf_253 (index mouseUV_34 1)
                       (let rotX_37 (- 0. anf_253)
                        (let anf_254 (index mouseUV_34 0)
                         (let rotY_38 (- 0. anf_254)
                          (let anf_255 (index ro_init_35 1)
                           (let anf_256 (index ro_init_35 2)
                            (let anf_257 (vec2 anf_255 anf_256)
                             (let ro_yz_39 (rotate_0 anf_257 rotX_37)
                              (let anf_258 (index rd_init_36 1)
                               (let anf_259 (index rd_init_36 2)
                                (let anf_260 (vec2 anf_258 anf_259)
                                 (let rd_yz_40 (rotate_0 anf_260 rotX_37)
                                  (let anf_261 (index ro_init_35 0)
                                   (let anf_262 (index ro_yz_39 0)
                                    (let anf_263 (index ro_yz_39 1)
                                     (let ro_41 (vec3 anf_261 anf_262 anf_263)
                                      (let anf_264 (index rd_init_36 0)
                                       (let anf_265 (index rd_yz_40 0)
                                        (let anf_266 (index rd_yz_40 1)
                                         (let rd_42
                                          (vec3 anf_264 anf_265 anf_266)
                                          (let anf_267 (index ro_41 0)
                                           (let anf_268 (index ro_41 2)
                                            (let anf_269 (vec2 anf_267 anf_268)
                                             (let ro_xz_43
                                              (rotate_0 anf_269 rotY_38)
                                              (let anf_270 (index rd_42 0)
                                               (let anf_271 (index rd_42 2)
                                                (let anf_272
                                                 (vec2 anf_270 anf_271)
                                                 (let rd_xz_44
                                                  (rotate_0 anf_272 rotY_38)
                                                  (let anf_273 (index ro_xz_43 0)
                                                   (let anf_274 (index ro_41 1)
                                                    (let anf_275
                                                     (index ro_xz_43 1)
                                                     (let ro_45
                                                      (vec3 anf_273 anf_274
                                                       anf_275)
                                                      (let anf_276
                                                       (index rd_xz_44 0)
                                                       (let anf_277
                                                        (index rd_42 1)
                                                        (let anf_278
                                                         (index rd_xz_44 1)
                                                         (let rd_46
                                                          (vec3 anf_276 anf_277
                                                           anf_278)
                                                          (let t_47
                                                           (march_24 ro_45 rd_46)
                                                           (let anf_279
                                                            (> t_47 100.)
                                                            (let col_48
                                                             (if anf_279
                                                              (return
                                                               (vec3 0.2 0.2 0.2))
                                                              (let anf_280
                                                               (* t_47 0.3)
                                                               (return
                                                                (palette_10
                                                                 anf_280))))
                                                             (let anf_281
                                                              (- uv_33
                                                               mouseUV_34)
                                                              (let anf_282
                                                               (length anf_281)
                                                               (let glow_49
                                                                (/ 0.02 anf_282)
                                                                (return
                                                                 (+ col_48
                                                                  glow_49))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (raymarch.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_190 (index p_1 0)
           (let anf_191 (* anf_190 c_4)
            (let anf_192 (index p_1 1)
             (let anf_193 (* anf_192 s_3)
              (let anf_194 (- anf_191 anf_193)
               (let anf_195 (index p_1 0)
                (let anf_196 (* anf_195 s_3)
                 (let anf_197 (index p_1 1)
                  (let anf_198 (* anf_197 c_4)
                   (let anf_199 (+ anf_196 anf_198)
                    (return (vec2 anf_194 anf_199))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define (name sMin_5) (args ((a_6 float) (b_7 float)))
       (body
        (let k_8 0.1
         (let anf_200 (- b_7 a_6)
          (let anf_201 (* 0.5 anf_200)
           (let anf_202 (/ anf_201 k_8)
            (let anf_203 (+ 0.5 anf_202)
             (let h_9 (clamp anf_203 0. 1.)
              (let anf_204 (mix b_7 a_6 h_9)
               (let anf_205 (* k_8 h_9)
                (let anf_206 (- 1. h_9)
                 (let anf_207 (* anf_205 anf_206) (return (- anf_204 anf_207))))))))))))))
      : (float -> (float -> float)))
     ((Define (name palette_10) (args ((t_11 float)))
       (body
        (let cfg_12 (vec3 0.3 0.416 0.557)
         (let anf_208 (+ cfg_12 t_11)
          (let anf_209 (* anf_208 6.28318)
           (let anf_210 (cos anf_209)
            (let anf_211 (* anf_210 0.5) (return (+ anf_211 0.5)))))))))
      : (float -> (vec 3)))
     ((Define (name sdTorus_13) (args ((p_14 (vec 3)) (t_15 (vec 2))))
       (body
        (let anf_212 (index p_14 0)
         (let anf_213 (index p_14 2)
          (let anf_214 (vec2 anf_212 anf_213)
           (let anf_215 (length anf_214)
            (let anf_216 (index t_15 0)
             (let anf_217 (- anf_215 anf_216)
              (let anf_218 (index p_14 1)
               (let q_16 (vec2 anf_217 anf_218)
                (let anf_219 (length q_16)
                 (let anf_220 (index t_15 1) (return (- anf_219 anf_220))))))))))))))
      : ((vec 3) -> ((vec 2) -> float)))
     ((Define (name map_17) (args ((p_18 (vec 3))))
       (body
        (let angle_19 (* u_time 2.)
         (let anf_221 (index p_18 0)
          (let anf_222 (index p_18 1)
           (let anf_223 (vec2 anf_221 anf_222)
            (let p_xy_20 (rotate_0 anf_223 angle_19)
             (let anf_224 (index p_xy_20 0)
              (let anf_225 (index p_xy_20 1)
               (let anf_226 (index p_18 2)
                (let p_prime_21 (vec3 anf_224 anf_225 anf_226)
                 (let anf_227 (index p_prime_21 1)
                  (let anf_228 (index p_prime_21 2)
                   (let anf_229 (vec2 anf_227 anf_228)
                    (let p_yz_22 (rotate_0 anf_229 angle_19)
                     (let anf_230 (index p_prime_21 0)
                      (let anf_231 (index p_yz_22 0)
                       (let anf_232 (index p_yz_22 1)
                        (let p_prime_23 (vec3 anf_230 anf_231 anf_232)
                         (let anf_233 (vec2 1. 0.3)
                          (let anf_234 (sdTorus_13 p_prime_23 anf_233)
                           (let anf_235 (vec2 2. 0.5)
                            (let anf_236 (sdTorus_13 p_18 anf_235)
                             (return (sMin_5 anf_234 anf_236)))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name march_27_189)
       (args ((rd_26 (vec 3)) (ro_25 (vec 3)) (t_28 float) (steps_29 int)))
       (body
        (let _iter_283 0
         (while (< _iter_283 1000)
          (let anf_237 (> steps_29 80)
           (return
            (if anf_237 (return t_28)
             (let anf_238 (* rd_26 t_28)
              (let anf_239 (+ ro_25 anf_238)
               (let d_30 (map_17 anf_239)
                (let anf_240 (< d_30 0.001)
                 (return
                  (if anf_240 (return t_28)
                   (let anf_241 (> t_28 100.)
                    (return
                     (if anf_241 (return 100.1)
                      (let anf_242 (+ t_28 d_30)
                       (let anf_243 (+ steps_29 1)
                        (set rd_26 rd_26
                         (set ro_25 ro_25
                          (set t_28 anf_242
                           (set steps_29 anf_243
                            (let _iter_inc_284 (+ _iter_283 1)
                             (set _iter_283 _iter_inc_284 continue))))))))))))))))))))
          (return 0.)))))
      : (float -> (int -> float)))
     ((Define (name march_24) (args ((ro_25 (vec 3)) (rd_26 (vec 3))))
       (body (return (march_27_189 rd_26 ro_25 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define (name main) (args ((coord_31 (vec 2))))
       (body
        (let anf_244 (index u_resolution 0)
         (let anf_245 (index u_resolution 1)
          (let res_min_32 (min anf_244 anf_245)
           (let anf_246 (* coord_31 2.)
            (let anf_247 (- anf_246 u_resolution)
             (let uv_33 (/ anf_247 res_min_32)
              (let anf_248 (* u_mouse 2.)
               (let anf_249 (- anf_248 u_resolution)
                (let mouseUV_34 (/ anf_249 res_min_32)
                 (let ro_init_35 (vec3 0. 0. -10.)
                  (let anf_250 (index uv_33 0)
                   (let anf_251 (index uv_33 1)
                    (let anf_252 (vec3 anf_250 anf_251 1.)
                     (let rd_init_36 (normalize anf_252)
                      (let anf_253 (index mouseUV_34 1)
                       (let rotX_37 (- 0. anf_253)
                        (let anf_254 (index mouseUV_34 0)
                         (let rotY_38 (- 0. anf_254)
                          (let anf_255 (index ro_init_35 1)
                           (let anf_256 (index ro_init_35 2)
                            (let anf_257 (vec2 anf_255 anf_256)
                             (let ro_yz_39 (rotate_0 anf_257 rotX_37)
                              (let anf_258 (index rd_init_36 1)
                               (let anf_259 (index rd_init_36 2)
                                (let anf_260 (vec2 anf_258 anf_259)
                                 (let rd_yz_40 (rotate_0 anf_260 rotX_37)
                                  (let anf_261 (index ro_init_35 0)
                                   (let anf_262 (index ro_yz_39 0)
                                    (let anf_263 (index ro_yz_39 1)
                                     (let ro_41 (vec3 anf_261 anf_262 anf_263)
                                      (let anf_264 (index rd_init_36 0)
                                       (let anf_265 (index rd_yz_40 0)
                                        (let anf_266 (index rd_yz_40 1)
                                         (let rd_42
                                          (vec3 anf_264 anf_265 anf_266)
                                          (let anf_267 (index ro_41 0)
                                           (let anf_268 (index ro_41 2)
                                            (let anf_269 (vec2 anf_267 anf_268)
                                             (let ro_xz_43
                                              (rotate_0 anf_269 rotY_38)
                                              (let anf_270 (index rd_42 0)
                                               (let anf_271 (index rd_42 2)
                                                (let anf_272
                                                 (vec2 anf_270 anf_271)
                                                 (let rd_xz_44
                                                  (rotate_0 anf_272 rotY_38)
                                                  (let anf_273 (index ro_xz_43 0)
                                                   (let anf_274 (index ro_41 1)
                                                    (let anf_275
                                                     (index ro_xz_43 1)
                                                     (let ro_45
                                                      (vec3 anf_273 anf_274
                                                       anf_275)
                                                      (let anf_276
                                                       (index rd_xz_44 0)
                                                       (let anf_277
                                                        (index rd_42 1)
                                                        (let anf_278
                                                         (index rd_xz_44 1)
                                                         (let rd_46
                                                          (vec3 anf_276 anf_277
                                                           anf_278)
                                                          (let t_47
                                                           (march_24 ro_45 rd_46)
                                                           (let anf_279
                                                            (> t_47 100.)
                                                            (let col_48
                                                             (if anf_279
                                                              (return
                                                               (vec3 0.2 0.2 0.2))
                                                              (let anf_280
                                                               (* t_47 0.3)
                                                               (return
                                                                (palette_10
                                                                 anf_280))))
                                                             (let anf_281
                                                              (- uv_33
                                                               mouseUV_34)
                                                              (let anf_282
                                                               (length anf_281)
                                                               (let glow_49
                                                                (/ 0.02 anf_282)
                                                                (return
                                                                 (+ col_48
                                                                  glow_49))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (raymarch.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Extern u_mouse) : (vec 2))
     ((Define (name rotate_0) (args ((p_1 (vec 2)) (angle_2 float)))
       (body
        (let s_3 (sin angle_2)
         (let c_4 (cos angle_2)
          (let anf_190 (index p_1 0)
           (let anf_191 (* anf_190 c_4)
            (let anf_192 (index p_1 1)
             (let anf_193 (* anf_192 s_3)
              (let anf_194 (- anf_191 anf_193)
               (let anf_195 (index p_1 0)
                (let anf_196 (* anf_195 s_3)
                 (let anf_197 (index p_1 1)
                  (let anf_198 (* anf_197 c_4)
                   (let anf_199 (+ anf_196 anf_198)
                    (return (vec2 anf_194 anf_199))))))))))))))))
      : ((vec 2) -> (float -> (vec 2))))
     ((Define (name sMin_5) (args ((a_6 float) (b_7 float)))
       (body
        (let k_8 0.1
         (let anf_200 (- b_7 a_6)
          (let anf_201 (* 0.5 anf_200)
           (let anf_202 (/ anf_201 k_8)
            (let anf_203 (+ 0.5 anf_202)
             (let h_9 (clamp anf_203 0. 1.)
              (let anf_204 (mix b_7 a_6 h_9)
               (let anf_205 (* k_8 h_9)
                (let anf_206 (- 1. h_9)
                 (let anf_207 (* anf_205 anf_206) (return (- anf_204 anf_207))))))))))))))
      : (float -> (float -> float)))
     ((Define (name palette_10) (args ((t_11 float)))
       (body
        (let cfg_12 (vec3 0.3 0.416 0.557)
         (let anf_208 (+ cfg_12 t_11)
          (let anf_209 (* anf_208 6.28318)
           (let anf_210 (cos anf_209)
            (let anf_211 (* anf_210 0.5) (return (+ anf_211 0.5)))))))))
      : (float -> (vec 3)))
     ((Define (name sdTorus_13) (args ((p_14 (vec 3)) (t_15 (vec 2))))
       (body
        (let anf_212 (index p_14 0)
         (let anf_213 (index p_14 2)
          (let anf_214 (vec2 anf_212 anf_213)
           (let anf_215 (length anf_214)
            (let anf_216 (index t_15 0)
             (let anf_217 (- anf_215 anf_216)
              (let anf_218 (index p_14 1)
               (let q_16 (vec2 anf_217 anf_218)
                (let anf_219 (length q_16)
                 (let anf_220 (index t_15 1) (return (- anf_219 anf_220))))))))))))))
      : ((vec 3) -> ((vec 2) -> float)))
     ((Define (name map_17) (args ((p_18 (vec 3))))
       (body
        (let angle_19 (* u_time 2.)
         (let anf_221 (index p_18 0)
          (let anf_222 (index p_18 1)
           (let anf_223 (vec2 anf_221 anf_222)
            (let p_xy_20 (rotate_0 anf_223 angle_19)
             (let anf_224 (index p_xy_20 0)
              (let anf_225 (index p_xy_20 1)
               (let anf_226 (index p_18 2)
                (let p_prime_21 (vec3 anf_224 anf_225 anf_226)
                 (let anf_227 (index p_prime_21 1)
                  (let anf_228 (index p_prime_21 2)
                   (let anf_229 (vec2 anf_227 anf_228)
                    (let p_yz_22 (rotate_0 anf_229 angle_19)
                     (let anf_230 (index p_prime_21 0)
                      (let anf_231 (index p_yz_22 0)
                       (let anf_232 (index p_yz_22 1)
                        (let p_prime_23 (vec3 anf_230 anf_231 anf_232)
                         (let anf_233 (vec2 1. 0.3)
                          (let anf_234 (sdTorus_13 p_prime_23 anf_233)
                           (let anf_235 (vec2 2. 0.5)
                            (let anf_236 (sdTorus_13 p_18 anf_235)
                             (return (sMin_5 anf_234 anf_236)))))))))))))))))))))))))
      : ((vec 3) -> float))
     ((Define (name march_27_189)
       (args ((rd_26 (vec 3)) (ro_25 (vec 3)) (t_28 float) (steps_29 int)))
       (body
        (let _iter_283 0
         (while (< _iter_283 1000)
          (let anf_237 (> steps_29 80)
           (return
            (if anf_237 (return t_28)
             (let anf_238 (* rd_26 t_28)
              (let anf_239 (+ ro_25 anf_238)
               (let d_30 (map_17 anf_239)
                (let anf_240 (< d_30 0.001)
                 (return
                  (if anf_240 (return t_28)
                   (let anf_241 (> t_28 100.)
                    (return
                     (if anf_241 (return 100.1)
                      (let anf_242 (+ t_28 d_30)
                       (let anf_243 (+ steps_29 1)
                        (set rd_26 rd_26
                         (set ro_25 ro_25
                          (set t_28 anf_242
                           (set steps_29 anf_243
                            (let _iter_inc_284 (+ _iter_283 1)
                             (set _iter_283 _iter_inc_284 continue))))))))))))))))))))
          (return 0.)))))
      : (float -> (int -> float)))
     ((Define (name march_24) (args ((ro_25 (vec 3)) (rd_26 (vec 3))))
       (body (return (march_27_189 rd_26 ro_25 0. 0))))
      : ((vec 3) -> ((vec 3) -> float)))
     ((Define (name main) (args ((coord_31 (vec 2))))
       (body
        (let anf_244 (index u_resolution 0)
         (let anf_245 (index u_resolution 1)
          (let res_min_32 (min anf_244 anf_245)
           (let anf_246 (* coord_31 2.)
            (let anf_247 (- anf_246 u_resolution)
             (let uv_33 (/ anf_247 res_min_32)
              (let anf_248 (* u_mouse 2.)
               (let anf_249 (- anf_248 u_resolution)
                (let mouseUV_34 (/ anf_249 res_min_32)
                 (let ro_init_35 (vec3 0. 0. -10.)
                  (let anf_250 (index uv_33 0)
                   (let anf_251 (index uv_33 1)
                    (let anf_252 (vec3 anf_250 anf_251 1.)
                     (let rd_init_36 (normalize anf_252)
                      (let anf_253 (index mouseUV_34 1)
                       (let rotX_37 (- 0. anf_253)
                        (let anf_254 (index mouseUV_34 0)
                         (let rotY_38 (- 0. anf_254)
                          (let anf_255 (index ro_init_35 1)
                           (let anf_256 (index ro_init_35 2)
                            (let anf_257 (vec2 anf_255 anf_256)
                             (let ro_yz_39 (rotate_0 anf_257 rotX_37)
                              (let anf_258 (index rd_init_36 1)
                               (let anf_259 (index rd_init_36 2)
                                (let anf_260 (vec2 anf_258 anf_259)
                                 (let rd_yz_40 (rotate_0 anf_260 rotX_37)
                                  (let anf_261 (index ro_init_35 0)
                                   (let anf_262 (index ro_yz_39 0)
                                    (let anf_263 (index ro_yz_39 1)
                                     (let ro_41 (vec3 anf_261 anf_262 anf_263)
                                      (let anf_264 (index rd_init_36 0)
                                       (let anf_265 (index rd_yz_40 0)
                                        (let anf_266 (index rd_yz_40 1)
                                         (let rd_42
                                          (vec3 anf_264 anf_265 anf_266)
                                          (let anf_267 (index ro_41 0)
                                           (let anf_268 (index ro_41 2)
                                            (let anf_269 (vec2 anf_267 anf_268)
                                             (let ro_xz_43
                                              (rotate_0 anf_269 rotY_38)
                                              (let anf_270 (index rd_42 0)
                                               (let anf_271 (index rd_42 2)
                                                (let anf_272
                                                 (vec2 anf_270 anf_271)
                                                 (let rd_xz_44
                                                  (rotate_0 anf_272 rotY_38)
                                                  (let anf_273 (index ro_xz_43 0)
                                                   (let anf_274 (index ro_41 1)
                                                    (let anf_275
                                                     (index ro_xz_43 1)
                                                     (let ro_45
                                                      (vec3 anf_273 anf_274
                                                       anf_275)
                                                      (let anf_276
                                                       (index rd_xz_44 0)
                                                       (let anf_277
                                                        (index rd_42 1)
                                                        (let anf_278
                                                         (index rd_xz_44 1)
                                                         (let rd_46
                                                          (vec3 anf_276 anf_277
                                                           anf_278)
                                                          (let t_47
                                                           (march_24 ro_45 rd_46)
                                                           (let anf_279
                                                            (> t_47 100.)
                                                            (let col_48
                                                             (if anf_279
                                                              (return
                                                               (vec3 0.2 0.2 0.2))
                                                              (let anf_280
                                                               (* t_47 0.3)
                                                               (return
                                                                (palette_10
                                                                 anf_280))))
                                                             (let anf_281
                                                              (- uv_33
                                                               mouseUV_34)
                                                              (let anf_282
                                                               (length anf_281)
                                                               (let glow_49
                                                                (/ 0.02 anf_282)
                                                                (return
                                                                 (+ col_48
                                                                  glow_49))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (raymarch.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ()) (Global Uniform (TyVec 2) u_mouse ())
      (Function (name rotate_0) (desc ())
       (params (((TyVec 2) p_1) (TyFloat angle_2))) (ret_type (TyVec 2))
       (body
        ((set () float s_3 (sin angle_2)) (set () float c_4 (cos angle_2))
         (set () float anf_190 (index p_1 0))
         (set () float anf_191 (* anf_190 c_4))
         (set () float anf_192 (index p_1 1))
         (set () float anf_193 (* anf_192 s_3))
         (set () float anf_194 (- anf_191 anf_193))
         (set () float anf_195 (index p_1 0))
         (set () float anf_196 (* anf_195 s_3))
         (set () float anf_197 (index p_1 1))
         (set () float anf_198 (* anf_197 c_4))
         (set () float anf_199 (+ anf_196 anf_198))
         (return (vec2 anf_194 anf_199)))))
      (Function (name sMin_5) (desc ()) (params ((TyFloat a_6) (TyFloat b_7)))
       (ret_type TyFloat)
       (body
        ((set () float k_8 0.1) (set () float anf_200 (- b_7 a_6))
         (set () float anf_201 (* 0.5 anf_200))
         (set () float anf_202 (/ anf_201 k_8))
         (set () float anf_203 (+ 0.5 anf_202))
         (set () float h_9 (clamp anf_203 0. 1.))
         (set () float anf_204 (mix b_7 a_6 h_9))
         (set () float anf_205 (* k_8 h_9)) (set () float anf_206 (- 1. h_9))
         (set () float anf_207 (* anf_205 anf_206)) (return (- anf_204 anf_207)))))
      (Function (name palette_10) (desc ()) (params ((TyFloat t_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec3 cfg_12 (vec3 0.3 0.416 0.557))
         (set () vec3 anf_208 (+ cfg_12 t_11))
         (set () vec3 anf_209 (* anf_208 6.28318))
         (set () vec3 anf_210 (cos anf_209))
         (set () vec3 anf_211 (* anf_210 0.5)) (return (+ anf_211 0.5)))))
      (Function (name sdTorus_13) (desc ())
       (params (((TyVec 3) p_14) ((TyVec 2) t_15))) (ret_type TyFloat)
       (body
        ((set () float anf_212 (index p_14 0))
         (set () float anf_213 (index p_14 2))
         (set () vec2 anf_214 (vec2 anf_212 anf_213))
         (set () float anf_215 (length anf_214))
         (set () float anf_216 (index t_15 0))
         (set () float anf_217 (- anf_215 anf_216))
         (set () float anf_218 (index p_14 1))
         (set () vec2 q_16 (vec2 anf_217 anf_218))
         (set () float anf_219 (length q_16))
         (set () float anf_220 (index t_15 1)) (return (- anf_219 anf_220)))))
      (Function (name map_17) (desc ()) (params (((TyVec 3) p_18)))
       (ret_type TyFloat)
       (body
        ((set () float angle_19 (* u_time 2.))
         (set () float anf_221 (index p_18 0))
         (set () float anf_222 (index p_18 1))
         (set () vec2 anf_223 (vec2 anf_221 anf_222))
         (set () vec2 p_xy_20 (rotate_0 anf_223 angle_19))
         (set () float anf_224 (index p_xy_20 0))
         (set () float anf_225 (index p_xy_20 1))
         (set () float anf_226 (index p_18 2))
         (set () vec3 p_prime_21 (vec3 anf_224 anf_225 anf_226))
         (set () float anf_227 (index p_prime_21 1))
         (set () float anf_228 (index p_prime_21 2))
         (set () vec2 anf_229 (vec2 anf_227 anf_228))
         (set () vec2 p_yz_22 (rotate_0 anf_229 angle_19))
         (set () float anf_230 (index p_prime_21 0))
         (set () float anf_231 (index p_yz_22 0))
         (set () float anf_232 (index p_yz_22 1))
         (set () vec3 p_prime_23 (vec3 anf_230 anf_231 anf_232))
         (set () vec2 anf_233 (vec2 1. 0.3))
         (set () float anf_234 (sdTorus_13 p_prime_23 anf_233))
         (set () vec2 anf_235 (vec2 2. 0.5))
         (set () float anf_236 (sdTorus_13 p_18 anf_235))
         (return (sMin_5 anf_234 anf_236)))))
      (Function (name march_27_189) (desc ())
       (params
        (((TyVec 3) rd_26) ((TyVec 3) ro_25) (TyFloat t_28) (TyInt steps_29)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_283 0)
         (while (< _iter_283 1000)
          (Block (set () bool anf_237 (> steps_29 80))
           (if anf_237 (Block (return t_28))
            (Block (set () vec3 anf_238 (* rd_26 t_28))
             (set () vec3 anf_239 (+ ro_25 anf_238))
             (set () float d_30 (map_17 anf_239))
             (set () bool anf_240 (< d_30 0.001))
             (if anf_240 (Block (return t_28))
              (Block (set () bool anf_241 (> t_28 100.))
               (if anf_241 (Block (return 100.1))
                (Block (set () float anf_242 (+ t_28 d_30))
                 (set () int anf_243 (+ steps_29 1)) (set rd_26 rd_26)
                 (set ro_25 ro_25) (set t_28 anf_242) (set steps_29 anf_243)
                 (set () int _iter_inc_284 (+ _iter_283 1))
                 (set _iter_283 _iter_inc_284) continue))))))))
         (return 0.))))
      (Function (name march_24) (desc ())
       (params (((TyVec 3) ro_25) ((TyVec 3) rd_26))) (ret_type TyFloat)
       (body ((return (march_27_189 rd_26 ro_25 0. 0)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_31)))
       (ret_type (TyVec 3))
       (body
        ((set () float anf_244 (index u_resolution 0))
         (set () float anf_245 (index u_resolution 1))
         (set () float res_min_32 (min anf_244 anf_245))
         (set () vec2 anf_246 (* coord_31 2.))
         (set () vec2 anf_247 (- anf_246 u_resolution))
         (set () vec2 uv_33 (/ anf_247 res_min_32))
         (set () vec2 anf_248 (* u_mouse 2.))
         (set () vec2 anf_249 (- anf_248 u_resolution))
         (set () vec2 mouseUV_34 (/ anf_249 res_min_32))
         (set () vec3 ro_init_35 (vec3 0. 0. -10.))
         (set () float anf_250 (index uv_33 0))
         (set () float anf_251 (index uv_33 1))
         (set () vec3 anf_252 (vec3 anf_250 anf_251 1.))
         (set () vec3 rd_init_36 (normalize anf_252))
         (set () float anf_253 (index mouseUV_34 1))
         (set () float rotX_37 (- 0. anf_253))
         (set () float anf_254 (index mouseUV_34 0))
         (set () float rotY_38 (- 0. anf_254))
         (set () float anf_255 (index ro_init_35 1))
         (set () float anf_256 (index ro_init_35 2))
         (set () vec2 anf_257 (vec2 anf_255 anf_256))
         (set () vec2 ro_yz_39 (rotate_0 anf_257 rotX_37))
         (set () float anf_258 (index rd_init_36 1))
         (set () float anf_259 (index rd_init_36 2))
         (set () vec2 anf_260 (vec2 anf_258 anf_259))
         (set () vec2 rd_yz_40 (rotate_0 anf_260 rotX_37))
         (set () float anf_261 (index ro_init_35 0))
         (set () float anf_262 (index ro_yz_39 0))
         (set () float anf_263 (index ro_yz_39 1))
         (set () vec3 ro_41 (vec3 anf_261 anf_262 anf_263))
         (set () float anf_264 (index rd_init_36 0))
         (set () float anf_265 (index rd_yz_40 0))
         (set () float anf_266 (index rd_yz_40 1))
         (set () vec3 rd_42 (vec3 anf_264 anf_265 anf_266))
         (set () float anf_267 (index ro_41 0))
         (set () float anf_268 (index ro_41 2))
         (set () vec2 anf_269 (vec2 anf_267 anf_268))
         (set () vec2 ro_xz_43 (rotate_0 anf_269 rotY_38))
         (set () float anf_270 (index rd_42 0))
         (set () float anf_271 (index rd_42 2))
         (set () vec2 anf_272 (vec2 anf_270 anf_271))
         (set () vec2 rd_xz_44 (rotate_0 anf_272 rotY_38))
         (set () float anf_273 (index ro_xz_43 0))
         (set () float anf_274 (index ro_41 1))
         (set () float anf_275 (index ro_xz_43 1))
         (set () vec3 ro_45 (vec3 anf_273 anf_274 anf_275))
         (set () float anf_276 (index rd_xz_44 0))
         (set () float anf_277 (index rd_42 1))
         (set () float anf_278 (index rd_xz_44 1))
         (set () vec3 rd_46 (vec3 anf_276 anf_277 anf_278))
         (set () float t_47 (march_24 ro_45 rd_46))
         (set () bool anf_279 (> t_47 100.)) (set () vec3 col_48 (vec3 0.))
         (if anf_279 (Block (set col_48 (vec3 0.2 0.2 0.2)))
          (Block (set () float anf_280 (* t_47 0.3))
           (set col_48 (palette_10 anf_280))))
         (set () vec2 anf_281 (- uv_33 mouseUV_34))
         (set () float anf_282 (length anf_281))
         (set () float glow_49 (/ 0.02 anf_282)) (return (+ col_48 glow_49)))))))

    === patch main (raymarch.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ()) (Global Uniform (TyVec 2) u_mouse ())
      (Function (name rotate_0) (desc ())
       (params (((TyVec 2) p_1) (TyFloat angle_2))) (ret_type (TyVec 2))
       (body
        ((set () float s_3 (sin angle_2)) (set () float c_4 (cos angle_2))
         (set () float anf_190 (index p_1 0))
         (set () float anf_191 (* anf_190 c_4))
         (set () float anf_192 (index p_1 1))
         (set () float anf_193 (* anf_192 s_3))
         (set () float anf_194 (- anf_191 anf_193))
         (set () float anf_195 (index p_1 0))
         (set () float anf_196 (* anf_195 s_3))
         (set () float anf_197 (index p_1 1))
         (set () float anf_198 (* anf_197 c_4))
         (set () float anf_199 (+ anf_196 anf_198))
         (return (vec2 anf_194 anf_199)))))
      (Function (name sMin_5) (desc ()) (params ((TyFloat a_6) (TyFloat b_7)))
       (ret_type TyFloat)
       (body
        ((set () float k_8 0.1) (set () float anf_200 (- b_7 a_6))
         (set () float anf_201 (* 0.5 anf_200))
         (set () float anf_202 (/ anf_201 k_8))
         (set () float anf_203 (+ 0.5 anf_202))
         (set () float h_9 (clamp anf_203 0. 1.))
         (set () float anf_204 (mix b_7 a_6 h_9))
         (set () float anf_205 (* k_8 h_9)) (set () float anf_206 (- 1. h_9))
         (set () float anf_207 (* anf_205 anf_206)) (return (- anf_204 anf_207)))))
      (Function (name palette_10) (desc ()) (params ((TyFloat t_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec3 cfg_12 (vec3 0.3 0.416 0.557))
         (set () vec3 anf_208 (+ cfg_12 t_11))
         (set () vec3 anf_209 (* anf_208 6.28318))
         (set () vec3 anf_210 (cos anf_209))
         (set () vec3 anf_211 (* anf_210 0.5)) (return (+ anf_211 0.5)))))
      (Function (name sdTorus_13) (desc ())
       (params (((TyVec 3) p_14) ((TyVec 2) t_15))) (ret_type TyFloat)
       (body
        ((set () float anf_212 (index p_14 0))
         (set () float anf_213 (index p_14 2))
         (set () vec2 anf_214 (vec2 anf_212 anf_213))
         (set () float anf_215 (length anf_214))
         (set () float anf_216 (index t_15 0))
         (set () float anf_217 (- anf_215 anf_216))
         (set () float anf_218 (index p_14 1))
         (set () vec2 q_16 (vec2 anf_217 anf_218))
         (set () float anf_219 (length q_16))
         (set () float anf_220 (index t_15 1)) (return (- anf_219 anf_220)))))
      (Function (name map_17) (desc ()) (params (((TyVec 3) p_18)))
       (ret_type TyFloat)
       (body
        ((set () float angle_19 (* u_time 2.))
         (set () float anf_221 (index p_18 0))
         (set () float anf_222 (index p_18 1))
         (set () vec2 anf_223 (vec2 anf_221 anf_222))
         (set () vec2 p_xy_20 (rotate_0 anf_223 angle_19))
         (set () float anf_224 (index p_xy_20 0))
         (set () float anf_225 (index p_xy_20 1))
         (set () float anf_226 (index p_18 2))
         (set () vec3 p_prime_21 (vec3 anf_224 anf_225 anf_226))
         (set () float anf_227 (index p_prime_21 1))
         (set () float anf_228 (index p_prime_21 2))
         (set () vec2 anf_229 (vec2 anf_227 anf_228))
         (set () vec2 p_yz_22 (rotate_0 anf_229 angle_19))
         (set () float anf_230 (index p_prime_21 0))
         (set () float anf_231 (index p_yz_22 0))
         (set () float anf_232 (index p_yz_22 1))
         (set () vec3 p_prime_23 (vec3 anf_230 anf_231 anf_232))
         (set () vec2 anf_233 (vec2 1. 0.3))
         (set () float anf_234 (sdTorus_13 p_prime_23 anf_233))
         (set () vec2 anf_235 (vec2 2. 0.5))
         (set () float anf_236 (sdTorus_13 p_18 anf_235))
         (return (sMin_5 anf_234 anf_236)))))
      (Function (name march_27_189) (desc ())
       (params
        (((TyVec 3) rd_26) ((TyVec 3) ro_25) (TyFloat t_28) (TyInt steps_29)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_283 0)
         (while (< _iter_283 1000)
          (Block (set () bool anf_237 (> steps_29 80))
           (if anf_237 (Block (return t_28))
            (Block (set () vec3 anf_238 (* rd_26 t_28))
             (set () vec3 anf_239 (+ ro_25 anf_238))
             (set () float d_30 (map_17 anf_239))
             (set () bool anf_240 (< d_30 0.001))
             (if anf_240 (Block (return t_28))
              (Block (set () bool anf_241 (> t_28 100.))
               (if anf_241 (Block (return 100.1))
                (Block (set () float anf_242 (+ t_28 d_30))
                 (set () int anf_243 (+ steps_29 1)) (set rd_26 rd_26)
                 (set ro_25 ro_25) (set t_28 anf_242) (set steps_29 anf_243)
                 (set () int _iter_inc_284 (+ _iter_283 1))
                 (set _iter_283 _iter_inc_284) continue))))))))
         (return 0.))))
      (Function (name march_24) (desc ())
       (params (((TyVec 3) ro_25) ((TyVec 3) rd_26))) (ret_type TyFloat)
       (body ((return (march_27_189 rd_26 ro_25 0. 0)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_31)))
       (ret_type (TyVec 3))
       (body
        ((set () float anf_244 (index u_resolution 0))
         (set () float anf_245 (index u_resolution 1))
         (set () float res_min_32 (min anf_244 anf_245))
         (set () vec2 anf_246 (* coord_31 2.))
         (set () vec2 anf_247 (- anf_246 u_resolution))
         (set () vec2 uv_33 (/ anf_247 res_min_32))
         (set () vec2 anf_248 (* u_mouse 2.))
         (set () vec2 anf_249 (- anf_248 u_resolution))
         (set () vec2 mouseUV_34 (/ anf_249 res_min_32))
         (set () vec3 ro_init_35 (vec3 0. 0. -10.))
         (set () float anf_250 (index uv_33 0))
         (set () float anf_251 (index uv_33 1))
         (set () vec3 anf_252 (vec3 anf_250 anf_251 1.))
         (set () vec3 rd_init_36 (normalize anf_252))
         (set () float anf_253 (index mouseUV_34 1))
         (set () float rotX_37 (- 0. anf_253))
         (set () float anf_254 (index mouseUV_34 0))
         (set () float rotY_38 (- 0. anf_254))
         (set () float anf_255 (index ro_init_35 1))
         (set () float anf_256 (index ro_init_35 2))
         (set () vec2 anf_257 (vec2 anf_255 anf_256))
         (set () vec2 ro_yz_39 (rotate_0 anf_257 rotX_37))
         (set () float anf_258 (index rd_init_36 1))
         (set () float anf_259 (index rd_init_36 2))
         (set () vec2 anf_260 (vec2 anf_258 anf_259))
         (set () vec2 rd_yz_40 (rotate_0 anf_260 rotX_37))
         (set () float anf_261 (index ro_init_35 0))
         (set () float anf_262 (index ro_yz_39 0))
         (set () float anf_263 (index ro_yz_39 1))
         (set () vec3 ro_41 (vec3 anf_261 anf_262 anf_263))
         (set () float anf_264 (index rd_init_36 0))
         (set () float anf_265 (index rd_yz_40 0))
         (set () float anf_266 (index rd_yz_40 1))
         (set () vec3 rd_42 (vec3 anf_264 anf_265 anf_266))
         (set () float anf_267 (index ro_41 0))
         (set () float anf_268 (index ro_41 2))
         (set () vec2 anf_269 (vec2 anf_267 anf_268))
         (set () vec2 ro_xz_43 (rotate_0 anf_269 rotY_38))
         (set () float anf_270 (index rd_42 0))
         (set () float anf_271 (index rd_42 2))
         (set () vec2 anf_272 (vec2 anf_270 anf_271))
         (set () vec2 rd_xz_44 (rotate_0 anf_272 rotY_38))
         (set () float anf_273 (index ro_xz_43 0))
         (set () float anf_274 (index ro_41 1))
         (set () float anf_275 (index ro_xz_43 1))
         (set () vec3 ro_45 (vec3 anf_273 anf_274 anf_275))
         (set () float anf_276 (index rd_xz_44 0))
         (set () float anf_277 (index rd_42 1))
         (set () float anf_278 (index rd_xz_44 1))
         (set () vec3 rd_46 (vec3 anf_276 anf_277 anf_278))
         (set () float t_47 (march_24 ro_45 rd_46))
         (set () bool anf_279 (> t_47 100.)) (set () vec3 col_48 (vec3 0.))
         (if anf_279 (Block (set col_48 (vec3 0.2 0.2 0.2)))
          (Block (set () float anf_280 (* t_47 0.3))
           (set col_48 (palette_10 anf_280))))
         (set () vec2 anf_281 (- uv_33 mouseUV_34))
         (set () float anf_282 (length anf_281))
         (set () float glow_49 (/ 0.02 anf_282)) (return (+ col_48 glow_49)))))
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
      (Define (Rec 1000) gcd
       (lambda (a ())
        (lambda (b ())
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
      (Define (Rec 1000) gcd_8
       (lambda (a_9 ())
        (lambda (b_10 ())
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
        ((lambda coord_1
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
        ((lambda angle_5
          ((let s_6 ((sin (angle_5 : float)) : float)
            ((let c_7 ((cos (angle_5 : float)) : float)
              ((mat2x2 (c_7 : float) ((* (-1. : float) (s_6 : float)) : float)
                (s_6 : float) (c_7 : float))
               : (mat 2 2)))
             : (mat 2 2)))
           : (mat 2 2)))
         : (float -> (mat 2 2))))
       : (float -> (mat 2 2)))
      ((Define (Rec 1000) gcd_8
        ((lambda a_9
          ((lambda b_10
            ((if ((< (a_9 : 'v_42) (0.05 : float)) : bool) (b_10 : 'v_42)
              ((if ((< (b_10 : 'v_42) (0.05 : float)) : bool) (a_9 : 'v_42)
                ((if ((> (a_9 : 'v_42) (b_10 : 'v_42)) : bool)
                  ((app
                    ((app (gcd_8 : ('v_42 -> ('v_42 -> 'v_42)))
                      ((- (a_9 : 'v_42) (b_10 : 'v_42)) : 'v_42))
                     : ('v_42 -> 'v_42))
                    (b_10 : 'v_42))
                   : 'v_42)
                  ((app
                    ((app (gcd_8 : ('v_42 -> ('v_42 -> 'v_42))) (a_9 : 'v_42)) :
                     ('v_42 -> 'v_42))
                    ((- (b_10 : 'v_42) (a_9 : 'v_42)) : 'v_42))
                   : 'v_42))
                 : 'v_42))
               : 'v_42))
             : 'v_42))
           : ('v_42 -> 'v_42)))
         : ('v_42 -> ('v_42 -> 'v_42))))
       :
       (forall
        ((Broadcast 'v_42 float 'v_33) (Comparable 'v_33)
         (Broadcast 'v_42 float 'v_35) (Comparable 'v_35)
         (Broadcast 'v_42 'v_42 'v_42) (Comparable 'v_42)
         (Broadcast 'v_42 'v_42 'v_42) (Broadcast 'v_42 'v_42 'v_42))
        ('v_42 -> ('v_42 -> 'v_42))))
      ((Define Nonrec main
        ((lambda coord_11
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

    === specialize params (recursion.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec get_uv_0
        ((lambda coord_1
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
        ((lambda angle_5
          ((let s_6 ((sin (angle_5 : float)) : float)
            ((let c_7 ((cos (angle_5 : float)) : float)
              ((mat2x2 (c_7 : float) ((* (-1. : float) (s_6 : float)) : float)
                (s_6 : float) (c_7 : float))
               : (mat 2 2)))
             : (mat 2 2)))
           : (mat 2 2)))
         : (float -> (mat 2 2))))
       : (float -> (mat 2 2)))
      ((Define (Rec 1000) gcd_8
        ((lambda a_9
          ((lambda b_10
            ((if ((< (a_9 : 'v_42) (0.05 : float)) : bool) (b_10 : 'v_42)
              ((if ((< (b_10 : 'v_42) (0.05 : float)) : bool) (a_9 : 'v_42)
                ((if ((> (a_9 : 'v_42) (b_10 : 'v_42)) : bool)
                  ((app
                    ((app (gcd_8 : ('v_42 -> ('v_42 -> 'v_42)))
                      ((- (a_9 : 'v_42) (b_10 : 'v_42)) : 'v_42))
                     : ('v_42 -> 'v_42))
                    (b_10 : 'v_42))
                   : 'v_42)
                  ((app
                    ((app (gcd_8 : ('v_42 -> ('v_42 -> 'v_42))) (a_9 : 'v_42)) :
                     ('v_42 -> 'v_42))
                    ((- (b_10 : 'v_42) (a_9 : 'v_42)) : 'v_42))
                   : 'v_42))
                 : 'v_42))
               : 'v_42))
             : 'v_42))
           : ('v_42 -> 'v_42)))
         : ('v_42 -> ('v_42 -> 'v_42))))
       :
       (forall
        ((Broadcast 'v_42 float 'v_33) (Comparable 'v_33)
         (Broadcast 'v_42 float 'v_35) (Comparable 'v_35)
         (Broadcast 'v_42 'v_42 'v_42) (Comparable 'v_42)
         (Broadcast 'v_42 'v_42 'v_42) (Broadcast 'v_42 'v_42 'v_42))
        ('v_42 -> ('v_42 -> 'v_42))))
      ((Define Nonrec main
        ((lambda coord_11
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
        ((lambda coord_1
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
        ((lambda angle_5
          ((let s_6 ((sin (angle_5 : float)) : float)
            ((let c_7 ((cos (angle_5 : float)) : float)
              ((mat2x2 (c_7 : float) ((* (-1. : float) (s_6 : float)) : float)
                (s_6 : float) (c_7 : float))
               : (mat 2 2)))
             : (mat 2 2)))
           : (mat 2 2)))
         : (float -> (mat 2 2))))
       : (float -> (mat 2 2)))
      ((Define (Rec 1000) gcd_8_float_to_float_to_float_70
        ((lambda a_9
          ((lambda b_10
            ((if ((< (a_9 : float) (0.05 : float)) : bool) (b_10 : float)
              ((if ((< (b_10 : float) (0.05 : float)) : bool) (a_9 : float)
                ((if ((> (a_9 : float) (b_10 : float)) : bool)
                  ((app
                    ((app
                      (gcd_8_float_to_float_to_float_70 :
                       (float -> (float -> float)))
                      ((- (a_9 : float) (b_10 : float)) : float))
                     : (float -> float))
                    (b_10 : float))
                   : float)
                  ((app
                    ((app
                      (gcd_8_float_to_float_to_float_70 :
                       (float -> (float -> float)))
                      (a_9 : float))
                     : (float -> float))
                    ((- (b_10 : float) (a_9 : float)) : float))
                   : float))
                 : float))
               : float))
             : float))
           : (float -> float)))
         : (float -> (float -> float))))
       : (float -> (float -> float)))
      ((Define Nonrec main
        ((lambda coord_11
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
                      ((app
                        (gcd_8_float_to_float_to_float_70 :
                         (float -> (float -> float)))
                        (x_14 : float))
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
      ((Define (Rec 1000) gcd_8_float_to_float_to_float_70
        (lambda ((a_9 float) (b_10 float))
         (if (< a_9 0.05) b_10
          (if (< b_10 0.05) a_9
           (if (> a_9 b_10)
            (app gcd_8_float_to_float_to_float_70 (- a_9 b_10) b_10)
            (app gcd_8_float_to_float_to_float_70 a_9 (- b_10 a_9)))))))
       : (float -> (float -> float)))
      ((Define Nonrec main
        (lambda ((coord_11 (vec 2)))
         (let uv_12 (app get_uv_0 coord_11)
          (let uv_13 (* (app rotate_4 u_time) uv_12)
           (let x_14 (abs (* (* (index uv_13 0) (sin (* u_time 2.))) 2.))
            (let y_15 (abs (* (* (index uv_13 1) (sin (* u_time 2.))) 2.))
             (let res_16 (app gcd_8_float_to_float_to_float_70 x_14 y_15)
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
     ((Define (Rec 1000) (name gcd_8_float_to_float_to_float_70)
       (args ((a_9 float) (b_10 float)))
       (body
        (if (< a_9 0.05) b_10
         (if (< b_10 0.05) a_9
          (if (> a_9 b_10)
           (app gcd_8_float_to_float_to_float_70 (- a_9 b_10) b_10)
           (app gcd_8_float_to_float_to_float_70 a_9 (- b_10 a_9)))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (app get_uv_0 coord_11)
         (let uv_13 (* (app rotate_4 u_time) uv_12)
          (let x_14 (abs (* (* (index uv_13 0) (sin (* u_time 2.))) 2.))
           (let y_15 (abs (* (* (index uv_13 1) (sin (* u_time 2.))) 2.))
            (let res_16 (app gcd_8_float_to_float_to_float_70 x_14 y_15)
             (vec3 res_16 (* res_16 0.5) (- 1. res_16)))))))))
      : ((vec 2) -> (vec 3))))

    === anf (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_71 (* 2. coord_1)
         (let top_2 (- anf_71 u_resolution)
          (let anf_72 (index u_resolution 0)
           (let anf_73 (index u_resolution 1)
            (let bot_3 (min anf_72 anf_73) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_74 (* -1. s_6) (return (mat2x2 c_7 anf_74 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (Rec 1000) (name gcd_8_float_to_float_to_float_70)
       (args ((a_9 float) (b_10 float)))
       (body
        (let anf_75 (< a_9 0.05)
         (return
          (if anf_75 (return b_10)
           (let anf_76 (< b_10 0.05)
            (return
             (if anf_76 (return a_9)
              (let anf_77 (> a_9 b_10)
               (return
                (if anf_77
                 (let anf_78 (- a_9 b_10)
                  (return (gcd_8_float_to_float_to_float_70 anf_78 b_10)))
                 (let anf_79 (- b_10 a_9)
                  (return (gcd_8_float_to_float_to_float_70 a_9 anf_79))))))))))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_80 (rotate_4 u_time)
          (let uv_13 (* anf_80 uv_12)
           (let anf_81 (index uv_13 0)
            (let anf_82 (* u_time 2.)
             (let anf_83 (sin anf_82)
              (let anf_84 (* anf_81 anf_83)
               (let anf_85 (* anf_84 2.)
                (let x_14 (abs anf_85)
                 (let anf_86 (index uv_13 1)
                  (let anf_87 (* u_time 2.)
                   (let anf_88 (sin anf_87)
                    (let anf_89 (* anf_86 anf_88)
                     (let anf_90 (* anf_89 2.)
                      (let y_15 (abs anf_90)
                       (let res_16 (gcd_8_float_to_float_to_float_70 x_14 y_15)
                        (let anf_91 (* res_16 0.5)
                         (let anf_92 (- 1. res_16)
                          (return (vec3 res_16 anf_91 anf_92))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_71 (* 2. coord_1)
         (let top_2 (- anf_71 u_resolution)
          (let anf_72 (index u_resolution 0)
           (let anf_73 (index u_resolution 1)
            (let bot_3 (min anf_72 anf_73) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_74 (* -1. s_6) (return (mat2x2 c_7 anf_74 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (name gcd_8_float_to_float_to_float_70)
       (args ((a_9 float) (b_10 float)))
       (body
        (let _iter_93 0
         (while (< _iter_93 1000)
          (let anf_75 (< a_9 0.05)
           (return
            (if anf_75 (return b_10)
             (let anf_76 (< b_10 0.05)
              (return
               (if anf_76 (return a_9)
                (let anf_77 (> a_9 b_10)
                 (return
                  (if anf_77
                   (let anf_78 (- a_9 b_10)
                    (set a_9 anf_78
                     (set b_10 b_10
                      (let _iter_inc_94 (+ _iter_93 1)
                       (set _iter_93 _iter_inc_94 continue)))))
                   (let anf_79 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_79
                      (let _iter_inc_95 (+ _iter_93 1)
                       (set _iter_93 _iter_inc_95 continue))))))))))))))
          (return 0.)))))
      : (float -> (float -> float)))
     ((Define (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_80 (rotate_4 u_time)
          (let uv_13 (* anf_80 uv_12)
           (let anf_81 (index uv_13 0)
            (let anf_82 (* u_time 2.)
             (let anf_83 (sin anf_82)
              (let anf_84 (* anf_81 anf_83)
               (let anf_85 (* anf_84 2.)
                (let x_14 (abs anf_85)
                 (let anf_86 (index uv_13 1)
                  (let anf_87 (* u_time 2.)
                   (let anf_88 (sin anf_87)
                    (let anf_89 (* anf_86 anf_88)
                     (let anf_90 (* anf_89 2.)
                      (let y_15 (abs anf_90)
                       (let res_16 (gcd_8_float_to_float_to_float_70 x_14 y_15)
                        (let anf_91 (* res_16 0.5)
                         (let anf_92 (- 1. res_16)
                          (return (vec3 res_16 anf_91 anf_92))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_71 (* 2. coord_1)
         (let top_2 (- anf_71 u_resolution)
          (let anf_72 (index u_resolution 0)
           (let anf_73 (index u_resolution 1)
            (let bot_3 (min anf_72 anf_73) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_74 (* -1. s_6) (return (mat2x2 c_7 anf_74 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (name gcd_8_float_to_float_to_float_70)
       (args ((a_9 float) (b_10 float)))
       (body
        (let _iter_93 0
         (while (< _iter_93 1000)
          (let anf_75 (< a_9 0.05)
           (return
            (if anf_75 (return b_10)
             (let anf_76 (< b_10 0.05)
              (return
               (if anf_76 (return a_9)
                (let anf_77 (> a_9 b_10)
                 (return
                  (if anf_77
                   (let anf_78 (- a_9 b_10)
                    (set a_9 anf_78
                     (set b_10 b_10
                      (let _iter_inc_94 (+ _iter_93 1)
                       (set _iter_93 _iter_inc_94 continue)))))
                   (let anf_79 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_79
                      (let _iter_inc_95 (+ _iter_93 1)
                       (set _iter_93 _iter_inc_95 continue))))))))))))))
          (return 0.)))))
      : (float -> (float -> float)))
     ((Define (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_80 (rotate_4 u_time)
          (let uv_13 (* anf_80 uv_12)
           (let anf_81 (index uv_13 0)
            (let anf_82 (* u_time 2.)
             (let anf_83 (sin anf_82)
              (let anf_84 (* anf_81 anf_83)
               (let anf_85 (* anf_84 2.)
                (let x_14 (abs anf_85)
                 (let anf_86 (index uv_13 1)
                  (let anf_87 (* u_time 2.)
                   (let anf_88 (sin anf_87)
                    (let anf_89 (* anf_86 anf_88)
                     (let anf_90 (* anf_89 2.)
                      (let y_15 (abs anf_90)
                       (let res_16 (gcd_8_float_to_float_to_float_70 x_14 y_15)
                        (let anf_91 (* res_16 0.5)
                         (let anf_92 (- 1. res_16)
                          (return (vec3 res_16 anf_91 anf_92))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_71 (* 2. coord_1)
         (let top_2 (- anf_71 u_resolution)
          (let anf_72 (index u_resolution 0)
           (let anf_73 (index u_resolution 1)
            (let bot_3 (min anf_72 anf_73) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_74 (* -1. s_6) (return (mat2x2 c_7 anf_74 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (name gcd_8_float_to_float_to_float_70)
       (args ((a_9 float) (b_10 float)))
       (body
        (let _iter_93 0
         (while (< _iter_93 1000)
          (let anf_75 (< a_9 0.05)
           (return
            (if anf_75 (return b_10)
             (let anf_76 (< b_10 0.05)
              (return
               (if anf_76 (return a_9)
                (let anf_77 (> a_9 b_10)
                 (return
                  (if anf_77
                   (let anf_78 (- a_9 b_10)
                    (set a_9 anf_78
                     (set b_10 b_10
                      (let _iter_inc_94 (+ _iter_93 1)
                       (set _iter_93 _iter_inc_94 continue)))))
                   (let anf_79 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_79
                      (let _iter_inc_95 (+ _iter_93 1)
                       (set _iter_93 _iter_inc_95 continue))))))))))))))
          (return 0.)))))
      : (float -> (float -> float)))
     ((Define (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_80 (rotate_4 u_time)
          (let uv_13 (* anf_80 uv_12)
           (let anf_81 (index uv_13 0)
            (let anf_82 (* u_time 2.)
             (let anf_83 (sin anf_82)
              (let anf_84 (* anf_81 anf_83)
               (let anf_85 (* anf_84 2.)
                (let x_14 (abs anf_85)
                 (let anf_86 (index uv_13 1)
                  (let anf_87 (* u_time 2.)
                   (let anf_88 (sin anf_87)
                    (let anf_89 (* anf_86 anf_88)
                     (let anf_90 (* anf_89 2.)
                      (let y_15 (abs anf_90)
                       (let res_16 (gcd_8_float_to_float_to_float_70 x_14 y_15)
                        (let anf_91 (* res_16 0.5)
                         (let anf_92 (- 1. res_16)
                          (return (vec3 res_16 anf_91 anf_92))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (recursion.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_71 (* 2. coord_1))
         (set () vec2 top_2 (- anf_71 u_resolution))
         (set () float anf_72 (index u_resolution 0))
         (set () float anf_73 (index u_resolution 1))
         (set () float bot_3 (min anf_72 anf_73)) (return (/ top_2 bot_3)))))
      (Function (name rotate_4) (desc ()) (params ((TyFloat angle_5)))
       (ret_type (TyMat 2 2))
       (body
        ((set () float s_6 (sin angle_5)) (set () float c_7 (cos angle_5))
         (set () float anf_74 (* -1. s_6)) (return (mat2 c_7 anf_74 s_6 c_7)))))
      (Function (name gcd_8_float_to_float_to_float_70) (desc ())
       (params ((TyFloat a_9) (TyFloat b_10))) (ret_type TyFloat)
       (body
        ((set () int _iter_93 0)
         (while (< _iter_93 1000)
          (Block (set () bool anf_75 (< a_9 0.05))
           (if anf_75 (Block (return b_10))
            (Block (set () bool anf_76 (< b_10 0.05))
             (if anf_76 (Block (return a_9))
              (Block (set () bool anf_77 (> a_9 b_10))
               (if anf_77
                (Block (set () float anf_78 (- a_9 b_10)) (set a_9 anf_78)
                 (set b_10 b_10) (set () int _iter_inc_94 (+ _iter_93 1))
                 (set _iter_93 _iter_inc_94) continue)
                (Block (set () float anf_79 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_79) (set () int _iter_inc_95 (+ _iter_93 1))
                 (set _iter_93 _iter_inc_95) continue))))))))
         (return 0.))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_12 (get_uv_0 coord_11))
         (set () mat2 anf_80 (rotate_4 u_time))
         (set () vec2 uv_13 (* anf_80 uv_12))
         (set () float anf_81 (index uv_13 0))
         (set () float anf_82 (* u_time 2.)) (set () float anf_83 (sin anf_82))
         (set () float anf_84 (* anf_81 anf_83))
         (set () float anf_85 (* anf_84 2.)) (set () float x_14 (abs anf_85))
         (set () float anf_86 (index uv_13 1))
         (set () float anf_87 (* u_time 2.)) (set () float anf_88 (sin anf_87))
         (set () float anf_89 (* anf_86 anf_88))
         (set () float anf_90 (* anf_89 2.)) (set () float y_15 (abs anf_90))
         (set () float res_16 (gcd_8_float_to_float_to_float_70 x_14 y_15))
         (set () float anf_91 (* res_16 0.5)) (set () float anf_92 (- 1. res_16))
         (return (vec3 res_16 anf_91 anf_92)))))))

    === patch main (recursion.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_71 (* 2. coord_1))
         (set () vec2 top_2 (- anf_71 u_resolution))
         (set () float anf_72 (index u_resolution 0))
         (set () float anf_73 (index u_resolution 1))
         (set () float bot_3 (min anf_72 anf_73)) (return (/ top_2 bot_3)))))
      (Function (name rotate_4) (desc ()) (params ((TyFloat angle_5)))
       (ret_type (TyMat 2 2))
       (body
        ((set () float s_6 (sin angle_5)) (set () float c_7 (cos angle_5))
         (set () float anf_74 (* -1. s_6)) (return (mat2 c_7 anf_74 s_6 c_7)))))
      (Function (name gcd_8_float_to_float_to_float_70) (desc ())
       (params ((TyFloat a_9) (TyFloat b_10))) (ret_type TyFloat)
       (body
        ((set () int _iter_93 0)
         (while (< _iter_93 1000)
          (Block (set () bool anf_75 (< a_9 0.05))
           (if anf_75 (Block (return b_10))
            (Block (set () bool anf_76 (< b_10 0.05))
             (if anf_76 (Block (return a_9))
              (Block (set () bool anf_77 (> a_9 b_10))
               (if anf_77
                (Block (set () float anf_78 (- a_9 b_10)) (set a_9 anf_78)
                 (set b_10 b_10) (set () int _iter_inc_94 (+ _iter_93 1))
                 (set _iter_93 _iter_inc_94) continue)
                (Block (set () float anf_79 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_79) (set () int _iter_inc_95 (+ _iter_93 1))
                 (set _iter_93 _iter_inc_95) continue))))))))
         (return 0.))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_12 (get_uv_0 coord_11))
         (set () mat2 anf_80 (rotate_4 u_time))
         (set () vec2 uv_13 (* anf_80 uv_12))
         (set () float anf_81 (index uv_13 0))
         (set () float anf_82 (* u_time 2.)) (set () float anf_83 (sin anf_82))
         (set () float anf_84 (* anf_81 anf_83))
         (set () float anf_85 (* anf_84 2.)) (set () float x_14 (abs anf_85))
         (set () float anf_86 (index uv_13 1))
         (set () float anf_87 (* u_time 2.)) (set () float anf_88 (sin anf_87))
         (set () float anf_89 (* anf_86 anf_88))
         (set () float anf_90 (* anf_89 2.)) (set () float y_15 (abs anf_90))
         (set () float res_16 (gcd_8_float_to_float_to_float_70 x_14 y_15))
         (set () float anf_91 (* res_16 0.5)) (set () float anf_92 (- 1. res_16))
         (return (vec3 res_16 anf_91 anf_92)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))

    ====== COMPILING EXAMPLE warped_noise.glml ======

    === stlc (warped_noise.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec smoothNoise
       (lambda (p ((vec 2)))
        (let i (floor p)
         (let pf (- p i)
          (let inter (* (* pf pf) (- 3. (* 2. pf)))
           (let v4 (vec4 0. 1. 27. 28.)
            (let seed (+ (+ v4 (index i 0)) (* (index i 1) 27.))
             (let hash (fract (* (sin (% seed 6.2831853)) 200000.))
              (let col0 (vec2 (index hash 0) (index hash 1))
               (let col1 (vec2 (index hash 2) (index hash 3))
                (let res_v
                 (+ (* col0 (- 1. (index inter 1))) (* col1 (index inter 1)))
                 (dot res_v (vec2 (- 1. (index inter 0)) (index inter 0))))))))))))))
      (Define Nonrec fractalNoise
       (lambda (p ())
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
                   (let spot_logic
                    (+ (* (vec3 0.8 0.4 1.) 0.35)
                     (*
                      (+
                       (* (vec3 1. 0.5 0.2) (smoothstep 0. 1. (- 1. spot1_dist)))
                       (* (vec3 0.2 0.4 1.) (smoothstep 0. 1. (- 1. spot2_dist))))
                      5.))
                    (let final_col (* col spot_logic) (sqrt (max final_col 0.)))))))))))))))))))

    === uniquify (warped_noise.glml) ===
    (Program
     ((Extern (vec 2) u_resolution) (Extern float u_time)
      (Define Nonrec smoothNoise_0
       (lambda (p_1 ((vec 2)))
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let inter_4 (* (* pf_3 pf_3) (- 3. (* 2. pf_3)))
           (let v4_5 (vec4 0. 1. 27. 28.)
            (let seed_6 (+ (+ v4_5 (index i_2 0)) (* (index i_2 1) 27.))
             (let hash_7 (fract (* (sin (% seed_6 6.2831853)) 200000.))
              (let col0_8 (vec2 (index hash_7 0) (index hash_7 1))
               (let col1_9 (vec2 (index hash_7 2) (index hash_7 3))
                (let res_v_10
                 (+ (* col0_8 (- 1. (index inter_4 1)))
                  (* col1_9 (index inter_4 1)))
                 (dot res_v_10 (vec2 (- 1. (index inter_4 0)) (index inter_4 0))))))))))))))
      (Define Nonrec fractalNoise_11
       (lambda (p_12 ())
        (+
         (+
          (+ (* (app smoothNoise_0 p_12) 0.5333)
           (* (app smoothNoise_0 (* p_12 2.)) 0.2667))
          (* (app smoothNoise_0 (* p_12 4.)) 0.1333))
         (* (app smoothNoise_0 (* p_12 8.)) 0.0667))))
      (Define Nonrec warpedNoise_13
       (lambda (p_14 ((vec 2)))
        (let m_15 (* (vec2 u_time (- 0. u_time)) 0.5)
         (let x_16 (app fractalNoise_11 (+ p_14 m_15))
          (let y_17
           (app fractalNoise_11
            (+ (+ p_14 (vec2 (index m_15 1) (index m_15 0))) x_16))
           (let z_18 (app fractalNoise_11 (+ (- (- p_14 m_15) x_16) y_17))
            (let warp_19
             (+ (+ (vec2 x_16 y_17) (vec2 y_17 z_18)) (vec2 z_18 x_16))
             (let mag_20 (* (length (vec3 x_16 y_17 z_18)) 0.25)
              (app fractalNoise_11 (+ (+ p_14 warp_19) mag_20))))))))))
      (Define Nonrec main
       (lambda (coord_21 ((vec 2)))
        (let uv_22 (/ (- coord_21 (* u_resolution 0.5)) (index u_resolution 1))
         (let n_23 (app warpedNoise_13 (* uv_22 6.))
          (let n2_24 (app warpedNoise_13 (- (* uv_22 6.) 0.02))
           (let bump_25 (* (/ (max (- n2_24 n_23) 0.) 0.02) 0.7071)
            (let bump2_26 (* (/ (max (- n_23 n2_24) 0.) 0.02) 0.7071)
             (let b1_27 (+ (* bump_25 bump_25) (* (pow bump_25 4.) 0.5))
              (let b2_28 (+ (* bump2_26 bump2_26) (* (pow bump2_26 4.) 0.5))
               (let base_col_29
                (+
                 (*
                  (* (vec3 1. 0.7 0.6)
                   (vec3 b1_27 (* (+ b1_27 b2_28) 0.4) b2_28))
                  0.3)
                 0.5)
                (let col_30 (* (* n_23 n_23) base_col_29)
                 (let spot1_dist_31 (length (- uv_22 0.65))
                  (let spot2_dist_32 (length (+ uv_22 0.5))
                   (let spot_logic_33
                    (+ (* (vec3 0.8 0.4 1.) 0.35)
                     (*
                      (+
                       (* (vec3 1. 0.5 0.2)
                        (smoothstep 0. 1. (- 1. spot1_dist_31)))
                       (* (vec3 0.2 0.4 1.)
                        (smoothstep 0. 1. (- 1. spot2_dist_32))))
                      5.))
                    (let final_col_34 (* col_30 spot_logic_33)
                     (sqrt (max final_col_34 0.)))))))))))))))))))

    === typecheck (warped_noise.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec smoothNoise_0
        ((lambda p_1
          ((let i_2 ((floor (p_1 : (vec 2))) : (vec 2))
            ((let pf_3 ((- (p_1 : (vec 2)) (i_2 : (vec 2))) : (vec 2))
              ((let inter_4
                ((* ((* (pf_3 : (vec 2)) (pf_3 : (vec 2))) : (vec 2))
                  ((- (3. : float) ((* (2. : float) (pf_3 : (vec 2))) : (vec 2)))
                   : (vec 2)))
                 : (vec 2))
                ((let v4_5
                  ((vec4 (0. : float) (1. : float) (27. : float) (28. : float)) :
                   (vec 4))
                  ((let seed_6
                    ((+
                      ((+ (v4_5 : (vec 4)) ((index (i_2 : (vec 2)) 0) : float)) :
                       (vec 4))
                      ((* ((index (i_2 : (vec 2)) 1) : float) (27. : float)) :
                       float))
                     : (vec 4))
                    ((let hash_7
                      ((fract
                        ((*
                          ((sin
                            ((% (seed_6 : (vec 4)) (6.2831853 : float)) :
                             (vec 4)))
                           : (vec 4))
                          (200000. : float))
                         : (vec 4)))
                       : (vec 4))
                      ((let col0_8
                        ((vec2 ((index (hash_7 : (vec 4)) 0) : float)
                          ((index (hash_7 : (vec 4)) 1) : float))
                         : (vec 2))
                        ((let col1_9
                          ((vec2 ((index (hash_7 : (vec 4)) 2) : float)
                            ((index (hash_7 : (vec 4)) 3) : float))
                           : (vec 2))
                          ((let res_v_10
                            ((+
                              ((* (col0_8 : (vec 2))
                                ((- (1. : float)
                                  ((index (inter_4 : (vec 2)) 1) : float))
                                 : float))
                               : (vec 2))
                              ((* (col1_9 : (vec 2))
                                ((index (inter_4 : (vec 2)) 1) : float))
                               : (vec 2)))
                             : (vec 2))
                            ((dot (res_v_10 : (vec 2))
                              ((vec2
                                ((- (1. : float)
                                  ((index (inter_4 : (vec 2)) 0) : float))
                                 : float)
                                ((index (inter_4 : (vec 2)) 0) : float))
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
      ((Define Nonrec fractalNoise_11
        ((lambda p_12
          ((+
            ((+
              ((+
                ((*
                  ((app (smoothNoise_0 : ((vec 2) -> float)) (p_12 : (vec 2))) :
                   float)
                  (0.5333 : float))
                 : float)
                ((*
                  ((app (smoothNoise_0 : ((vec 2) -> float))
                    ((* (p_12 : (vec 2)) (2. : float)) : (vec 2)))
                   : float)
                  (0.2667 : float))
                 : float))
               : float)
              ((*
                ((app (smoothNoise_0 : ((vec 2) -> float))
                  ((* (p_12 : (vec 2)) (4. : float)) : (vec 2)))
                 : float)
                (0.1333 : float))
               : float))
             : float)
            ((*
              ((app (smoothNoise_0 : ((vec 2) -> float))
                ((* (p_12 : (vec 2)) (8. : float)) : (vec 2)))
               : float)
              (0.0667 : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec warpedNoise_13
        ((lambda p_14
          ((let m_15
            ((*
              ((vec2 (u_time : float)
                ((- (0. : float) (u_time : float)) : float))
               : (vec 2))
              (0.5 : float))
             : (vec 2))
            ((let x_16
              ((app (fractalNoise_11 : ((vec 2) -> float))
                ((+ (p_14 : (vec 2)) (m_15 : (vec 2))) : (vec 2)))
               : float)
              ((let y_17
                ((app (fractalNoise_11 : ((vec 2) -> float))
                  ((+
                    ((+ (p_14 : (vec 2))
                      ((vec2 ((index (m_15 : (vec 2)) 1) : float)
                        ((index (m_15 : (vec 2)) 0) : float))
                       : (vec 2)))
                     : (vec 2))
                    (x_16 : float))
                   : (vec 2)))
                 : float)
                ((let z_18
                  ((app (fractalNoise_11 : ((vec 2) -> float))
                    ((+
                      ((- ((- (p_14 : (vec 2)) (m_15 : (vec 2))) : (vec 2))
                        (x_16 : float))
                       : (vec 2))
                      (y_17 : float))
                     : (vec 2)))
                   : float)
                  ((let warp_19
                    ((+
                      ((+ ((vec2 (x_16 : float) (y_17 : float)) : (vec 2))
                        ((vec2 (y_17 : float) (z_18 : float)) : (vec 2)))
                       : (vec 2))
                      ((vec2 (z_18 : float) (x_16 : float)) : (vec 2)))
                     : (vec 2))
                    ((let mag_20
                      ((*
                        ((length
                          ((vec3 (x_16 : float) (y_17 : float) (z_18 : float)) :
                           (vec 3)))
                         : float)
                        (0.25 : float))
                       : float)
                      ((app (fractalNoise_11 : ((vec 2) -> float))
                        ((+ ((+ (p_14 : (vec 2)) (warp_19 : (vec 2))) : (vec 2))
                          (mag_20 : float))
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
        ((lambda coord_21
          ((let uv_22
            ((/
              ((- (coord_21 : (vec 2))
                ((* (u_resolution : (vec 2)) (0.5 : float)) : (vec 2)))
               : (vec 2))
              ((index (u_resolution : (vec 2)) 1) : float))
             : (vec 2))
            ((let n_23
              ((app (warpedNoise_13 : ((vec 2) -> float))
                ((* (uv_22 : (vec 2)) (6. : float)) : (vec 2)))
               : float)
              ((let n2_24
                ((app (warpedNoise_13 : ((vec 2) -> float))
                  ((- ((* (uv_22 : (vec 2)) (6. : float)) : (vec 2))
                    (0.02 : float))
                   : (vec 2)))
                 : float)
                ((let bump_25
                  ((*
                    ((/
                      ((max ((- (n2_24 : float) (n_23 : float)) : float)
                        (0. : float))
                       : float)
                      (0.02 : float))
                     : float)
                    (0.7071 : float))
                   : float)
                  ((let bump2_26
                    ((*
                      ((/
                        ((max ((- (n_23 : float) (n2_24 : float)) : float)
                          (0. : float))
                         : float)
                        (0.02 : float))
                       : float)
                      (0.7071 : float))
                     : float)
                    ((let b1_27
                      ((+ ((* (bump_25 : float) (bump_25 : float)) : float)
                        ((* ((pow (bump_25 : float) (4. : float)) : float)
                          (0.5 : float))
                         : float))
                       : float)
                      ((let b2_28
                        ((+ ((* (bump2_26 : float) (bump2_26 : float)) : float)
                          ((* ((pow (bump2_26 : float) (4. : float)) : float)
                            (0.5 : float))
                           : float))
                         : float)
                        ((let base_col_29
                          ((+
                            ((*
                              ((*
                                ((vec3 (1. : float) (0.7 : float) (0.6 : float))
                                 : (vec 3))
                                ((vec3 (b1_27 : float)
                                  ((*
                                    ((+ (b1_27 : float) (b2_28 : float)) : float)
                                    (0.4 : float))
                                   : float)
                                  (b2_28 : float))
                                 : (vec 3)))
                               : (vec 3))
                              (0.3 : float))
                             : (vec 3))
                            (0.5 : float))
                           : (vec 3))
                          ((let col_30
                            ((* ((* (n_23 : float) (n_23 : float)) : float)
                              (base_col_29 : (vec 3)))
                             : (vec 3))
                            ((let spot1_dist_31
                              ((length
                                ((- (uv_22 : (vec 2)) (0.65 : float)) : (vec 2)))
                               : float)
                              ((let spot2_dist_32
                                ((length
                                  ((+ (uv_22 : (vec 2)) (0.5 : float)) : (vec 2)))
                                 : float)
                                ((let spot_logic_33
                                  ((+
                                    ((*
                                      ((vec3 (0.8 : float) (0.4 : float)
                                        (1. : float))
                                       : (vec 3))
                                      (0.35 : float))
                                     : (vec 3))
                                    ((*
                                      ((+
                                        ((*
                                          ((vec3 (1. : float) (0.5 : float)
                                            (0.2 : float))
                                           : (vec 3))
                                          ((smoothstep (0. : float) (1. : float)
                                            ((- (1. : float)
                                              (spot1_dist_31 : float))
                                             : float))
                                           : float))
                                         : (vec 3))
                                        ((*
                                          ((vec3 (0.2 : float) (0.4 : float)
                                            (1. : float))
                                           : (vec 3))
                                          ((smoothstep (0. : float) (1. : float)
                                            ((- (1. : float)
                                              (spot2_dist_32 : float))
                                             : float))
                                           : float))
                                         : (vec 3)))
                                       : (vec 3))
                                      (5. : float))
                                     : (vec 3)))
                                   : (vec 3))
                                  ((let final_col_34
                                    ((* (col_30 : (vec 3))
                                      (spot_logic_33 : (vec 3)))
                                     : (vec 3))
                                    ((sqrt
                                      ((max (final_col_34 : (vec 3))
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
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === specialize params (warped_noise.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec smoothNoise_0
        ((lambda p_1
          ((let i_2 ((floor (p_1 : (vec 2))) : (vec 2))
            ((let pf_3 ((- (p_1 : (vec 2)) (i_2 : (vec 2))) : (vec 2))
              ((let inter_4
                ((* ((* (pf_3 : (vec 2)) (pf_3 : (vec 2))) : (vec 2))
                  ((- (3. : float) ((* (2. : float) (pf_3 : (vec 2))) : (vec 2)))
                   : (vec 2)))
                 : (vec 2))
                ((let v4_5
                  ((vec4 (0. : float) (1. : float) (27. : float) (28. : float)) :
                   (vec 4))
                  ((let seed_6
                    ((+
                      ((+ (v4_5 : (vec 4)) ((index (i_2 : (vec 2)) 0) : float)) :
                       (vec 4))
                      ((* ((index (i_2 : (vec 2)) 1) : float) (27. : float)) :
                       float))
                     : (vec 4))
                    ((let hash_7
                      ((fract
                        ((*
                          ((sin
                            ((% (seed_6 : (vec 4)) (6.2831853 : float)) :
                             (vec 4)))
                           : (vec 4))
                          (200000. : float))
                         : (vec 4)))
                       : (vec 4))
                      ((let col0_8
                        ((vec2 ((index (hash_7 : (vec 4)) 0) : float)
                          ((index (hash_7 : (vec 4)) 1) : float))
                         : (vec 2))
                        ((let col1_9
                          ((vec2 ((index (hash_7 : (vec 4)) 2) : float)
                            ((index (hash_7 : (vec 4)) 3) : float))
                           : (vec 2))
                          ((let res_v_10
                            ((+
                              ((* (col0_8 : (vec 2))
                                ((- (1. : float)
                                  ((index (inter_4 : (vec 2)) 1) : float))
                                 : float))
                               : (vec 2))
                              ((* (col1_9 : (vec 2))
                                ((index (inter_4 : (vec 2)) 1) : float))
                               : (vec 2)))
                             : (vec 2))
                            ((dot (res_v_10 : (vec 2))
                              ((vec2
                                ((- (1. : float)
                                  ((index (inter_4 : (vec 2)) 0) : float))
                                 : float)
                                ((index (inter_4 : (vec 2)) 0) : float))
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
      ((Define Nonrec fractalNoise_11
        ((lambda p_12
          ((+
            ((+
              ((+
                ((*
                  ((app (smoothNoise_0 : ((vec 2) -> float)) (p_12 : (vec 2))) :
                   float)
                  (0.5333 : float))
                 : float)
                ((*
                  ((app (smoothNoise_0 : ((vec 2) -> float))
                    ((* (p_12 : (vec 2)) (2. : float)) : (vec 2)))
                   : float)
                  (0.2667 : float))
                 : float))
               : float)
              ((*
                ((app (smoothNoise_0 : ((vec 2) -> float))
                  ((* (p_12 : (vec 2)) (4. : float)) : (vec 2)))
                 : float)
                (0.1333 : float))
               : float))
             : float)
            ((*
              ((app (smoothNoise_0 : ((vec 2) -> float))
                ((* (p_12 : (vec 2)) (8. : float)) : (vec 2)))
               : float)
              (0.0667 : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec warpedNoise_13
        ((lambda p_14
          ((let m_15
            ((*
              ((vec2 (u_time : float)
                ((- (0. : float) (u_time : float)) : float))
               : (vec 2))
              (0.5 : float))
             : (vec 2))
            ((let x_16
              ((app (fractalNoise_11 : ((vec 2) -> float))
                ((+ (p_14 : (vec 2)) (m_15 : (vec 2))) : (vec 2)))
               : float)
              ((let y_17
                ((app (fractalNoise_11 : ((vec 2) -> float))
                  ((+
                    ((+ (p_14 : (vec 2))
                      ((vec2 ((index (m_15 : (vec 2)) 1) : float)
                        ((index (m_15 : (vec 2)) 0) : float))
                       : (vec 2)))
                     : (vec 2))
                    (x_16 : float))
                   : (vec 2)))
                 : float)
                ((let z_18
                  ((app (fractalNoise_11 : ((vec 2) -> float))
                    ((+
                      ((- ((- (p_14 : (vec 2)) (m_15 : (vec 2))) : (vec 2))
                        (x_16 : float))
                       : (vec 2))
                      (y_17 : float))
                     : (vec 2)))
                   : float)
                  ((let warp_19
                    ((+
                      ((+ ((vec2 (x_16 : float) (y_17 : float)) : (vec 2))
                        ((vec2 (y_17 : float) (z_18 : float)) : (vec 2)))
                       : (vec 2))
                      ((vec2 (z_18 : float) (x_16 : float)) : (vec 2)))
                     : (vec 2))
                    ((let mag_20
                      ((*
                        ((length
                          ((vec3 (x_16 : float) (y_17 : float) (z_18 : float)) :
                           (vec 3)))
                         : float)
                        (0.25 : float))
                       : float)
                      ((app (fractalNoise_11 : ((vec 2) -> float))
                        ((+ ((+ (p_14 : (vec 2)) (warp_19 : (vec 2))) : (vec 2))
                          (mag_20 : float))
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
        ((lambda coord_21
          ((let uv_22
            ((/
              ((- (coord_21 : (vec 2))
                ((* (u_resolution : (vec 2)) (0.5 : float)) : (vec 2)))
               : (vec 2))
              ((index (u_resolution : (vec 2)) 1) : float))
             : (vec 2))
            ((let n_23
              ((app (warpedNoise_13 : ((vec 2) -> float))
                ((* (uv_22 : (vec 2)) (6. : float)) : (vec 2)))
               : float)
              ((let n2_24
                ((app (warpedNoise_13 : ((vec 2) -> float))
                  ((- ((* (uv_22 : (vec 2)) (6. : float)) : (vec 2))
                    (0.02 : float))
                   : (vec 2)))
                 : float)
                ((let bump_25
                  ((*
                    ((/
                      ((max ((- (n2_24 : float) (n_23 : float)) : float)
                        (0. : float))
                       : float)
                      (0.02 : float))
                     : float)
                    (0.7071 : float))
                   : float)
                  ((let bump2_26
                    ((*
                      ((/
                        ((max ((- (n_23 : float) (n2_24 : float)) : float)
                          (0. : float))
                         : float)
                        (0.02 : float))
                       : float)
                      (0.7071 : float))
                     : float)
                    ((let b1_27
                      ((+ ((* (bump_25 : float) (bump_25 : float)) : float)
                        ((* ((pow (bump_25 : float) (4. : float)) : float)
                          (0.5 : float))
                         : float))
                       : float)
                      ((let b2_28
                        ((+ ((* (bump2_26 : float) (bump2_26 : float)) : float)
                          ((* ((pow (bump2_26 : float) (4. : float)) : float)
                            (0.5 : float))
                           : float))
                         : float)
                        ((let base_col_29
                          ((+
                            ((*
                              ((*
                                ((vec3 (1. : float) (0.7 : float) (0.6 : float))
                                 : (vec 3))
                                ((vec3 (b1_27 : float)
                                  ((*
                                    ((+ (b1_27 : float) (b2_28 : float)) : float)
                                    (0.4 : float))
                                   : float)
                                  (b2_28 : float))
                                 : (vec 3)))
                               : (vec 3))
                              (0.3 : float))
                             : (vec 3))
                            (0.5 : float))
                           : (vec 3))
                          ((let col_30
                            ((* ((* (n_23 : float) (n_23 : float)) : float)
                              (base_col_29 : (vec 3)))
                             : (vec 3))
                            ((let spot1_dist_31
                              ((length
                                ((- (uv_22 : (vec 2)) (0.65 : float)) : (vec 2)))
                               : float)
                              ((let spot2_dist_32
                                ((length
                                  ((+ (uv_22 : (vec 2)) (0.5 : float)) : (vec 2)))
                                 : float)
                                ((let spot_logic_33
                                  ((+
                                    ((*
                                      ((vec3 (0.8 : float) (0.4 : float)
                                        (1. : float))
                                       : (vec 3))
                                      (0.35 : float))
                                     : (vec 3))
                                    ((*
                                      ((+
                                        ((*
                                          ((vec3 (1. : float) (0.5 : float)
                                            (0.2 : float))
                                           : (vec 3))
                                          ((smoothstep (0. : float) (1. : float)
                                            ((- (1. : float)
                                              (spot1_dist_31 : float))
                                             : float))
                                           : float))
                                         : (vec 3))
                                        ((*
                                          ((vec3 (0.2 : float) (0.4 : float)
                                            (1. : float))
                                           : (vec 3))
                                          ((smoothstep (0. : float) (1. : float)
                                            ((- (1. : float)
                                              (spot2_dist_32 : float))
                                             : float))
                                           : float))
                                         : (vec 3)))
                                       : (vec 3))
                                      (5. : float))
                                     : (vec 3)))
                                   : (vec 3))
                                  ((let final_col_34
                                    ((* (col_30 : (vec 3))
                                      (spot_logic_33 : (vec 3)))
                                     : (vec 3))
                                    ((sqrt
                                      ((max (final_col_34 : (vec 3))
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
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === monomorphize (warped_noise.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec smoothNoise_0
        ((lambda p_1
          ((let i_2 ((floor (p_1 : (vec 2))) : (vec 2))
            ((let pf_3 ((- (p_1 : (vec 2)) (i_2 : (vec 2))) : (vec 2))
              ((let inter_4
                ((* ((* (pf_3 : (vec 2)) (pf_3 : (vec 2))) : (vec 2))
                  ((- (3. : float) ((* (2. : float) (pf_3 : (vec 2))) : (vec 2)))
                   : (vec 2)))
                 : (vec 2))
                ((let v4_5
                  ((vec4 (0. : float) (1. : float) (27. : float) (28. : float)) :
                   (vec 4))
                  ((let seed_6
                    ((+
                      ((+ (v4_5 : (vec 4)) ((index (i_2 : (vec 2)) 0) : float)) :
                       (vec 4))
                      ((* ((index (i_2 : (vec 2)) 1) : float) (27. : float)) :
                       float))
                     : (vec 4))
                    ((let hash_7
                      ((fract
                        ((*
                          ((sin
                            ((% (seed_6 : (vec 4)) (6.2831853 : float)) :
                             (vec 4)))
                           : (vec 4))
                          (200000. : float))
                         : (vec 4)))
                       : (vec 4))
                      ((let col0_8
                        ((vec2 ((index (hash_7 : (vec 4)) 0) : float)
                          ((index (hash_7 : (vec 4)) 1) : float))
                         : (vec 2))
                        ((let col1_9
                          ((vec2 ((index (hash_7 : (vec 4)) 2) : float)
                            ((index (hash_7 : (vec 4)) 3) : float))
                           : (vec 2))
                          ((let res_v_10
                            ((+
                              ((* (col0_8 : (vec 2))
                                ((- (1. : float)
                                  ((index (inter_4 : (vec 2)) 1) : float))
                                 : float))
                               : (vec 2))
                              ((* (col1_9 : (vec 2))
                                ((index (inter_4 : (vec 2)) 1) : float))
                               : (vec 2)))
                             : (vec 2))
                            ((dot (res_v_10 : (vec 2))
                              ((vec2
                                ((- (1. : float)
                                  ((index (inter_4 : (vec 2)) 0) : float))
                                 : float)
                                ((index (inter_4 : (vec 2)) 0) : float))
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
      ((Define Nonrec fractalNoise_11
        ((lambda p_12
          ((+
            ((+
              ((+
                ((*
                  ((app (smoothNoise_0 : ((vec 2) -> float)) (p_12 : (vec 2))) :
                   float)
                  (0.5333 : float))
                 : float)
                ((*
                  ((app (smoothNoise_0 : ((vec 2) -> float))
                    ((* (p_12 : (vec 2)) (2. : float)) : (vec 2)))
                   : float)
                  (0.2667 : float))
                 : float))
               : float)
              ((*
                ((app (smoothNoise_0 : ((vec 2) -> float))
                  ((* (p_12 : (vec 2)) (4. : float)) : (vec 2)))
                 : float)
                (0.1333 : float))
               : float))
             : float)
            ((*
              ((app (smoothNoise_0 : ((vec 2) -> float))
                ((* (p_12 : (vec 2)) (8. : float)) : (vec 2)))
               : float)
              (0.0667 : float))
             : float))
           : float))
         : ((vec 2) -> float)))
       : ((vec 2) -> float))
      ((Define Nonrec warpedNoise_13
        ((lambda p_14
          ((let m_15
            ((*
              ((vec2 (u_time : float)
                ((- (0. : float) (u_time : float)) : float))
               : (vec 2))
              (0.5 : float))
             : (vec 2))
            ((let x_16
              ((app (fractalNoise_11 : ((vec 2) -> float))
                ((+ (p_14 : (vec 2)) (m_15 : (vec 2))) : (vec 2)))
               : float)
              ((let y_17
                ((app (fractalNoise_11 : ((vec 2) -> float))
                  ((+
                    ((+ (p_14 : (vec 2))
                      ((vec2 ((index (m_15 : (vec 2)) 1) : float)
                        ((index (m_15 : (vec 2)) 0) : float))
                       : (vec 2)))
                     : (vec 2))
                    (x_16 : float))
                   : (vec 2)))
                 : float)
                ((let z_18
                  ((app (fractalNoise_11 : ((vec 2) -> float))
                    ((+
                      ((- ((- (p_14 : (vec 2)) (m_15 : (vec 2))) : (vec 2))
                        (x_16 : float))
                       : (vec 2))
                      (y_17 : float))
                     : (vec 2)))
                   : float)
                  ((let warp_19
                    ((+
                      ((+ ((vec2 (x_16 : float) (y_17 : float)) : (vec 2))
                        ((vec2 (y_17 : float) (z_18 : float)) : (vec 2)))
                       : (vec 2))
                      ((vec2 (z_18 : float) (x_16 : float)) : (vec 2)))
                     : (vec 2))
                    ((let mag_20
                      ((*
                        ((length
                          ((vec3 (x_16 : float) (y_17 : float) (z_18 : float)) :
                           (vec 3)))
                         : float)
                        (0.25 : float))
                       : float)
                      ((app (fractalNoise_11 : ((vec 2) -> float))
                        ((+ ((+ (p_14 : (vec 2)) (warp_19 : (vec 2))) : (vec 2))
                          (mag_20 : float))
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
        ((lambda coord_21
          ((let uv_22
            ((/
              ((- (coord_21 : (vec 2))
                ((* (u_resolution : (vec 2)) (0.5 : float)) : (vec 2)))
               : (vec 2))
              ((index (u_resolution : (vec 2)) 1) : float))
             : (vec 2))
            ((let n_23
              ((app (warpedNoise_13 : ((vec 2) -> float))
                ((* (uv_22 : (vec 2)) (6. : float)) : (vec 2)))
               : float)
              ((let n2_24
                ((app (warpedNoise_13 : ((vec 2) -> float))
                  ((- ((* (uv_22 : (vec 2)) (6. : float)) : (vec 2))
                    (0.02 : float))
                   : (vec 2)))
                 : float)
                ((let bump_25
                  ((*
                    ((/
                      ((max ((- (n2_24 : float) (n_23 : float)) : float)
                        (0. : float))
                       : float)
                      (0.02 : float))
                     : float)
                    (0.7071 : float))
                   : float)
                  ((let bump2_26
                    ((*
                      ((/
                        ((max ((- (n_23 : float) (n2_24 : float)) : float)
                          (0. : float))
                         : float)
                        (0.02 : float))
                       : float)
                      (0.7071 : float))
                     : float)
                    ((let b1_27
                      ((+ ((* (bump_25 : float) (bump_25 : float)) : float)
                        ((* ((pow (bump_25 : float) (4. : float)) : float)
                          (0.5 : float))
                         : float))
                       : float)
                      ((let b2_28
                        ((+ ((* (bump2_26 : float) (bump2_26 : float)) : float)
                          ((* ((pow (bump2_26 : float) (4. : float)) : float)
                            (0.5 : float))
                           : float))
                         : float)
                        ((let base_col_29
                          ((+
                            ((*
                              ((*
                                ((vec3 (1. : float) (0.7 : float) (0.6 : float))
                                 : (vec 3))
                                ((vec3 (b1_27 : float)
                                  ((*
                                    ((+ (b1_27 : float) (b2_28 : float)) : float)
                                    (0.4 : float))
                                   : float)
                                  (b2_28 : float))
                                 : (vec 3)))
                               : (vec 3))
                              (0.3 : float))
                             : (vec 3))
                            (0.5 : float))
                           : (vec 3))
                          ((let col_30
                            ((* ((* (n_23 : float) (n_23 : float)) : float)
                              (base_col_29 : (vec 3)))
                             : (vec 3))
                            ((let spot1_dist_31
                              ((length
                                ((- (uv_22 : (vec 2)) (0.65 : float)) : (vec 2)))
                               : float)
                              ((let spot2_dist_32
                                ((length
                                  ((+ (uv_22 : (vec 2)) (0.5 : float)) : (vec 2)))
                                 : float)
                                ((let spot_logic_33
                                  ((+
                                    ((*
                                      ((vec3 (0.8 : float) (0.4 : float)
                                        (1. : float))
                                       : (vec 3))
                                      (0.35 : float))
                                     : (vec 3))
                                    ((*
                                      ((+
                                        ((*
                                          ((vec3 (1. : float) (0.5 : float)
                                            (0.2 : float))
                                           : (vec 3))
                                          ((smoothstep (0. : float) (1. : float)
                                            ((- (1. : float)
                                              (spot1_dist_31 : float))
                                             : float))
                                           : float))
                                         : (vec 3))
                                        ((*
                                          ((vec3 (0.2 : float) (0.4 : float)
                                            (1. : float))
                                           : (vec 3))
                                          ((smoothstep (0. : float) (1. : float)
                                            ((- (1. : float)
                                              (spot2_dist_32 : float))
                                             : float))
                                           : float))
                                         : (vec 3)))
                                       : (vec 3))
                                      (5. : float))
                                     : (vec 3)))
                                   : (vec 3))
                                  ((let final_col_34
                                    ((* (col_30 : (vec 3))
                                      (spot_logic_33 : (vec 3)))
                                     : (vec 3))
                                    ((sqrt
                                      ((max (final_col_34 : (vec 3))
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
         : ((vec 2) -> (vec 3))))
       : ((vec 2) -> (vec 3)))))

    === uncurry (warped_noise.glml) ===
    (Program
     (((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
      ((Define Nonrec smoothNoise_0
        (lambda ((p_1 (vec 2)))
         (let i_2 (floor p_1)
          (let pf_3 (- p_1 i_2)
           (let inter_4 (* (* pf_3 pf_3) (- 3. (* 2. pf_3)))
            (let v4_5 (vec4 0. 1. 27. 28.)
             (let seed_6 (+ (+ v4_5 (index i_2 0)) (* (index i_2 1) 27.))
              (let hash_7 (fract (* (sin (% seed_6 6.2831853)) 200000.))
               (let col0_8 (vec2 (index hash_7 0) (index hash_7 1))
                (let col1_9 (vec2 (index hash_7 2) (index hash_7 3))
                 (let res_v_10
                  (+ (* col0_8 (- 1. (index inter_4 1)))
                   (* col1_9 (index inter_4 1)))
                  (dot res_v_10
                   (vec2 (- 1. (index inter_4 0)) (index inter_4 0))))))))))))))
       : ((vec 2) -> float))
      ((Define Nonrec fractalNoise_11
        (lambda ((p_12 (vec 2)))
         (+
          (+
           (+ (* (app smoothNoise_0 p_12) 0.5333)
            (* (app smoothNoise_0 (* p_12 2.)) 0.2667))
           (* (app smoothNoise_0 (* p_12 4.)) 0.1333))
          (* (app smoothNoise_0 (* p_12 8.)) 0.0667))))
       : ((vec 2) -> float))
      ((Define Nonrec warpedNoise_13
        (lambda ((p_14 (vec 2)))
         (let m_15 (* (vec2 u_time (- 0. u_time)) 0.5)
          (let x_16 (app fractalNoise_11 (+ p_14 m_15))
           (let y_17
            (app fractalNoise_11
             (+ (+ p_14 (vec2 (index m_15 1) (index m_15 0))) x_16))
            (let z_18 (app fractalNoise_11 (+ (- (- p_14 m_15) x_16) y_17))
             (let warp_19
              (+ (+ (vec2 x_16 y_17) (vec2 y_17 z_18)) (vec2 z_18 x_16))
              (let mag_20 (* (length (vec3 x_16 y_17 z_18)) 0.25)
               (app fractalNoise_11 (+ (+ p_14 warp_19) mag_20))))))))))
       : ((vec 2) -> float))
      ((Define Nonrec main
        (lambda ((coord_21 (vec 2)))
         (let uv_22 (/ (- coord_21 (* u_resolution 0.5)) (index u_resolution 1))
          (let n_23 (app warpedNoise_13 (* uv_22 6.))
           (let n2_24 (app warpedNoise_13 (- (* uv_22 6.) 0.02))
            (let bump_25 (* (/ (max (- n2_24 n_23) 0.) 0.02) 0.7071)
             (let bump2_26 (* (/ (max (- n_23 n2_24) 0.) 0.02) 0.7071)
              (let b1_27 (+ (* bump_25 bump_25) (* (pow bump_25 4.) 0.5))
               (let b2_28 (+ (* bump2_26 bump2_26) (* (pow bump2_26 4.) 0.5))
                (let base_col_29
                 (+
                  (*
                   (* (vec3 1. 0.7 0.6)
                    (vec3 b1_27 (* (+ b1_27 b2_28) 0.4) b2_28))
                   0.3)
                  0.5)
                 (let col_30 (* (* n_23 n_23) base_col_29)
                  (let spot1_dist_31 (length (- uv_22 0.65))
                   (let spot2_dist_32 (length (+ uv_22 0.5))
                    (let spot_logic_33
                     (+ (* (vec3 0.8 0.4 1.) 0.35)
                      (*
                       (+
                        (* (vec3 1. 0.5 0.2)
                         (smoothstep 0. 1. (- 1. spot1_dist_31)))
                        (* (vec3 0.2 0.4 1.)
                         (smoothstep 0. 1. (- 1. spot2_dist_32))))
                       5.))
                     (let final_col_34 (* col_30 spot_logic_33)
                      (sqrt (max final_col_34 0.)))))))))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name smoothNoise_0) (args ((p_1 (vec 2))))
       (body
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let inter_4 (* (* pf_3 pf_3) (- 3. (* 2. pf_3)))
           (let v4_5 (vec4 0. 1. 27. 28.)
            (let seed_6 (+ (+ v4_5 (index i_2 0)) (* (index i_2 1) 27.))
             (let hash_7 (fract (* (sin (% seed_6 6.2831853)) 200000.))
              (let col0_8 (vec2 (index hash_7 0) (index hash_7 1))
               (let col1_9 (vec2 (index hash_7 2) (index hash_7 3))
                (let res_v_10
                 (+ (* col0_8 (- 1. (index inter_4 1)))
                  (* col1_9 (index inter_4 1)))
                 (dot res_v_10 (vec2 (- 1. (index inter_4 0)) (index inter_4 0))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (+
         (+
          (+ (* (app smoothNoise_0 p_12) 0.5333)
           (* (app smoothNoise_0 (* p_12 2.)) 0.2667))
          (* (app smoothNoise_0 (* p_12 4.)) 0.1333))
         (* (app smoothNoise_0 (* p_12 8.)) 0.0667))))
      : ((vec 2) -> float))
     ((Define Nonrec (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let m_15 (* (vec2 u_time (- 0. u_time)) 0.5)
         (let x_16 (app fractalNoise_11 (+ p_14 m_15))
          (let y_17
           (app fractalNoise_11
            (+ (+ p_14 (vec2 (index m_15 1) (index m_15 0))) x_16))
           (let z_18 (app fractalNoise_11 (+ (- (- p_14 m_15) x_16) y_17))
            (let warp_19
             (+ (+ (vec2 x_16 y_17) (vec2 y_17 z_18)) (vec2 z_18 x_16))
             (let mag_20 (* (length (vec3 x_16 y_17 z_18)) 0.25)
              (app fractalNoise_11 (+ (+ p_14 warp_19) mag_20))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name main) (args ((coord_21 (vec 2))))
       (body
        (let uv_22 (/ (- coord_21 (* u_resolution 0.5)) (index u_resolution 1))
         (let n_23 (app warpedNoise_13 (* uv_22 6.))
          (let n2_24 (app warpedNoise_13 (- (* uv_22 6.) 0.02))
           (let bump_25 (* (/ (max (- n2_24 n_23) 0.) 0.02) 0.7071)
            (let bump2_26 (* (/ (max (- n_23 n2_24) 0.) 0.02) 0.7071)
             (let b1_27 (+ (* bump_25 bump_25) (* (pow bump_25 4.) 0.5))
              (let b2_28 (+ (* bump2_26 bump2_26) (* (pow bump2_26 4.) 0.5))
               (let base_col_29
                (+
                 (*
                  (* (vec3 1. 0.7 0.6)
                   (vec3 b1_27 (* (+ b1_27 b2_28) 0.4) b2_28))
                  0.3)
                 0.5)
                (let col_30 (* (* n_23 n_23) base_col_29)
                 (let spot1_dist_31 (length (- uv_22 0.65))
                  (let spot2_dist_32 (length (+ uv_22 0.5))
                   (let spot_logic_33
                    (+ (* (vec3 0.8 0.4 1.) 0.35)
                     (*
                      (+
                       (* (vec3 1. 0.5 0.2)
                        (smoothstep 0. 1. (- 1. spot1_dist_31)))
                       (* (vec3 0.2 0.4 1.)
                        (smoothstep 0. 1. (- 1. spot2_dist_32))))
                      5.))
                    (let final_col_34 (* col_30 spot_logic_33)
                     (sqrt (max final_col_34 0.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === anf (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name smoothNoise_0) (args ((p_1 (vec 2))))
       (body
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let anf_165 (* pf_3 pf_3)
           (let anf_166 (* 2. pf_3)
            (let anf_167 (- 3. anf_166)
             (let inter_4 (* anf_165 anf_167)
              (let v4_5 (vec4 0. 1. 27. 28.)
               (let anf_168 (index i_2 0)
                (let anf_169 (+ v4_5 anf_168)
                 (let anf_170 (index i_2 1)
                  (let anf_171 (* anf_170 27.)
                   (let seed_6 (+ anf_169 anf_171)
                    (let anf_172 (% seed_6 6.2831853)
                     (let anf_173 (sin anf_172)
                      (let anf_174 (* anf_173 200000.)
                       (let hash_7 (fract anf_174)
                        (let anf_175 (index hash_7 0)
                         (let anf_176 (index hash_7 1)
                          (let col0_8 (vec2 anf_175 anf_176)
                           (let anf_177 (index hash_7 2)
                            (let anf_178 (index hash_7 3)
                             (let col1_9 (vec2 anf_177 anf_178)
                              (let anf_179 (index inter_4 1)
                               (let anf_180 (- 1. anf_179)
                                (let anf_181 (* col0_8 anf_180)
                                 (let anf_182 (index inter_4 1)
                                  (let anf_183 (* col1_9 anf_182)
                                   (let res_v_10 (+ anf_181 anf_183)
                                    (let anf_184 (index inter_4 0)
                                     (let anf_185 (- 1. anf_184)
                                      (let anf_186 (index inter_4 0)
                                       (let anf_187 (vec2 anf_185 anf_186)
                                        (return (dot res_v_10 anf_187))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (let anf_188 (smoothNoise_0 p_12)
         (let anf_189 (* anf_188 0.5333)
          (let anf_190 (* p_12 2.)
           (let anf_191 (smoothNoise_0 anf_190)
            (let anf_192 (* anf_191 0.2667)
             (let anf_193 (+ anf_189 anf_192)
              (let anf_194 (* p_12 4.)
               (let anf_195 (smoothNoise_0 anf_194)
                (let anf_196 (* anf_195 0.1333)
                 (let anf_197 (+ anf_193 anf_196)
                  (let anf_198 (* p_12 8.)
                   (let anf_199 (smoothNoise_0 anf_198)
                    (let anf_200 (* anf_199 0.0667) (return (+ anf_197 anf_200)))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let anf_201 (- 0. u_time)
         (let anf_202 (vec2 u_time anf_201)
          (let m_15 (* anf_202 0.5)
           (let anf_203 (+ p_14 m_15)
            (let x_16 (fractalNoise_11 anf_203)
             (let anf_204 (index m_15 1)
              (let anf_205 (index m_15 0)
               (let anf_206 (vec2 anf_204 anf_205)
                (let anf_207 (+ p_14 anf_206)
                 (let anf_208 (+ anf_207 x_16)
                  (let y_17 (fractalNoise_11 anf_208)
                   (let anf_209 (- p_14 m_15)
                    (let anf_210 (- anf_209 x_16)
                     (let anf_211 (+ anf_210 y_17)
                      (let z_18 (fractalNoise_11 anf_211)
                       (let anf_212 (vec2 x_16 y_17)
                        (let anf_213 (vec2 y_17 z_18)
                         (let anf_214 (+ anf_212 anf_213)
                          (let anf_215 (vec2 z_18 x_16)
                           (let warp_19 (+ anf_214 anf_215)
                            (let anf_216 (vec3 x_16 y_17 z_18)
                             (let anf_217 (length anf_216)
                              (let mag_20 (* anf_217 0.25)
                               (let anf_218 (+ p_14 warp_19)
                                (let anf_219 (+ anf_218 mag_20)
                                 (return (fractalNoise_11 anf_219)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name main) (args ((coord_21 (vec 2))))
       (body
        (let anf_220 (* u_resolution 0.5)
         (let anf_221 (- coord_21 anf_220)
          (let anf_222 (index u_resolution 1)
           (let uv_22 (/ anf_221 anf_222)
            (let anf_223 (* uv_22 6.)
             (let n_23 (warpedNoise_13 anf_223)
              (let anf_224 (* uv_22 6.)
               (let anf_225 (- anf_224 0.02)
                (let n2_24 (warpedNoise_13 anf_225)
                 (let anf_226 (- n2_24 n_23)
                  (let anf_227 (max anf_226 0.)
                   (let anf_228 (/ anf_227 0.02)
                    (let bump_25 (* anf_228 0.7071)
                     (let anf_229 (- n_23 n2_24)
                      (let anf_230 (max anf_229 0.)
                       (let anf_231 (/ anf_230 0.02)
                        (let bump2_26 (* anf_231 0.7071)
                         (let anf_232 (* bump_25 bump_25)
                          (let anf_233 (pow bump_25 4.)
                           (let anf_234 (* anf_233 0.5)
                            (let b1_27 (+ anf_232 anf_234)
                             (let anf_235 (* bump2_26 bump2_26)
                              (let anf_236 (pow bump2_26 4.)
                               (let anf_237 (* anf_236 0.5)
                                (let b2_28 (+ anf_235 anf_237)
                                 (let anf_238 (vec3 1. 0.7 0.6)
                                  (let anf_239 (+ b1_27 b2_28)
                                   (let anf_240 (* anf_239 0.4)
                                    (let anf_241 (vec3 b1_27 anf_240 b2_28)
                                     (let anf_242 (* anf_238 anf_241)
                                      (let anf_243 (* anf_242 0.3)
                                       (let base_col_29 (+ anf_243 0.5)
                                        (let anf_244 (* n_23 n_23)
                                         (let col_30 (* anf_244 base_col_29)
                                          (let anf_245 (- uv_22 0.65)
                                           (let spot1_dist_31 (length anf_245)
                                            (let anf_246 (+ uv_22 0.5)
                                             (let spot2_dist_32 (length anf_246)
                                              (let anf_247 (vec3 0.8 0.4 1.)
                                               (let anf_248 (* anf_247 0.35)
                                                (let anf_249 (vec3 1. 0.5 0.2)
                                                 (let anf_250
                                                  (- 1. spot1_dist_31)
                                                  (let anf_251
                                                   (smoothstep 0. 1. anf_250)
                                                   (let anf_252
                                                    (* anf_249 anf_251)
                                                    (let anf_253
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_254
                                                      (- 1. spot2_dist_32)
                                                      (let anf_255
                                                       (smoothstep 0. 1. anf_254)
                                                       (let anf_256
                                                        (* anf_253 anf_255)
                                                        (let anf_257
                                                         (+ anf_252 anf_256)
                                                         (let anf_258
                                                          (* anf_257 5.)
                                                          (let spot_logic_33
                                                           (+ anf_248 anf_258)
                                                           (let final_col_34
                                                            (* col_30
                                                             spot_logic_33)
                                                            (let anf_259
                                                             (max final_col_34
                                                              0.)
                                                             (return
                                                              (sqrt anf_259)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name smoothNoise_0) (args ((p_1 (vec 2))))
       (body
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let anf_165 (* pf_3 pf_3)
           (let anf_166 (* 2. pf_3)
            (let anf_167 (- 3. anf_166)
             (let inter_4 (* anf_165 anf_167)
              (let v4_5 (vec4 0. 1. 27. 28.)
               (let anf_168 (index i_2 0)
                (let anf_169 (+ v4_5 anf_168)
                 (let anf_170 (index i_2 1)
                  (let anf_171 (* anf_170 27.)
                   (let seed_6 (+ anf_169 anf_171)
                    (let anf_172 (% seed_6 6.2831853)
                     (let anf_173 (sin anf_172)
                      (let anf_174 (* anf_173 200000.)
                       (let hash_7 (fract anf_174)
                        (let anf_175 (index hash_7 0)
                         (let anf_176 (index hash_7 1)
                          (let col0_8 (vec2 anf_175 anf_176)
                           (let anf_177 (index hash_7 2)
                            (let anf_178 (index hash_7 3)
                             (let col1_9 (vec2 anf_177 anf_178)
                              (let anf_179 (index inter_4 1)
                               (let anf_180 (- 1. anf_179)
                                (let anf_181 (* col0_8 anf_180)
                                 (let anf_182 (index inter_4 1)
                                  (let anf_183 (* col1_9 anf_182)
                                   (let res_v_10 (+ anf_181 anf_183)
                                    (let anf_184 (index inter_4 0)
                                     (let anf_185 (- 1. anf_184)
                                      (let anf_186 (index inter_4 0)
                                       (let anf_187 (vec2 anf_185 anf_186)
                                        (return (dot res_v_10 anf_187))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (let anf_188 (smoothNoise_0 p_12)
         (let anf_189 (* anf_188 0.5333)
          (let anf_190 (* p_12 2.)
           (let anf_191 (smoothNoise_0 anf_190)
            (let anf_192 (* anf_191 0.2667)
             (let anf_193 (+ anf_189 anf_192)
              (let anf_194 (* p_12 4.)
               (let anf_195 (smoothNoise_0 anf_194)
                (let anf_196 (* anf_195 0.1333)
                 (let anf_197 (+ anf_193 anf_196)
                  (let anf_198 (* p_12 8.)
                   (let anf_199 (smoothNoise_0 anf_198)
                    (let anf_200 (* anf_199 0.0667) (return (+ anf_197 anf_200)))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let anf_201 (- 0. u_time)
         (let anf_202 (vec2 u_time anf_201)
          (let m_15 (* anf_202 0.5)
           (let anf_203 (+ p_14 m_15)
            (let x_16 (fractalNoise_11 anf_203)
             (let anf_204 (index m_15 1)
              (let anf_205 (index m_15 0)
               (let anf_206 (vec2 anf_204 anf_205)
                (let anf_207 (+ p_14 anf_206)
                 (let anf_208 (+ anf_207 x_16)
                  (let y_17 (fractalNoise_11 anf_208)
                   (let anf_209 (- p_14 m_15)
                    (let anf_210 (- anf_209 x_16)
                     (let anf_211 (+ anf_210 y_17)
                      (let z_18 (fractalNoise_11 anf_211)
                       (let anf_212 (vec2 x_16 y_17)
                        (let anf_213 (vec2 y_17 z_18)
                         (let anf_214 (+ anf_212 anf_213)
                          (let anf_215 (vec2 z_18 x_16)
                           (let warp_19 (+ anf_214 anf_215)
                            (let anf_216 (vec3 x_16 y_17 z_18)
                             (let anf_217 (length anf_216)
                              (let mag_20 (* anf_217 0.25)
                               (let anf_218 (+ p_14 warp_19)
                                (let anf_219 (+ anf_218 mag_20)
                                 (return (fractalNoise_11 anf_219)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name main) (args ((coord_21 (vec 2))))
       (body
        (let anf_220 (* u_resolution 0.5)
         (let anf_221 (- coord_21 anf_220)
          (let anf_222 (index u_resolution 1)
           (let uv_22 (/ anf_221 anf_222)
            (let anf_223 (* uv_22 6.)
             (let n_23 (warpedNoise_13 anf_223)
              (let anf_224 (* uv_22 6.)
               (let anf_225 (- anf_224 0.02)
                (let n2_24 (warpedNoise_13 anf_225)
                 (let anf_226 (- n2_24 n_23)
                  (let anf_227 (max anf_226 0.)
                   (let anf_228 (/ anf_227 0.02)
                    (let bump_25 (* anf_228 0.7071)
                     (let anf_229 (- n_23 n2_24)
                      (let anf_230 (max anf_229 0.)
                       (let anf_231 (/ anf_230 0.02)
                        (let bump2_26 (* anf_231 0.7071)
                         (let anf_232 (* bump_25 bump_25)
                          (let anf_233 (pow bump_25 4.)
                           (let anf_234 (* anf_233 0.5)
                            (let b1_27 (+ anf_232 anf_234)
                             (let anf_235 (* bump2_26 bump2_26)
                              (let anf_236 (pow bump2_26 4.)
                               (let anf_237 (* anf_236 0.5)
                                (let b2_28 (+ anf_235 anf_237)
                                 (let anf_238 (vec3 1. 0.7 0.6)
                                  (let anf_239 (+ b1_27 b2_28)
                                   (let anf_240 (* anf_239 0.4)
                                    (let anf_241 (vec3 b1_27 anf_240 b2_28)
                                     (let anf_242 (* anf_238 anf_241)
                                      (let anf_243 (* anf_242 0.3)
                                       (let base_col_29 (+ anf_243 0.5)
                                        (let anf_244 (* n_23 n_23)
                                         (let col_30 (* anf_244 base_col_29)
                                          (let anf_245 (- uv_22 0.65)
                                           (let spot1_dist_31 (length anf_245)
                                            (let anf_246 (+ uv_22 0.5)
                                             (let spot2_dist_32 (length anf_246)
                                              (let anf_247 (vec3 0.8 0.4 1.)
                                               (let anf_248 (* anf_247 0.35)
                                                (let anf_249 (vec3 1. 0.5 0.2)
                                                 (let anf_250
                                                  (- 1. spot1_dist_31)
                                                  (let anf_251
                                                   (smoothstep 0. 1. anf_250)
                                                   (let anf_252
                                                    (* anf_249 anf_251)
                                                    (let anf_253
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_254
                                                      (- 1. spot2_dist_32)
                                                      (let anf_255
                                                       (smoothstep 0. 1. anf_254)
                                                       (let anf_256
                                                        (* anf_253 anf_255)
                                                        (let anf_257
                                                         (+ anf_252 anf_256)
                                                         (let anf_258
                                                          (* anf_257 5.)
                                                          (let spot_logic_33
                                                           (+ anf_248 anf_258)
                                                           (let final_col_34
                                                            (* col_30
                                                             spot_logic_33)
                                                            (let anf_259
                                                             (max final_col_34
                                                              0.)
                                                             (return
                                                              (sqrt anf_259)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name smoothNoise_0) (args ((p_1 (vec 2))))
       (body
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let anf_165 (* pf_3 pf_3)
           (let anf_166 (* 2. pf_3)
            (let anf_167 (- 3. anf_166)
             (let inter_4 (* anf_165 anf_167)
              (let v4_5 (vec4 0. 1. 27. 28.)
               (let anf_168 (index i_2 0)
                (let anf_169 (+ v4_5 anf_168)
                 (let anf_170 (index i_2 1)
                  (let anf_171 (* anf_170 27.)
                   (let seed_6 (+ anf_169 anf_171)
                    (let anf_172 (% seed_6 6.2831853)
                     (let anf_173 (sin anf_172)
                      (let anf_174 (* anf_173 200000.)
                       (let hash_7 (fract anf_174)
                        (let anf_175 (index hash_7 0)
                         (let anf_176 (index hash_7 1)
                          (let col0_8 (vec2 anf_175 anf_176)
                           (let anf_177 (index hash_7 2)
                            (let anf_178 (index hash_7 3)
                             (let col1_9 (vec2 anf_177 anf_178)
                              (let anf_179 (index inter_4 1)
                               (let anf_180 (- 1. anf_179)
                                (let anf_181 (* col0_8 anf_180)
                                 (let anf_182 (index inter_4 1)
                                  (let anf_183 (* col1_9 anf_182)
                                   (let res_v_10 (+ anf_181 anf_183)
                                    (let anf_184 (index inter_4 0)
                                     (let anf_185 (- 1. anf_184)
                                      (let anf_186 (index inter_4 0)
                                       (let anf_187 (vec2 anf_185 anf_186)
                                        (return (dot res_v_10 anf_187))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (let anf_188 (smoothNoise_0 p_12)
         (let anf_189 (* anf_188 0.5333)
          (let anf_190 (* p_12 2.)
           (let anf_191 (smoothNoise_0 anf_190)
            (let anf_192 (* anf_191 0.2667)
             (let anf_193 (+ anf_189 anf_192)
              (let anf_194 (* p_12 4.)
               (let anf_195 (smoothNoise_0 anf_194)
                (let anf_196 (* anf_195 0.1333)
                 (let anf_197 (+ anf_193 anf_196)
                  (let anf_198 (* p_12 8.)
                   (let anf_199 (smoothNoise_0 anf_198)
                    (let anf_200 (* anf_199 0.0667) (return (+ anf_197 anf_200)))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let anf_201 (- 0. u_time)
         (let anf_202 (vec2 u_time anf_201)
          (let m_15 (* anf_202 0.5)
           (let anf_203 (+ p_14 m_15)
            (let x_16 (fractalNoise_11 anf_203)
             (let anf_204 (index m_15 1)
              (let anf_205 (index m_15 0)
               (let anf_206 (vec2 anf_204 anf_205)
                (let anf_207 (+ p_14 anf_206)
                 (let anf_208 (+ anf_207 x_16)
                  (let y_17 (fractalNoise_11 anf_208)
                   (let anf_209 (- p_14 m_15)
                    (let anf_210 (- anf_209 x_16)
                     (let anf_211 (+ anf_210 y_17)
                      (let z_18 (fractalNoise_11 anf_211)
                       (let anf_212 (vec2 x_16 y_17)
                        (let anf_213 (vec2 y_17 z_18)
                         (let anf_214 (+ anf_212 anf_213)
                          (let anf_215 (vec2 z_18 x_16)
                           (let warp_19 (+ anf_214 anf_215)
                            (let anf_216 (vec3 x_16 y_17 z_18)
                             (let anf_217 (length anf_216)
                              (let mag_20 (* anf_217 0.25)
                               (let anf_218 (+ p_14 warp_19)
                                (let anf_219 (+ anf_218 mag_20)
                                 (return (fractalNoise_11 anf_219)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name main) (args ((coord_21 (vec 2))))
       (body
        (let anf_220 (* u_resolution 0.5)
         (let anf_221 (- coord_21 anf_220)
          (let anf_222 (index u_resolution 1)
           (let uv_22 (/ anf_221 anf_222)
            (let anf_223 (* uv_22 6.)
             (let n_23 (warpedNoise_13 anf_223)
              (let anf_224 (* uv_22 6.)
               (let anf_225 (- anf_224 0.02)
                (let n2_24 (warpedNoise_13 anf_225)
                 (let anf_226 (- n2_24 n_23)
                  (let anf_227 (max anf_226 0.)
                   (let anf_228 (/ anf_227 0.02)
                    (let bump_25 (* anf_228 0.7071)
                     (let anf_229 (- n_23 n2_24)
                      (let anf_230 (max anf_229 0.)
                       (let anf_231 (/ anf_230 0.02)
                        (let bump2_26 (* anf_231 0.7071)
                         (let anf_232 (* bump_25 bump_25)
                          (let anf_233 (pow bump_25 4.)
                           (let anf_234 (* anf_233 0.5)
                            (let b1_27 (+ anf_232 anf_234)
                             (let anf_235 (* bump2_26 bump2_26)
                              (let anf_236 (pow bump2_26 4.)
                               (let anf_237 (* anf_236 0.5)
                                (let b2_28 (+ anf_235 anf_237)
                                 (let anf_238 (vec3 1. 0.7 0.6)
                                  (let anf_239 (+ b1_27 b2_28)
                                   (let anf_240 (* anf_239 0.4)
                                    (let anf_241 (vec3 b1_27 anf_240 b2_28)
                                     (let anf_242 (* anf_238 anf_241)
                                      (let anf_243 (* anf_242 0.3)
                                       (let base_col_29 (+ anf_243 0.5)
                                        (let anf_244 (* n_23 n_23)
                                         (let col_30 (* anf_244 base_col_29)
                                          (let anf_245 (- uv_22 0.65)
                                           (let spot1_dist_31 (length anf_245)
                                            (let anf_246 (+ uv_22 0.5)
                                             (let spot2_dist_32 (length anf_246)
                                              (let anf_247 (vec3 0.8 0.4 1.)
                                               (let anf_248 (* anf_247 0.35)
                                                (let anf_249 (vec3 1. 0.5 0.2)
                                                 (let anf_250
                                                  (- 1. spot1_dist_31)
                                                  (let anf_251
                                                   (smoothstep 0. 1. anf_250)
                                                   (let anf_252
                                                    (* anf_249 anf_251)
                                                    (let anf_253
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_254
                                                      (- 1. spot2_dist_32)
                                                      (let anf_255
                                                       (smoothstep 0. 1. anf_254)
                                                       (let anf_256
                                                        (* anf_253 anf_255)
                                                        (let anf_257
                                                         (+ anf_252 anf_256)
                                                         (let anf_258
                                                          (* anf_257 5.)
                                                          (let spot_logic_33
                                                           (+ anf_248 anf_258)
                                                           (let final_col_34
                                                            (* col_30
                                                             spot_logic_33)
                                                            (let anf_259
                                                             (max final_col_34
                                                              0.)
                                                             (return
                                                              (sqrt anf_259)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name smoothNoise_0) (args ((p_1 (vec 2))))
       (body
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let anf_165 (* pf_3 pf_3)
           (let anf_166 (* 2. pf_3)
            (let anf_167 (- 3. anf_166)
             (let inter_4 (* anf_165 anf_167)
              (let v4_5 (vec4 0. 1. 27. 28.)
               (let anf_168 (index i_2 0)
                (let anf_169 (+ v4_5 anf_168)
                 (let anf_170 (index i_2 1)
                  (let anf_171 (* anf_170 27.)
                   (let seed_6 (+ anf_169 anf_171)
                    (let anf_172 (% seed_6 6.2831853)
                     (let anf_173 (sin anf_172)
                      (let anf_174 (* anf_173 200000.)
                       (let hash_7 (fract anf_174)
                        (let anf_175 (index hash_7 0)
                         (let anf_176 (index hash_7 1)
                          (let col0_8 (vec2 anf_175 anf_176)
                           (let anf_177 (index hash_7 2)
                            (let anf_178 (index hash_7 3)
                             (let col1_9 (vec2 anf_177 anf_178)
                              (let anf_179 (index inter_4 1)
                               (let anf_180 (- 1. anf_179)
                                (let anf_181 (* col0_8 anf_180)
                                 (let anf_182 (index inter_4 1)
                                  (let anf_183 (* col1_9 anf_182)
                                   (let res_v_10 (+ anf_181 anf_183)
                                    (let anf_184 (index inter_4 0)
                                     (let anf_185 (- 1. anf_184)
                                      (let anf_186 (index inter_4 0)
                                       (let anf_187 (vec2 anf_185 anf_186)
                                        (return (dot res_v_10 anf_187))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (let anf_188 (smoothNoise_0 p_12)
         (let anf_189 (* anf_188 0.5333)
          (let anf_190 (* p_12 2.)
           (let anf_191 (smoothNoise_0 anf_190)
            (let anf_192 (* anf_191 0.2667)
             (let anf_193 (+ anf_189 anf_192)
              (let anf_194 (* p_12 4.)
               (let anf_195 (smoothNoise_0 anf_194)
                (let anf_196 (* anf_195 0.1333)
                 (let anf_197 (+ anf_193 anf_196)
                  (let anf_198 (* p_12 8.)
                   (let anf_199 (smoothNoise_0 anf_198)
                    (let anf_200 (* anf_199 0.0667) (return (+ anf_197 anf_200)))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let anf_201 (- 0. u_time)
         (let anf_202 (vec2 u_time anf_201)
          (let m_15 (* anf_202 0.5)
           (let anf_203 (+ p_14 m_15)
            (let x_16 (fractalNoise_11 anf_203)
             (let anf_204 (index m_15 1)
              (let anf_205 (index m_15 0)
               (let anf_206 (vec2 anf_204 anf_205)
                (let anf_207 (+ p_14 anf_206)
                 (let anf_208 (+ anf_207 x_16)
                  (let y_17 (fractalNoise_11 anf_208)
                   (let anf_209 (- p_14 m_15)
                    (let anf_210 (- anf_209 x_16)
                     (let anf_211 (+ anf_210 y_17)
                      (let z_18 (fractalNoise_11 anf_211)
                       (let anf_212 (vec2 x_16 y_17)
                        (let anf_213 (vec2 y_17 z_18)
                         (let anf_214 (+ anf_212 anf_213)
                          (let anf_215 (vec2 z_18 x_16)
                           (let warp_19 (+ anf_214 anf_215)
                            (let anf_216 (vec3 x_16 y_17 z_18)
                             (let anf_217 (length anf_216)
                              (let mag_20 (* anf_217 0.25)
                               (let anf_218 (+ p_14 warp_19)
                                (let anf_219 (+ anf_218 mag_20)
                                 (return (fractalNoise_11 anf_219)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name main) (args ((coord_21 (vec 2))))
       (body
        (let anf_220 (* u_resolution 0.5)
         (let anf_221 (- coord_21 anf_220)
          (let anf_222 (index u_resolution 1)
           (let uv_22 (/ anf_221 anf_222)
            (let anf_223 (* uv_22 6.)
             (let n_23 (warpedNoise_13 anf_223)
              (let anf_224 (* uv_22 6.)
               (let anf_225 (- anf_224 0.02)
                (let n2_24 (warpedNoise_13 anf_225)
                 (let anf_226 (- n2_24 n_23)
                  (let anf_227 (max anf_226 0.)
                   (let anf_228 (/ anf_227 0.02)
                    (let bump_25 (* anf_228 0.7071)
                     (let anf_229 (- n_23 n2_24)
                      (let anf_230 (max anf_229 0.)
                       (let anf_231 (/ anf_230 0.02)
                        (let bump2_26 (* anf_231 0.7071)
                         (let anf_232 (* bump_25 bump_25)
                          (let anf_233 (pow bump_25 4.)
                           (let anf_234 (* anf_233 0.5)
                            (let b1_27 (+ anf_232 anf_234)
                             (let anf_235 (* bump2_26 bump2_26)
                              (let anf_236 (pow bump2_26 4.)
                               (let anf_237 (* anf_236 0.5)
                                (let b2_28 (+ anf_235 anf_237)
                                 (let anf_238 (vec3 1. 0.7 0.6)
                                  (let anf_239 (+ b1_27 b2_28)
                                   (let anf_240 (* anf_239 0.4)
                                    (let anf_241 (vec3 b1_27 anf_240 b2_28)
                                     (let anf_242 (* anf_238 anf_241)
                                      (let anf_243 (* anf_242 0.3)
                                       (let base_col_29 (+ anf_243 0.5)
                                        (let anf_244 (* n_23 n_23)
                                         (let col_30 (* anf_244 base_col_29)
                                          (let anf_245 (- uv_22 0.65)
                                           (let spot1_dist_31 (length anf_245)
                                            (let anf_246 (+ uv_22 0.5)
                                             (let spot2_dist_32 (length anf_246)
                                              (let anf_247 (vec3 0.8 0.4 1.)
                                               (let anf_248 (* anf_247 0.35)
                                                (let anf_249 (vec3 1. 0.5 0.2)
                                                 (let anf_250
                                                  (- 1. spot1_dist_31)
                                                  (let anf_251
                                                   (smoothstep 0. 1. anf_250)
                                                   (let anf_252
                                                    (* anf_249 anf_251)
                                                    (let anf_253
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_254
                                                      (- 1. spot2_dist_32)
                                                      (let anf_255
                                                       (smoothstep 0. 1. anf_254)
                                                       (let anf_256
                                                        (* anf_253 anf_255)
                                                        (let anf_257
                                                         (+ anf_252 anf_256)
                                                         (let anf_258
                                                          (* anf_257 5.)
                                                          (let spot_logic_33
                                                           (+ anf_248 anf_258)
                                                           (let final_col_34
                                                            (* col_30
                                                             spot_logic_33)
                                                            (let anf_259
                                                             (max final_col_34
                                                              0.)
                                                             (return
                                                              (sqrt anf_259)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (warped_noise.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name smoothNoise_0) (desc ()) (params (((TyVec 2) p_1)))
       (ret_type TyFloat)
       (body
        ((set () vec2 i_2 (floor p_1)) (set () vec2 pf_3 (- p_1 i_2))
         (set () vec2 anf_165 (* pf_3 pf_3)) (set () vec2 anf_166 (* 2. pf_3))
         (set () vec2 anf_167 (- 3. anf_166))
         (set () vec2 inter_4 (* anf_165 anf_167))
         (set () vec4 v4_5 (vec4 0. 1. 27. 28.))
         (set () float anf_168 (index i_2 0))
         (set () vec4 anf_169 (+ v4_5 anf_168))
         (set () float anf_170 (index i_2 1))
         (set () float anf_171 (* anf_170 27.))
         (set () vec4 seed_6 (+ anf_169 anf_171))
         (set () vec4 anf_172 (% seed_6 6.2831853))
         (set () vec4 anf_173 (sin anf_172))
         (set () vec4 anf_174 (* anf_173 200000.))
         (set () vec4 hash_7 (fract anf_174))
         (set () float anf_175 (index hash_7 0))
         (set () float anf_176 (index hash_7 1))
         (set () vec2 col0_8 (vec2 anf_175 anf_176))
         (set () float anf_177 (index hash_7 2))
         (set () float anf_178 (index hash_7 3))
         (set () vec2 col1_9 (vec2 anf_177 anf_178))
         (set () float anf_179 (index inter_4 1))
         (set () float anf_180 (- 1. anf_179))
         (set () vec2 anf_181 (* col0_8 anf_180))
         (set () float anf_182 (index inter_4 1))
         (set () vec2 anf_183 (* col1_9 anf_182))
         (set () vec2 res_v_10 (+ anf_181 anf_183))
         (set () float anf_184 (index inter_4 0))
         (set () float anf_185 (- 1. anf_184))
         (set () float anf_186 (index inter_4 0))
         (set () vec2 anf_187 (vec2 anf_185 anf_186))
         (return (dot res_v_10 anf_187)))))
      (Function (name fractalNoise_11) (desc ()) (params (((TyVec 2) p_12)))
       (ret_type TyFloat)
       (body
        ((set () float anf_188 (smoothNoise_0 p_12))
         (set () float anf_189 (* anf_188 0.5333))
         (set () vec2 anf_190 (* p_12 2.))
         (set () float anf_191 (smoothNoise_0 anf_190))
         (set () float anf_192 (* anf_191 0.2667))
         (set () float anf_193 (+ anf_189 anf_192))
         (set () vec2 anf_194 (* p_12 4.))
         (set () float anf_195 (smoothNoise_0 anf_194))
         (set () float anf_196 (* anf_195 0.1333))
         (set () float anf_197 (+ anf_193 anf_196))
         (set () vec2 anf_198 (* p_12 8.))
         (set () float anf_199 (smoothNoise_0 anf_198))
         (set () float anf_200 (* anf_199 0.0667)) (return (+ anf_197 anf_200)))))
      (Function (name warpedNoise_13) (desc ()) (params (((TyVec 2) p_14)))
       (ret_type TyFloat)
       (body
        ((set () float anf_201 (- 0. u_time))
         (set () vec2 anf_202 (vec2 u_time anf_201))
         (set () vec2 m_15 (* anf_202 0.5)) (set () vec2 anf_203 (+ p_14 m_15))
         (set () float x_16 (fractalNoise_11 anf_203))
         (set () float anf_204 (index m_15 1))
         (set () float anf_205 (index m_15 0))
         (set () vec2 anf_206 (vec2 anf_204 anf_205))
         (set () vec2 anf_207 (+ p_14 anf_206))
         (set () vec2 anf_208 (+ anf_207 x_16))
         (set () float y_17 (fractalNoise_11 anf_208))
         (set () vec2 anf_209 (- p_14 m_15))
         (set () vec2 anf_210 (- anf_209 x_16))
         (set () vec2 anf_211 (+ anf_210 y_17))
         (set () float z_18 (fractalNoise_11 anf_211))
         (set () vec2 anf_212 (vec2 x_16 y_17))
         (set () vec2 anf_213 (vec2 y_17 z_18))
         (set () vec2 anf_214 (+ anf_212 anf_213))
         (set () vec2 anf_215 (vec2 z_18 x_16))
         (set () vec2 warp_19 (+ anf_214 anf_215))
         (set () vec3 anf_216 (vec3 x_16 y_17 z_18))
         (set () float anf_217 (length anf_216))
         (set () float mag_20 (* anf_217 0.25))
         (set () vec2 anf_218 (+ p_14 warp_19))
         (set () vec2 anf_219 (+ anf_218 mag_20))
         (return (fractalNoise_11 anf_219)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_21)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 anf_220 (* u_resolution 0.5))
         (set () vec2 anf_221 (- coord_21 anf_220))
         (set () float anf_222 (index u_resolution 1))
         (set () vec2 uv_22 (/ anf_221 anf_222))
         (set () vec2 anf_223 (* uv_22 6.))
         (set () float n_23 (warpedNoise_13 anf_223))
         (set () vec2 anf_224 (* uv_22 6.))
         (set () vec2 anf_225 (- anf_224 0.02))
         (set () float n2_24 (warpedNoise_13 anf_225))
         (set () float anf_226 (- n2_24 n_23))
         (set () float anf_227 (max anf_226 0.))
         (set () float anf_228 (/ anf_227 0.02))
         (set () float bump_25 (* anf_228 0.7071))
         (set () float anf_229 (- n_23 n2_24))
         (set () float anf_230 (max anf_229 0.))
         (set () float anf_231 (/ anf_230 0.02))
         (set () float bump2_26 (* anf_231 0.7071))
         (set () float anf_232 (* bump_25 bump_25))
         (set () float anf_233 (pow bump_25 4.))
         (set () float anf_234 (* anf_233 0.5))
         (set () float b1_27 (+ anf_232 anf_234))
         (set () float anf_235 (* bump2_26 bump2_26))
         (set () float anf_236 (pow bump2_26 4.))
         (set () float anf_237 (* anf_236 0.5))
         (set () float b2_28 (+ anf_235 anf_237))
         (set () vec3 anf_238 (vec3 1. 0.7 0.6))
         (set () float anf_239 (+ b1_27 b2_28))
         (set () float anf_240 (* anf_239 0.4))
         (set () vec3 anf_241 (vec3 b1_27 anf_240 b2_28))
         (set () vec3 anf_242 (* anf_238 anf_241))
         (set () vec3 anf_243 (* anf_242 0.3))
         (set () vec3 base_col_29 (+ anf_243 0.5))
         (set () float anf_244 (* n_23 n_23))
         (set () vec3 col_30 (* anf_244 base_col_29))
         (set () vec2 anf_245 (- uv_22 0.65))
         (set () float spot1_dist_31 (length anf_245))
         (set () vec2 anf_246 (+ uv_22 0.5))
         (set () float spot2_dist_32 (length anf_246))
         (set () vec3 anf_247 (vec3 0.8 0.4 1.))
         (set () vec3 anf_248 (* anf_247 0.35))
         (set () vec3 anf_249 (vec3 1. 0.5 0.2))
         (set () float anf_250 (- 1. spot1_dist_31))
         (set () float anf_251 (smoothstep 0. 1. anf_250))
         (set () vec3 anf_252 (* anf_249 anf_251))
         (set () vec3 anf_253 (vec3 0.2 0.4 1.))
         (set () float anf_254 (- 1. spot2_dist_32))
         (set () float anf_255 (smoothstep 0. 1. anf_254))
         (set () vec3 anf_256 (* anf_253 anf_255))
         (set () vec3 anf_257 (+ anf_252 anf_256))
         (set () vec3 anf_258 (* anf_257 5.))
         (set () vec3 spot_logic_33 (+ anf_248 anf_258))
         (set () vec3 final_col_34 (* col_30 spot_logic_33))
         (set () vec3 anf_259 (max final_col_34 0.)) (return (sqrt anf_259)))))))

    === patch main (warped_noise.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name smoothNoise_0) (desc ()) (params (((TyVec 2) p_1)))
       (ret_type TyFloat)
       (body
        ((set () vec2 i_2 (floor p_1)) (set () vec2 pf_3 (- p_1 i_2))
         (set () vec2 anf_165 (* pf_3 pf_3)) (set () vec2 anf_166 (* 2. pf_3))
         (set () vec2 anf_167 (- 3. anf_166))
         (set () vec2 inter_4 (* anf_165 anf_167))
         (set () vec4 v4_5 (vec4 0. 1. 27. 28.))
         (set () float anf_168 (index i_2 0))
         (set () vec4 anf_169 (+ v4_5 anf_168))
         (set () float anf_170 (index i_2 1))
         (set () float anf_171 (* anf_170 27.))
         (set () vec4 seed_6 (+ anf_169 anf_171))
         (set () vec4 anf_172 (% seed_6 6.2831853))
         (set () vec4 anf_173 (sin anf_172))
         (set () vec4 anf_174 (* anf_173 200000.))
         (set () vec4 hash_7 (fract anf_174))
         (set () float anf_175 (index hash_7 0))
         (set () float anf_176 (index hash_7 1))
         (set () vec2 col0_8 (vec2 anf_175 anf_176))
         (set () float anf_177 (index hash_7 2))
         (set () float anf_178 (index hash_7 3))
         (set () vec2 col1_9 (vec2 anf_177 anf_178))
         (set () float anf_179 (index inter_4 1))
         (set () float anf_180 (- 1. anf_179))
         (set () vec2 anf_181 (* col0_8 anf_180))
         (set () float anf_182 (index inter_4 1))
         (set () vec2 anf_183 (* col1_9 anf_182))
         (set () vec2 res_v_10 (+ anf_181 anf_183))
         (set () float anf_184 (index inter_4 0))
         (set () float anf_185 (- 1. anf_184))
         (set () float anf_186 (index inter_4 0))
         (set () vec2 anf_187 (vec2 anf_185 anf_186))
         (return (dot res_v_10 anf_187)))))
      (Function (name fractalNoise_11) (desc ()) (params (((TyVec 2) p_12)))
       (ret_type TyFloat)
       (body
        ((set () float anf_188 (smoothNoise_0 p_12))
         (set () float anf_189 (* anf_188 0.5333))
         (set () vec2 anf_190 (* p_12 2.))
         (set () float anf_191 (smoothNoise_0 anf_190))
         (set () float anf_192 (* anf_191 0.2667))
         (set () float anf_193 (+ anf_189 anf_192))
         (set () vec2 anf_194 (* p_12 4.))
         (set () float anf_195 (smoothNoise_0 anf_194))
         (set () float anf_196 (* anf_195 0.1333))
         (set () float anf_197 (+ anf_193 anf_196))
         (set () vec2 anf_198 (* p_12 8.))
         (set () float anf_199 (smoothNoise_0 anf_198))
         (set () float anf_200 (* anf_199 0.0667)) (return (+ anf_197 anf_200)))))
      (Function (name warpedNoise_13) (desc ()) (params (((TyVec 2) p_14)))
       (ret_type TyFloat)
       (body
        ((set () float anf_201 (- 0. u_time))
         (set () vec2 anf_202 (vec2 u_time anf_201))
         (set () vec2 m_15 (* anf_202 0.5)) (set () vec2 anf_203 (+ p_14 m_15))
         (set () float x_16 (fractalNoise_11 anf_203))
         (set () float anf_204 (index m_15 1))
         (set () float anf_205 (index m_15 0))
         (set () vec2 anf_206 (vec2 anf_204 anf_205))
         (set () vec2 anf_207 (+ p_14 anf_206))
         (set () vec2 anf_208 (+ anf_207 x_16))
         (set () float y_17 (fractalNoise_11 anf_208))
         (set () vec2 anf_209 (- p_14 m_15))
         (set () vec2 anf_210 (- anf_209 x_16))
         (set () vec2 anf_211 (+ anf_210 y_17))
         (set () float z_18 (fractalNoise_11 anf_211))
         (set () vec2 anf_212 (vec2 x_16 y_17))
         (set () vec2 anf_213 (vec2 y_17 z_18))
         (set () vec2 anf_214 (+ anf_212 anf_213))
         (set () vec2 anf_215 (vec2 z_18 x_16))
         (set () vec2 warp_19 (+ anf_214 anf_215))
         (set () vec3 anf_216 (vec3 x_16 y_17 z_18))
         (set () float anf_217 (length anf_216))
         (set () float mag_20 (* anf_217 0.25))
         (set () vec2 anf_218 (+ p_14 warp_19))
         (set () vec2 anf_219 (+ anf_218 mag_20))
         (return (fractalNoise_11 anf_219)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_21)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 anf_220 (* u_resolution 0.5))
         (set () vec2 anf_221 (- coord_21 anf_220))
         (set () float anf_222 (index u_resolution 1))
         (set () vec2 uv_22 (/ anf_221 anf_222))
         (set () vec2 anf_223 (* uv_22 6.))
         (set () float n_23 (warpedNoise_13 anf_223))
         (set () vec2 anf_224 (* uv_22 6.))
         (set () vec2 anf_225 (- anf_224 0.02))
         (set () float n2_24 (warpedNoise_13 anf_225))
         (set () float anf_226 (- n2_24 n_23))
         (set () float anf_227 (max anf_226 0.))
         (set () float anf_228 (/ anf_227 0.02))
         (set () float bump_25 (* anf_228 0.7071))
         (set () float anf_229 (- n_23 n2_24))
         (set () float anf_230 (max anf_229 0.))
         (set () float anf_231 (/ anf_230 0.02))
         (set () float bump2_26 (* anf_231 0.7071))
         (set () float anf_232 (* bump_25 bump_25))
         (set () float anf_233 (pow bump_25 4.))
         (set () float anf_234 (* anf_233 0.5))
         (set () float b1_27 (+ anf_232 anf_234))
         (set () float anf_235 (* bump2_26 bump2_26))
         (set () float anf_236 (pow bump2_26 4.))
         (set () float anf_237 (* anf_236 0.5))
         (set () float b2_28 (+ anf_235 anf_237))
         (set () vec3 anf_238 (vec3 1. 0.7 0.6))
         (set () float anf_239 (+ b1_27 b2_28))
         (set () float anf_240 (* anf_239 0.4))
         (set () vec3 anf_241 (vec3 b1_27 anf_240 b2_28))
         (set () vec3 anf_242 (* anf_238 anf_241))
         (set () vec3 anf_243 (* anf_242 0.3))
         (set () vec3 base_col_29 (+ anf_243 0.5))
         (set () float anf_244 (* n_23 n_23))
         (set () vec3 col_30 (* anf_244 base_col_29))
         (set () vec2 anf_245 (- uv_22 0.65))
         (set () float spot1_dist_31 (length anf_245))
         (set () vec2 anf_246 (+ uv_22 0.5))
         (set () float spot2_dist_32 (length anf_246))
         (set () vec3 anf_247 (vec3 0.8 0.4 1.))
         (set () vec3 anf_248 (* anf_247 0.35))
         (set () vec3 anf_249 (vec3 1. 0.5 0.2))
         (set () float anf_250 (- 1. spot1_dist_31))
         (set () float anf_251 (smoothstep 0. 1. anf_250))
         (set () vec3 anf_252 (* anf_249 anf_251))
         (set () vec3 anf_253 (vec3 0.2 0.4 1.))
         (set () float anf_254 (- 1. spot2_dist_32))
         (set () float anf_255 (smoothstep 0. 1. anf_254))
         (set () vec3 anf_256 (* anf_253 anf_255))
         (set () vec3 anf_257 (+ anf_252 anf_256))
         (set () vec3 anf_258 (* anf_257 5.))
         (set () vec3 spot_logic_33 (+ anf_248 anf_258))
         (set () vec3 final_col_34 (* col_30 spot_logic_33))
         (set () vec3 anf_259 (max final_col_34 0.)) (return (sqrt anf_259)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))
    |}]
;;
