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
       (VariantDecl ((Circle (float)) (Rect (float float)) (Empty ()))))
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
       (VariantDecl ((Circle (float)) (Rect (float float)) (Empty ()))))
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
        (VariantDecl ((Circle (float)) (Rect (float float)) (Empty ()))))
       : shape)
      ((Define Nonrec sdf_0
        ((lambda (s_1 shape)
          ((lambda (p_2 (vec 2))
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
        ((lambda (p_8 (vec 2))
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
        ((lambda (coord_12 'v_49)
          ((let top_13
            ((- ((* (2. : float) (coord_12 : 'v_49)) : 'v_49)
              (u_resolution : (vec 2)))
             : 'v_51)
            ((let bot_14
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_13 : 'v_51) (bot_14 : float)) : 'v_51))
             : 'v_51))
           : 'v_51))
         : ('v_49 -> 'v_51)))
       :
       (forall
        ((Broadcast 'v_49 (vec 2) 'v_51) (MulBroadcast float 'v_49 'v_49)
         (MulBroadcast 'v_51 float 'v_51))
        ('v_49 -> 'v_51)))
      ((Define Nonrec main
        ((lambda (coord_15 (vec 2))
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
        ((lambda (s_1 shape)
          ((lambda (p_2 (vec 2))
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
        ((lambda (p_8 (vec 2))
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
      ((Define Nonrec get_uv_11_vec2_to_vec2_94
        ((lambda (coord_12 (vec 2))
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
        ((lambda (coord_15 (vec 2))
          ((let p_16
            ((app (get_uv_11_vec2_to_vec2_94 : ((vec 2) -> (vec 2)))
              (coord_15 : (vec 2)))
             : (vec 2))
            ((let m_17
              ((app (get_uv_11_vec2_to_vec2_94 : ((vec 2) -> (vec 2)))
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
      ((Define Nonrec get_uv_11_vec2_to_vec2_94
        (lambda ((coord_12 (vec 2)))
         (let top_13 (- (* 2. coord_12) u_resolution)
          (let bot_14 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_13 bot_14)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_15 (vec 2)))
         (let p_16 (app get_uv_11_vec2_to_vec2_94 coord_15)
          (let m_17 (app get_uv_11_vec2_to_vec2_94 u_mouse)
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
     ((Define Nonrec (name get_uv_11_vec2_to_vec2_94) (args ((coord_12 (vec 2))))
       (body
        (let top_13 (- (* 2. coord_12) u_resolution)
         (let bot_14 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_13 bot_14)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (app get_uv_11_vec2_to_vec2_94 coord_15)
         (let m_17 (app get_uv_11_vec2_to_vec2_94 u_mouse)
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
          ((Circle r_3) (let anf_95 (length p_2) (return (- anf_95 r_3))))
          ((Rect w_4 h_5)
           (let anf_96 (index p_2 0)
            (let anf_97 (abs anf_96)
             (let anf_98 (- anf_97 w_4)
              (let anf_99 (index p_2 1)
               (let anf_100 (abs anf_99)
                (let anf_101 (- anf_100 h_5)
                 (let d_6 (vec2 anf_98 anf_101)
                  (let anf_102 (vec2 0. 0.)
                   (let anf_103 (max d_6 anf_102)
                    (let anf_104 (length anf_103)
                     (let anf_105 (index d_6 0)
                      (let anf_106 (index d_6 1)
                       (let anf_107 (max anf_105 anf_106)
                        (let anf_108 (min anf_107 0.)
                         (return (+ anf_104 anf_108)))))))))))))))))
          ((Empty) (return 1.))))))
      : (shape -> ((vec 2) -> float)))
     ((Define Nonrec (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let anf_109 (Variant shape Circle 0.3)
         (let circle_9 (sdf_0 anf_109 p_8)
          (let anf_110 (Variant shape Rect 0.7 0.1)
           (let rect_10 (sdf_0 anf_110 p_8) (return (min circle_9 rect_10))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name get_uv_11_vec2_to_vec2_94) (args ((coord_12 (vec 2))))
       (body
        (let anf_111 (* 2. coord_12)
         (let top_13 (- anf_111 u_resolution)
          (let anf_112 (index u_resolution 0)
           (let anf_113 (index u_resolution 1)
            (let bot_14 (min anf_112 anf_113) (return (/ top_13 bot_14)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (get_uv_11_vec2_to_vec2_94 coord_15)
         (let m_17 (get_uv_11_vec2_to_vec2_94 u_mouse)
          (let d_18 (scene_7 p_16)
           (let anf_114 (> d_18 0.)
            (let col_19
             (if anf_114 (return (vec3 0.9 0.6 0.3))
              (return (vec3 0.65 0.85 1.)))
             (let anf_115 (abs d_18)
              (let anf_116 (* -6. anf_115)
               (let anf_117 (exp anf_116)
                (let darken_21 (- 1. anf_117)
                 (let anf_118 (* 150. d_18)
                  (let anf_119 (cos anf_118)
                   (let anf_120 (* 0.2 anf_119)
                    (let rings_22 (+ 0.8 anf_120)
                     (let anf_121 (* col_19 darken_21)
                      (let col_20 (* anf_121 rings_22)
                       (let anf_122 (vec3 1. 1. 1.)
                        (let anf_123 (abs d_18)
                         (let anf_124 (smoothstep 0. 0.01 anf_123)
                          (let anf_125 (- 1. anf_124)
                           (let col_23 (mix col_20 anf_122 anf_125)
                            (let anf_126 (scene_7 m_17)
                             (let d_25 (abs anf_126)
                              (let anf_127 (- p_16 m_17)
                               (let dm_26 (length anf_127)
                                (let anf_128 (- dm_26 d_25)
                                 (let anf_129 (abs anf_128)
                                  (let anf_130 (- anf_129 0.0025)
                                   (let anf_131 (- dm_26 0.015)
                                    (let d_27 (min anf_130 anf_131)
                                     (let anf_132 (vec3 1. 1. 0.)
                                      (let anf_133 (smoothstep 0. 0.005 d_27)
                                       (let anf_134 (- 1. anf_133)
                                        (let col_24 (mix col_23 anf_132 anf_134)
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
          ((Circle r_3) (let anf_95 (length p_2) (return (- anf_95 r_3))))
          ((Rect w_4 h_5)
           (let anf_96 (index p_2 0)
            (let anf_97 (abs anf_96)
             (let anf_98 (- anf_97 w_4)
              (let anf_99 (index p_2 1)
               (let anf_100 (abs anf_99)
                (let anf_101 (- anf_100 h_5)
                 (let d_6 (vec2 anf_98 anf_101)
                  (let anf_102 (vec2 0. 0.)
                   (let anf_103 (max d_6 anf_102)
                    (let anf_104 (length anf_103)
                     (let anf_105 (index d_6 0)
                      (let anf_106 (index d_6 1)
                       (let anf_107 (max anf_105 anf_106)
                        (let anf_108 (min anf_107 0.)
                         (return (+ anf_104 anf_108)))))))))))))))))
          ((Empty) (return 1.))))))
      : (shape -> ((vec 2) -> float)))
     ((Define (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let anf_109 (Variant shape Circle 0.3)
         (let circle_9 (sdf_0 anf_109 p_8)
          (let anf_110 (Variant shape Rect 0.7 0.1)
           (let rect_10 (sdf_0 anf_110 p_8) (return (min circle_9 rect_10))))))))
      : ((vec 2) -> float))
     ((Define (name get_uv_11_vec2_to_vec2_94) (args ((coord_12 (vec 2))))
       (body
        (let anf_111 (* 2. coord_12)
         (let top_13 (- anf_111 u_resolution)
          (let anf_112 (index u_resolution 0)
           (let anf_113 (index u_resolution 1)
            (let bot_14 (min anf_112 anf_113) (return (/ top_13 bot_14)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (get_uv_11_vec2_to_vec2_94 coord_15)
         (let m_17 (get_uv_11_vec2_to_vec2_94 u_mouse)
          (let d_18 (scene_7 p_16)
           (let anf_114 (> d_18 0.)
            (let col_19
             (if anf_114 (return (vec3 0.9 0.6 0.3))
              (return (vec3 0.65 0.85 1.)))
             (let anf_115 (abs d_18)
              (let anf_116 (* -6. anf_115)
               (let anf_117 (exp anf_116)
                (let darken_21 (- 1. anf_117)
                 (let anf_118 (* 150. d_18)
                  (let anf_119 (cos anf_118)
                   (let anf_120 (* 0.2 anf_119)
                    (let rings_22 (+ 0.8 anf_120)
                     (let anf_121 (* col_19 darken_21)
                      (let col_20 (* anf_121 rings_22)
                       (let anf_122 (vec3 1. 1. 1.)
                        (let anf_123 (abs d_18)
                         (let anf_124 (smoothstep 0. 0.01 anf_123)
                          (let anf_125 (- 1. anf_124)
                           (let col_23 (mix col_20 anf_122 anf_125)
                            (let anf_126 (scene_7 m_17)
                             (let d_25 (abs anf_126)
                              (let anf_127 (- p_16 m_17)
                               (let dm_26 (length anf_127)
                                (let anf_128 (- dm_26 d_25)
                                 (let anf_129 (abs anf_128)
                                  (let anf_130 (- anf_129 0.0025)
                                   (let anf_131 (- dm_26 0.015)
                                    (let d_27 (min anf_130 anf_131)
                                     (let anf_132 (vec3 1. 1. 0.)
                                      (let anf_133 (smoothstep 0. 0.005 d_27)
                                       (let anf_134 (- 1. anf_133)
                                        (let col_24 (mix col_23 anf_132 anf_134)
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
        (let _lv_tag_135 (. s_1 tag)
         (return
          (switch _lv_tag_135
           (0
            (let r_3 (. s_1 Circle_0)
             (let anf_95 (length p_2) (return (- anf_95 r_3)))))
           (1
            (let w_4 (. s_1 Rect_0)
             (let h_5 (. s_1 Rect_1)
              (let anf_96 (index p_2 0)
               (let anf_97 (abs anf_96)
                (let anf_98 (- anf_97 w_4)
                 (let anf_99 (index p_2 1)
                  (let anf_100 (abs anf_99)
                   (let anf_101 (- anf_100 h_5)
                    (let d_6 (vec2 anf_98 anf_101)
                     (let anf_102 (vec2 0. 0.)
                      (let anf_103 (max d_6 anf_102)
                       (let anf_104 (length anf_103)
                        (let anf_105 (index d_6 0)
                         (let anf_106 (index d_6 1)
                          (let anf_107 (max anf_105 anf_106)
                           (let anf_108 (min anf_107 0.)
                            (return (+ anf_104 anf_108)))))))))))))))))))
           (default (return 1.)))))))
      : (shape -> ((vec 2) -> float)))
     ((Define (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let anf_109 (shape 0 0.3 0. 0.)
         (let circle_9 (sdf_0 anf_109 p_8)
          (let anf_110 (shape 1 0. 0.7 0.1)
           (let rect_10 (sdf_0 anf_110 p_8) (return (min circle_9 rect_10))))))))
      : ((vec 2) -> float))
     ((Define (name get_uv_11_vec2_to_vec2_94) (args ((coord_12 (vec 2))))
       (body
        (let anf_111 (* 2. coord_12)
         (let top_13 (- anf_111 u_resolution)
          (let anf_112 (index u_resolution 0)
           (let anf_113 (index u_resolution 1)
            (let bot_14 (min anf_112 anf_113) (return (/ top_13 bot_14)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (get_uv_11_vec2_to_vec2_94 coord_15)
         (let m_17 (get_uv_11_vec2_to_vec2_94 u_mouse)
          (let d_18 (scene_7 p_16)
           (let anf_114 (> d_18 0.)
            (let col_19
             (if anf_114 (return (vec3 0.9 0.6 0.3))
              (return (vec3 0.65 0.85 1.)))
             (let anf_115 (abs d_18)
              (let anf_116 (* -6. anf_115)
               (let anf_117 (exp anf_116)
                (let darken_21 (- 1. anf_117)
                 (let anf_118 (* 150. d_18)
                  (let anf_119 (cos anf_118)
                   (let anf_120 (* 0.2 anf_119)
                    (let rings_22 (+ 0.8 anf_120)
                     (let anf_121 (* col_19 darken_21)
                      (let col_20 (* anf_121 rings_22)
                       (let anf_122 (vec3 1. 1. 1.)
                        (let anf_123 (abs d_18)
                         (let anf_124 (smoothstep 0. 0.01 anf_123)
                          (let anf_125 (- 1. anf_124)
                           (let col_23 (mix col_20 anf_122 anf_125)
                            (let anf_126 (scene_7 m_17)
                             (let d_25 (abs anf_126)
                              (let anf_127 (- p_16 m_17)
                               (let dm_26 (length anf_127)
                                (let anf_128 (- dm_26 d_25)
                                 (let anf_129 (abs anf_128)
                                  (let anf_130 (- anf_129 0.0025)
                                   (let anf_131 (- dm_26 0.015)
                                    (let d_27 (min anf_130 anf_131)
                                     (let anf_132 (vec3 1. 1. 0.)
                                      (let anf_133 (smoothstep 0. 0.005 d_27)
                                       (let anf_134 (- 1. anf_133)
                                        (let col_24 (mix col_23 anf_132 anf_134)
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
        (let _lv_tag_135 (. s_1 tag)
         (return
          (switch _lv_tag_135
           (0
            (let r_3 (. s_1 Circle_0)
             (let anf_95 (length p_2) (return (- anf_95 r_3)))))
           (1
            (let w_4 (. s_1 Rect_0)
             (let h_5 (. s_1 Rect_1)
              (let anf_96 (index p_2 0)
               (let anf_97 (abs anf_96)
                (let anf_98 (- anf_97 w_4)
                 (let anf_99 (index p_2 1)
                  (let anf_100 (abs anf_99)
                   (let anf_101 (- anf_100 h_5)
                    (let d_6 (vec2 anf_98 anf_101)
                     (let anf_102 (vec2 0. 0.)
                      (let anf_103 (max d_6 anf_102)
                       (let anf_104 (length anf_103)
                        (let anf_105 (index d_6 0)
                         (let anf_106 (index d_6 1)
                          (let anf_107 (max anf_105 anf_106)
                           (let anf_108 (min anf_107 0.)
                            (return (+ anf_104 anf_108)))))))))))))))))))
           (default (return 1.)))))))
      : (shape -> ((vec 2) -> float)))
     ((Define (name scene_7) (args ((p_8 (vec 2))))
       (body
        (let anf_109 (shape 0 0.3 0. 0.)
         (let circle_9 (sdf_0 anf_109 p_8)
          (let anf_110 (shape 1 0. 0.7 0.1)
           (let rect_10 (sdf_0 anf_110 p_8) (return (min circle_9 rect_10))))))))
      : ((vec 2) -> float))
     ((Define (name get_uv_11_vec2_to_vec2_94) (args ((coord_12 (vec 2))))
       (body
        (let anf_111 (* 2. coord_12)
         (let top_13 (- anf_111 u_resolution)
          (let anf_112 (index u_resolution 0)
           (let anf_113 (index u_resolution 1)
            (let bot_14 (min anf_112 anf_113) (return (/ top_13 bot_14)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_15 (vec 2))))
       (body
        (let p_16 (get_uv_11_vec2_to_vec2_94 coord_15)
         (let m_17 (get_uv_11_vec2_to_vec2_94 u_mouse)
          (let d_18 (scene_7 p_16)
           (let anf_114 (> d_18 0.)
            (let col_19
             (if anf_114 (return (vec3 0.9 0.6 0.3))
              (return (vec3 0.65 0.85 1.)))
             (let anf_115 (abs d_18)
              (let anf_116 (* -6. anf_115)
               (let anf_117 (exp anf_116)
                (let darken_21 (- 1. anf_117)
                 (let anf_118 (* 150. d_18)
                  (let anf_119 (cos anf_118)
                   (let anf_120 (* 0.2 anf_119)
                    (let rings_22 (+ 0.8 anf_120)
                     (let anf_121 (* col_19 darken_21)
                      (let col_20 (* anf_121 rings_22)
                       (let anf_122 (vec3 1. 1. 1.)
                        (let anf_123 (abs d_18)
                         (let anf_124 (smoothstep 0. 0.01 anf_123)
                          (let anf_125 (- 1. anf_124)
                           (let col_23 (mix col_20 anf_122 anf_125)
                            (let anf_126 (scene_7 m_17)
                             (let d_25 (abs anf_126)
                              (let anf_127 (- p_16 m_17)
                               (let dm_26 (length anf_127)
                                (let anf_128 (- dm_26 d_25)
                                 (let anf_129 (abs anf_128)
                                  (let anf_130 (- anf_129 0.0025)
                                   (let anf_131 (- dm_26 0.015)
                                    (let d_27 (min anf_130 anf_131)
                                     (let anf_132 (vec3 1. 1. 0.)
                                      (let anf_133 (smoothstep 0. 0.005 d_27)
                                       (let anf_134 (- 1. anf_133)
                                        (let col_24 (mix col_23 anf_132 anf_134)
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
        ((set () int _lv_tag_135 (. s_1 tag))
         (switch _lv_tag_135
          (0 (set () float r_3 (. s_1 Circle_0))
           (set () float anf_95 (length p_2)) (return (- anf_95 r_3)) break)
          (1 (set () float w_4 (. s_1 Rect_0)) (set () float h_5 (. s_1 Rect_1))
           (set () float anf_96 (index p_2 0)) (set () float anf_97 (abs anf_96))
           (set () float anf_98 (- anf_97 w_4))
           (set () float anf_99 (index p_2 1))
           (set () float anf_100 (abs anf_99))
           (set () float anf_101 (- anf_100 h_5))
           (set () vec2 d_6 (vec2 anf_98 anf_101))
           (set () vec2 anf_102 (vec2 0. 0.))
           (set () vec2 anf_103 (max d_6 anf_102))
           (set () float anf_104 (length anf_103))
           (set () float anf_105 (index d_6 0))
           (set () float anf_106 (index d_6 1))
           (set () float anf_107 (max anf_105 anf_106))
           (set () float anf_108 (min anf_107 0.)) (return (+ anf_104 anf_108))
           break)
          (default (return 1.) break)))))
      (Function (name scene_7) (desc ()) (params (((TyVec 2) p_8)))
       (ret_type TyFloat)
       (body
        ((set () shape anf_109 (shape 0 0.3 0. 0.))
         (set () float circle_9 (sdf_0 anf_109 p_8))
         (set () shape anf_110 (shape 1 0. 0.7 0.1))
         (set () float rect_10 (sdf_0 anf_110 p_8))
         (return (min circle_9 rect_10)))))
      (Function (name get_uv_11_vec2_to_vec2_94) (desc ())
       (params (((TyVec 2) coord_12))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_111 (* 2. coord_12))
         (set () vec2 top_13 (- anf_111 u_resolution))
         (set () float anf_112 (index u_resolution 0))
         (set () float anf_113 (index u_resolution 1))
         (set () float bot_14 (min anf_112 anf_113)) (return (/ top_13 bot_14)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_15)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 p_16 (get_uv_11_vec2_to_vec2_94 coord_15))
         (set () vec2 m_17 (get_uv_11_vec2_to_vec2_94 u_mouse))
         (set () float d_18 (scene_7 p_16)) (set () bool anf_114 (> d_18 0.))
         (set () vec3 col_19 (vec3 0.))
         (if anf_114 (Block (set col_19 (vec3 0.9 0.6 0.3)))
          (Block (set col_19 (vec3 0.65 0.85 1.))))
         (set () float anf_115 (abs d_18)) (set () float anf_116 (* -6. anf_115))
         (set () float anf_117 (exp anf_116))
         (set () float darken_21 (- 1. anf_117))
         (set () float anf_118 (* 150. d_18))
         (set () float anf_119 (cos anf_118))
         (set () float anf_120 (* 0.2 anf_119))
         (set () float rings_22 (+ 0.8 anf_120))
         (set () vec3 anf_121 (* col_19 darken_21))
         (set () vec3 col_20 (* anf_121 rings_22))
         (set () vec3 anf_122 (vec3 1. 1. 1.)) (set () float anf_123 (abs d_18))
         (set () float anf_124 (smoothstep 0. 0.01 anf_123))
         (set () float anf_125 (- 1. anf_124))
         (set () vec3 col_23 (mix col_20 anf_122 anf_125))
         (set () float anf_126 (scene_7 m_17)) (set () float d_25 (abs anf_126))
         (set () vec2 anf_127 (- p_16 m_17))
         (set () float dm_26 (length anf_127))
         (set () float anf_128 (- dm_26 d_25))
         (set () float anf_129 (abs anf_128))
         (set () float anf_130 (- anf_129 0.0025))
         (set () float anf_131 (- dm_26 0.015))
         (set () float d_27 (min anf_130 anf_131))
         (set () vec3 anf_132 (vec3 1. 1. 0.))
         (set () float anf_133 (smoothstep 0. 0.005 d_27))
         (set () float anf_134 (- 1. anf_133))
         (set () vec3 col_24 (mix col_23 anf_132 anf_134)) (return col_24))))))

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
        ((set () int _lv_tag_135 (. s_1 tag))
         (switch _lv_tag_135
          (0 (set () float r_3 (. s_1 Circle_0))
           (set () float anf_95 (length p_2)) (return (- anf_95 r_3)) break)
          (1 (set () float w_4 (. s_1 Rect_0)) (set () float h_5 (. s_1 Rect_1))
           (set () float anf_96 (index p_2 0)) (set () float anf_97 (abs anf_96))
           (set () float anf_98 (- anf_97 w_4))
           (set () float anf_99 (index p_2 1))
           (set () float anf_100 (abs anf_99))
           (set () float anf_101 (- anf_100 h_5))
           (set () vec2 d_6 (vec2 anf_98 anf_101))
           (set () vec2 anf_102 (vec2 0. 0.))
           (set () vec2 anf_103 (max d_6 anf_102))
           (set () float anf_104 (length anf_103))
           (set () float anf_105 (index d_6 0))
           (set () float anf_106 (index d_6 1))
           (set () float anf_107 (max anf_105 anf_106))
           (set () float anf_108 (min anf_107 0.)) (return (+ anf_104 anf_108))
           break)
          (default (return 1.) break)))))
      (Function (name scene_7) (desc ()) (params (((TyVec 2) p_8)))
       (ret_type TyFloat)
       (body
        ((set () shape anf_109 (shape 0 0.3 0. 0.))
         (set () float circle_9 (sdf_0 anf_109 p_8))
         (set () shape anf_110 (shape 1 0. 0.7 0.1))
         (set () float rect_10 (sdf_0 anf_110 p_8))
         (return (min circle_9 rect_10)))))
      (Function (name get_uv_11_vec2_to_vec2_94) (desc ())
       (params (((TyVec 2) coord_12))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_111 (* 2. coord_12))
         (set () vec2 top_13 (- anf_111 u_resolution))
         (set () float anf_112 (index u_resolution 0))
         (set () float anf_113 (index u_resolution 1))
         (set () float bot_14 (min anf_112 anf_113)) (return (/ top_13 bot_14)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_15)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 p_16 (get_uv_11_vec2_to_vec2_94 coord_15))
         (set () vec2 m_17 (get_uv_11_vec2_to_vec2_94 u_mouse))
         (set () float d_18 (scene_7 p_16)) (set () bool anf_114 (> d_18 0.))
         (set () vec3 col_19 (vec3 0.))
         (if anf_114 (Block (set col_19 (vec3 0.9 0.6 0.3)))
          (Block (set col_19 (vec3 0.65 0.85 1.))))
         (set () float anf_115 (abs d_18)) (set () float anf_116 (* -6. anf_115))
         (set () float anf_117 (exp anf_116))
         (set () float darken_21 (- 1. anf_117))
         (set () float anf_118 (* 150. d_18))
         (set () float anf_119 (cos anf_118))
         (set () float anf_120 (* 0.2 anf_119))
         (set () float rings_22 (+ 0.8 anf_120))
         (set () vec3 anf_121 (* col_19 darken_21))
         (set () vec3 col_20 (* anf_121 rings_22))
         (set () vec3 anf_122 (vec3 1. 1. 1.)) (set () float anf_123 (abs d_18))
         (set () float anf_124 (smoothstep 0. 0.01 anf_123))
         (set () float anf_125 (- 1. anf_124))
         (set () vec3 col_23 (mix col_20 anf_122 anf_125))
         (set () float anf_126 (scene_7 m_17)) (set () float d_25 (abs anf_126))
         (set () vec2 anf_127 (- p_16 m_17))
         (set () float dm_26 (length anf_127))
         (set () float anf_128 (- dm_26 d_25))
         (set () float anf_129 (abs anf_128))
         (set () float anf_130 (- anf_129 0.0025))
         (set () float anf_131 (- dm_26 0.015))
         (set () float d_27 (min anf_130 anf_131))
         (set () vec3 anf_132 (vec3 1. 1. 0.))
         (set () float anf_133 (smoothstep 0. 0.005 d_27))
         (set () float anf_134 (- 1. anf_133))
         (set () vec3 col_24 (mix col_23 anf_132 anf_134)) (return col_24))))
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
        ((lambda (coord_1 'v_11)
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_11)) : 'v_11)
              (u_resolution : (vec 2)))
             : 'v_13)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_13) (bot_3 : float)) : 'v_13))
             : 'v_13))
           : 'v_13))
         : ('v_11 -> 'v_13)))
       :
       (forall
        ((Broadcast 'v_11 (vec 2) 'v_13) (MulBroadcast float 'v_11 'v_11)
         (MulBroadcast 'v_13 float 'v_13))
        ('v_11 -> 'v_13)))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_35
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
            ((app (get_uv_0_vec2_to_vec2_35 : ((vec 2) -> (vec 2)))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_35
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0_vec2_to_vec2_35 coord_4)
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
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_35) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0_vec2_to_vec2_35 coord_4)
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
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_35) (args ((coord_1 (vec 2))))
       (body
        (let anf_36 (* 2. coord_1)
         (let top_2 (- anf_36 u_resolution)
          (let anf_37 (index u_resolution 0)
           (let anf_38 (index u_resolution 1)
            (let bot_3 (min anf_37 anf_38) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_35 coord_4)
         (let size_6 5.
          (let anf_39 (index uv_5 0)
           (let anf_40 (* anf_39 size_6)
            (let anf_41 (* u_time 2.)
             (let anf_42 (+ anf_40 anf_41)
              (let cx_7 (floor anf_42)
               (let anf_43 (index uv_5 1)
                (let anf_44 (* anf_43 size_6)
                 (let cy_8 (floor anf_44)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_45 (/ checker_sum_9 2.)
                    (let anf_46 (floor anf_45)
                     (let anf_47 (* anf_46 2.)
                      (let is_even_10 (- checker_sum_9 anf_47)
                       (let anf_48 (< is_even_10 0.5)
                        (return
                         (if anf_48 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_35) (args ((coord_1 (vec 2))))
       (body
        (let anf_36 (* 2. coord_1)
         (let top_2 (- anf_36 u_resolution)
          (let anf_37 (index u_resolution 0)
           (let anf_38 (index u_resolution 1)
            (let bot_3 (min anf_37 anf_38) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_35 coord_4)
         (let size_6 5.
          (let anf_39 (index uv_5 0)
           (let anf_40 (* anf_39 size_6)
            (let anf_41 (* u_time 2.)
             (let anf_42 (+ anf_40 anf_41)
              (let cx_7 (floor anf_42)
               (let anf_43 (index uv_5 1)
                (let anf_44 (* anf_43 size_6)
                 (let cy_8 (floor anf_44)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_45 (/ checker_sum_9 2.)
                    (let anf_46 (floor anf_45)
                     (let anf_47 (* anf_46 2.)
                      (let is_even_10 (- checker_sum_9 anf_47)
                       (let anf_48 (< is_even_10 0.5)
                        (return
                         (if anf_48 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_35) (args ((coord_1 (vec 2))))
       (body
        (let anf_36 (* 2. coord_1)
         (let top_2 (- anf_36 u_resolution)
          (let anf_37 (index u_resolution 0)
           (let anf_38 (index u_resolution 1)
            (let bot_3 (min anf_37 anf_38) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_35 coord_4)
         (let size_6 5.
          (let anf_39 (index uv_5 0)
           (let anf_40 (* anf_39 size_6)
            (let anf_41 (* u_time 2.)
             (let anf_42 (+ anf_40 anf_41)
              (let cx_7 (floor anf_42)
               (let anf_43 (index uv_5 1)
                (let anf_44 (* anf_43 size_6)
                 (let cy_8 (floor anf_44)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_45 (/ checker_sum_9 2.)
                    (let anf_46 (floor anf_45)
                     (let anf_47 (* anf_46 2.)
                      (let is_even_10 (- checker_sum_9 anf_47)
                       (let anf_48 (< is_even_10 0.5)
                        (return
                         (if anf_48 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (checkerboard.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_35) (args ((coord_1 (vec 2))))
       (body
        (let anf_36 (* 2. coord_1)
         (let top_2 (- anf_36 u_resolution)
          (let anf_37 (index u_resolution 0)
           (let anf_38 (index u_resolution 1)
            (let bot_3 (min anf_37 anf_38) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_35 coord_4)
         (let size_6 5.
          (let anf_39 (index uv_5 0)
           (let anf_40 (* anf_39 size_6)
            (let anf_41 (* u_time 2.)
             (let anf_42 (+ anf_40 anf_41)
              (let cx_7 (floor anf_42)
               (let anf_43 (index uv_5 1)
                (let anf_44 (* anf_43 size_6)
                 (let cy_8 (floor anf_44)
                  (let checker_sum_9 (+ cx_7 cy_8)
                   (let anf_45 (/ checker_sum_9 2.)
                    (let anf_46 (floor anf_45)
                     (let anf_47 (* anf_46 2.)
                      (let is_even_10 (- checker_sum_9 anf_47)
                       (let anf_48 (< is_even_10 0.5)
                        (return
                         (if anf_48 (return (vec3 0.2 0.2 0.2))
                          (return (vec3 0.8 0.8 0.8))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (checkerboard.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_35) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_36 (* 2. coord_1))
         (set () vec2 top_2 (- anf_36 u_resolution))
         (set () float anf_37 (index u_resolution 0))
         (set () float anf_38 (index u_resolution 1))
         (set () float bot_3 (min anf_37 anf_38)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_35 coord_4))
         (set () float size_6 5.) (set () float anf_39 (index uv_5 0))
         (set () float anf_40 (* anf_39 size_6))
         (set () float anf_41 (* u_time 2.))
         (set () float anf_42 (+ anf_40 anf_41))
         (set () float cx_7 (floor anf_42)) (set () float anf_43 (index uv_5 1))
         (set () float anf_44 (* anf_43 size_6))
         (set () float cy_8 (floor anf_44))
         (set () float checker_sum_9 (+ cx_7 cy_8))
         (set () float anf_45 (/ checker_sum_9 2.))
         (set () float anf_46 (floor anf_45)) (set () float anf_47 (* anf_46 2.))
         (set () float is_even_10 (- checker_sum_9 anf_47))
         (set () bool anf_48 (< is_even_10 0.5))
         (if anf_48 (Block (return (vec3 0.2 0.2 0.2)))
          (Block (return (vec3 0.8 0.8 0.8)))))))))

    === patch main (checkerboard.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_35) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_36 (* 2. coord_1))
         (set () vec2 top_2 (- anf_36 u_resolution))
         (set () float anf_37 (index u_resolution 0))
         (set () float anf_38 (index u_resolution 1))
         (set () float bot_3 (min anf_37 anf_38)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_35 coord_4))
         (set () float size_6 5.) (set () float anf_39 (index uv_5 0))
         (set () float anf_40 (* anf_39 size_6))
         (set () float anf_41 (* u_time 2.))
         (set () float anf_42 (+ anf_40 anf_41))
         (set () float cx_7 (floor anf_42)) (set () float anf_43 (index uv_5 1))
         (set () float anf_44 (* anf_43 size_6))
         (set () float cy_8 (floor anf_44))
         (set () float checker_sum_9 (+ cx_7 cy_8))
         (set () float anf_45 (/ checker_sum_9 2.))
         (set () float anf_46 (floor anf_45)) (set () float anf_47 (* anf_46 2.))
         (set () float is_even_10 (- checker_sum_9 anf_47))
         (set () bool anf_48 (< is_even_10 0.5))
         (if anf_48 (Block (return (vec3 0.2 0.2 0.2)))
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
        ((lambda (coord_1 'v_19)
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_19)) : 'v_19)
              (u_resolution : (vec 2)))
             : 'v_21)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_21) (bot_3 : float)) : 'v_21))
             : 'v_21))
           : 'v_21))
         : ('v_19 -> 'v_21)))
       :
       (forall
        ((Broadcast 'v_19 (vec 2) 'v_21) (MulBroadcast float 'v_19 'v_19)
         (MulBroadcast 'v_21 float 'v_21))
        ('v_19 -> 'v_21)))
      ((Define (Rec 1000) mandel_4
        ((lambda (zx_5 'v_39)
          ((lambda (zy_6 'v_42)
            ((lambda (cx_7 'v_29)
              ((lambda (cy_8 'v_30)
                ((lambda (i_9 float)
                  ((if
                    ((||
                      ((>
                        ((length
                          ((vec2 (zx_5 : 'v_39) (zy_6 : 'v_42)) : (vec 2)))
                         : float)
                        (2. : float))
                       : bool)
                      ((> (i_9 : float) (150. : float)) : bool))
                     : bool)
                    (i_9 : float)
                    ((let next_zx_10
                      ((+
                        ((- ((* (zx_5 : 'v_39) (zx_5 : 'v_39)) : 'v_39)
                          ((* (zy_6 : 'v_42) (zy_6 : 'v_42)) : 'v_42))
                         : 'v_38)
                        (cx_7 : 'v_29))
                       : 'v_39)
                      ((let next_zy_11
                        ((+
                          ((* ((* (2. : float) (zx_5 : 'v_39)) : 'v_39)
                            (zy_6 : 'v_42))
                           : 'v_41)
                          (cy_8 : 'v_30))
                         : 'v_42)
                        ((app
                          ((app
                            ((app
                              ((app
                                ((app
                                  (mandel_4 :
                                   ('v_39 ->
                                    ('v_42 ->
                                     ('v_29 -> ('v_30 -> (float -> float))))))
                                  (next_zx_10 : 'v_39))
                                 :
                                 ('v_42 ->
                                  ('v_29 -> ('v_30 -> (float -> float)))))
                                (next_zy_11 : 'v_42))
                               : ('v_29 -> ('v_30 -> (float -> float))))
                              (cx_7 : 'v_29))
                             : ('v_30 -> (float -> float)))
                            (cy_8 : 'v_30))
                           : (float -> float))
                          ((+ (i_9 : float) (1. : float)) : float))
                         : float))
                       : float))
                     : float))
                   : float))
                 : (float -> float)))
               : ('v_30 -> (float -> float))))
             : ('v_29 -> ('v_30 -> (float -> float)))))
           : ('v_42 -> ('v_29 -> ('v_30 -> (float -> float))))))
         : ('v_39 -> ('v_42 -> ('v_29 -> ('v_30 -> (float -> float)))))))
       :
       (forall
        ((Comparable 'v_42) (Comparable 'v_39) (Broadcast 'v_38 'v_29 'v_39)
         (Broadcast 'v_39 'v_42 'v_38) (MulBroadcast 'v_39 'v_39 'v_39)
         (MulBroadcast 'v_42 'v_42 'v_42) (Broadcast 'v_41 'v_30 'v_42)
         (MulBroadcast 'v_39 'v_42 'v_41) (MulBroadcast float 'v_39 'v_39))
        ('v_39 -> ('v_42 -> ('v_29 -> ('v_30 -> (float -> float)))))))
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
        mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
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
                                  (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_82
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
        ((lambda (coord_12 (vec 2))
          ((let uv_13
            ((app (get_uv_0_vec2_to_vec2_82 : ((vec 2) -> (vec 2)))
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
                              (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
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
        mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
        (lambda ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float))
         (if (|| (> (length (vec2 zx_5 zy_6)) 2.) (> i_9 150.)) i_9
          (let next_zx_10 (+ (- (* zx_5 zx_5) (* zy_6 zy_6)) cx_7)
           (let next_zy_11 (+ (* (* 2. zx_5) zy_6) cy_8)
            (app mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
             next_zx_10 next_zy_11 cx_7 cy_8 (+ i_9 1.)))))))
       : (float -> (float -> (float -> (float -> (float -> float))))))
      ((Define Nonrec get_uv_0_vec2_to_vec2_82
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_12 (vec 2)))
         (let uv_13 (app get_uv_0_vec2_to_vec2_82 coord_12)
          (let zoom_14 (exp (+ (* (sin (* u_time 0.4)) 4.5) 3.5))
           (let cx_15 (+ -0.7453 (/ (index uv_13 0) zoom_14))
            (let cy_16 (+ 0.1127 (/ (index uv_13 1) zoom_14))
             (let iter_17
              (app mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
               0. 0. cx_15 cy_16 0.)
              (if (> iter_17 149.) (vec3 0. 0. 0.)
               (let n_18 (/ iter_17 150.)
                (+ (* (sin (+ (* n_18 (vec3 10. 20. 30.)) u_time)) 0.5) 0.5))))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (Rec 1000)
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_81)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (if (|| (> (length (vec2 zx_5 zy_6)) 2.) (> i_9 150.)) i_9
         (let next_zx_10 (+ (- (* zx_5 zx_5) (* zy_6 zy_6)) cx_7)
          (let next_zy_11 (+ (* (* 2. zx_5) zy_6) cy_8)
           (app mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
            next_zx_10 next_zy_11 cx_7 cy_8 (+ i_9 1.)))))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_82) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (app get_uv_0_vec2_to_vec2_82 coord_12)
         (let zoom_14 (exp (+ (* (sin (* u_time 0.4)) 4.5) 3.5))
          (let cx_15 (+ -0.7453 (/ (index uv_13 0) zoom_14))
           (let cy_16 (+ 0.1127 (/ (index uv_13 1) zoom_14))
            (let iter_17
             (app mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
              0. 0. cx_15 cy_16 0.)
             (if (> iter_17 149.) (vec3 0. 0. 0.)
              (let n_18 (/ iter_17 150.)
               (+ (* (sin (+ (* n_18 (vec3 10. 20. 30.)) u_time)) 0.5) 0.5))))))))))
      : ((vec 2) -> (vec 3))))

    === anf (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (Rec 1000)
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_81)
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
                       (return
                        (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
                         next_zx_10 next_zy_11 cx_7 cy_8 anf_93)))))))))))))))))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_82) (args ((coord_1 (vec 2))))
       (body
        (let anf_94 (* 2. coord_1)
         (let top_2 (- anf_94 u_resolution)
          (let anf_95 (index u_resolution 0)
           (let anf_96 (index u_resolution 1)
            (let bot_3 (min anf_95 anf_96) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0_vec2_to_vec2_82 coord_12)
         (let anf_97 (* u_time 0.4)
          (let anf_98 (sin anf_97)
           (let anf_99 (* anf_98 4.5)
            (let anf_100 (+ anf_99 3.5)
             (let zoom_14 (exp anf_100)
              (let anf_101 (index uv_13 0)
               (let anf_102 (/ anf_101 zoom_14)
                (let cx_15 (+ -0.7453 anf_102)
                 (let anf_103 (index uv_13 1)
                  (let anf_104 (/ anf_103 zoom_14)
                   (let cy_16 (+ 0.1127 anf_104)
                    (let iter_17
                     (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
                      0. 0. cx_15 cy_16 0.)
                     (let anf_105 (> iter_17 149.)
                      (return
                       (if anf_105 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_106 (vec3 10. 20. 30.)
                          (let anf_107 (* n_18 anf_106)
                           (let anf_108 (+ anf_107 u_time)
                            (let anf_109 (sin anf_108)
                             (let anf_110 (* anf_109 0.5)
                              (return (+ anf_110 0.5))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_81)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let _iter_111 0
         (while (< _iter_111 1000)
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
                              (let _iter_inc_112 (+ _iter_111 1)
                               (set _iter_111 _iter_inc_112 continue))))))))))))))))))))))
          (return 0.)))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define (name get_uv_0_vec2_to_vec2_82) (args ((coord_1 (vec 2))))
       (body
        (let anf_94 (* 2. coord_1)
         (let top_2 (- anf_94 u_resolution)
          (let anf_95 (index u_resolution 0)
           (let anf_96 (index u_resolution 1)
            (let bot_3 (min anf_95 anf_96) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0_vec2_to_vec2_82 coord_12)
         (let anf_97 (* u_time 0.4)
          (let anf_98 (sin anf_97)
           (let anf_99 (* anf_98 4.5)
            (let anf_100 (+ anf_99 3.5)
             (let zoom_14 (exp anf_100)
              (let anf_101 (index uv_13 0)
               (let anf_102 (/ anf_101 zoom_14)
                (let cx_15 (+ -0.7453 anf_102)
                 (let anf_103 (index uv_13 1)
                  (let anf_104 (/ anf_103 zoom_14)
                   (let cy_16 (+ 0.1127 anf_104)
                    (let iter_17
                     (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
                      0. 0. cx_15 cy_16 0.)
                     (let anf_105 (> iter_17 149.)
                      (return
                       (if anf_105 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_106 (vec3 10. 20. 30.)
                          (let anf_107 (* n_18 anf_106)
                           (let anf_108 (+ anf_107 u_time)
                            (let anf_109 (sin anf_108)
                             (let anf_110 (* anf_109 0.5)
                              (return (+ anf_110 0.5))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_81)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let _iter_111 0
         (while (< _iter_111 1000)
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
                              (let _iter_inc_112 (+ _iter_111 1)
                               (set _iter_111 _iter_inc_112 continue))))))))))))))))))))))
          (return 0.)))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define (name get_uv_0_vec2_to_vec2_82) (args ((coord_1 (vec 2))))
       (body
        (let anf_94 (* 2. coord_1)
         (let top_2 (- anf_94 u_resolution)
          (let anf_95 (index u_resolution 0)
           (let anf_96 (index u_resolution 1)
            (let bot_3 (min anf_95 anf_96) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0_vec2_to_vec2_82 coord_12)
         (let anf_97 (* u_time 0.4)
          (let anf_98 (sin anf_97)
           (let anf_99 (* anf_98 4.5)
            (let anf_100 (+ anf_99 3.5)
             (let zoom_14 (exp anf_100)
              (let anf_101 (index uv_13 0)
               (let anf_102 (/ anf_101 zoom_14)
                (let cx_15 (+ -0.7453 anf_102)
                 (let anf_103 (index uv_13 1)
                  (let anf_104 (/ anf_103 zoom_14)
                   (let cy_16 (+ 0.1127 anf_104)
                    (let iter_17
                     (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
                      0. 0. cx_15 cy_16 0.)
                     (let anf_105 (> iter_17 149.)
                      (return
                       (if anf_105 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_106 (vec3 10. 20. 30.)
                          (let anf_107 (* n_18 anf_106)
                           (let anf_108 (+ anf_107 u_time)
                            (let anf_109 (sin anf_108)
                             (let anf_110 (* anf_109 0.5)
                              (return (+ anf_110 0.5))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (mandelbrot.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_81)
       (args ((zx_5 float) (zy_6 float) (cx_7 float) (cy_8 float) (i_9 float)))
       (body
        (let _iter_111 0
         (while (< _iter_111 1000)
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
                              (let _iter_inc_112 (+ _iter_111 1)
                               (set _iter_111 _iter_inc_112 continue))))))))))))))))))))))
          (return 0.)))))
      : (float -> (float -> (float -> (float -> (float -> float))))))
     ((Define (name get_uv_0_vec2_to_vec2_82) (args ((coord_1 (vec 2))))
       (body
        (let anf_94 (* 2. coord_1)
         (let top_2 (- anf_94 u_resolution)
          (let anf_95 (index u_resolution 0)
           (let anf_96 (index u_resolution 1)
            (let bot_3 (min anf_95 anf_96) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_12 (vec 2))))
       (body
        (let uv_13 (get_uv_0_vec2_to_vec2_82 coord_12)
         (let anf_97 (* u_time 0.4)
          (let anf_98 (sin anf_97)
           (let anf_99 (* anf_98 4.5)
            (let anf_100 (+ anf_99 3.5)
             (let zoom_14 (exp anf_100)
              (let anf_101 (index uv_13 0)
               (let anf_102 (/ anf_101 zoom_14)
                (let cx_15 (+ -0.7453 anf_102)
                 (let anf_103 (index uv_13 1)
                  (let anf_104 (/ anf_103 zoom_14)
                   (let cy_16 (+ 0.1127 anf_104)
                    (let iter_17
                     (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81
                      0. 0. cx_15 cy_16 0.)
                     (let anf_105 (> iter_17 149.)
                      (return
                       (if anf_105 (return (vec3 0. 0. 0.))
                        (let n_18 (/ iter_17 150.)
                         (let anf_106 (vec3 10. 20. 30.)
                          (let anf_107 (* n_18 anf_106)
                           (let anf_108 (+ anf_107 u_time)
                            (let anf_109 (sin anf_108)
                             (let anf_110 (* anf_109 0.5)
                              (return (+ anf_110 0.5))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (mandelbrot.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_81)
       (desc ())
       (params
        ((TyFloat zx_5) (TyFloat zy_6) (TyFloat cx_7) (TyFloat cy_8)
         (TyFloat i_9)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_111 0)
         (while (< _iter_111 1000)
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
             (set i_9 anf_93) (set () int _iter_inc_112 (+ _iter_111 1))
             (set _iter_111 _iter_inc_112) continue))))
         (return 0.))))
      (Function (name get_uv_0_vec2_to_vec2_82) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_94 (* 2. coord_1))
         (set () vec2 top_2 (- anf_94 u_resolution))
         (set () float anf_95 (index u_resolution 0))
         (set () float anf_96 (index u_resolution 1))
         (set () float bot_3 (min anf_95 anf_96)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_12)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_13 (get_uv_0_vec2_to_vec2_82 coord_12))
         (set () float anf_97 (* u_time 0.4)) (set () float anf_98 (sin anf_97))
         (set () float anf_99 (* anf_98 4.5))
         (set () float anf_100 (+ anf_99 3.5))
         (set () float zoom_14 (exp anf_100))
         (set () float anf_101 (index uv_13 0))
         (set () float anf_102 (/ anf_101 zoom_14))
         (set () float cx_15 (+ -0.7453 anf_102))
         (set () float anf_103 (index uv_13 1))
         (set () float anf_104 (/ anf_103 zoom_14))
         (set () float cy_16 (+ 0.1127 anf_104))
         (set () float iter_17
          (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81 0. 0.
           cx_15 cy_16 0.))
         (set () bool anf_105 (> iter_17 149.))
         (if anf_105 (Block (return (vec3 0. 0. 0.)))
          (Block (set () float n_18 (/ iter_17 150.))
           (set () vec3 anf_106 (vec3 10. 20. 30.))
           (set () vec3 anf_107 (* n_18 anf_106))
           (set () vec3 anf_108 (+ anf_107 u_time))
           (set () vec3 anf_109 (sin anf_108))
           (set () vec3 anf_110 (* anf_109 0.5)) (return (+ anf_110 0.5)))))))))

    === patch main (mandelbrot.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function
       (name mandel_4_float_to_float_to_float_to_float_to_float_to_float_81)
       (desc ())
       (params
        ((TyFloat zx_5) (TyFloat zy_6) (TyFloat cx_7) (TyFloat cy_8)
         (TyFloat i_9)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_111 0)
         (while (< _iter_111 1000)
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
             (set i_9 anf_93) (set () int _iter_inc_112 (+ _iter_111 1))
             (set _iter_111 _iter_inc_112) continue))))
         (return 0.))))
      (Function (name get_uv_0_vec2_to_vec2_82) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_94 (* 2. coord_1))
         (set () vec2 top_2 (- anf_94 u_resolution))
         (set () float anf_95 (index u_resolution 0))
         (set () float anf_96 (index u_resolution 1))
         (set () float bot_3 (min anf_95 anf_96)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_12)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_13 (get_uv_0_vec2_to_vec2_82 coord_12))
         (set () float anf_97 (* u_time 0.4)) (set () float anf_98 (sin anf_97))
         (set () float anf_99 (* anf_98 4.5))
         (set () float anf_100 (+ anf_99 3.5))
         (set () float zoom_14 (exp anf_100))
         (set () float anf_101 (index uv_13 0))
         (set () float anf_102 (/ anf_101 zoom_14))
         (set () float cx_15 (+ -0.7453 anf_102))
         (set () float anf_103 (index uv_13 1))
         (set () float anf_104 (/ anf_103 zoom_14))
         (set () float cy_16 (+ 0.1127 anf_104))
         (set () float iter_17
          (mandel_4_float_to_float_to_float_to_float_to_float_to_float_81 0. 0.
           cx_15 cy_16 0.))
         (set () bool anf_105 (> iter_17 149.))
         (if anf_105 (Block (return (vec3 0. 0. 0.)))
          (Block (set () float n_18 (/ iter_17 150.))
           (set () vec3 anf_106 (vec3 10. 20. 30.))
           (set () vec3 anf_107 (* n_18 anf_106))
           (set () vec3 anf_108 (+ anf_107 u_time))
           (set () vec3 anf_109 (sin anf_108))
           (set () vec3 anf_110 (* anf_109 0.5)) (return (+ anf_110 0.5)))))))
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
        ((lambda (coord_1 'v_8)
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_8)) : 'v_8)
              (u_resolution : (vec 2)))
             : 'v_10)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_10) (bot_3 : float)) : 'v_10))
             : 'v_10))
           : 'v_10))
         : ('v_8 -> 'v_10)))
       :
       (forall
        ((Broadcast 'v_8 (vec 2) 'v_10) (MulBroadcast float 'v_8 'v_8)
         (MulBroadcast 'v_10 float 'v_10))
        ('v_8 -> 'v_10)))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_28
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
            ((app (get_uv_0_vec2_to_vec2_28 : ((vec 2) -> (vec 2)))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_28
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0_vec2_to_vec2_28 coord_4)
          (let mouseUV_6
           (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
           (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
            (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
             (vec3 0.5 0.5 1.)))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_28) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0_vec2_to_vec2_28 coord_4)
         (let mouseUV_6
          (/ (- (* 2. u_mouse) u_resolution) (index u_resolution 1))
          (let radius_7 (+ (* (sin (* u_time 2.)) 0.1) 0.15)
           (if (< (distance uv_5 mouseUV_6) radius_7) (vec3 0. 0. 0.5)
            (vec3 0.5 0.5 1.)))))))
      : ((vec 2) -> (vec 3))))

    === anf (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_28) (args ((coord_1 (vec 2))))
       (body
        (let anf_29 (* 2. coord_1)
         (let top_2 (- anf_29 u_resolution)
          (let anf_30 (index u_resolution 0)
           (let anf_31 (index u_resolution 1)
            (let bot_3 (min anf_30 anf_31) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_28 coord_4)
         (let anf_32 (* 2. u_mouse)
          (let anf_33 (- anf_32 u_resolution)
           (let anf_34 (index u_resolution 1)
            (let mouseUV_6 (/ anf_33 anf_34)
             (let anf_35 (* u_time 2.)
              (let anf_36 (sin anf_35)
               (let anf_37 (* anf_36 0.1)
                (let radius_7 (+ anf_37 0.15)
                 (let anf_38 (distance uv_5 mouseUV_6)
                  (let anf_39 (< anf_38 radius_7)
                   (return
                    (if anf_39 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_28) (args ((coord_1 (vec 2))))
       (body
        (let anf_29 (* 2. coord_1)
         (let top_2 (- anf_29 u_resolution)
          (let anf_30 (index u_resolution 0)
           (let anf_31 (index u_resolution 1)
            (let bot_3 (min anf_30 anf_31) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_28 coord_4)
         (let anf_32 (* 2. u_mouse)
          (let anf_33 (- anf_32 u_resolution)
           (let anf_34 (index u_resolution 1)
            (let mouseUV_6 (/ anf_33 anf_34)
             (let anf_35 (* u_time 2.)
              (let anf_36 (sin anf_35)
               (let anf_37 (* anf_36 0.1)
                (let radius_7 (+ anf_37 0.15)
                 (let anf_38 (distance uv_5 mouseUV_6)
                  (let anf_39 (< anf_38 radius_7)
                   (return
                    (if anf_39 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_28) (args ((coord_1 (vec 2))))
       (body
        (let anf_29 (* 2. coord_1)
         (let top_2 (- anf_29 u_resolution)
          (let anf_30 (index u_resolution 0)
           (let anf_31 (index u_resolution 1)
            (let bot_3 (min anf_30 anf_31) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_28 coord_4)
         (let anf_32 (* 2. u_mouse)
          (let anf_33 (- anf_32 u_resolution)
           (let anf_34 (index u_resolution 1)
            (let mouseUV_6 (/ anf_33 anf_34)
             (let anf_35 (* u_time 2.)
              (let anf_36 (sin anf_35)
               (let anf_37 (* anf_36 0.1)
                (let radius_7 (+ anf_37 0.15)
                 (let anf_38 (distance uv_5 mouseUV_6)
                  (let anf_39 (< anf_38 radius_7)
                   (return
                    (if anf_39 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (mouse_circle.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_mouse) : (vec 2))
     ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_28) (args ((coord_1 (vec 2))))
       (body
        (let anf_29 (* 2. coord_1)
         (let top_2 (- anf_29 u_resolution)
          (let anf_30 (index u_resolution 0)
           (let anf_31 (index u_resolution 1)
            (let bot_3 (min anf_30 anf_31) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_28 coord_4)
         (let anf_32 (* 2. u_mouse)
          (let anf_33 (- anf_32 u_resolution)
           (let anf_34 (index u_resolution 1)
            (let mouseUV_6 (/ anf_33 anf_34)
             (let anf_35 (* u_time 2.)
              (let anf_36 (sin anf_35)
               (let anf_37 (* anf_36 0.1)
                (let radius_7 (+ anf_37 0.15)
                 (let anf_38 (distance uv_5 mouseUV_6)
                  (let anf_39 (< anf_38 radius_7)
                   (return
                    (if anf_39 (return (vec3 0. 0. 0.5))
                     (return (vec3 0.5 0.5 1.)))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (mouse_circle.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform (TyVec 2) u_mouse ()) (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_28) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_29 (* 2. coord_1))
         (set () vec2 top_2 (- anf_29 u_resolution))
         (set () float anf_30 (index u_resolution 0))
         (set () float anf_31 (index u_resolution 1))
         (set () float bot_3 (min anf_30 anf_31)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_28 coord_4))
         (set () vec2 anf_32 (* 2. u_mouse))
         (set () vec2 anf_33 (- anf_32 u_resolution))
         (set () float anf_34 (index u_resolution 1))
         (set () vec2 mouseUV_6 (/ anf_33 anf_34))
         (set () float anf_35 (* u_time 2.)) (set () float anf_36 (sin anf_35))
         (set () float anf_37 (* anf_36 0.1))
         (set () float radius_7 (+ anf_37 0.15))
         (set () float anf_38 (distance uv_5 mouseUV_6))
         (set () bool anf_39 (< anf_38 radius_7))
         (if anf_39 (Block (return (vec3 0. 0. 0.5)))
          (Block (return (vec3 0.5 0.5 1.)))))))))

    === patch main (mouse_circle.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform (TyVec 2) u_mouse ()) (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_28) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_29 (* 2. coord_1))
         (set () vec2 top_2 (- anf_29 u_resolution))
         (set () float anf_30 (index u_resolution 0))
         (set () float anf_31 (index u_resolution 1))
         (set () float bot_3 (min anf_30 anf_31)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_28 coord_4))
         (set () vec2 anf_32 (* 2. u_mouse))
         (set () vec2 anf_33 (- anf_32 u_resolution))
         (set () float anf_34 (index u_resolution 1))
         (set () vec2 mouseUV_6 (/ anf_33 anf_34))
         (set () float anf_35 (* u_time 2.)) (set () float anf_36 (sin anf_35))
         (set () float anf_37 (* anf_36 0.1))
         (set () float radius_7 (+ anf_37 0.15))
         (set () float anf_38 (distance uv_5 mouseUV_6))
         (set () bool anf_39 (< anf_38 radius_7))
         (if anf_39 (Block (return (vec3 0. 0. 0.5)))
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
        ((lambda (coord_1 'v_10)
          ((let top_2
            ((- ((* (2. : float) (coord_1 : 'v_10)) : 'v_10)
              (u_resolution : (vec 2)))
             : 'v_12)
            ((let bot_3
              ((min ((index (u_resolution : (vec 2)) 0) : float)
                ((index (u_resolution : (vec 2)) 1) : float))
               : float)
              ((/ (top_2 : 'v_12) (bot_3 : float)) : 'v_12))
             : 'v_12))
           : 'v_12))
         : ('v_10 -> 'v_12)))
       :
       (forall
        ((Broadcast 'v_10 (vec 2) 'v_12) (MulBroadcast float 'v_10 'v_10)
         (MulBroadcast 'v_12 float 'v_12))
        ('v_10 -> 'v_12)))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_36
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
            ((app (get_uv_0_vec2_to_vec2_36 : ((vec 2) -> (vec 2)))
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
      ((Define Nonrec get_uv_0_vec2_to_vec2_36
        (lambda ((coord_1 (vec 2)))
         (let top_2 (- (* 2. coord_1) u_resolution)
          (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
           (/ top_2 bot_3)))))
       : ((vec 2) -> (vec 2)))
      ((Define Nonrec main
        (lambda ((coord_4 (vec 2)))
         (let uv_5 (app get_uv_0_vec2_to_vec2_36 coord_4)
          (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
           (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
            (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
             (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))
       : ((vec 2) -> (vec 3)))))

    === lambda lift (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_36) (args ((coord_1 (vec 2))))
       (body
        (let top_2 (- (* 2. coord_1) u_resolution)
         (let bot_3 (min (index u_resolution 0) (index u_resolution 1))
          (/ top_2 bot_3)))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (app get_uv_0_vec2_to_vec2_36 coord_4)
         (let wave_6 (+ (* 5. (+ (index uv_5 0) (index uv_5 1))) u_time)
          (let r_7 (+ (* (sin wave_6) 0.3) 0.7)
           (let g_8 (+ (* (sin (+ wave_6 2.)) 0.3) 0.7)
            (let b_9 (+ (* (sin (+ wave_6 4.)) 0.3) 0.7) (vec3 r_7 g_8 b_9))))))))
      : ((vec 2) -> (vec 3))))

    === anf (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define Nonrec (name get_uv_0_vec2_to_vec2_36) (args ((coord_1 (vec 2))))
       (body
        (let anf_37 (* 2. coord_1)
         (let top_2 (- anf_37 u_resolution)
          (let anf_38 (index u_resolution 0)
           (let anf_39 (index u_resolution 1)
            (let bot_3 (min anf_38 anf_39) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_36 coord_4)
         (let anf_40 (index uv_5 0)
          (let anf_41 (index uv_5 1)
           (let anf_42 (+ anf_40 anf_41)
            (let anf_43 (* 5. anf_42)
             (let wave_6 (+ anf_43 u_time)
              (let anf_44 (sin wave_6)
               (let anf_45 (* anf_44 0.3)
                (let r_7 (+ anf_45 0.7)
                 (let anf_46 (+ wave_6 2.)
                  (let anf_47 (sin anf_46)
                   (let anf_48 (* anf_47 0.3)
                    (let g_8 (+ anf_48 0.7)
                     (let anf_49 (+ wave_6 4.)
                      (let anf_50 (sin anf_49)
                       (let anf_51 (* anf_50 0.3)
                        (let b_9 (+ anf_51 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_36) (args ((coord_1 (vec 2))))
       (body
        (let anf_37 (* 2. coord_1)
         (let top_2 (- anf_37 u_resolution)
          (let anf_38 (index u_resolution 0)
           (let anf_39 (index u_resolution 1)
            (let bot_3 (min anf_38 anf_39) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_36 coord_4)
         (let anf_40 (index uv_5 0)
          (let anf_41 (index uv_5 1)
           (let anf_42 (+ anf_40 anf_41)
            (let anf_43 (* 5. anf_42)
             (let wave_6 (+ anf_43 u_time)
              (let anf_44 (sin wave_6)
               (let anf_45 (* anf_44 0.3)
                (let r_7 (+ anf_45 0.7)
                 (let anf_46 (+ wave_6 2.)
                  (let anf_47 (sin anf_46)
                   (let anf_48 (* anf_47 0.3)
                    (let g_8 (+ anf_48 0.7)
                     (let anf_49 (+ wave_6 4.)
                      (let anf_50 (sin anf_49)
                       (let anf_51 (* anf_50 0.3)
                        (let b_9 (+ anf_51 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_36) (args ((coord_1 (vec 2))))
       (body
        (let anf_37 (* 2. coord_1)
         (let top_2 (- anf_37 u_resolution)
          (let anf_38 (index u_resolution 0)
           (let anf_39 (index u_resolution 1)
            (let bot_3 (min anf_38 anf_39) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_36 coord_4)
         (let anf_40 (index uv_5 0)
          (let anf_41 (index uv_5 1)
           (let anf_42 (+ anf_40 anf_41)
            (let anf_43 (* 5. anf_42)
             (let wave_6 (+ anf_43 u_time)
              (let anf_44 (sin wave_6)
               (let anf_45 (* anf_44 0.3)
                (let r_7 (+ anf_45 0.7)
                 (let anf_46 (+ wave_6 2.)
                  (let anf_47 (sin anf_46)
                   (let anf_48 (* anf_47 0.3)
                    (let g_8 (+ anf_48 0.7)
                     (let anf_49 (+ wave_6 4.)
                      (let anf_50 (sin anf_49)
                       (let anf_51 (* anf_50 0.3)
                        (let b_9 (+ anf_51 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (rainbow.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0_vec2_to_vec2_36) (args ((coord_1 (vec 2))))
       (body
        (let anf_37 (* 2. coord_1)
         (let top_2 (- anf_37 u_resolution)
          (let anf_38 (index u_resolution 0)
           (let anf_39 (index u_resolution 1)
            (let bot_3 (min anf_38 anf_39) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name main) (args ((coord_4 (vec 2))))
       (body
        (let uv_5 (get_uv_0_vec2_to_vec2_36 coord_4)
         (let anf_40 (index uv_5 0)
          (let anf_41 (index uv_5 1)
           (let anf_42 (+ anf_40 anf_41)
            (let anf_43 (* 5. anf_42)
             (let wave_6 (+ anf_43 u_time)
              (let anf_44 (sin wave_6)
               (let anf_45 (* anf_44 0.3)
                (let r_7 (+ anf_45 0.7)
                 (let anf_46 (+ wave_6 2.)
                  (let anf_47 (sin anf_46)
                   (let anf_48 (* anf_47 0.3)
                    (let g_8 (+ anf_48 0.7)
                     (let anf_49 (+ wave_6 4.)
                      (let anf_50 (sin anf_49)
                       (let anf_51 (* anf_50 0.3)
                        (let b_9 (+ anf_51 0.7) (return (vec3 r_7 g_8 b_9)))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (rainbow.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_36) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_37 (* 2. coord_1))
         (set () vec2 top_2 (- anf_37 u_resolution))
         (set () float anf_38 (index u_resolution 0))
         (set () float anf_39 (index u_resolution 1))
         (set () float bot_3 (min anf_38 anf_39)) (return (/ top_2 bot_3)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_36 coord_4))
         (set () float anf_40 (index uv_5 0))
         (set () float anf_41 (index uv_5 1))
         (set () float anf_42 (+ anf_40 anf_41))
         (set () float anf_43 (* 5. anf_42))
         (set () float wave_6 (+ anf_43 u_time))
         (set () float anf_44 (sin wave_6)) (set () float anf_45 (* anf_44 0.3))
         (set () float r_7 (+ anf_45 0.7)) (set () float anf_46 (+ wave_6 2.))
         (set () float anf_47 (sin anf_46)) (set () float anf_48 (* anf_47 0.3))
         (set () float g_8 (+ anf_48 0.7)) (set () float anf_49 (+ wave_6 4.))
         (set () float anf_50 (sin anf_49)) (set () float anf_51 (* anf_50 0.3))
         (set () float b_9 (+ anf_51 0.7)) (return (vec3 r_7 g_8 b_9)))))))

    === patch main (rainbow.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0_vec2_to_vec2_36) (desc ())
       (params (((TyVec 2) coord_1))) (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_37 (* 2. coord_1))
         (set () vec2 top_2 (- anf_37 u_resolution))
         (set () float anf_38 (index u_resolution 0))
         (set () float anf_39 (index u_resolution 1))
         (set () float bot_3 (min anf_38 anf_39)) (return (/ top_2 bot_3)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_4)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_5 (get_uv_0_vec2_to_vec2_36 coord_4))
         (set () float anf_40 (index uv_5 0))
         (set () float anf_41 (index uv_5 1))
         (set () float anf_42 (+ anf_40 anf_41))
         (set () float anf_43 (* 5. anf_42))
         (set () float wave_6 (+ anf_43 u_time))
         (set () float anf_44 (sin wave_6)) (set () float anf_45 (* anf_44 0.3))
         (set () float r_7 (+ anf_45 0.7)) (set () float anf_46 (+ wave_6 2.))
         (set () float anf_47 (sin anf_46)) (set () float anf_48 (* anf_47 0.3))
         (set () float g_8 (+ anf_48 0.7)) (set () float anf_49 (+ wave_6 4.))
         (set () float anf_50 (sin anf_49)) (set () float anf_51 (* anf_50 0.3))
         (set () float b_9 (+ anf_51 0.7)) (return (vec3 r_7 g_8 b_9)))))
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
            ((let (rec 1000) march_27
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
            ((let (rec 1000) march_27
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
     ((Define (Rec 1000) (name march_27_175)
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
     ((Define (Rec 1000) (name march_27_175)
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

    === tail call (raymarch.glml) ===
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

    === lower variants (raymarch.glml) ===
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

    === promote ints (raymarch.glml) ===
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
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ()) (Global Uniform (TyVec 2) u_mouse ())
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

    === patch main (raymarch.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ()) (Global Uniform (TyVec 2) u_mouse ())
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
      ((Define (Rec 1000) gcd_8
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
      ((Define (Rec 1000) gcd_8
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
      ((Define (Rec 1000) gcd_8
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
     ((Define (Rec 1000) (name gcd_8) (args ((a_9 float) (b_10 float)))
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
        (let anf_57 (* 2. coord_1)
         (let top_2 (- anf_57 u_resolution)
          (let anf_58 (index u_resolution 0)
           (let anf_59 (index u_resolution 1)
            (let bot_3 (min anf_58 anf_59) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define Nonrec (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_60 (* -1. s_6) (return (mat2x2 c_7 anf_60 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (Rec 1000) (name gcd_8) (args ((a_9 float) (b_10 float)))
       (body
        (let anf_61 (< a_9 0.05)
         (return
          (if anf_61 (return b_10)
           (let anf_62 (< b_10 0.05)
            (return
             (if anf_62 (return a_9)
              (let anf_63 (> a_9 b_10)
               (return
                (if anf_63 (let anf_64 (- a_9 b_10) (return (gcd_8 anf_64 b_10)))
                 (let anf_65 (- b_10 a_9) (return (gcd_8 a_9 anf_65))))))))))))))
      : (float -> (float -> float)))
     ((Define Nonrec (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_66 (rotate_4 u_time)
          (let uv_13 (* anf_66 uv_12)
           (let anf_67 (index uv_13 0)
            (let anf_68 (* u_time 2.)
             (let anf_69 (sin anf_68)
              (let anf_70 (* anf_67 anf_69)
               (let anf_71 (* anf_70 2.)
                (let x_14 (abs anf_71)
                 (let anf_72 (index uv_13 1)
                  (let anf_73 (* u_time 2.)
                   (let anf_74 (sin anf_73)
                    (let anf_75 (* anf_72 anf_74)
                     (let anf_76 (* anf_75 2.)
                      (let y_15 (abs anf_76)
                       (let res_16 (gcd_8 x_14 y_15)
                        (let anf_77 (* res_16 0.5)
                         (let anf_78 (- 1. res_16)
                          (return (vec3 res_16 anf_77 anf_78))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_57 (* 2. coord_1)
         (let top_2 (- anf_57 u_resolution)
          (let anf_58 (index u_resolution 0)
           (let anf_59 (index u_resolution 1)
            (let bot_3 (min anf_58 anf_59) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_60 (* -1. s_6) (return (mat2x2 c_7 anf_60 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (name gcd_8) (args ((a_9 float) (b_10 float)))
       (body
        (let _iter_79 0
         (while (< _iter_79 1000)
          (let anf_61 (< a_9 0.05)
           (return
            (if anf_61 (return b_10)
             (let anf_62 (< b_10 0.05)
              (return
               (if anf_62 (return a_9)
                (let anf_63 (> a_9 b_10)
                 (return
                  (if anf_63
                   (let anf_64 (- a_9 b_10)
                    (set a_9 anf_64
                     (set b_10 b_10
                      (let _iter_inc_80 (+ _iter_79 1)
                       (set _iter_79 _iter_inc_80 continue)))))
                   (let anf_65 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_65
                      (let _iter_inc_81 (+ _iter_79 1)
                       (set _iter_79 _iter_inc_81 continue))))))))))))))
          (return 0.)))))
      : (float -> (float -> float)))
     ((Define (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_66 (rotate_4 u_time)
          (let uv_13 (* anf_66 uv_12)
           (let anf_67 (index uv_13 0)
            (let anf_68 (* u_time 2.)
             (let anf_69 (sin anf_68)
              (let anf_70 (* anf_67 anf_69)
               (let anf_71 (* anf_70 2.)
                (let x_14 (abs anf_71)
                 (let anf_72 (index uv_13 1)
                  (let anf_73 (* u_time 2.)
                   (let anf_74 (sin anf_73)
                    (let anf_75 (* anf_72 anf_74)
                     (let anf_76 (* anf_75 2.)
                      (let y_15 (abs anf_76)
                       (let res_16 (gcd_8 x_14 y_15)
                        (let anf_77 (* res_16 0.5)
                         (let anf_78 (- 1. res_16)
                          (return (vec3 res_16 anf_77 anf_78))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_57 (* 2. coord_1)
         (let top_2 (- anf_57 u_resolution)
          (let anf_58 (index u_resolution 0)
           (let anf_59 (index u_resolution 1)
            (let bot_3 (min anf_58 anf_59) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_60 (* -1. s_6) (return (mat2x2 c_7 anf_60 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (name gcd_8) (args ((a_9 float) (b_10 float)))
       (body
        (let _iter_79 0
         (while (< _iter_79 1000)
          (let anf_61 (< a_9 0.05)
           (return
            (if anf_61 (return b_10)
             (let anf_62 (< b_10 0.05)
              (return
               (if anf_62 (return a_9)
                (let anf_63 (> a_9 b_10)
                 (return
                  (if anf_63
                   (let anf_64 (- a_9 b_10)
                    (set a_9 anf_64
                     (set b_10 b_10
                      (let _iter_inc_80 (+ _iter_79 1)
                       (set _iter_79 _iter_inc_80 continue)))))
                   (let anf_65 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_65
                      (let _iter_inc_81 (+ _iter_79 1)
                       (set _iter_79 _iter_inc_81 continue))))))))))))))
          (return 0.)))))
      : (float -> (float -> float)))
     ((Define (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_66 (rotate_4 u_time)
          (let uv_13 (* anf_66 uv_12)
           (let anf_67 (index uv_13 0)
            (let anf_68 (* u_time 2.)
             (let anf_69 (sin anf_68)
              (let anf_70 (* anf_67 anf_69)
               (let anf_71 (* anf_70 2.)
                (let x_14 (abs anf_71)
                 (let anf_72 (index uv_13 1)
                  (let anf_73 (* u_time 2.)
                   (let anf_74 (sin anf_73)
                    (let anf_75 (* anf_72 anf_74)
                     (let anf_76 (* anf_75 2.)
                      (let y_15 (abs anf_76)
                       (let res_16 (gcd_8 x_14 y_15)
                        (let anf_77 (* res_16 0.5)
                         (let anf_78 (- 1. res_16)
                          (return (vec3 res_16 anf_77 anf_78))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (recursion.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name get_uv_0) (args ((coord_1 (vec 2))))
       (body
        (let anf_57 (* 2. coord_1)
         (let top_2 (- anf_57 u_resolution)
          (let anf_58 (index u_resolution 0)
           (let anf_59 (index u_resolution 1)
            (let bot_3 (min anf_58 anf_59) (return (/ top_2 bot_3)))))))))
      : ((vec 2) -> (vec 2)))
     ((Define (name rotate_4) (args ((angle_5 float)))
       (body
        (let s_6 (sin angle_5)
         (let c_7 (cos angle_5)
          (let anf_60 (* -1. s_6) (return (mat2x2 c_7 anf_60 s_6 c_7)))))))
      : (float -> (mat 2 2)))
     ((Define (name gcd_8) (args ((a_9 float) (b_10 float)))
       (body
        (let _iter_79 0
         (while (< _iter_79 1000)
          (let anf_61 (< a_9 0.05)
           (return
            (if anf_61 (return b_10)
             (let anf_62 (< b_10 0.05)
              (return
               (if anf_62 (return a_9)
                (let anf_63 (> a_9 b_10)
                 (return
                  (if anf_63
                   (let anf_64 (- a_9 b_10)
                    (set a_9 anf_64
                     (set b_10 b_10
                      (let _iter_inc_80 (+ _iter_79 1)
                       (set _iter_79 _iter_inc_80 continue)))))
                   (let anf_65 (- b_10 a_9)
                    (set a_9 a_9
                     (set b_10 anf_65
                      (let _iter_inc_81 (+ _iter_79 1)
                       (set _iter_79 _iter_inc_81 continue))))))))))))))
          (return 0.)))))
      : (float -> (float -> float)))
     ((Define (name main) (args ((coord_11 (vec 2))))
       (body
        (let uv_12 (get_uv_0 coord_11)
         (let anf_66 (rotate_4 u_time)
          (let uv_13 (* anf_66 uv_12)
           (let anf_67 (index uv_13 0)
            (let anf_68 (* u_time 2.)
             (let anf_69 (sin anf_68)
              (let anf_70 (* anf_67 anf_69)
               (let anf_71 (* anf_70 2.)
                (let x_14 (abs anf_71)
                 (let anf_72 (index uv_13 1)
                  (let anf_73 (* u_time 2.)
                   (let anf_74 (sin anf_73)
                    (let anf_75 (* anf_72 anf_74)
                     (let anf_76 (* anf_75 2.)
                      (let y_15 (abs anf_76)
                       (let res_16 (gcd_8 x_14 y_15)
                        (let anf_77 (* res_16 0.5)
                         (let anf_78 (- 1. res_16)
                          (return (vec3 res_16 anf_77 anf_78))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (recursion.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_57 (* 2. coord_1))
         (set () vec2 top_2 (- anf_57 u_resolution))
         (set () float anf_58 (index u_resolution 0))
         (set () float anf_59 (index u_resolution 1))
         (set () float bot_3 (min anf_58 anf_59)) (return (/ top_2 bot_3)))))
      (Function (name rotate_4) (desc ()) (params ((TyFloat angle_5)))
       (ret_type (TyMat 2 2))
       (body
        ((set () float s_6 (sin angle_5)) (set () float c_7 (cos angle_5))
         (set () float anf_60 (* -1. s_6)) (return (mat2 c_7 anf_60 s_6 c_7)))))
      (Function (name gcd_8) (desc ()) (params ((TyFloat a_9) (TyFloat b_10)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_79 0)
         (while (< _iter_79 1000)
          (Block (set () bool anf_61 (< a_9 0.05))
           (if anf_61 (Block (return b_10))
            (Block (set () bool anf_62 (< b_10 0.05))
             (if anf_62 (Block (return a_9))
              (Block (set () bool anf_63 (> a_9 b_10))
               (if anf_63
                (Block (set () float anf_64 (- a_9 b_10)) (set a_9 anf_64)
                 (set b_10 b_10) (set () int _iter_inc_80 (+ _iter_79 1))
                 (set _iter_79 _iter_inc_80) continue)
                (Block (set () float anf_65 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_65) (set () int _iter_inc_81 (+ _iter_79 1))
                 (set _iter_79 _iter_inc_81) continue))))))))
         (return 0.))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_12 (get_uv_0 coord_11))
         (set () mat2 anf_66 (rotate_4 u_time))
         (set () vec2 uv_13 (* anf_66 uv_12))
         (set () float anf_67 (index uv_13 0))
         (set () float anf_68 (* u_time 2.)) (set () float anf_69 (sin anf_68))
         (set () float anf_70 (* anf_67 anf_69))
         (set () float anf_71 (* anf_70 2.)) (set () float x_14 (abs anf_71))
         (set () float anf_72 (index uv_13 1))
         (set () float anf_73 (* u_time 2.)) (set () float anf_74 (sin anf_73))
         (set () float anf_75 (* anf_72 anf_74))
         (set () float anf_76 (* anf_75 2.)) (set () float y_15 (abs anf_76))
         (set () float res_16 (gcd_8 x_14 y_15))
         (set () float anf_77 (* res_16 0.5)) (set () float anf_78 (- 1. res_16))
         (return (vec3 res_16 anf_77 anf_78)))))))

    === patch main (recursion.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name get_uv_0) (desc ()) (params (((TyVec 2) coord_1)))
       (ret_type (TyVec 2))
       (body
        ((set () vec2 anf_57 (* 2. coord_1))
         (set () vec2 top_2 (- anf_57 u_resolution))
         (set () float anf_58 (index u_resolution 0))
         (set () float anf_59 (index u_resolution 1))
         (set () float bot_3 (min anf_58 anf_59)) (return (/ top_2 bot_3)))))
      (Function (name rotate_4) (desc ()) (params ((TyFloat angle_5)))
       (ret_type (TyMat 2 2))
       (body
        ((set () float s_6 (sin angle_5)) (set () float c_7 (cos angle_5))
         (set () float anf_60 (* -1. s_6)) (return (mat2 c_7 anf_60 s_6 c_7)))))
      (Function (name gcd_8) (desc ()) (params ((TyFloat a_9) (TyFloat b_10)))
       (ret_type TyFloat)
       (body
        ((set () int _iter_79 0)
         (while (< _iter_79 1000)
          (Block (set () bool anf_61 (< a_9 0.05))
           (if anf_61 (Block (return b_10))
            (Block (set () bool anf_62 (< b_10 0.05))
             (if anf_62 (Block (return a_9))
              (Block (set () bool anf_63 (> a_9 b_10))
               (if anf_63
                (Block (set () float anf_64 (- a_9 b_10)) (set a_9 anf_64)
                 (set b_10 b_10) (set () int _iter_inc_80 (+ _iter_79 1))
                 (set _iter_79 _iter_inc_80) continue)
                (Block (set () float anf_65 (- b_10 a_9)) (set a_9 a_9)
                 (set b_10 anf_65) (set () int _iter_inc_81 (+ _iter_79 1))
                 (set _iter_79 _iter_inc_81) continue))))))))
         (return 0.))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_11)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 uv_12 (get_uv_0 coord_11))
         (set () mat2 anf_66 (rotate_4 u_time))
         (set () vec2 uv_13 (* anf_66 uv_12))
         (set () float anf_67 (index uv_13 0))
         (set () float anf_68 (* u_time 2.)) (set () float anf_69 (sin anf_68))
         (set () float anf_70 (* anf_67 anf_69))
         (set () float anf_71 (* anf_70 2.)) (set () float x_14 (abs anf_71))
         (set () float anf_72 (index uv_13 1))
         (set () float anf_73 (* u_time 2.)) (set () float anf_74 (sin anf_73))
         (set () float anf_75 (* anf_72 anf_74))
         (set () float anf_76 (* anf_75 2.)) (set () float y_15 (abs anf_76))
         (set () float res_16 (gcd_8 x_14 y_15))
         (set () float anf_77 (* res_16 0.5)) (set () float anf_78 (- 1. res_16))
         (return (vec3 res_16 anf_77 anf_78)))))
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
        ((lambda (p_1 (vec 2))
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
        ((lambda (p_12 (vec 2))
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
        ((lambda (p_14 (vec 2))
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
        ((lambda (coord_21 (vec 2))
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
        ((lambda (p_1 (vec 2))
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
        ((lambda (p_12 (vec 2))
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
        ((lambda (p_14 (vec 2))
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
        ((lambda (coord_21 (vec 2))
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
          (let anf_150 (* pf_3 pf_3)
           (let anf_151 (* 2. pf_3)
            (let anf_152 (- 3. anf_151)
             (let inter_4 (* anf_150 anf_152)
              (let v4_5 (vec4 0. 1. 27. 28.)
               (let anf_153 (index i_2 0)
                (let anf_154 (+ v4_5 anf_153)
                 (let anf_155 (index i_2 1)
                  (let anf_156 (* anf_155 27.)
                   (let seed_6 (+ anf_154 anf_156)
                    (let anf_157 (% seed_6 6.2831853)
                     (let anf_158 (sin anf_157)
                      (let anf_159 (* anf_158 200000.)
                       (let hash_7 (fract anf_159)
                        (let anf_160 (index hash_7 0)
                         (let anf_161 (index hash_7 1)
                          (let col0_8 (vec2 anf_160 anf_161)
                           (let anf_162 (index hash_7 2)
                            (let anf_163 (index hash_7 3)
                             (let col1_9 (vec2 anf_162 anf_163)
                              (let anf_164 (index inter_4 1)
                               (let anf_165 (- 1. anf_164)
                                (let anf_166 (* col0_8 anf_165)
                                 (let anf_167 (index inter_4 1)
                                  (let anf_168 (* col1_9 anf_167)
                                   (let res_v_10 (+ anf_166 anf_168)
                                    (let anf_169 (index inter_4 0)
                                     (let anf_170 (- 1. anf_169)
                                      (let anf_171 (index inter_4 0)
                                       (let anf_172 (vec2 anf_170 anf_171)
                                        (return (dot res_v_10 anf_172))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (let anf_173 (smoothNoise_0 p_12)
         (let anf_174 (* anf_173 0.5333)
          (let anf_175 (* p_12 2.)
           (let anf_176 (smoothNoise_0 anf_175)
            (let anf_177 (* anf_176 0.2667)
             (let anf_178 (+ anf_174 anf_177)
              (let anf_179 (* p_12 4.)
               (let anf_180 (smoothNoise_0 anf_179)
                (let anf_181 (* anf_180 0.1333)
                 (let anf_182 (+ anf_178 anf_181)
                  (let anf_183 (* p_12 8.)
                   (let anf_184 (smoothNoise_0 anf_183)
                    (let anf_185 (* anf_184 0.0667) (return (+ anf_182 anf_185)))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let anf_186 (- 0. u_time)
         (let anf_187 (vec2 u_time anf_186)
          (let m_15 (* anf_187 0.5)
           (let anf_188 (+ p_14 m_15)
            (let x_16 (fractalNoise_11 anf_188)
             (let anf_189 (index m_15 1)
              (let anf_190 (index m_15 0)
               (let anf_191 (vec2 anf_189 anf_190)
                (let anf_192 (+ p_14 anf_191)
                 (let anf_193 (+ anf_192 x_16)
                  (let y_17 (fractalNoise_11 anf_193)
                   (let anf_194 (- p_14 m_15)
                    (let anf_195 (- anf_194 x_16)
                     (let anf_196 (+ anf_195 y_17)
                      (let z_18 (fractalNoise_11 anf_196)
                       (let anf_197 (vec2 x_16 y_17)
                        (let anf_198 (vec2 y_17 z_18)
                         (let anf_199 (+ anf_197 anf_198)
                          (let anf_200 (vec2 z_18 x_16)
                           (let warp_19 (+ anf_199 anf_200)
                            (let anf_201 (vec3 x_16 y_17 z_18)
                             (let anf_202 (length anf_201)
                              (let mag_20 (* anf_202 0.25)
                               (let anf_203 (+ p_14 warp_19)
                                (let anf_204 (+ anf_203 mag_20)
                                 (return (fractalNoise_11 anf_204)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define Nonrec (name main) (args ((coord_21 (vec 2))))
       (body
        (let anf_205 (* u_resolution 0.5)
         (let anf_206 (- coord_21 anf_205)
          (let anf_207 (index u_resolution 1)
           (let uv_22 (/ anf_206 anf_207)
            (let anf_208 (* uv_22 6.)
             (let n_23 (warpedNoise_13 anf_208)
              (let anf_209 (* uv_22 6.)
               (let anf_210 (- anf_209 0.02)
                (let n2_24 (warpedNoise_13 anf_210)
                 (let anf_211 (- n2_24 n_23)
                  (let anf_212 (max anf_211 0.)
                   (let anf_213 (/ anf_212 0.02)
                    (let bump_25 (* anf_213 0.7071)
                     (let anf_214 (- n_23 n2_24)
                      (let anf_215 (max anf_214 0.)
                       (let anf_216 (/ anf_215 0.02)
                        (let bump2_26 (* anf_216 0.7071)
                         (let anf_217 (* bump_25 bump_25)
                          (let anf_218 (pow bump_25 4.)
                           (let anf_219 (* anf_218 0.5)
                            (let b1_27 (+ anf_217 anf_219)
                             (let anf_220 (* bump2_26 bump2_26)
                              (let anf_221 (pow bump2_26 4.)
                               (let anf_222 (* anf_221 0.5)
                                (let b2_28 (+ anf_220 anf_222)
                                 (let anf_223 (vec3 1. 0.7 0.6)
                                  (let anf_224 (+ b1_27 b2_28)
                                   (let anf_225 (* anf_224 0.4)
                                    (let anf_226 (vec3 b1_27 anf_225 b2_28)
                                     (let anf_227 (* anf_223 anf_226)
                                      (let anf_228 (* anf_227 0.3)
                                       (let base_col_29 (+ anf_228 0.5)
                                        (let anf_229 (* n_23 n_23)
                                         (let col_30 (* anf_229 base_col_29)
                                          (let anf_230 (- uv_22 0.65)
                                           (let spot1_dist_31 (length anf_230)
                                            (let anf_231 (+ uv_22 0.5)
                                             (let spot2_dist_32 (length anf_231)
                                              (let anf_232 (vec3 0.8 0.4 1.)
                                               (let anf_233 (* anf_232 0.35)
                                                (let anf_234 (vec3 1. 0.5 0.2)
                                                 (let anf_235
                                                  (- 1. spot1_dist_31)
                                                  (let anf_236
                                                   (smoothstep 0. 1. anf_235)
                                                   (let anf_237
                                                    (* anf_234 anf_236)
                                                    (let anf_238
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_239
                                                      (- 1. spot2_dist_32)
                                                      (let anf_240
                                                       (smoothstep 0. 1. anf_239)
                                                       (let anf_241
                                                        (* anf_238 anf_240)
                                                        (let anf_242
                                                         (+ anf_237 anf_241)
                                                         (let anf_243
                                                          (* anf_242 5.)
                                                          (let spot_logic_33
                                                           (+ anf_233 anf_243)
                                                           (let final_col_34
                                                            (* col_30
                                                             spot_logic_33)
                                                            (let anf_244
                                                             (max final_col_34
                                                              0.)
                                                             (return
                                                              (sqrt anf_244)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === tail call (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name smoothNoise_0) (args ((p_1 (vec 2))))
       (body
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let anf_150 (* pf_3 pf_3)
           (let anf_151 (* 2. pf_3)
            (let anf_152 (- 3. anf_151)
             (let inter_4 (* anf_150 anf_152)
              (let v4_5 (vec4 0. 1. 27. 28.)
               (let anf_153 (index i_2 0)
                (let anf_154 (+ v4_5 anf_153)
                 (let anf_155 (index i_2 1)
                  (let anf_156 (* anf_155 27.)
                   (let seed_6 (+ anf_154 anf_156)
                    (let anf_157 (% seed_6 6.2831853)
                     (let anf_158 (sin anf_157)
                      (let anf_159 (* anf_158 200000.)
                       (let hash_7 (fract anf_159)
                        (let anf_160 (index hash_7 0)
                         (let anf_161 (index hash_7 1)
                          (let col0_8 (vec2 anf_160 anf_161)
                           (let anf_162 (index hash_7 2)
                            (let anf_163 (index hash_7 3)
                             (let col1_9 (vec2 anf_162 anf_163)
                              (let anf_164 (index inter_4 1)
                               (let anf_165 (- 1. anf_164)
                                (let anf_166 (* col0_8 anf_165)
                                 (let anf_167 (index inter_4 1)
                                  (let anf_168 (* col1_9 anf_167)
                                   (let res_v_10 (+ anf_166 anf_168)
                                    (let anf_169 (index inter_4 0)
                                     (let anf_170 (- 1. anf_169)
                                      (let anf_171 (index inter_4 0)
                                       (let anf_172 (vec2 anf_170 anf_171)
                                        (return (dot res_v_10 anf_172))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (let anf_173 (smoothNoise_0 p_12)
         (let anf_174 (* anf_173 0.5333)
          (let anf_175 (* p_12 2.)
           (let anf_176 (smoothNoise_0 anf_175)
            (let anf_177 (* anf_176 0.2667)
             (let anf_178 (+ anf_174 anf_177)
              (let anf_179 (* p_12 4.)
               (let anf_180 (smoothNoise_0 anf_179)
                (let anf_181 (* anf_180 0.1333)
                 (let anf_182 (+ anf_178 anf_181)
                  (let anf_183 (* p_12 8.)
                   (let anf_184 (smoothNoise_0 anf_183)
                    (let anf_185 (* anf_184 0.0667) (return (+ anf_182 anf_185)))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let anf_186 (- 0. u_time)
         (let anf_187 (vec2 u_time anf_186)
          (let m_15 (* anf_187 0.5)
           (let anf_188 (+ p_14 m_15)
            (let x_16 (fractalNoise_11 anf_188)
             (let anf_189 (index m_15 1)
              (let anf_190 (index m_15 0)
               (let anf_191 (vec2 anf_189 anf_190)
                (let anf_192 (+ p_14 anf_191)
                 (let anf_193 (+ anf_192 x_16)
                  (let y_17 (fractalNoise_11 anf_193)
                   (let anf_194 (- p_14 m_15)
                    (let anf_195 (- anf_194 x_16)
                     (let anf_196 (+ anf_195 y_17)
                      (let z_18 (fractalNoise_11 anf_196)
                       (let anf_197 (vec2 x_16 y_17)
                        (let anf_198 (vec2 y_17 z_18)
                         (let anf_199 (+ anf_197 anf_198)
                          (let anf_200 (vec2 z_18 x_16)
                           (let warp_19 (+ anf_199 anf_200)
                            (let anf_201 (vec3 x_16 y_17 z_18)
                             (let anf_202 (length anf_201)
                              (let mag_20 (* anf_202 0.25)
                               (let anf_203 (+ p_14 warp_19)
                                (let anf_204 (+ anf_203 mag_20)
                                 (return (fractalNoise_11 anf_204)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name main) (args ((coord_21 (vec 2))))
       (body
        (let anf_205 (* u_resolution 0.5)
         (let anf_206 (- coord_21 anf_205)
          (let anf_207 (index u_resolution 1)
           (let uv_22 (/ anf_206 anf_207)
            (let anf_208 (* uv_22 6.)
             (let n_23 (warpedNoise_13 anf_208)
              (let anf_209 (* uv_22 6.)
               (let anf_210 (- anf_209 0.02)
                (let n2_24 (warpedNoise_13 anf_210)
                 (let anf_211 (- n2_24 n_23)
                  (let anf_212 (max anf_211 0.)
                   (let anf_213 (/ anf_212 0.02)
                    (let bump_25 (* anf_213 0.7071)
                     (let anf_214 (- n_23 n2_24)
                      (let anf_215 (max anf_214 0.)
                       (let anf_216 (/ anf_215 0.02)
                        (let bump2_26 (* anf_216 0.7071)
                         (let anf_217 (* bump_25 bump_25)
                          (let anf_218 (pow bump_25 4.)
                           (let anf_219 (* anf_218 0.5)
                            (let b1_27 (+ anf_217 anf_219)
                             (let anf_220 (* bump2_26 bump2_26)
                              (let anf_221 (pow bump2_26 4.)
                               (let anf_222 (* anf_221 0.5)
                                (let b2_28 (+ anf_220 anf_222)
                                 (let anf_223 (vec3 1. 0.7 0.6)
                                  (let anf_224 (+ b1_27 b2_28)
                                   (let anf_225 (* anf_224 0.4)
                                    (let anf_226 (vec3 b1_27 anf_225 b2_28)
                                     (let anf_227 (* anf_223 anf_226)
                                      (let anf_228 (* anf_227 0.3)
                                       (let base_col_29 (+ anf_228 0.5)
                                        (let anf_229 (* n_23 n_23)
                                         (let col_30 (* anf_229 base_col_29)
                                          (let anf_230 (- uv_22 0.65)
                                           (let spot1_dist_31 (length anf_230)
                                            (let anf_231 (+ uv_22 0.5)
                                             (let spot2_dist_32 (length anf_231)
                                              (let anf_232 (vec3 0.8 0.4 1.)
                                               (let anf_233 (* anf_232 0.35)
                                                (let anf_234 (vec3 1. 0.5 0.2)
                                                 (let anf_235
                                                  (- 1. spot1_dist_31)
                                                  (let anf_236
                                                   (smoothstep 0. 1. anf_235)
                                                   (let anf_237
                                                    (* anf_234 anf_236)
                                                    (let anf_238
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_239
                                                      (- 1. spot2_dist_32)
                                                      (let anf_240
                                                       (smoothstep 0. 1. anf_239)
                                                       (let anf_241
                                                        (* anf_238 anf_240)
                                                        (let anf_242
                                                         (+ anf_237 anf_241)
                                                         (let anf_243
                                                          (* anf_242 5.)
                                                          (let spot_logic_33
                                                           (+ anf_233 anf_243)
                                                           (let final_col_34
                                                            (* col_30
                                                             spot_logic_33)
                                                            (let anf_244
                                                             (max final_col_34
                                                              0.)
                                                             (return
                                                              (sqrt anf_244)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === lower variants (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name smoothNoise_0) (args ((p_1 (vec 2))))
       (body
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let anf_150 (* pf_3 pf_3)
           (let anf_151 (* 2. pf_3)
            (let anf_152 (- 3. anf_151)
             (let inter_4 (* anf_150 anf_152)
              (let v4_5 (vec4 0. 1. 27. 28.)
               (let anf_153 (index i_2 0)
                (let anf_154 (+ v4_5 anf_153)
                 (let anf_155 (index i_2 1)
                  (let anf_156 (* anf_155 27.)
                   (let seed_6 (+ anf_154 anf_156)
                    (let anf_157 (% seed_6 6.2831853)
                     (let anf_158 (sin anf_157)
                      (let anf_159 (* anf_158 200000.)
                       (let hash_7 (fract anf_159)
                        (let anf_160 (index hash_7 0)
                         (let anf_161 (index hash_7 1)
                          (let col0_8 (vec2 anf_160 anf_161)
                           (let anf_162 (index hash_7 2)
                            (let anf_163 (index hash_7 3)
                             (let col1_9 (vec2 anf_162 anf_163)
                              (let anf_164 (index inter_4 1)
                               (let anf_165 (- 1. anf_164)
                                (let anf_166 (* col0_8 anf_165)
                                 (let anf_167 (index inter_4 1)
                                  (let anf_168 (* col1_9 anf_167)
                                   (let res_v_10 (+ anf_166 anf_168)
                                    (let anf_169 (index inter_4 0)
                                     (let anf_170 (- 1. anf_169)
                                      (let anf_171 (index inter_4 0)
                                       (let anf_172 (vec2 anf_170 anf_171)
                                        (return (dot res_v_10 anf_172))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (let anf_173 (smoothNoise_0 p_12)
         (let anf_174 (* anf_173 0.5333)
          (let anf_175 (* p_12 2.)
           (let anf_176 (smoothNoise_0 anf_175)
            (let anf_177 (* anf_176 0.2667)
             (let anf_178 (+ anf_174 anf_177)
              (let anf_179 (* p_12 4.)
               (let anf_180 (smoothNoise_0 anf_179)
                (let anf_181 (* anf_180 0.1333)
                 (let anf_182 (+ anf_178 anf_181)
                  (let anf_183 (* p_12 8.)
                   (let anf_184 (smoothNoise_0 anf_183)
                    (let anf_185 (* anf_184 0.0667) (return (+ anf_182 anf_185)))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let anf_186 (- 0. u_time)
         (let anf_187 (vec2 u_time anf_186)
          (let m_15 (* anf_187 0.5)
           (let anf_188 (+ p_14 m_15)
            (let x_16 (fractalNoise_11 anf_188)
             (let anf_189 (index m_15 1)
              (let anf_190 (index m_15 0)
               (let anf_191 (vec2 anf_189 anf_190)
                (let anf_192 (+ p_14 anf_191)
                 (let anf_193 (+ anf_192 x_16)
                  (let y_17 (fractalNoise_11 anf_193)
                   (let anf_194 (- p_14 m_15)
                    (let anf_195 (- anf_194 x_16)
                     (let anf_196 (+ anf_195 y_17)
                      (let z_18 (fractalNoise_11 anf_196)
                       (let anf_197 (vec2 x_16 y_17)
                        (let anf_198 (vec2 y_17 z_18)
                         (let anf_199 (+ anf_197 anf_198)
                          (let anf_200 (vec2 z_18 x_16)
                           (let warp_19 (+ anf_199 anf_200)
                            (let anf_201 (vec3 x_16 y_17 z_18)
                             (let anf_202 (length anf_201)
                              (let mag_20 (* anf_202 0.25)
                               (let anf_203 (+ p_14 warp_19)
                                (let anf_204 (+ anf_203 mag_20)
                                 (return (fractalNoise_11 anf_204)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name main) (args ((coord_21 (vec 2))))
       (body
        (let anf_205 (* u_resolution 0.5)
         (let anf_206 (- coord_21 anf_205)
          (let anf_207 (index u_resolution 1)
           (let uv_22 (/ anf_206 anf_207)
            (let anf_208 (* uv_22 6.)
             (let n_23 (warpedNoise_13 anf_208)
              (let anf_209 (* uv_22 6.)
               (let anf_210 (- anf_209 0.02)
                (let n2_24 (warpedNoise_13 anf_210)
                 (let anf_211 (- n2_24 n_23)
                  (let anf_212 (max anf_211 0.)
                   (let anf_213 (/ anf_212 0.02)
                    (let bump_25 (* anf_213 0.7071)
                     (let anf_214 (- n_23 n2_24)
                      (let anf_215 (max anf_214 0.)
                       (let anf_216 (/ anf_215 0.02)
                        (let bump2_26 (* anf_216 0.7071)
                         (let anf_217 (* bump_25 bump_25)
                          (let anf_218 (pow bump_25 4.)
                           (let anf_219 (* anf_218 0.5)
                            (let b1_27 (+ anf_217 anf_219)
                             (let anf_220 (* bump2_26 bump2_26)
                              (let anf_221 (pow bump2_26 4.)
                               (let anf_222 (* anf_221 0.5)
                                (let b2_28 (+ anf_220 anf_222)
                                 (let anf_223 (vec3 1. 0.7 0.6)
                                  (let anf_224 (+ b1_27 b2_28)
                                   (let anf_225 (* anf_224 0.4)
                                    (let anf_226 (vec3 b1_27 anf_225 b2_28)
                                     (let anf_227 (* anf_223 anf_226)
                                      (let anf_228 (* anf_227 0.3)
                                       (let base_col_29 (+ anf_228 0.5)
                                        (let anf_229 (* n_23 n_23)
                                         (let col_30 (* anf_229 base_col_29)
                                          (let anf_230 (- uv_22 0.65)
                                           (let spot1_dist_31 (length anf_230)
                                            (let anf_231 (+ uv_22 0.5)
                                             (let spot2_dist_32 (length anf_231)
                                              (let anf_232 (vec3 0.8 0.4 1.)
                                               (let anf_233 (* anf_232 0.35)
                                                (let anf_234 (vec3 1. 0.5 0.2)
                                                 (let anf_235
                                                  (- 1. spot1_dist_31)
                                                  (let anf_236
                                                   (smoothstep 0. 1. anf_235)
                                                   (let anf_237
                                                    (* anf_234 anf_236)
                                                    (let anf_238
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_239
                                                      (- 1. spot2_dist_32)
                                                      (let anf_240
                                                       (smoothstep 0. 1. anf_239)
                                                       (let anf_241
                                                        (* anf_238 anf_240)
                                                        (let anf_242
                                                         (+ anf_237 anf_241)
                                                         (let anf_243
                                                          (* anf_242 5.)
                                                          (let spot_logic_33
                                                           (+ anf_233 anf_243)
                                                           (let final_col_34
                                                            (* col_30
                                                             spot_logic_33)
                                                            (let anf_244
                                                             (max final_col_34
                                                              0.)
                                                             (return
                                                              (sqrt anf_244)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === promote ints (warped_noise.glml) ===
    (Program ((Extern u_resolution) : (vec 2)) ((Extern u_time) : float)
     ((Define (name smoothNoise_0) (args ((p_1 (vec 2))))
       (body
        (let i_2 (floor p_1)
         (let pf_3 (- p_1 i_2)
          (let anf_150 (* pf_3 pf_3)
           (let anf_151 (* 2. pf_3)
            (let anf_152 (- 3. anf_151)
             (let inter_4 (* anf_150 anf_152)
              (let v4_5 (vec4 0. 1. 27. 28.)
               (let anf_153 (index i_2 0)
                (let anf_154 (+ v4_5 anf_153)
                 (let anf_155 (index i_2 1)
                  (let anf_156 (* anf_155 27.)
                   (let seed_6 (+ anf_154 anf_156)
                    (let anf_157 (% seed_6 6.2831853)
                     (let anf_158 (sin anf_157)
                      (let anf_159 (* anf_158 200000.)
                       (let hash_7 (fract anf_159)
                        (let anf_160 (index hash_7 0)
                         (let anf_161 (index hash_7 1)
                          (let col0_8 (vec2 anf_160 anf_161)
                           (let anf_162 (index hash_7 2)
                            (let anf_163 (index hash_7 3)
                             (let col1_9 (vec2 anf_162 anf_163)
                              (let anf_164 (index inter_4 1)
                               (let anf_165 (- 1. anf_164)
                                (let anf_166 (* col0_8 anf_165)
                                 (let anf_167 (index inter_4 1)
                                  (let anf_168 (* col1_9 anf_167)
                                   (let res_v_10 (+ anf_166 anf_168)
                                    (let anf_169 (index inter_4 0)
                                     (let anf_170 (- 1. anf_169)
                                      (let anf_171 (index inter_4 0)
                                       (let anf_172 (vec2 anf_170 anf_171)
                                        (return (dot res_v_10 anf_172))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name fractalNoise_11) (args ((p_12 (vec 2))))
       (body
        (let anf_173 (smoothNoise_0 p_12)
         (let anf_174 (* anf_173 0.5333)
          (let anf_175 (* p_12 2.)
           (let anf_176 (smoothNoise_0 anf_175)
            (let anf_177 (* anf_176 0.2667)
             (let anf_178 (+ anf_174 anf_177)
              (let anf_179 (* p_12 4.)
               (let anf_180 (smoothNoise_0 anf_179)
                (let anf_181 (* anf_180 0.1333)
                 (let anf_182 (+ anf_178 anf_181)
                  (let anf_183 (* p_12 8.)
                   (let anf_184 (smoothNoise_0 anf_183)
                    (let anf_185 (* anf_184 0.0667) (return (+ anf_182 anf_185)))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name warpedNoise_13) (args ((p_14 (vec 2))))
       (body
        (let anf_186 (- 0. u_time)
         (let anf_187 (vec2 u_time anf_186)
          (let m_15 (* anf_187 0.5)
           (let anf_188 (+ p_14 m_15)
            (let x_16 (fractalNoise_11 anf_188)
             (let anf_189 (index m_15 1)
              (let anf_190 (index m_15 0)
               (let anf_191 (vec2 anf_189 anf_190)
                (let anf_192 (+ p_14 anf_191)
                 (let anf_193 (+ anf_192 x_16)
                  (let y_17 (fractalNoise_11 anf_193)
                   (let anf_194 (- p_14 m_15)
                    (let anf_195 (- anf_194 x_16)
                     (let anf_196 (+ anf_195 y_17)
                      (let z_18 (fractalNoise_11 anf_196)
                       (let anf_197 (vec2 x_16 y_17)
                        (let anf_198 (vec2 y_17 z_18)
                         (let anf_199 (+ anf_197 anf_198)
                          (let anf_200 (vec2 z_18 x_16)
                           (let warp_19 (+ anf_199 anf_200)
                            (let anf_201 (vec3 x_16 y_17 z_18)
                             (let anf_202 (length anf_201)
                              (let mag_20 (* anf_202 0.25)
                               (let anf_203 (+ p_14 warp_19)
                                (let anf_204 (+ anf_203 mag_20)
                                 (return (fractalNoise_11 anf_204)))))))))))))))))))))))))))))
      : ((vec 2) -> float))
     ((Define (name main) (args ((coord_21 (vec 2))))
       (body
        (let anf_205 (* u_resolution 0.5)
         (let anf_206 (- coord_21 anf_205)
          (let anf_207 (index u_resolution 1)
           (let uv_22 (/ anf_206 anf_207)
            (let anf_208 (* uv_22 6.)
             (let n_23 (warpedNoise_13 anf_208)
              (let anf_209 (* uv_22 6.)
               (let anf_210 (- anf_209 0.02)
                (let n2_24 (warpedNoise_13 anf_210)
                 (let anf_211 (- n2_24 n_23)
                  (let anf_212 (max anf_211 0.)
                   (let anf_213 (/ anf_212 0.02)
                    (let bump_25 (* anf_213 0.7071)
                     (let anf_214 (- n_23 n2_24)
                      (let anf_215 (max anf_214 0.)
                       (let anf_216 (/ anf_215 0.02)
                        (let bump2_26 (* anf_216 0.7071)
                         (let anf_217 (* bump_25 bump_25)
                          (let anf_218 (pow bump_25 4.)
                           (let anf_219 (* anf_218 0.5)
                            (let b1_27 (+ anf_217 anf_219)
                             (let anf_220 (* bump2_26 bump2_26)
                              (let anf_221 (pow bump2_26 4.)
                               (let anf_222 (* anf_221 0.5)
                                (let b2_28 (+ anf_220 anf_222)
                                 (let anf_223 (vec3 1. 0.7 0.6)
                                  (let anf_224 (+ b1_27 b2_28)
                                   (let anf_225 (* anf_224 0.4)
                                    (let anf_226 (vec3 b1_27 anf_225 b2_28)
                                     (let anf_227 (* anf_223 anf_226)
                                      (let anf_228 (* anf_227 0.3)
                                       (let base_col_29 (+ anf_228 0.5)
                                        (let anf_229 (* n_23 n_23)
                                         (let col_30 (* anf_229 base_col_29)
                                          (let anf_230 (- uv_22 0.65)
                                           (let spot1_dist_31 (length anf_230)
                                            (let anf_231 (+ uv_22 0.5)
                                             (let spot2_dist_32 (length anf_231)
                                              (let anf_232 (vec3 0.8 0.4 1.)
                                               (let anf_233 (* anf_232 0.35)
                                                (let anf_234 (vec3 1. 0.5 0.2)
                                                 (let anf_235
                                                  (- 1. spot1_dist_31)
                                                  (let anf_236
                                                   (smoothstep 0. 1. anf_235)
                                                   (let anf_237
                                                    (* anf_234 anf_236)
                                                    (let anf_238
                                                     (vec3 0.2 0.4 1.)
                                                     (let anf_239
                                                      (- 1. spot2_dist_32)
                                                      (let anf_240
                                                       (smoothstep 0. 1. anf_239)
                                                       (let anf_241
                                                        (* anf_238 anf_240)
                                                        (let anf_242
                                                         (+ anf_237 anf_241)
                                                         (let anf_243
                                                          (* anf_242 5.)
                                                          (let spot_logic_33
                                                           (+ anf_233 anf_243)
                                                           (let final_col_34
                                                            (* col_30
                                                             spot_logic_33)
                                                            (let anf_244
                                                             (max final_col_34
                                                              0.)
                                                             (return
                                                              (sqrt anf_244)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      : ((vec 2) -> (vec 3))))

    === translate (warped_noise.glml) ===
    (Program
     ((Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name smoothNoise_0) (desc ()) (params (((TyVec 2) p_1)))
       (ret_type TyFloat)
       (body
        ((set () vec2 i_2 (floor p_1)) (set () vec2 pf_3 (- p_1 i_2))
         (set () vec2 anf_150 (* pf_3 pf_3)) (set () vec2 anf_151 (* 2. pf_3))
         (set () vec2 anf_152 (- 3. anf_151))
         (set () vec2 inter_4 (* anf_150 anf_152))
         (set () vec4 v4_5 (vec4 0. 1. 27. 28.))
         (set () float anf_153 (index i_2 0))
         (set () vec4 anf_154 (+ v4_5 anf_153))
         (set () float anf_155 (index i_2 1))
         (set () float anf_156 (* anf_155 27.))
         (set () vec4 seed_6 (+ anf_154 anf_156))
         (set () vec4 anf_157 (% seed_6 6.2831853))
         (set () vec4 anf_158 (sin anf_157))
         (set () vec4 anf_159 (* anf_158 200000.))
         (set () vec4 hash_7 (fract anf_159))
         (set () float anf_160 (index hash_7 0))
         (set () float anf_161 (index hash_7 1))
         (set () vec2 col0_8 (vec2 anf_160 anf_161))
         (set () float anf_162 (index hash_7 2))
         (set () float anf_163 (index hash_7 3))
         (set () vec2 col1_9 (vec2 anf_162 anf_163))
         (set () float anf_164 (index inter_4 1))
         (set () float anf_165 (- 1. anf_164))
         (set () vec2 anf_166 (* col0_8 anf_165))
         (set () float anf_167 (index inter_4 1))
         (set () vec2 anf_168 (* col1_9 anf_167))
         (set () vec2 res_v_10 (+ anf_166 anf_168))
         (set () float anf_169 (index inter_4 0))
         (set () float anf_170 (- 1. anf_169))
         (set () float anf_171 (index inter_4 0))
         (set () vec2 anf_172 (vec2 anf_170 anf_171))
         (return (dot res_v_10 anf_172)))))
      (Function (name fractalNoise_11) (desc ()) (params (((TyVec 2) p_12)))
       (ret_type TyFloat)
       (body
        ((set () float anf_173 (smoothNoise_0 p_12))
         (set () float anf_174 (* anf_173 0.5333))
         (set () vec2 anf_175 (* p_12 2.))
         (set () float anf_176 (smoothNoise_0 anf_175))
         (set () float anf_177 (* anf_176 0.2667))
         (set () float anf_178 (+ anf_174 anf_177))
         (set () vec2 anf_179 (* p_12 4.))
         (set () float anf_180 (smoothNoise_0 anf_179))
         (set () float anf_181 (* anf_180 0.1333))
         (set () float anf_182 (+ anf_178 anf_181))
         (set () vec2 anf_183 (* p_12 8.))
         (set () float anf_184 (smoothNoise_0 anf_183))
         (set () float anf_185 (* anf_184 0.0667)) (return (+ anf_182 anf_185)))))
      (Function (name warpedNoise_13) (desc ()) (params (((TyVec 2) p_14)))
       (ret_type TyFloat)
       (body
        ((set () float anf_186 (- 0. u_time))
         (set () vec2 anf_187 (vec2 u_time anf_186))
         (set () vec2 m_15 (* anf_187 0.5)) (set () vec2 anf_188 (+ p_14 m_15))
         (set () float x_16 (fractalNoise_11 anf_188))
         (set () float anf_189 (index m_15 1))
         (set () float anf_190 (index m_15 0))
         (set () vec2 anf_191 (vec2 anf_189 anf_190))
         (set () vec2 anf_192 (+ p_14 anf_191))
         (set () vec2 anf_193 (+ anf_192 x_16))
         (set () float y_17 (fractalNoise_11 anf_193))
         (set () vec2 anf_194 (- p_14 m_15))
         (set () vec2 anf_195 (- anf_194 x_16))
         (set () vec2 anf_196 (+ anf_195 y_17))
         (set () float z_18 (fractalNoise_11 anf_196))
         (set () vec2 anf_197 (vec2 x_16 y_17))
         (set () vec2 anf_198 (vec2 y_17 z_18))
         (set () vec2 anf_199 (+ anf_197 anf_198))
         (set () vec2 anf_200 (vec2 z_18 x_16))
         (set () vec2 warp_19 (+ anf_199 anf_200))
         (set () vec3 anf_201 (vec3 x_16 y_17 z_18))
         (set () float anf_202 (length anf_201))
         (set () float mag_20 (* anf_202 0.25))
         (set () vec2 anf_203 (+ p_14 warp_19))
         (set () vec2 anf_204 (+ anf_203 mag_20))
         (return (fractalNoise_11 anf_204)))))
      (Function (name main) (desc ()) (params (((TyVec 2) coord_21)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 anf_205 (* u_resolution 0.5))
         (set () vec2 anf_206 (- coord_21 anf_205))
         (set () float anf_207 (index u_resolution 1))
         (set () vec2 uv_22 (/ anf_206 anf_207))
         (set () vec2 anf_208 (* uv_22 6.))
         (set () float n_23 (warpedNoise_13 anf_208))
         (set () vec2 anf_209 (* uv_22 6.))
         (set () vec2 anf_210 (- anf_209 0.02))
         (set () float n2_24 (warpedNoise_13 anf_210))
         (set () float anf_211 (- n2_24 n_23))
         (set () float anf_212 (max anf_211 0.))
         (set () float anf_213 (/ anf_212 0.02))
         (set () float bump_25 (* anf_213 0.7071))
         (set () float anf_214 (- n_23 n2_24))
         (set () float anf_215 (max anf_214 0.))
         (set () float anf_216 (/ anf_215 0.02))
         (set () float bump2_26 (* anf_216 0.7071))
         (set () float anf_217 (* bump_25 bump_25))
         (set () float anf_218 (pow bump_25 4.))
         (set () float anf_219 (* anf_218 0.5))
         (set () float b1_27 (+ anf_217 anf_219))
         (set () float anf_220 (* bump2_26 bump2_26))
         (set () float anf_221 (pow bump2_26 4.))
         (set () float anf_222 (* anf_221 0.5))
         (set () float b2_28 (+ anf_220 anf_222))
         (set () vec3 anf_223 (vec3 1. 0.7 0.6))
         (set () float anf_224 (+ b1_27 b2_28))
         (set () float anf_225 (* anf_224 0.4))
         (set () vec3 anf_226 (vec3 b1_27 anf_225 b2_28))
         (set () vec3 anf_227 (* anf_223 anf_226))
         (set () vec3 anf_228 (* anf_227 0.3))
         (set () vec3 base_col_29 (+ anf_228 0.5))
         (set () float anf_229 (* n_23 n_23))
         (set () vec3 col_30 (* anf_229 base_col_29))
         (set () vec2 anf_230 (- uv_22 0.65))
         (set () float spot1_dist_31 (length anf_230))
         (set () vec2 anf_231 (+ uv_22 0.5))
         (set () float spot2_dist_32 (length anf_231))
         (set () vec3 anf_232 (vec3 0.8 0.4 1.))
         (set () vec3 anf_233 (* anf_232 0.35))
         (set () vec3 anf_234 (vec3 1. 0.5 0.2))
         (set () float anf_235 (- 1. spot1_dist_31))
         (set () float anf_236 (smoothstep 0. 1. anf_235))
         (set () vec3 anf_237 (* anf_234 anf_236))
         (set () vec3 anf_238 (vec3 0.2 0.4 1.))
         (set () float anf_239 (- 1. spot2_dist_32))
         (set () float anf_240 (smoothstep 0. 1. anf_239))
         (set () vec3 anf_241 (* anf_238 anf_240))
         (set () vec3 anf_242 (+ anf_237 anf_241))
         (set () vec3 anf_243 (* anf_242 5.))
         (set () vec3 spot_logic_33 (+ anf_233 anf_243))
         (set () vec3 final_col_34 (* col_30 spot_logic_33))
         (set () vec3 anf_244 (max final_col_34 0.)) (return (sqrt anf_244)))))))

    === patch main (warped_noise.glml) ===
    (Program
     ((Global Out (TyVec 4) fragColor ())
      (Global Uniform (TyVec 2) u_resolution ())
      (Global Uniform TyFloat u_time ())
      (Function (name smoothNoise_0) (desc ()) (params (((TyVec 2) p_1)))
       (ret_type TyFloat)
       (body
        ((set () vec2 i_2 (floor p_1)) (set () vec2 pf_3 (- p_1 i_2))
         (set () vec2 anf_150 (* pf_3 pf_3)) (set () vec2 anf_151 (* 2. pf_3))
         (set () vec2 anf_152 (- 3. anf_151))
         (set () vec2 inter_4 (* anf_150 anf_152))
         (set () vec4 v4_5 (vec4 0. 1. 27. 28.))
         (set () float anf_153 (index i_2 0))
         (set () vec4 anf_154 (+ v4_5 anf_153))
         (set () float anf_155 (index i_2 1))
         (set () float anf_156 (* anf_155 27.))
         (set () vec4 seed_6 (+ anf_154 anf_156))
         (set () vec4 anf_157 (% seed_6 6.2831853))
         (set () vec4 anf_158 (sin anf_157))
         (set () vec4 anf_159 (* anf_158 200000.))
         (set () vec4 hash_7 (fract anf_159))
         (set () float anf_160 (index hash_7 0))
         (set () float anf_161 (index hash_7 1))
         (set () vec2 col0_8 (vec2 anf_160 anf_161))
         (set () float anf_162 (index hash_7 2))
         (set () float anf_163 (index hash_7 3))
         (set () vec2 col1_9 (vec2 anf_162 anf_163))
         (set () float anf_164 (index inter_4 1))
         (set () float anf_165 (- 1. anf_164))
         (set () vec2 anf_166 (* col0_8 anf_165))
         (set () float anf_167 (index inter_4 1))
         (set () vec2 anf_168 (* col1_9 anf_167))
         (set () vec2 res_v_10 (+ anf_166 anf_168))
         (set () float anf_169 (index inter_4 0))
         (set () float anf_170 (- 1. anf_169))
         (set () float anf_171 (index inter_4 0))
         (set () vec2 anf_172 (vec2 anf_170 anf_171))
         (return (dot res_v_10 anf_172)))))
      (Function (name fractalNoise_11) (desc ()) (params (((TyVec 2) p_12)))
       (ret_type TyFloat)
       (body
        ((set () float anf_173 (smoothNoise_0 p_12))
         (set () float anf_174 (* anf_173 0.5333))
         (set () vec2 anf_175 (* p_12 2.))
         (set () float anf_176 (smoothNoise_0 anf_175))
         (set () float anf_177 (* anf_176 0.2667))
         (set () float anf_178 (+ anf_174 anf_177))
         (set () vec2 anf_179 (* p_12 4.))
         (set () float anf_180 (smoothNoise_0 anf_179))
         (set () float anf_181 (* anf_180 0.1333))
         (set () float anf_182 (+ anf_178 anf_181))
         (set () vec2 anf_183 (* p_12 8.))
         (set () float anf_184 (smoothNoise_0 anf_183))
         (set () float anf_185 (* anf_184 0.0667)) (return (+ anf_182 anf_185)))))
      (Function (name warpedNoise_13) (desc ()) (params (((TyVec 2) p_14)))
       (ret_type TyFloat)
       (body
        ((set () float anf_186 (- 0. u_time))
         (set () vec2 anf_187 (vec2 u_time anf_186))
         (set () vec2 m_15 (* anf_187 0.5)) (set () vec2 anf_188 (+ p_14 m_15))
         (set () float x_16 (fractalNoise_11 anf_188))
         (set () float anf_189 (index m_15 1))
         (set () float anf_190 (index m_15 0))
         (set () vec2 anf_191 (vec2 anf_189 anf_190))
         (set () vec2 anf_192 (+ p_14 anf_191))
         (set () vec2 anf_193 (+ anf_192 x_16))
         (set () float y_17 (fractalNoise_11 anf_193))
         (set () vec2 anf_194 (- p_14 m_15))
         (set () vec2 anf_195 (- anf_194 x_16))
         (set () vec2 anf_196 (+ anf_195 y_17))
         (set () float z_18 (fractalNoise_11 anf_196))
         (set () vec2 anf_197 (vec2 x_16 y_17))
         (set () vec2 anf_198 (vec2 y_17 z_18))
         (set () vec2 anf_199 (+ anf_197 anf_198))
         (set () vec2 anf_200 (vec2 z_18 x_16))
         (set () vec2 warp_19 (+ anf_199 anf_200))
         (set () vec3 anf_201 (vec3 x_16 y_17 z_18))
         (set () float anf_202 (length anf_201))
         (set () float mag_20 (* anf_202 0.25))
         (set () vec2 anf_203 (+ p_14 warp_19))
         (set () vec2 anf_204 (+ anf_203 mag_20))
         (return (fractalNoise_11 anf_204)))))
      (Function (name main_pure) (desc ()) (params (((TyVec 2) coord_21)))
       (ret_type (TyVec 3))
       (body
        ((set () vec2 anf_205 (* u_resolution 0.5))
         (set () vec2 anf_206 (- coord_21 anf_205))
         (set () float anf_207 (index u_resolution 1))
         (set () vec2 uv_22 (/ anf_206 anf_207))
         (set () vec2 anf_208 (* uv_22 6.))
         (set () float n_23 (warpedNoise_13 anf_208))
         (set () vec2 anf_209 (* uv_22 6.))
         (set () vec2 anf_210 (- anf_209 0.02))
         (set () float n2_24 (warpedNoise_13 anf_210))
         (set () float anf_211 (- n2_24 n_23))
         (set () float anf_212 (max anf_211 0.))
         (set () float anf_213 (/ anf_212 0.02))
         (set () float bump_25 (* anf_213 0.7071))
         (set () float anf_214 (- n_23 n2_24))
         (set () float anf_215 (max anf_214 0.))
         (set () float anf_216 (/ anf_215 0.02))
         (set () float bump2_26 (* anf_216 0.7071))
         (set () float anf_217 (* bump_25 bump_25))
         (set () float anf_218 (pow bump_25 4.))
         (set () float anf_219 (* anf_218 0.5))
         (set () float b1_27 (+ anf_217 anf_219))
         (set () float anf_220 (* bump2_26 bump2_26))
         (set () float anf_221 (pow bump2_26 4.))
         (set () float anf_222 (* anf_221 0.5))
         (set () float b2_28 (+ anf_220 anf_222))
         (set () vec3 anf_223 (vec3 1. 0.7 0.6))
         (set () float anf_224 (+ b1_27 b2_28))
         (set () float anf_225 (* anf_224 0.4))
         (set () vec3 anf_226 (vec3 b1_27 anf_225 b2_28))
         (set () vec3 anf_227 (* anf_223 anf_226))
         (set () vec3 anf_228 (* anf_227 0.3))
         (set () vec3 base_col_29 (+ anf_228 0.5))
         (set () float anf_229 (* n_23 n_23))
         (set () vec3 col_30 (* anf_229 base_col_29))
         (set () vec2 anf_230 (- uv_22 0.65))
         (set () float spot1_dist_31 (length anf_230))
         (set () vec2 anf_231 (+ uv_22 0.5))
         (set () float spot2_dist_32 (length anf_231))
         (set () vec3 anf_232 (vec3 0.8 0.4 1.))
         (set () vec3 anf_233 (* anf_232 0.35))
         (set () vec3 anf_234 (vec3 1. 0.5 0.2))
         (set () float anf_235 (- 1. spot1_dist_31))
         (set () float anf_236 (smoothstep 0. 1. anf_235))
         (set () vec3 anf_237 (* anf_234 anf_236))
         (set () vec3 anf_238 (vec3 0.2 0.4 1.))
         (set () float anf_239 (- 1. spot2_dist_32))
         (set () float anf_240 (smoothstep 0. 1. anf_239))
         (set () vec3 anf_241 (* anf_238 anf_240))
         (set () vec3 anf_242 (+ anf_237 anf_241))
         (set () vec3 anf_243 (* anf_242 5.))
         (set () vec3 spot_logic_33 (+ anf_233 anf_243))
         (set () vec3 final_col_34 (* col_30 spot_logic_33))
         (set () vec3 anf_244 (max final_col_34 0.)) (return (sqrt anf_244)))))
      (Function (name main) (desc ()) (params ()) (ret_type TyVoid)
       (body
        ((set () vec3 color (main_pure (. gl_FragCoord xy)))
         (set fragColor (clamp (vec4 (. color xyz) 1.) 0. 1.)))))))
    |}]
;;
