let example_source =
  {|
  ((global uniform vec2 u_resolution)
   (global uniform vec2 u_mouse)
   (global uniform float u_time)

   (global out vec4 fragColor)

   ; Exponential Smooth Minimum
   (fun sMin ((float a) (float b) (float k)) float
     (assign k (* k 1.0))
     (assign float r (+ (exp2 (/ (* -1.0 a) k))
                        (exp2 (/ (* -1.0 b) k))))
     (return (* (* -1.0 k) (log2 r))))

   ; SDF for an circle
   (fun sdCircle ((vec2 p) (float r)) float
     (return (- (length p) r)))

   ; SDF for an equilateral triangle
   (fun sdTriangle ((vec2 p) (float r)) float
     (assign const float k (sqrt 3.0))
     (assign (. p x) (- (abs (. p x)) r))
     (assign (. p y) (+ (. p y) (/ r k)))
     (if (> (+ (. p x) (* k (. p y))) 0.0)
         (assign p (/ (vec2 (- (. p x) (* k (. p y)))
                            (- (* (* -1.0 k) (. p x)) (. p y)))
                      2.0)))
     (assign (. p x) (- (. p x) (clamp (. p x) (* -2.0 r) 0.0)))
     (return (* (* -1.0 (length p)) (sign (. p y)))))

   (fun sdBoxFrame ((vec3 p) (vec3 b) (float e)) float
     (assign p (- (abs p) b))
     (assign vec3 q (- (abs (+ p e)) e))
     (return (min (min (+ (length (max (vec3 (. p x) (. q y) (. q z)) 0.0))
                          (min (max (. p x) (max (. q y) (. q z))) 0.0))
                       (+ (length (max (vec3 (. q x) (. p y) (. q z)) 0.0))
                          (min (max (. q x) (max (. p y) (. q z))) 0.0)))
                  (+ (length (max (vec3 (. q x) (. q y) (. p z)) 0.0))
                     (min (max (. q x) (max (. q y) (. p z))) 0.0)))))

   (fun sdSphere ((vec3 p)) float
     (return (- (length p) 1.0)))

   (fun sdTorus ((vec3 p) (vec2 t)) float
     (assign vec2 q (vec2 (- (length (. p xz)) (. t x)) (. p y)))
     (return (- (length q) (. t y))))

   (fun rot2D ((float angle)) mat2
     (assign float s (sin angle))
     (assign float c (cos angle))
     (return (mat2 c (* -1.0 s) s c)))

   (fun palette ((float t)) vec3
     (return (+ 0.5 (* 0.5 (cos (* 6.28318 (+ t (vec3 0.3 0.416 0.557))))))))

   (fun main () void
     ; Normalize coordinates (-1 to 1) and fix aspect ratio
     (assign vec2 uv (/ (- (* 2.0 (. gl_FragCoord xy)) (. u_resolution xy))
                        (min (. u_resolution y) (. u_resolution x))))
     (assign vec2 mouseUV (- uv (/ (- (* 2.0 (. u_mouse xy)) (. u_resolution xy))
                                   (min (. u_resolution y) (. u_resolution x)))))

     ; Raymarching Logic with ray origin and ray direction
     (assign vec3 ro (vec3 0.0 0.0 -10.0))
     (assign vec3 rd (normalize (vec3 uv 1.0)))
     (assign vec3 col (vec3 0.0))

     (assign (. ro yz) (* (. ro yz) (rot2D (* -1.0 (. mouseUV y)))))
     (assign (. rd yz) (* (. rd yz) (rot2D (* -1.0 (. mouseUV y)))))
     (assign (. ro xz) (* (. ro xz) (rot2D (* -1.0 (. mouseUV x)))))
     (assign (. rd xz) (* (. rd xz) (rot2D (* -1.0 (. mouseUV x)))))

     ; total distance travelled
     (assign float t 0.0)
     (for (assign int i 0) (< i 80) (assign i (+ i 1))
          (block
           (assign vec3 p (+ ro (* rd t)))

           ; Rotating Camera
           (assign (. p xy) (* (. p xy) (rot2D (/ u_time 1000.0))))
           (assign (. p yz) (* (. p yz) (rot2D (/ u_time 1000.0))))

           (assign float d (sMin (sdTorus p (vec2 1.0 0.5))
                                 (sdBoxFrame p (vec3 2.0 2.0 3.0) 0.5)
                                 0.1))
           (assign t (+ t d))
           (if (|| (< d 0.001) (> t 100.0)) (break))))

     (assign col (palette (* t 0.3)))
     (if (> t 100.0) (assign col (vec3 0.2 0.2 0.2)))

     ; Add glow around mouse
     (assign float mouseDist (length mouseUV))
     (assign col (+ col (* 0.2 (/ 1.0 (* mouseDist 10.0)))))

     (assign fragColor (vec4 col 1.0))))
  |}
;;
