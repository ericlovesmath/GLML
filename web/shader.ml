let example_source =
  {|
  ((global uniform vec2 u_resolution)
   (global uniform vec2 u_mouse)
   (global uniform float u_time)

   (global out vec4 fragColor)

   ; Exponential Smooth Minimum
   (fun sMin ((float a) (float b) (float k)) float
     (set k (* k 1.0))
     (set float r (+ (exp2 (/ (* -1.0 a) k))
                        (exp2 (/ (* -1.0 b) k))))
     (return (* (* -1.0 k) (log2 r))))

   ; SDF for an circle
   (fun sdCircle ((vec2 p) (float r)) float
     (return (- (length p) r)))

   ; SDF for an equilateral triangle
   (fun sdTriangle ((vec2 p) (float r)) float
     (set const float k (sqrt 3.0))
     (set (. p x) (- (abs (. p x)) r))
     (set (. p y) (+ (. p y) (/ r k)))
     (if (> (+ (. p x) (* k (. p y))) 0.0)
         (set p (/ (vec2 (- (. p x) (* k (. p y)))
                            (- (* (* -1.0 k) (. p x)) (. p y)))
                      2.0)))
     (set (. p x) (- (. p x) (clamp (. p x) (* -2.0 r) 0.0)))
     (return (* (* -1.0 (length p)) (sign (. p y)))))

   (fun sdBoxFrame ((vec3 p) (vec3 b) (float e)) float
     (set p (- (abs p) b))
     (set vec3 q (- (abs (+ p e)) e))
     (return (min (min (+ (length (max (vec3 (. p x) (. q y) (. q z)) 0.0))
                          (min (max (. p x) (max (. q y) (. q z))) 0.0))
                       (+ (length (max (vec3 (. q x) (. p y) (. q z)) 0.0))
                          (min (max (. q x) (max (. p y) (. q z))) 0.0)))
                  (+ (length (max (vec3 (. q x) (. q y) (. p z)) 0.0))
                     (min (max (. q x) (max (. q y) (. p z))) 0.0)))))

   (fun sdSphere ((vec3 p)) float
     (return (- (length p) 1.0)))

   (fun sdTorus ((vec3 p) (vec2 t)) float
     (set vec2 q (vec2 (- (length (. p xz)) (. t x)) (. p y)))
     (return (- (length q) (. t y))))

   (fun rot2D ((float angle)) mat2
     (set float s (sin angle))
     (set float c (cos angle))
     (return (mat2 c (* -1.0 s) s c)))

   (fun palette ((float t)) vec3
     (return (+ 0.5 (* 0.5 (cos (* 6.28318 (+ t (vec3 0.3 0.416 0.557))))))))

   (fun main () void
     ; Normalize coordinates (-1 to 1) and fix aspect ratio
     (set vec2 uv (/ (- (* 2.0 (. gl_FragCoord xy)) (. u_resolution xy))
                        (min (. u_resolution y) (. u_resolution x))))
     (set vec2 mouseUV (- uv (/ (- (* 2.0 (. u_mouse xy)) (. u_resolution xy))
                                   (min (. u_resolution y) (. u_resolution x)))))

     ; Raymarching Logic with ray origin and ray direction
     (set vec3 ro (vec3 0.0 0.0 -10.0))
     (set vec3 rd (normalize (vec3 uv 1.0)))
     (set vec3 col (vec3 0.0))

     (set (. ro yz) (* (. ro yz) (rot2D (* -1.0 (. mouseUV y)))))
     (set (. rd yz) (* (. rd yz) (rot2D (* -1.0 (. mouseUV y)))))
     (set (. ro xz) (* (. ro xz) (rot2D (* -1.0 (. mouseUV x)))))
     (set (. rd xz) (* (. rd xz) (rot2D (* -1.0 (. mouseUV x)))))

     ; total distance travelled
     (set float t 0.0)
     (for (set int i 0) (< i 80) (set i (+ i 1))
          (block
           (set vec3 p (+ ro (* rd t)))

           ; Rotating Camera
           (set (. p xy) (* (. p xy) (rot2D (/ u_time 1000.0))))
           (set (. p yz) (* (. p yz) (rot2D (/ u_time 1000.0))))

           (set float d (sMin (sdTorus p (vec2 1.0 0.5))
                                 (sdBoxFrame p (vec3 2.0 2.0 3.0) 0.5)
                                 0.1))
           (set t (+ t d))
           (if (|| (< d 0.001) (> t 100.0)) (break))))

     (set col (palette (* t 0.3)))
     (if (> t 100.0) (set col (vec3 0.2 0.2 0.2)))

     ; Add glow around mouse
     (set float mouseDist (length mouseUV))
     (set col (+ col (* 0.2 (/ 1.0 (* mouseDist 10.0)))))

     (set fragColor (vec4 col 1.0))))
  |}
;;
