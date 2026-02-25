let circle_at_mouse =
  {|((extern vec2 u_resolution)
(extern vec2 u_mouse)
(extern float u_time)

(let get_uv (coord : vec2) =
  (let top = (- (* 2.0 coord) u_resolution) in
  (let bot = (min (. u_resolution 0) (. u_resolution 1)) in
  (/ top bot))))

(let main (coord : vec2) =
  (let uv = (get_uv coord) in
  (let mouseUV = (/ (- (* 2.0 u_mouse) u_resolution) (. u_resolution 1)) in
  (let radius = (+ 0.2 (* 0.1 (sin (/ u_time 500.)))) in
  (if (< (distance uv mouseUV) radius)
    (vec3 0. 0. 0.5)
    (vec3 0.5 0.5 1.)))))))|}
;;

let pastel_rainbow =
  {|((extern vec2 u_resolution)
(extern vec2 u_mouse)
(extern float u_time)

(let get_uv (coord : vec2) =
  (let top = (- (* 2.0 coord) u_resolution) in
  (let bot = (min (. u_resolution 0) (. u_resolution 1)) in
  (/ top bot))))

(let main (coord : vec2) =
  (let uv = (get_uv coord) in
  (let wave = (+ (* 5.0 (+ (. uv 0) (. uv 1))) (/ u_time 1000.)) in
   
  (let r = (+ 0.7 (* 0.3 (sin wave))) in
  (let g = (+ 0.7 (* 0.3 (sin (+ wave 2.0)))) in
  (let b = (+ 0.7 (* 0.3 (sin (+ wave 4.0)))) in
   
  (vec3 r g b))))))))|}
;;
