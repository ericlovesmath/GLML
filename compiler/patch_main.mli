open Core

(** Takes pure [main : (coord : vec2) -> (color : vec3)] function
    and replaces it with an impure [main : void -> void] function that
    uses the real global variables representing location and color.

    [coord] is taken from [gl_FragCoord.xy] while the returned color is
    [clamp]'d to a [0, 1] range and fixed into global [vec4] [fragColor]. *)
val patch : Glsl.t -> Glsl.t Or_error.t
