(** Rewrites the pure [main : vec2 -> vec4] function inplace to
    impure [main : void -> void] function that uses the real global variables
    representing location and color *)
val patch : Glsl.t -> Glsl.t Compiler_error.t
