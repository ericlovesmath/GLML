module Glsl = Glsl

let compile_source src = Glsl.to_shader (Glsl.of_string src)
