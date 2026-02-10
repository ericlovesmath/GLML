module Glsl = Glsl
module Stlc = Stlc
module Uniquify = Uniquify
module Typecheck = Typecheck
module Translate = Translate

(** Run full compiler pipeline, transpiling GLML to GLSL strings *)
val compile_source : string -> string
