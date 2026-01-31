open Core
module Glsl = Glsl
module Stlc = Stlc
module Uniquify = Uniquify
module Typecheck = Typecheck
module Anf = Anf
module Translate = Translate

let compile_source src = Glsl.to_shader (Glsl.of_string src)

(* TODO: Move this to compile_source *)
let compile_stlc (t : Stlc.t) =
  let t = Uniquify.uniquify t in
  let anf = Anf.anf t in
  let ctx = Typecheck.typecheck anf in
  let ctx =
    let () = failwith "TODO ctx unwrapping" in
    Or_error.ok_exn ctx
  in
  let glsl = Translate.translate ctx anf in
  Glsl.to_shader glsl
