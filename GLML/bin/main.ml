open Js_of_ocaml

let () =
  Js.export "glml"
    (object%js
       val description = "GLML Language Compiler"
       val shader = [%blob "./shader.frag"]
    end)
