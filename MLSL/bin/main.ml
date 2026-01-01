open Js_of_ocaml

let () =
  Js.export "mlsl"
    (object%js
       val description = "MLSL Language Compiler"
       val shader = [%blob "./shader.frag"]
    end)
