let () =
  Js_of_ocaml.Js.export
    "glml"
    (object%js
       val description = "GLML Language Compiler"
       val compile = Glml_compiler.compile
    end)
;;
