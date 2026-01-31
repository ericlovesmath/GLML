let () =
  Js_of_ocaml.Js.export
    "glml"
    (object%js
       val description = "GLML Language Compiler"
       val shader = Glml_compiler.compile_source Shader.example_source
    end)
;;
