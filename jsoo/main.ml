let () =
  Js_of_ocaml.Js.export
    "glml"
    (object%js
       val description = "GLML Language Compiler"
       val shader = Glml_compiler.Glsl.compile_source Shader.example_source
    end)
;;
