open Js_of_ocaml
open Glml_compiler
open Core

let str s = Js.some (Js.string s)

let js_compile (source : Js.js_string Js.t) : _ Js.t =
  try
    let source = Js.to_string source in
    match compile source with
    | Ok glsl ->
      object%js
        val glsl = str glsl
        val error = Js.null
      end
    | Error err ->
      object%js
        val glsl = Js.null
        val error = str (Compiler_error.to_string_hum ~source err)
      end
  with
  | exn ->
    object%js
      val glsl = Js.null
      val error = str (Error.to_string_hum (Error.of_exn exn))
    end
;;

let () =
  Js.export
    "glml"
    (object%js
       val compile = Js.wrap_callback js_compile
    end)
;;
