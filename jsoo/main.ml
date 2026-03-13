open Js_of_ocaml
open Core

let js_compile (src : Js.js_string Js.t) : _ Js.t =
  let result =
    Or_error.try_with_join (fun () -> Glml_compiler.compile (Js.to_string src))
  in
  match result with
  | Ok glsl ->
    object%js
      val glsl = Js.some (Js.string glsl)
      val error = Js.null
    end
  | Error err ->
    object%js
      val glsl = Js.null
      val error = Js.some (Js.string (Error.to_string_hum err))
    end
;;

let () =
  Js.export
    "glml"
    (object%js
       val compile = Js.wrap_callback js_compile
    end)
;;
