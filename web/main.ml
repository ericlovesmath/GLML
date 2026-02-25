open Core
open Bonsai_web
open Bonsai.Let_syntax
open Codemirror
open Js_of_ocaml
open Virtual_dom
module Form = Bonsai_web_ui_form.With_automatic_view
module Codemirror = Bonsai_web_ui_codemirror

external init_canvas : unit -> unit = "init"
external compile_and_link : string -> Js.js_string Js.t Js.opt = "compileAndLinkGLSL"

module Ocaml_syntax_highlighting = struct
  let doc =
    {|open! Core

(* Syntax highlight for ocaml *)

let x = List.map [ 1; 2; 3; 4; 5 ] ~f:(fun x -> x + 1)

let y =
  let z = 3 in
  let a = 4 in
  z + a
;;
|}
  ;;

  let codemirror_editor ~name =
    let create_extensions state =
      [ Basic_setup.basic_setup
      ; Codemirror_ocaml.ocaml_stream_parser
        |> Stream_parser.Stream_language.define
        |> Stream_parser.Stream_language.to_language
        |> Language.extension
      ; Codemirror_themes.get state
      ]
    in
    let create_state extensions =
      State.Editor_state.create (State.Editor_state_config.create ~doc ~extensions ())
    in
    Codemirror.with_dynamic_extensions
      ~name
      ~sexp_of:[%sexp_of: Codemirror_themes.t]
      ~equal:[%equal: Codemirror_themes.t]
      ~initial_state:(create_state (create_extensions Codemirror_themes.Material_dark))
      ~compute_extensions:(Bonsai.return create_extensions)
      (return Codemirror_themes.Material_dark)
  ;;
end

module Scheme_syntax_highlighting = struct
  let doc = Shader.example_glml

  let codemirror_editor ~name =
    Codemirror.of_initial_state
      ~name
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:
              [ Basic_setup.basic_setup
              ; Scheme.scheme
                |> Stream_parser.Stream_language.define
                |> Stream_parser.Stream_language.to_language
                |> Language.extension
              ; Codemirror_themes.get Codemirror_themes.Material_dark
              ]
            ()))
  ;;
end

module Which_editor = struct
  type t =
    | Scheme
    | Ocaml
  [@@deriving enumerate, sexp, equal, compare]

  let to_string = function
    | Scheme -> "Scheme syntax highlighting"
    | Ocaml -> "OCaml syntax highlighting"
  ;;
end

let component graph =
  let language_picker =
    Form.Elements.Dropdown.enumerable
      ~to_string:Which_editor.to_string
      (module Which_editor)
      graph
  in
  let chosen_language =
    let%arr language_picker = language_picker in
    Form.value language_picker |> Or_error.ok_exn
  in
  let%sub codemirror =
    match%sub chosen_language with
    | Ocaml -> Ocaml_syntax_highlighting.codemirror_editor ~name:"ocaml" graph
    | Scheme -> Scheme_syntax_highlighting.codemirror_editor ~name:"scheme" graph
  in
  let error, set_error =
    Bonsai.state_opt graph ~equal:String.equal ~sexp_of_model:String.sexp_of_t
  in
  let compile_effect =
    let%arr codemirror = codemirror
    and set_error = set_error in
    Ui_effect.bind (Ui_effect.return ()) ~f:(fun () ->
      match
        Or_error.try_with_join (fun () ->
          Glml_compiler.compile_stlc (Codemirror.text codemirror))
      with
      | Error err -> set_error (Some (Error.to_string_hum err))
      | Ok glsl ->
        glsl
        |> compile_and_link
        |> Js.Opt.to_option
        |> Option.map ~f:Js.to_string
        |> set_error)
  in
  let () =
    let on_activate =
      let%arr compile_effect = compile_effect in
      Ui_effect.bind (Ui_effect.of_thunk init_canvas) ~f:(fun () -> compile_effect)
    in
    Bonsai.Edge.lifecycle ~on_activate graph
  in
  let%arr codemirror = codemirror
  and compile_effect = compile_effect
  and error = error in
  let open Vdom.Node in
  let open Vdom.Attr in
  div
    ~attrs:[ class_ "main-container" ]
    [ div
        ~attrs:[ class_ "sidebar" ]
        [ div ~attrs:[ id "canvas-container" ] [ canvas ~attrs:[ id "gl-canvas" ] [] ]
        ; div
            ~attrs:[ class_ "controls" ]
            [ button ~attrs:[ on_click (fun _ -> compile_effect) ] [ text "Compile" ]
            ; (match error with
               | None -> none
               | Some err -> div ~attrs:[ class_ "error-message" ] [ text err ])
            ]
        ]
    ; div ~attrs:[ class_ "editor-container" ] [ Codemirror.view codemirror ]
    ]
;;

let () = Bonsai_web.Start.start component
