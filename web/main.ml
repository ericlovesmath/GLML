open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Codemirror
open Virtual_dom
module Form = Bonsai_web_ui_form.With_automatic_view
module Codemirror = Bonsai_web_ui_codemirror

external init_canvas : unit -> unit = "init"
external compile_and_link : string -> unit = "compileAndLinkGLSL"

let () =
  Inline_css.Private.append
    {|
  body {
    margin: 0;
    padding: 0;
    overflow: hidden;
  }

  .main-container {
    display: flex;
    flex-direction: row;
    width: 100vw;
    height: 100vh;
  }

  .sidebar {
    flex: 0 0 600px;
    height: 100vh;
    background-color: #f0f0f0;
    display: flex;
    flex-direction: column;
    border-right: 1px solid #ccc;
    overflow-y: auto;
  }

  .editor-container {
    flex: 1;
    height: 100vh;
    display: flex;
    flex-direction: column;
  }

  /* Make CodeMirror fill the editor-container */
  .cm-editor {
    height: 100% !important;
  }

  #gl-canvas {
    width: 600px;
    height: 600px;
    background-color: black;
    display: block;
  }

  .controls {
    padding: 10px;
  }
  |}
;;

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

  type keybindings =
    | Normal
    | Vim
    | Emacs

  let codemirror_editor ?name ~(keybindings : keybindings) ~theme =
    let create_extensions state =
      let theme = Codemirror_themes.get state in
      let extensions =
        [ Basic_setup.basic_setup
        ; Codemirror_ocaml.ocaml_stream_parser
          |> Stream_parser.Stream_language.define
          |> Stream_parser.Stream_language.to_language
          |> Language.extension
        ; theme
        ]
      in
      match keybindings with
      | Normal -> extensions
      | Vim -> failwith "VIM TODO"
      | Emacs -> failwith "EMACS TODO"
    in
    let create_state extensions =
      State.Editor_state.create (State.Editor_state_config.create ~doc ~extensions ())
    in
    Codemirror.with_dynamic_extensions
      ?name
      ~sexp_of:[%sexp_of: Codemirror_themes.t]
      ~equal:[%equal: Codemirror_themes.t]
      ~initial_state:(create_state (create_extensions Codemirror_themes.Material_dark))
      ~compute_extensions:(Bonsai.return create_extensions)
      theme
  ;;
end

module Ocaml_syntax_highlighting_dynamic_prime = struct
  let doc =
    {|open! Core

(* Syntax highlight for ocaml *)

let x = List.map [ 1; 2; 3; 4; 5 ] ~f:(fun x -> x + 1)

      oooo

let y =
  let z = 3 in
  let a = 4 in
  z + a
;;
|}
  ;;

  let codemirror_editor ~with_vim_keybindings ~theme =
    let extensions =
      let%arr theme = theme in
      let theme = Codemirror_themes.get theme in
      let extensions =
        [ Basic_setup.basic_setup
        ; Codemirror_ocaml.ocaml_stream_parser
          |> Stream_parser.Stream_language.define
          |> Stream_parser.Stream_language.to_language
          |> Language.extension
        ; theme
        ]
      in
      match with_vim_keybindings with
      | false -> extensions
      | true -> failwith "TODO VIM :("
    in
    Codemirror.with_dynamic_extensions' ~initial_text:doc ~extensions
  ;;
end

module Scheme_syntax_highlighting = struct
  let doc = Shader.example_source

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
              ]
            ()))
  ;;
end

module Which_editor = struct
  type t =
    | Scheme
    | Ocaml
    | Ocaml_dynamic_prime
    | Ocaml_with_vim_keybindings
    | Ocaml_with_emacs_keybindings
  [@@deriving enumerate, sexp, equal, compare]

  let to_string = function
    | Scheme -> "Scheme syntax highlighting"
    | Ocaml -> "OCaml syntax highlighting"
    | Ocaml_dynamic_prime -> "OCaml syntax highlighting with_dynamic_extensions'"
    | Ocaml_with_vim_keybindings -> "OCaml syntax highlighting with vim keybindings"
    | Ocaml_with_emacs_keybindings -> "OCaml syntax highlighting with emacs keybindings"
  ;;
end

let no_theme_picker x =
  let%arr x = x in
  None, x
;;

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
  let%sub _theme_picker, codemirror =
    (* Note: [Codemirror.with_dynamic_extensions] is generally preferred to [match%sub]ing
       and choosing a codemirror editor instance. For the purposes of this demo, the code
       is optimized for showing off the ease with which people can create different
       codemirror editors, so we do the less-preferred option. *)
    let ocaml_editor ~keybindings graph =
      let theme_picker =
        Form.Elements.Dropdown.enumerable
          ~to_string:Codemirror_themes.to_string
          (module Codemirror_themes)
          graph
        |> Bonsai.map ~f:(Form.label "theme")
      in
      let chosen_theme =
        let%arr theme_picker = theme_picker in
        Form.value theme_picker |> Or_error.ok_exn
      in
      let c =
        Ocaml_syntax_highlighting.codemirror_editor
          ~name:"ocaml"
          ~theme:chosen_theme
          ~keybindings
          graph
      in
      let%arr c = c
      and theme_picker = theme_picker in
      Some theme_picker, c
    in
    match%sub chosen_language with
    | Ocaml -> ocaml_editor ~keybindings:Normal graph
    | Ocaml_with_vim_keybindings -> ocaml_editor ~keybindings:Vim graph
    | Ocaml_with_emacs_keybindings -> ocaml_editor ~keybindings:Emacs graph
    | Ocaml_dynamic_prime ->
      let theme_picker =
        Form.Elements.Dropdown.enumerable
          ~to_string:Codemirror_themes.to_string
          (module Codemirror_themes)
          graph
        |> Bonsai.map ~f:(Form.label "theme")
      in
      let chosen_theme =
        let%arr theme_picker = theme_picker in
        Form.value theme_picker |> Or_error.ok_exn
      in
      let c =
        Ocaml_syntax_highlighting_dynamic_prime.codemirror_editor
          ~name:"ocaml_dynamic_prime"
          ~theme:chosen_theme
          ~with_vim_keybindings:false
          graph
      in
      let%arr c = c
      and theme_picker = theme_picker in
      Some theme_picker, c
    | Scheme ->
      no_theme_picker @@ Scheme_syntax_highlighting.codemirror_editor ~name:"scheme" graph
  in
  let codemirror_view =
    let%arr codemirror = codemirror in
    Codemirror.view codemirror
  in
  let codemirror_text =
    let%arr codemirror = codemirror in
    Codemirror.text codemirror
  in
  let () =
    Bonsai.Edge.lifecycle ~on_activate:(return (Ui_effect.of_thunk init_canvas)) graph
  in
  let%arr codemirror_view = codemirror_view
  and codemirror_text = codemirror_text in
  let open Vdom.Node in
  let open Vdom.Attr in
  div
    ~attrs:[ class_ "main-container" ]
    [ div
        ~attrs:[ class_ "sidebar" ]
        [ div ~attrs:[ id "canvas-container" ] [ canvas ~attrs:[ id "gl-canvas" ] [] ]
        ; div
            ~attrs:[ class_ "controls" ]
            [ button
                ~attrs:
                  [ on_click (fun _ ->
                      Ui_effect.of_thunk (fun _ ->
                        compile_and_link (Glml_compiler.compile_source codemirror_text)))
                  ]
                [ text "Compile and Link" ]
            ]
        ]
    ; div ~attrs:[ class_ "editor-container" ] [ codemirror_view ]
    ]
;;

let () = Bonsai_web.Start.start component
