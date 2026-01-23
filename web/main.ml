open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Codemirror
open Virtual_dom
module Form = Bonsai_web_ui_form.With_automatic_view
module Codemirror = Bonsai_web_ui_codemirror

(* Make the codemirror editor take up most of the view *)
let () =
  Inline_css.Private.append
    {|
  .cm-editor {
    height: 80vh;
  }

  label {
    font-weight: normal;
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
  let doc =
    {|;; Building a list of squares from 0 to 9:
;; Note: loop is simply an arbitrary symbol used as a label. Any symbol will do.

(define (list-of-squares n)
  (let loop ((i n) (res '()))
    (if (< i 0)
        res
        (loop (- i 1) (cons (* i i) res)))))|}
  ;;

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
    | Ocaml
    | Ocaml_dynamic_prime
    | Ocaml_with_vim_keybindings
    | Ocaml_with_emacs_keybindings
    | Scheme
  [@@deriving enumerate, sexp, equal, compare]

  let to_string = function
    | Ocaml -> "OCaml syntax highlighting"
    | Ocaml_dynamic_prime -> "OCaml syntax highlighting with_dynamic_extensions'"
    | Ocaml_with_vim_keybindings -> "OCaml syntax highlighting with vim keybindings"
    | Ocaml_with_emacs_keybindings -> "OCaml syntax highlighting with emacs keybindings"
    | Scheme -> "Scheme syntax highlighting"
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
  let%sub theme_picker, codemirror =
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
  let%arr codemirror_view = codemirror_view
  and codemirror_text = codemirror_text
  and language_picker = language_picker
  and theme_picker = theme_picker in
  Vdom.Node.div
    [ Vdom.Node.text "Choose your editor extension:"
    ; Vdom.Node.text codemirror_text
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.style (Css_gen.flex_container ~direction:`Row ()) ]
        [ Form.view_as_vdom language_picker
        ; Option.value_map ~default:Vdom.Node.none ~f:Form.view_as_vdom theme_picker
        ]
    ; codemirror_view
    ]
;;

let () = Bonsai_web.Start.start component
