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

type syntax =
  | SexpLike [@warning "-37"]
  | OCamlLike

(* Temporary, until my syntax actually moves away from Sexps *)
let chosen_syntax = OCamlLike

module Example = struct
  type t =
    | Metalic_liquid
    | Mouse_circle
    | Pastel_rainbow
    | Checkboard
    | Recursive_star
    | Mandelbrot
  [@@deriving sexp, compare, equal, enumerate, to_string ~capitalize:"Title Case"]

  let to_glml = function
    | Metalic_liquid -> [%blob "../examples/warped_noise.glml"]
    | Mouse_circle -> [%blob "../examples/mouse_circle.glml"]
    | Pastel_rainbow -> [%blob "../examples/rainbow.glml"]
    | Checkboard -> [%blob "../examples/checkerboard.glml"]
    | Recursive_star -> [%blob "../examples/recursion.glml"]
    | Mandelbrot -> [%blob "../examples/mandelbrot.glml"]
  ;;
end

module Codemirror_editor = struct
  let component language =
    let name, language_stream_parser =
      match language with
      | SexpLike -> "scheme", Scheme.scheme
      | OCamlLike -> "ocaml", Codemirror_ocaml.ocaml_stream_parser
    in
    Codemirror.of_initial_state
      ~name
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~extensions:
              [ Basic_setup.basic_setup
              ; Codemirror_themes.get Codemirror_themes.Material_dark
              ; language_stream_parser
                |> Stream_parser.Stream_language.define
                |> Stream_parser.Stream_language.to_language
                |> Language.extension
              ]
            ()))
  ;;
end

let component graph =
  let error, set_error =
    Bonsai.state_opt graph ~equal:String.equal ~sexp_of_model:String.sexp_of_t
  in
  let compile_effect =
    let%arr set_error = set_error in
    fun glml ->
      Ui_effect.bind (Ui_effect.return ()) ~f:(fun () ->
        match Or_error.try_with_join (fun () -> Glml_compiler.compile glml) with
        | Error err -> set_error (Some (Error.to_string_hum err))
        | Ok glsl ->
          glsl
          |> compile_and_link
          |> Js.Opt.to_option
          |> Option.map ~f:Js.to_string
          |> set_error)
  in
  let%sub codemirror = Codemirror_editor.component chosen_syntax graph in
  let%sub example_form =
    Form.Elements.Dropdown.enumerable (module Example) ~to_string:Example.to_string graph
  in
  let () =
    Form.Dynamic.on_change
      ~equal:[%compare.equal: Example.t]
      ~f:
        (let%arr codemirror = codemirror
         and compile_effect = compile_effect in
         fun example ->
           let text = Example.to_glml example in
           Ui_effect.Many
             [ Codemirror.set_lines codemirror (String.split_lines text)
             ; compile_effect text
             ])
      example_form
      graph
  in
  let () =
    (* Load sample shader on start *)
    let on_activate =
      let%arr codemirror = codemirror
      and compile_effect = compile_effect in
      Ui_effect.bind (Ui_effect.of_thunk init_canvas) ~f:(fun () ->
        compile_effect (Codemirror.text codemirror))
    in
    Bonsai.Edge.lifecycle ~on_activate graph
  in
  let%arr codemirror = codemirror
  and compile_effect = compile_effect
  and error = error
  and example_form = example_form in
  let open Vdom.Node in
  let open Vdom.Attr in
  div
    ~attrs:[ class_ "main-container" ]
    [ div
        ~attrs:[ class_ "sidebar" ]
        [ div ~attrs:[ id "canvas-container" ] [ canvas ~attrs:[ id "gl-canvas" ] [] ]
        ; div
            ~attrs:[ class_ "controls" ]
            [ Form.view_as_vdom example_form
            ; button
                ~attrs:[ on_click (fun _ -> compile_effect (Codemirror.text codemirror)) ]
                [ text "Compile" ]
            ; (match error with
               | None -> none
               | Some err -> div ~attrs:[ class_ "error-message" ] [ text err ])
            ]
        ]
    ; div ~attrs:[ class_ "editor-container" ] [ Codemirror.view codemirror ]
    ]
;;

(* NOTE: As much as I would love to add vim keybindings, I can't extern
   @replit/codemirror-vim because Codemirror explicitly forbids multiple
   instances of codemirror, and there's not a good way to work around this
   since [Bonsai_web_ui_codemirror] does not expose the vim bindings. There is
   a [codemirror_vim] library that exists supposedly internally at Jane Street but
   this is unfortunately not public... maybe I give up and write this in Javascript
   like a normal person. *)
let () = Bonsai_web.Start.start component
