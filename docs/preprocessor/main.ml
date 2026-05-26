(* Preprocessor to convert inline GLML to GLSL at compile time and embed shader *)

open Core

let live_fence =
  Re.compile
    (Re.seq
       [ Re.bol
       ; Re.str "```glml,live"
       ; Re.rep (Re.set " \t")
       ; Re.char '\n'
       ; Re.group (Re.non_greedy (Re.rep Re.any))
       ; Re.str "\n```"
       ])
;;

let slugify =
  String.map ~f:(fun c -> if Char.is_alphanum c then Char.lowercase c else '-')
;;

let live_block ~id ~src ~glsl =
  String.concat
    ~sep:"\n"
    [ "```glml"
    ; src
    ; "```"
    ; ""
    ; sprintf {|<canvas class="glml-canvas" data-glml-src="%s"></canvas>|} id
    ; sprintf {|<script type="text/x-glsl" id="%s">|} id
    ; glsl
    ; "</script>"
    ; ""
    ; "<details><summary>Compiled GLSL</summary>"
    ; ""
    ; "```glsl"
    ; glsl
    ; "```"
    ; ""
    ; "</details>"
    ]
;;

let error_block ~src ~err =
  let quoted =
    Glml_compiler.Compiler_error.to_string_hum ~source:src err
    |> String.split_lines
    |> List.map ~f:(fun l -> "> " ^ l)
    |> String.concat ~sep:"\n"
  in
  sprintf "```glml\n%s\n```\n\n> **GLML compile error:**\n%s\n" src quoted
;;

let render_block ~slug ~idx ~src =
  match Glml_compiler.compile src with
  | Ok glsl -> live_block ~id:(sprintf "glml-shader-%s-%d" slug idx) ~src ~glsl
  | Error err -> error_block ~src ~err
;;

let transform_content ~slug content =
  Re.split_full live_fence content
  |> List.folding_map ~init:0 ~f:(fun idx -> function
    | `Text s -> idx, s
    | `Delim g -> idx + 1, render_block ~slug ~idx ~src:(Re.Group.get g 1))
  |> String.concat
;;

let chapter_path = function
  | `Assoc fields ->
    (match List.Assoc.find fields "path" ~equal:String.equal with
     | Some (`String s) -> Some s
     | _ -> None)
  | _ -> None
;;

let map_field key ~f = function
  | `Assoc fields ->
    `Assoc
      (List.map fields ~f:(fun (k, v) -> if String.equal k key then k, f v else k, v))
  | other -> other
;;

let rec transform_item = function
  | `Assoc [ ("Chapter", chapter) ] as item ->
    let slug = chapter_path chapter |> Option.value ~default:"chapter" |> slugify in
    let chapter =
      chapter
      |> map_field "content" ~f:(function
        | `String c -> `String (transform_content ~slug c)
        | v -> v)
      |> map_field "sub_items" ~f:(function
        | `List subs -> `List (List.map subs ~f:transform_item)
        | v -> v)
    in
    (match chapter with
     | `Assoc _ -> `Assoc [ "Chapter", chapter ]
     | _ -> item)
  | other -> other
;;

let transform_book =
  map_field "sections" ~f:(function
    | `List secs -> `List (List.map secs ~f:transform_item)
    | v -> v)
;;

let () =
  match Array.to_list (Sys.get_argv ()) with
  | _ :: "supports" :: _ -> exit 0
  | _ ->
    let book =
      match Yojson.Safe.from_channel In_channel.stdin with
      | `List [ _ctx; book ] -> book
      | _ -> failwith "expected [context, book] from mdbook on stdin"
    in
    print_string (Yojson.Safe.to_string (transform_book book))
;;
