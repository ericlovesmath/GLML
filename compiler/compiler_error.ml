open Core
open Sexplib.Sexp

type pos =
  { i : int
  ; line : int
  ; col : int
  }

let sexp_of_pos { i = _; line; col } = Sexp.Atom [%string "%{line#Int}:%{col#Int}"]

type loc = pos * pos

let sexp_of_loc (l, r) = List [ sexp_of_pos l; Atom "-"; sexp_of_pos r ]
let string_of_pos { line; col; _ } = Int.to_string line ^ ":" ^ Int.to_string col
let string_of_loc (l, r) = string_of_pos l ^ "-" ^ string_of_pos r

type error =
  { pass : string
  ; loc : loc option
  ; msg : string
  ; details : Sexp.t option
  }
[@@deriving sexp_of]

let format_details (sexp : Sexp.t) : string =
  (* TODO: Assumes data is key/value pairs mostly... assumes ppx_message *)
  match sexp with
  | List [ Atom key; value ] -> "  " ^ key ^ ": " ^ Sexp.to_string_hum value
  | List pairs ->
    pairs
    |> List.map ~f:(function
      | List [ Atom key; value ] -> "  " ^ key ^ ": " ^ Sexp.to_string_hum value
      | s -> "  " ^ Sexp.to_string_hum s)
    |> String.concat ~sep:"\n"
  | Atom _ -> "  " ^ Sexp.to_string_hum sexp
;;

let to_string_hum ?source { pass; loc; msg; details } =
  let header_and_details =
    let loc_str =
      match loc with
      | Some l -> " at " ^ string_of_loc l
      | None -> ""
    in
    let base = Printf.sprintf "[%s]%s: %s" pass loc_str msg in
    match details with
    | None -> base
    | Some d -> base ^ "\n" ^ format_details d
  in
  let is_real_loc (l, r) = l.line >= 1 && r.line >= 1 in
  match Option.both source loc with
  | None -> header_and_details
  | Some (_, loc) when not (is_real_loc loc) -> header_and_details
  | Some (source, (l, r)) ->
    let lines = String.split_lines source in
    let num_lines = List.length lines in
    let get_line n =
      if n >= 1 && n <= num_lines
      then Option.value (List.nth lines (n - 1)) ~default:""
      else ""
    in
    let line_num_width = String.length (Int.to_string r.line) in
    let pad_num n =
      let s = Int.to_string n in
      String.make (max 0 (line_num_width - String.length s)) ' ' ^ s
    in
    let gutter_blank = String.make line_num_width ' ' ^ " | " in
    let show_line n = pad_num n ^ " | " ^ get_line n in
    let context =
      if l.line = r.line
      then (
        (* Single-line span *)
        let underline =
          gutter_blank
          ^ String.make (max 0 (l.col - 1)) ' '
          ^ String.make (max 1 (r.col - l.col)) '^'
        in
        [ show_line l.line; underline ])
      else (
        (* Multi-line span, collapse if too tall *)
        let all_lines = List.range l.line (r.line + 1) |> List.map ~f:show_line in
        let n = List.length all_lines in
        let lines =
          if n <= 6
          then all_lines
          else (
            let head = List.take all_lines 3 in
            let tail = List.drop all_lines (n - 2) in
            head @ [ gutter_blank ^ "..." ] @ tail)
        in
        lines @ [ gutter_blank ])
    in
    String.concat ~sep:"\n" (header_and_details :: gutter_blank :: context)
;;

exception Compile_error of error

type 'a t = ('a, error) Result.t

let ok_exn = function
  | Ok x -> x
  | Error error -> raise (Compile_error error)
;;

let raise ~pass ?loc ?d msg = raise (Compile_error { pass; loc; msg; details = d })
let return x = Ok x
let fail ~pass ?loc ?d msg = Error { pass; loc; msg; details = d }

let of_option ~pass ?loc ?d msg = function
  | Some x -> Ok x
  | None -> Error { pass; loc; msg; details = d }
;;

let of_or_error ~pass ?loc (r : 'a Or_error.t) : 'a t =
  match r with
  | Ok x -> Ok x
  | Error err -> Error { pass; loc; msg = Error.to_string_hum err; details = None }
;;

let to_or_error (r : 'a t) : 'a Or_error.t =
  match r with
  | Ok x -> Ok x
  | Error err -> Or_error.error_string (to_string_hum err)
;;

let try_with (f : unit -> 'a) : 'a t =
  try
    let result = f () in
    Ok result
  with
  | Compile_error error -> Error error
;;

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return

    let bind r ~f =
      match r with
      | Ok x -> f x
      | Error _ as e -> e
    ;;

    let map = `Define_using_bind
  end)

(* TODO: I kind of hate this but have no better idea *)
module Pass (P : sig
    val name : string
  end) =
struct
  let raise ?loc ?d msg = raise ~pass:P.name ?loc ?d msg
  let ok_exn = ok_exn
  let try_with = try_with
  let fail ?loc ?d msg = fail ~pass:P.name ?loc ?d msg
  let of_option ?loc ?d msg x = of_option ~pass:P.name ?loc ?d msg x
  let of_or_error ?loc r = of_or_error ~pass:P.name ?loc r
end
