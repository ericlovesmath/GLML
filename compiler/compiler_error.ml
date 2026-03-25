open Core
open Sexplib.Sexp

(* TODO: [to_string_hum[? *)
(* TODO: Refactor this whole file :P *)
(* TODO: Function to take in raw glml string and underline [loc] *)

type pos =
  { i : int
  ; line : int
  ; col : int
  }

let sexp_of_pos { i = _; line; col } = Sexp.Atom [%string "%{line#Int}:%{col#Int}"]

type loc = pos * pos

let sexp_of_loc (l, r) = List [ sexp_of_pos l; Atom "-"; sexp_of_pos r ]

type error =
  { pass : string
  ; loc : loc option
  ; msg : string
  ; details : Sexp.t option
  }
[@@deriving sexp_of]

type 'a t = ('a, error) Result.t

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

(* TODO: This is so bad *)
let to_or_error (r : 'a t) : 'a Or_error.t =
  match r with
  | Ok x -> Ok x
  | Error { pass; loc; msg; details } ->
    let parts =
      [ Sexp.Atom (pass ^ ": " ^ msg) ]
      @ (match loc with
         | Some loc -> [ [%message (loc : loc)] ]
         | None -> [])
      @ [ Option.sexp_of_t Fn.id details ]
    in
    Or_error.error_s (Sexp.List parts)
;;

let ok_exn (r : 'a t) : 'a =
  match r with
  | Ok x -> x
  | Error _ -> to_or_error r |> Or_error.ok_exn
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

module Pass (P : sig
    val name : string
  end) =
struct
  let fail ?loc ?d msg = fail ~pass:P.name ?loc ?d msg
  let of_option ?loc ?d msg x = of_option ~pass:P.name ?loc ?d msg x
  let of_or_error ?loc r = of_or_error ~pass:P.name ?loc r
end
