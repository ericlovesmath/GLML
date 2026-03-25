open Core
open Lexer
open Compiler_error

module Maybe = struct
  type error_info =
    { message : string
    ; found : (token * loc) option
    ; contexts : (string * loc option) list
    }

  type 'a t =
    | Success of 'a
    | Fail of error_info
    | Fatal of error_info

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let apply mf mx =
        match mf with
        | Fail e -> Fail e
        | Fatal e -> Fatal e
        | Success f ->
          (match mx with
           | Fail e -> Fail e
           | Fatal e -> Fatal e
           | Success x -> Success (f x))
      ;;

      let return x = Success x
      let map = `Define_using_apply
    end)

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let bind m ~f =
        match m with
        | Success x -> f x
        | Fail e -> Fail e
        | Fatal e -> Fatal e
      ;;

      let return = return
      let map = `Define_using_bind
    end)
end

open Maybe

type 'a maybe = 'a Maybe.t

type stream =
  { seq : (token * loc) Sequence.t
  ; last_loc : loc
  }

type 'a t = stream -> ('a * stream) Maybe.t

include Applicative.Make (struct
    type nonrec 'a t = 'a t

    let apply pf px st =
      let open Maybe.Let_syntax in
      let%bind f, st' = pf st in
      let%bind x, st'' = px st' in
      Success (f x, st'')
    ;;

    let return x st = Success (x, st)
    let map = `Define_using_apply
  end)

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let bind p ~f =
      fun st ->
      let%bind.Maybe a, st' = p st in
      f a st'
    ;;

    let return = return
    let map = `Define_using_bind
  end)

module Infix_syntax = struct
  include Applicative_infix

  let ( <$> ) f x = map ~f x
  let ( <$ ) f p = Fun.const f <$> p
  let ( $> ) p f = f <$ p
  let ( *> ) i p = ignore_m i *> p
  let ( <* ) p i = p <* ignore_m i

  let ( <*>| ) pf px =
    let open Maybe.Let_syntax in
    fun st ->
      let%bind f, st' = pf st in
      let%bind x, st'' = (Lazy.force px) st' in
      Success (f x, st'')
  ;;

  let ( <|> ) p p' =
    fun st ->
    match p st with
    | Success res -> Success res
    | Fatal e -> Fatal e
    | Fail _ ->
      (match p' st with
       | Success res -> Success res
       | Fatal e' -> Fatal e'
       | Fail e' -> Fail e')
  ;;

  let loc_of_stream st = Option.map ~f:(fun ((_, loc), _) -> loc) (Sequence.next st.seq)

  let ( <??> ) p tag =
    fun st ->
    match p st with
    | Success res -> Success res
    | Fatal e -> Fatal { e with contexts = (tag, loc_of_stream st) :: e.contexts }
    | Fail e -> Fail { e with contexts = (tag, loc_of_stream st) :: e.contexts }
  ;;
end

open Let_syntax
open Infix_syntax

let fail ?loc ?tok message = Fail { message; found = Option.both tok loc; contexts = [] }

let commit p =
  fun st ->
  match p st with
  | Success v -> Success v
  | Fail e | Fatal e -> Fatal e
;;

let satisfy (pred : token -> bool) : token t =
  fun st ->
  match Sequence.next st.seq with
  | Some ((tok, loc), seq) ->
    if pred tok
    then Success (tok, { seq; last_loc = loc })
    else fail ~loc ~tok "satisfy_fail"
  | None -> fail "satisfy_eof"
;;

let peek : token t =
  fun st ->
  match Sequence.next st.seq with
  | Some ((c, _), _) -> Success (c, st)
  | None -> fail "peek_eof"
;;

let satisfy_map (pred : token -> 'a option) : 'a t =
  fun st ->
  match Sequence.next st.seq with
  | None -> fail "satisfy_map_eof"
  | Some ((c, loc), seq) ->
    (match pred c with
     | Some c -> Success (c, { seq; last_loc = loc })
     | None -> fail ~loc "satisfy_map_fail")
;;

let rec many1 p = List.cons <$> p <*>| lazy (many p)
and many p = many1 p <|> return []

let sep_by1 sep p = List.cons <$> p <*> many (sep *> p)
let sep_by sep p = sep_by1 sep p <|> return []

let chainl1 (p : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
  let rec go acc =
    (let%bind f = op in
     let%bind rhs = p in
     go (f acc rhs))
    <|> return acc
  in
  let%bind lhs = p in
  go lhs
;;

let optional (p : 'a t) : 'a option t = p >>| Option.some <|> return None

let run p s =
  let maybe =
    let last_loc = Lexer.init_loc in
    let%bind.Maybe x, st = p { seq = Sequence.of_list s; last_loc } in
    match Sequence.next st.seq with
    | Some ((_, loc), _) -> fail ~loc "run_stream_not_fully_consumed"
    | None -> Success x
  in
  match maybe with
  | Success x -> Ok x
  | Fail info | Fatal info ->
    let loc = Option.map info.found ~f:snd in
    let msg =
      match info.found with
      | None -> info.message
      | Some (token, _) ->
        let token = Sexp.to_string_hum (sexp_of_token token) in
        Printf.sprintf "%s on <%s>" info.message token
    in
    let d =
      (* TODO: Refactor this *)
      if List.is_empty info.contexts
      then None
      else (
        let string_of_context = function
          | s, None -> s
          | s, Some loc ->
            let loc = Sexp.to_string_hum (sexp_of_loc loc) in
            s ^ " " ^ loc
        in
        let contexts = List.rev_map ~f:string_of_context info.contexts in
        Some (Sexp.List [ [%message (contexts : string list)] ]))
    in
    Compiler_error.fail ~pass:"parser" ?loc ?d msg
;;

let tok t = satisfy (equal_token t)
let fail message = Fn.const (Fail { message; found = None; contexts = [] })
let fatal message = Fn.const (Fatal { message; found = None; contexts = [] })

let with_loc (p : 'a t) : ('a * loc) t =
  fun st ->
  let start_loc =
    match Sequence.next st.seq with
    | Some ((_, loc), _) -> loc
    | None -> Lexer.loc_end st.last_loc
  in
  let%map.Maybe v, st = p st in
  (v, Lexer.merge_loc start_loc st.last_loc), st
;;
