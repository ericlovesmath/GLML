open Core
open Lexer
open Compiler_error

module Maybe = struct
  type error_info =
    { message : string
    ; expected : string list
    ; found : (token option * loc) option
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
    | Fail e ->
      (match p' st with
       | Success res -> Success res
       | Fatal e' -> Fatal e'
       | Fail e' ->
         let expected =
           List.stable_dedup ~compare:String.compare (e.expected @ e'.expected)
         in
         Fail { e' with expected })
  ;;

  let loc_of_stream st = Option.map ~f:(fun ((_, loc), _) -> loc) (Sequence.next st.seq)

  let ( <??> ) p tag =
    fun st ->
    match p st with
    | Success res -> Success res
    | Fatal e -> Fatal { e with contexts = (tag, loc_of_stream st) :: e.contexts }
    | Fail e -> Fail { e with contexts = (tag, loc_of_stream st) :: e.contexts }
  ;;

  let ( <?> ) p label =
    fun st ->
    match p st with
    | Success res -> Success res
    | Fail e -> Fail { e with expected = [ label ] }
    | Fatal e -> Fatal { e with expected = [ label ] }
  ;;
end

open Let_syntax
open Infix_syntax

let fail ?loc ?tok message =
  Fail
    { message
    ; expected = []
    ; found = Option.map loc ~f:(Tuple2.create tok)
    ; contexts = []
    }
;;

let commit p =
  fun st ->
  match p st with
  | Success v -> Success v
  | Fail e | Fatal e -> Fatal e
;;

let satisfy_map (pred : token -> 'a option) : 'a t =
  fun st ->
  match Sequence.next st.seq with
  | None ->
    let loc =
      let _, p_end = st.last_loc in
      if p_end.line = 0 then None else Some (Lexer.loc_end st.last_loc)
    in
    fail ?loc "unexpected end of input"
  | Some ((tok, loc), seq) ->
    (match pred tok with
     | Some c -> Success (c, { seq; last_loc = loc })
     | None -> fail ~loc ~tok "unexpected")
;;

let satisfy (pred : token -> bool) : token t =
  satisfy_map (fun tok -> Option.some_if (pred tok) tok)
;;

let peek : token t =
  fun st ->
  match Sequence.next st.seq with
  | Some ((c, _), _) -> Success (c, st)
  | None -> fail "peek_eof"
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
    | Some ((tok, loc), _) -> fail ~loc ~tok "unexpected"
    | None -> Success x
  in
  match maybe with
  | Success x -> Ok x
  | Fail info | Fatal info ->
    let loc = Option.map info.found ~f:snd in
    let msg =
      let open Printf in
      (* Some description outputs, but maybe overkill *)
      match info.expected, info.found with
      | [], None -> info.message
      | [], Some (None, _) -> "unexpected end of input"
      | [], Some (Some tok, _) -> sprintf "unexpected %s" (string_of_token tok)
      | [ label ], None -> sprintf "expected %s" label
      | [ label ], Some (None, _) -> sprintf "expected %s but found end of input" label
      | [ label ], Some (Some tok, _) ->
        sprintf "expected %s but found %s" label (string_of_token tok)
      | labels, None -> sprintf "expected one of: %s" (String.concat ~sep:", " labels)
      | labels, Some (None, _) ->
        sprintf
          "expected one of: %s but found end of input"
          (String.concat ~sep:", " labels)
      | labels, Some (Some tok, _) ->
        sprintf
          "expected one of: %s but found %s"
          (String.concat ~sep:", " labels)
          (string_of_token tok)
    in
    let d =
      let fmt label loc_opt =
        match loc_opt with
        | None -> label
        | Some loc -> label ^ " at " ^ string_of_loc loc
      in
      match List.rev info.contexts with
      | [] -> None
      | contexts ->
        let others =
          List.map contexts ~f:(fun (ctx, loc) ->
            Sexp.List [ Sexp.Atom "in"; Sexp.Atom (fmt ctx loc) ])
        in
        Some (Sexp.List others)
    in
    Compiler_error.fail ~pass:"parser" ?loc ?d msg
;;

let tok t = satisfy (equal_token t) <?> string_of_token t
let fail message = Fn.const (Fail { message; expected = []; found = None; contexts = [] })

let fatal message =
  Fn.const (Fatal { message; expected = []; found = None; contexts = [] })
;;

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
