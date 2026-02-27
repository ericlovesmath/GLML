open Core
open Sexplib.Sexp

(* TODO: Improve Lexer in general *)
(* TODO: Report location on fail *)

type pos =
  { i : int
  ; line : int
  ; col : int
  }

let sexp_of_pos { i = _; line; col } = Atom [%string "%{line#Int}:%{col#Int}"]
let initial_pos = { i = 0; line = 0; col = 0 }

type token =
  | TRUE
  | FALSE
  | EQ
  | ARROW
  | LPAREN
  | RPAREN
  | DOT
  | LANGLE
  | RANGLE
  | LBRACKET
  | RBRACKET
  | SEMI
  | COLON
  | COMMA
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | BAR
  | MATCH
  | WITH
  | LCURLY
  | RCURLY
  | BOOL
  | INT
  | FLOAT
  | TICK
  | VEC
  | MAT
  | ADD
  | SUB
  | DIV
  | MUL
  | NUMERIC of int
  | ID of string
[@@deriving sexp, equal]

type t =
  { str : string
  ; len : int
  ; mutable pos : pos
  }

let of_string s = { str = s; len = String.length s; pos = { i = 0; line = 1; col = 1 } }
let eof t = t.pos.i = t.len

let advance c t =
  t.pos
  <- (if Char.equal c '\n'
      then { i = t.pos.i + 1; line = t.pos.line + 1; col = 1 }
      else { i = t.pos.i + 1; line = t.pos.line; col = t.pos.col + 1 })
;;

let peek t = if eof t then failwith "peek: EOF" else String.get t.str t.pos.i
let skip t = advance (peek t) t

let rec strip t =
  if (not (eof t)) && Char.is_whitespace (peek t)
  then (
    skip t;
    strip t)
;;

let read_while f t =
  let rec go () =
    if eof t
    then []
    else (
      let c = peek t in
      if f c
      then (
        skip t;
        c :: go ())
      else [])
  in
  String.of_list (go ())
;;

let read_lexeme t =
  let pos = t.pos in
  let token =
    match peek t with
    | '(' -> skip t; LPAREN
    | '=' -> skip t; EQ
    | ':' -> skip t; COLON
    | ')' -> skip t; RPAREN
    | '{' -> skip t; LCURLY
    | '}' -> skip t; RCURLY
    | '<' -> skip t; LANGLE
    | '>' -> skip t; RANGLE
    | '[' -> skip t; LBRACKET
    | ']' -> skip t; RBRACKET
    | ';' -> skip t; SEMI
    | ',' -> skip t; COMMA
    | '.' -> skip t; DOT
    | '|' -> skip t; BAR
    | '\'' -> skip t; TICK
    | '+' -> skip t; ADD
    | '-' -> skip t;
        (match peek t with
        | '>' -> skip t; ARROW
        | _ -> SUB)
    | '/' -> skip t; DIV
    | '*' -> skip t; MUL
    | c when Char.is_digit c ->
        NUMERIC (Int.of_string (read_while Char.is_digit t))
    | c when Char.is_alpha c -> (
        let s = read_while Char.is_alpha t in
        match s with
        | "true" -> TRUE
        | "false" -> FALSE
        | "let" -> LET
        | "in" -> IN
        | "if" -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | "fun" -> FUN
        | "match" -> MATCH
        | "with" -> WITH
        | "bool" -> BOOL
        | "int" -> INT
        | "float" -> FLOAT
        | "vec" -> VEC
        | "mat" -> MAT
        | _ -> ID s)
    | c -> failwith ("invalid char " ^ Char.to_string c)
    in
    strip t;
    (token, pos)
  [@@ocamlformat "disable"]

(* TODO: Have real non-failable behavior instead of [Or_error.try_with] *)
let lex t =
  Or_error.try_with (fun () ->
    let rec aux acc = if eof t then List.rev acc else aux (read_lexeme t :: acc) in
    strip t;
    aux [])
;;

let%expect_test "lexer" =
  let test s =
    s
    |> of_string
    |> lex
    |> Or_error.map ~f:(List.map ~f:fst)
    |> Or_error.sexp_of_t (List.sexp_of_t sexp_of_token)
    |> print_s
  in
  test "true false = -> ( ) . < >";
  test "{ } ; : , if then else let";
  test "in fun | match with { }";
  test "bool int float ' 10 stringy";
  test "+ - / *";
  [%expect
    {|
    (Ok (TRUE FALSE EQ ARROW LPAREN RPAREN DOT LANGLE RANGLE))
    (Ok (LCURLY RCURLY SEMI COLON COMMA IF THEN ELSE LET))
    (Ok (IN FUN BAR MATCH WITH LCURLY RCURLY))
    (Ok (BOOL INT FLOAT TICK (NUMERIC 10) (ID stringy)))
    (Ok (ADD SUB DIV MUL))
    |}];
  test "let{x:int}=match|a->fun->(f<x>*2)";
  [%expect
    {|
    (Ok
     (LET LCURLY (ID x) COLON INT RCURLY EQ MATCH BAR (ID a) ARROW FUN ARROW
      LPAREN (ID f) LANGLE (ID x) RANGLE MUL (NUMERIC 2) RPAREN))
    |}];
  test "vec2 mat2x3";
  [%expect {| (Ok (VEC (NUMERIC 2) MAT (NUMERIC 2) (ID x) (NUMERIC 3))) |}]
;;
