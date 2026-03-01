open Core

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
  | HASH
  | LEQ
  | GEQ
  | PERCENT
  | LAND
  | LOR
  | EXTERN
  | NUMERIC of int
  | ID of string
[@@deriving sexp, equal]

type t
type loc [@@deriving sexp_of]

val of_string : string -> t
val lex : t -> (token * loc) list Or_error.t
