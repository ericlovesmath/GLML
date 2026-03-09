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
  | REC
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
  | VEC of int
  | MAT of int * int
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
  | TYPE
  | NUMERIC of int
  | FLOAT_LIT of float
  | ID of string
[@@deriving sexp, equal]

type t
type loc [@@deriving sexp_of]

val init_loc : loc
val merge_loc : loc -> loc -> loc
val loc_end : loc -> loc
val init : string -> t
val lex : t -> (token * loc) list Or_error.t
