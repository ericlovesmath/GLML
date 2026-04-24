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
  | FUNCTION
  | BAR
  | MATCH
  | WITH
  | LCURLY
  | RCURLY
  | BOOL
  | INT
  | FLOAT
  | TICK
  | TYVAR of string
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
  | PIPE
  | EXTERN
  | TYPE
  | OF
  | CONSTRUCTOR of string
  | NUMERIC of int
  | FLOAT_LIT of float
  | ID of string
[@@deriving sexp, equal]

type t
type pos = Compiler_error.pos [@@deriving sexp_of]
type loc = Compiler_error.loc [@@deriving sexp_of]

val init_loc : loc
val merge_loc : loc -> loc -> loc
val loc_end : loc -> loc

(** Initialize imperative lexer implementation *)
val init : string -> t

val lex : t -> (token * loc) list Compiler_error.t
