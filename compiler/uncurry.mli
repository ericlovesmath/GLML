type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of (string * Stlc.ty) list * term
  | App of term * term list
  | Let of Stlc.recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of string * term list
  | Field of term * string
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of Stlc.recur * string * term
  | Extern of string
  | RecordDef of string * (string * Stlc.ty) list
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

val uncurry : Typecheck.t -> t
