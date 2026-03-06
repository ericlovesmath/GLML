type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int
  | TyMat of int * int
  | TyArrow of ty * ty
[@@deriving sexp_of, equal]

(* TODO: The [ty] here is only needed because of the STLC style typechecking,
   once we do HM or Bidirectional, this should not be needed *)
type recur =
  (** NOTE: [int] is for the maximum number of recs allowed *)
  | Rec of int * ty
  | Nonrec
[@@deriving sexp_of]

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty * term
  | App of term * term
  | Let of recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of recur * string * term
  | Extern of ty * string
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]
