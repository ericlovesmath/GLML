open Core

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int
  | TyMat of int * int
  | TyArrow of ty * ty
  | TyRecord of string
  | TyVariant of string
[@@deriving sexp_of, equal]

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty * term
  | App of term * term
  | Let of Stlc.recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of string * term list
  | Field of term * string
  | Variant of string * string * term list
  | Match of term * (string * string list * term) list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
[@@deriving sexp_of]

type top_desc =
  | Define of Stlc.recur * string * term
  | Extern of string
  | TypeDef of string * type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Specializes polymorphic functions, generating a new instance for every concrete
    monomorphic type that lives in the general polymorphic type *)
val monomorphize : Typecheck.t -> t Or_error.t
