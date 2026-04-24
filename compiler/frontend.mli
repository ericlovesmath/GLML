type pat =
  | PatCtor of string * string list
  | PatLitBool of bool
  | PatLitInt of int
  | PatLitFloat of float
  | PatVar of string
[@@deriving sexp_of, equal]

val pat_bound_vars : pat -> string list

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int
  | TyMat of int * int
  | TyArrow of ty * ty
  | TyName of string
  | TyVar of string
  | TyApp of string * ty list
[@@deriving sexp_of, equal]

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
  | AliasDecl of ty
[@@deriving sexp_of]

type recur =
  (** NOTE: [int] is for the maximum number of recs allowed *)
  | Rec of int
  | Nonrec
[@@deriving sexp_of]

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty option * term
  | App of term * term
  | Pipe of term * term
  | Let of recur * string * ty option * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of (string * term) list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (pat * term) list
  | Function of (pat * term) list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of recur * string * ty option * term
  | Extern of ty * string
  (* TypeDef (var, params, type) *)
  | TypeDef of string * string list * type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]
