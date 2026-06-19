type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyRecord of string
  | TySampler
[@@deriving sexp_of]

type type_decl = RecordDecl of (string * ty) list [@@deriving sexp_of]

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | App of string * term list
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of term list
  | Init_struct of (string * term) list
  | Field of term * string
  | Switch of term * (Glsl.switch_case * term) list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of
      { name : string
      ; recur : Frontend.recur
      ; args : (string * ty) list
      ; body : term
      ; ret_ty : ty
      }
  | Const of string * term
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

(** Removes variants and replaces them with tagged structs *)
val lower : Lambda_lift.t -> t Compiler_error.t
