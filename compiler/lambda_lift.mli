open Core

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | App of term * term list
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of
      { name : string
      ; args : (string * Stlc.ty) list
      ; body : term
      ; ret_ty : Stlc.ty
      }
  | Const of string * term
  | Extern of string
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Lambda lifting, moving all lambda forms to the toplevel *)
val lift : Uncurry.t -> t Or_error.t
