open Core
open Stlc

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  (* TODO: Vec, Math, Lam all don't need to store the size/ty now *)
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
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

(* TODO: After typechecking, does [Define] and [Let] even need to
   include the annotated type, I think I can delete it *)
type top_desc =
  | Define of recur * string * term
  | Extern of string
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Returns a Typed AST, returns [Error _] if types are not sound *)
val typecheck : Stlc.t -> t Or_error.t
