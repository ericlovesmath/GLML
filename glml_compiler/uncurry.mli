open Core

type term =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of (string * Stlc.ty) list * term
  | App of term * term list
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
[@@deriving sexp_of]

type top =
  | Define of string * term
  | Extern of Stlc.ty * string
[@@deriving sexp_of]

type t = Program of Stlc.ty String.Map.t * top list [@@deriving sexp_of]

val uncurry : Typecheck.t -> t
