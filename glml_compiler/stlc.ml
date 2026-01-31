open Core

type ty = TyFloat | TyInt | TyBool | TyArrow of ty * ty [@@deriving sexp_of]

type t =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Lam of string * ty * t
  | App of t * t
  | Let of string * t * t
  | If of t * t * t
  | Bop of Glsl.binary_op * t * t
[@@deriving sexp_of]
