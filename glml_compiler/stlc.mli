open Core

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec3
  | TyArrow of ty * ty
[@@deriving sexp_of, equal]

type term =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec3 of term * term * term
  | Lam of string * ty * term
  | App of term * term
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
[@@deriving sexp_of]

type top =
  | Define of string * term
  | Extern of ty * string
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Reads string sexp for simple STLC representation, intended to be temporary
    until a real parser will be written. Failiable. *)
val of_string : string -> t Or_error.t
