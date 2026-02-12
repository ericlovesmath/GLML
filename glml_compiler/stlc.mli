open Core

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec3
  | TyArrow of ty * ty
[@@deriving sexp_of, equal]

type t =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec3 of t * t * t
  | Lam of string * ty * t
  | App of t * t
  | Let of string * t * t
  | If of t * t * t
  | Bop of Glsl.binary_op * t * t
[@@deriving sexp_of]

(** Reads string sexp for simple STLC representation, intended to be temporary
    until a real parser will be written. Failiable. *)
val of_string : string -> t Or_error.t
