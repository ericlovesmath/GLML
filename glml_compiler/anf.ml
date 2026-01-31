open! Core

type atom = Var of string | Float of float | Int of int | Bool of bool
[@@deriving sexp_of]

type term =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | App of string * atom list
  | If of atom * t * t
[@@deriving sexp_of]

and t = Let of string * term * t | Result of term [@@deriving sexp_of]

let anf (_ : Stlc.t) : t = failwith "TODO"
