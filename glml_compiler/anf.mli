open Core

type atom =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Unit
[@@deriving sexp_of]

type term =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | App of atom * atom
  | If of atom * anf * anf
  | Lam of string * Stlc.ty * anf
[@@deriving sexp_of]

(** A-normal form representation of the STLC *)
and anf =
  | Let of string * term * anf
  | Return of term
[@@deriving sexp_of]

type top =
  | Define of string * anf
  | Extern of Stlc.ty * string
[@@deriving sexp_of]

type t = Program of Stlc.ty String.Map.t * top list [@@deriving sexp_of]

(** Converts [t] to A-normal form, updating the [type map] to account for
    the new created variables. Variables are named in the form [anf_num]. *)
val to_anf : Typecheck.t -> t
