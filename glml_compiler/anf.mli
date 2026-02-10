open Core

type atom =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
[@@deriving sexp_of]

type term =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec3 of atom * atom * atom
  | App of atom * atom
  | If of atom * t * t
  | Lam of string * Stlc.ty * t
[@@deriving sexp_of]

(** A-normal form representation of the STLC *)
and t =
  | Let of string * term * t
  | Return of term
[@@deriving sexp_of]

(** Converts [t] to A-normal form, updating the [type map] to account for
    the new created variables. Variables are named in the form [anf_num]. *)
val normalize : Stlc.ty String.Map.t -> Stlc.t -> Stlc.ty String.Map.t * t
