open Core
open Stlc

type type_class =
  | GenType
  | GenBType
  | GenIType
  | MatType
  | Numeric
  | Comparable
  | Equatable
[@@deriving sexp_of]

type constr =
  | Eq of Lexer.loc * ty * ty
  | HasClass of Lexer.loc * type_class * ty
  | Broadcast of Lexer.loc * ty * ty * ty
  | MulBroadcast of Lexer.loc * ty * ty * ty
  | IndexAccess of Lexer.loc * ty * int * ty
  | FieldAccess of Lexer.loc * ty * string * ty
[@@deriving sexp_of]

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty * term
  | App of term * term
  | Let of recur * string * constr list * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of string * term list
  | Field of term * string
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  (** TODO: This recur doesn't need to be here anymore, or at least doesn't need ty *)
  | Define of recur * string * term
  | Extern of string
  | RecordDef of string * (string * ty) list
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  ; scheme_constrs : constr list
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

type substitution = (string * Stlc.ty) list

(* TODO: Add documentation to functions below (used in monomorphization) *)
val subst_term : substitution -> term -> term
val solve_scheme_constrs : constr list -> substitution -> substitution Or_error.t
val typecheck : Stlc.t -> t Or_error.t
