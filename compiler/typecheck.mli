(** Hindley-Milner inference extended with typeclasses and broadcasting for
    GLSL operator overloading. The typechecker takes a [Desugar.t] (sugar-free
    surface AST), runs inference, and emits a typed AST where every term and
    top-level binding carries its resolved type and any deferred scheme constraints. *)

open Core
open Type_system

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Lam of string * term
  | App of term * term
  (** [Let] carries deferred [constr list] for monomorphization pass *)
  (* TODO: Shift constraints to a map in [t] *)
  | Let of Frontend.recur * string * constr list * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of string * term list
  | Field of term * string
  | Variant of string * string * term list
  | Match of term * (Frontend.pat * term) list
  | Coerce of ty * term
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of Frontend.recur * string * term
  | Extern of string
  | TypeDef of string * type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  ; scheme_constrs : constr list
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(* TODO: Move [term] def to [Type_system] so this can live in [monomorphize] *)

(** Instantiates a polymorphic scheme at a concrete type.
    Given a polymorphic term, its deferred scheme constraints, and a substitution
    mapping its type variables to concrete types, validates the constraints then
    applies the substitution to all type annotations in the term.
    Used by [Monomorphize] to specialize polymorphic bindings. *)
val instantiate_scheme
  :  ?structs:(string list * (string * ty) list) String.Map.t
  -> constr list
  -> term
  -> substitution
  -> term Compiler_error.t

(** Typechecker for GLML *)
val typecheck : Desugar.t -> t Compiler_error.t
