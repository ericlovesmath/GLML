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

type constr_desc =
  | Eq of ty * ty
  | HasClass of type_class * ty
  | Broadcast of ty * ty * ty
  | MulBroadcast of ty * ty * ty
  | IndexAccess of ty * int * ty
  | FieldAccess of ty * string * ty
[@@deriving sexp_of]

type constr =
  { desc : constr_desc
  ; loc : Lexer.loc
  }
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

(** Applies a [substitution] to all type annotations in a typed [term].
    Used by [Monomorphize] to instantiate polymorphic bindings at concrete types. *)
val subst_term : substitution -> term -> term

(** Given a scheme's deferred constraints and a substitution mapping its tyvars
    to concrete types, applies [substitution], then solves the remaining constraints.
    Used by [Monomorphize] after instantiate polymorphic bindings. *)
val solve_scheme_constrs : constr list -> substitution -> substitution Or_error.t

(** Typechecker using Hindley-Milner extended with GLSL-specific constraints.

    Inference generates typed terms and collects [constr] goals, solved with unification,
    and contains the following constraints (closed universe of "typeclasses")

    - [Broadcast (l, r, ret)] — Broadcasting binary operators for Scalar/Vec/Mats, e.g. [float op vec3 = vec3], [vec3 op vec3 = vec3]
    - [MulBroadcast (l, r, ret)] — Broadcasting [*] for Vec/Mat, e.g. [mat3x4 * vec4 = vec3], [mat2x3 * mat4x2 = mat4x3]
    - [IndexAccess (t, i, ret)] — Vec/Mat indexing, e.g. [vec.0 = float]
    - [FieldAccess (t, field, ret)] — Record field access
    - [Eq (ty, ty')] — Polymorphic equality
    - [HasClass (cls, ty)] — Enforces typeclass membership (used to defer constraint checking), e.g. [#sin] has class [GenType] (float, vec)

    Helpful references that were used:

    - Typing Haskell in Haskell: [https://web.cecs.pdx.edu/~mpj/thih/thih.pdf]
    - Type Inference with Constrained Types: [https://www.cs.tufts.edu/~nr/cs257/archive/martin-odersky/hmx.pdf]
    - Demystifying Typeclasses: [https://okmij.org/ftp/Computation/typeclass.html]
    *)
val typecheck : Stlc.t -> t Or_error.t
