(** Shared type system between typechecker/monomorphization-esque passes *)

open Core

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyRecord of string * ty list
  | TyVariant of string * ty list
  | TyVar of string
[@@deriving sexp_of, equal, compare]

(** Top-level type declarations. The [string list] holds type parameter names
    (e.g. ["'a"; "'b"]) and comes first to mirror the surface syntax
    [type box ['a] = { value : 'a }]. *)
type type_decl =
  | RecordDecl of string list * (string * ty) list
  | VariantDecl of string list * (string * ty list) list
[@@deriving sexp_of]

(** Built-in typeclasses used to model GLSL's overloaded operators and builtins.
    Membership is decided structurally by [Constraint_solver.check_class].

    - [GenType] - generic floating-point: [TyFloat] or [TyVec _ TyFloat]
    - [GenIType] - int scalar
    - [GenBType] - bool scalar
    - [MatType] - matrix: [TyVec _ (TyVec _ TyFloat)]
    - [Numeric] - anything in the float/vec/mat tower
    - [Comparable] - admits [<], [>] etc. (floats only in GLSL)
    - [Equatable] - admits [=] and [!=] *)
type type_class =
  | GenType
  | GenBType
  | GenIType
  | MatType
  | Numeric
  | Comparable
  | Equatable
[@@deriving sexp_of]

(** A goal emitted by inference. Some are equalities to be unified, others are
    typeclass-style facts that the solver discharges or defers.

    - [Eq (a, b)] - unify [a] with [b]
    - [HasClass (cls, t)] - [t] must belong to [cls]
    - [Broadcast (l, r, ret)] - scalar/vector broadcasting for [+ - * /] etc.,
      e.g. [float + vec3 = vec3]
    - [MulBroadcast (l, r, ret)] - matrix multiplication's special rules,
      e.g. [mat3x4 * vec4 = vec3]
    - [IndexAccess (t, i, ret)] - [t.i] yields [ret]
    - [FieldAccess (t, f, ret)] - record field access
    - [Coerce (a, b)] - [a] is implicitly coercible to [b] (e.g. [TyInt] to
      [TyFloat]), deferred so we don't lock a tyvar to [int] prematurely *)
type constr_desc =
  | Eq of ty * ty
  | HasClass of type_class * ty
  | Broadcast of ty * ty * ty
  | MulBroadcast of ty * ty * ty
  | IndexAccess of ty * int * ty
  | FieldAccess of ty * string * ty
  | Coerce of ty * ty
[@@deriving sexp_of]

type constr =
  { desc : constr_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

(** Mapping from type-variable names to their resolved types. Substitutions are
    represented as association lists rather than maps because they are usually
    very small and order matters when composing. *)
type substitution = (string * ty) list [@@deriving sexp_of]

val fresh_tyvar : unit -> ty
val subst_ty : substitution -> ty -> ty
val subst_constraints : substitution -> constr list -> constr list
val compose_sub : substitution -> substitution -> substitution
val ftv_of_ty : ty -> String.Set.t
val ftv_of_constraint : constr -> String.Set.t
