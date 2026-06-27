type pat =
  | PatCtor of string * pat list
  | PatLitBool of bool
  | PatLitInt of int
  | PatLitFloat of float
  | PatWildcard
  | PatVar of string
  | PatBracket of pat list (** PatRecord of [(binging * pattern) list * is_partial] *)
  | PatRecord of (string * pat) list * bool
  | PatTuple of pat list
[@@deriving sexp_of, equal]

val pat_bound_vars : pat -> string list
val pat_map_vars : pat -> f:(string -> string) -> pat
val pat_fold_vars : pat -> init:'a -> f:('a -> string -> 'a) -> 'a

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyName of string
  | TyVar of string
  | TyApp of string * ty list
  | TyTuple of ty list
  | TySampler
[@@deriving sexp_of, equal]

type constr_desc =
  | CNumeric of ty
  | CBroadcast of ty * ty * ty
  | CMulBroadcast of ty * ty * ty
[@@deriving sexp_of]

type constr =
  { desc : constr_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
  | AliasDecl of ty
[@@deriving sexp_of]

type recur =
  (* NOTE: [int] is for the maximum number of recs allowed *)
  | Rec of int
  | Nonrec
[@@deriving sexp_of]

type term_desc =
  | Var of string
  | Qual of string * string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Lam of string * ty option * term
  | App of term * term
  | Pipe of term * term
  | Let of recur * pat * ty option * constr list * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin
  | Sample of string * term
  | BopSection of Glsl.binary_op
  | PipeSection
  | Record of (string * term) list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (pat * term) list
  | Function of (pat * term) list
  | Tuple of term list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of recur * string * ty option * constr list * term
  | Extern of ty * string
  (* TypeDef (var, params, type) *)
  | TypeDef of string * string list * type_decl
  | Module of string * top list
  | Open of string
[@@deriving sexp_of]

and top =
  { desc : top_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]
