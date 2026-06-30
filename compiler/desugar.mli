open Frontend

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
  | AliasDecl of ty
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
  | Let of recur * string * ty option * constr list * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Sample of string * term
  | Record of (string * term) list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (pat * term) list
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
  | Module of string * Frontend.sig_ref option * top list
  | ModuleType of string * Frontend.spec list
  | Open of string
[@@deriving sexp_of]

and top =
  { desc : top_desc
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Desugars GLML code into simpler AST *)
val desugar : Frontend.t -> t Compiler_error.t
