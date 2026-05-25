type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | App of term * term list
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of term list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (Frontend.pat * term) list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of
      { name : string
      ; recur : Frontend.recur
      ; args : (string * Lower_tuples.ty) list
      ; body : term
      ; ret_ty : Lower_tuples.ty
      }
  | Const of string * term
  | Extern of string
  | TypeDef of string * Lower_tuples.type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Lambda lifting, moving all lambda forms to the toplevel *)
val lift : Uncurry.t -> t Compiler_error.t
