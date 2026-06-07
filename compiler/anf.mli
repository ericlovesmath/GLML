open Lower_variants

type atom_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
[@@deriving sexp_of]

type atom =
  { desc : atom_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type term_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | If of atom * anf * anf
  | Record of atom list
  | Field of atom * string
  | Switch of atom * (Glsl.switch_case * anf) list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

and anf_desc =
  | Let of string * term * anf
  | Return of term
[@@deriving sexp_of]

and anf =
  { desc : anf_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of
      { name : string
      ; recur : Frontend.recur
      ; args : (string * ty) list
      ; body : anf
      ; ret_ty : ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Converts [Lower_variants.t] to A-normal form. *)
val to_anf : Lower_variants.t -> t Compiler_error.t
