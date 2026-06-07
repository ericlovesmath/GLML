open Anf

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
  ; ty : Lower_variants.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

and anf_desc =
  | Let of string * term * anf
  | Return of term
  | Loop of (string * atom) list * anf
  | Continue of atom list
[@@deriving sexp_of]

and anf =
  { desc : anf_desc
  ; ty : Lower_variants.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of
      { name : string
      ; args : (string * Lower_variants.ty) list
      ; body : anf
      ; ret_ty : Lower_variants.ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * Lower_variants.type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : Lower_variants.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Removes recursive functions and replaces them with bounded loops *)
val remove_rec : Anf.t -> t Compiler_error.t
