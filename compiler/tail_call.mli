open Anf

type value_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | Record of atom list
  | Init_struct of (string * atom) list
  | Field of atom * string
[@@deriving sexp_of]

and term_desc =
  | Value of value_desc
  | If of atom * anf * anf
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
  | Loop of
      { counter : string
      ; limit : int
      ; params : string list
      ; body : anf
      ; on_exceed : anf
      }
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
