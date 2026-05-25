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
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

and anf_desc =
  | Let of string * term * anf
  | Return of term
  | While of term * anf * anf
  | Set of string * atom * anf
  | Continue
[@@deriving sexp_of]

and anf =
  { desc : anf_desc
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of
      { name : string
      ; args : (string * Lower_tuples.ty) list
      ; body : anf
      ; ret_ty : Lower_tuples.ty
      }
  | Const of string * anf
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

(** Removes variants and replaces them with tagged structs *)
val lower : Tail_call.t -> t Compiler_error.t
