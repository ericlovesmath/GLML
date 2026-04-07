type atom_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
[@@deriving sexp_of]

type atom =
  { desc : atom_desc
  ; ty : Monomorphize.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type term_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Mat of int * int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | If of atom * anf * anf
  | Record of string * atom list
  | Field of atom * string
  | Switch of atom * (Glsl.switch_case * anf) list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : Monomorphize.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

and anf_desc =
  | Let of string * term * anf
  | Placeholder of string * anf
  | Return of term
  | While of term * anf * anf
  | Set of string * atom * anf
  | Continue
[@@deriving sexp_of]

and anf =
  { desc : anf_desc
  ; ty : Monomorphize.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of
      { name : string
      ; args : (string * Monomorphize.ty) list
      ; body : anf
      ; ret_ty : Monomorphize.ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * Monomorphize.type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : Monomorphize.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Removes [Temp] placeholder values and replaces them with proper terms *)
val remove : Lower_variants.t -> t
