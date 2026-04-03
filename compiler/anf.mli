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
  | Variant of string * string * atom list
  | Match of atom * (Stlc.pat * anf) list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : Monomorphize.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

and anf_desc =
  | Let of string * term * anf
  | Return of term
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
      ; recur : Stlc.recur
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

(** Converts [t] to A-normal form, updating the [type map] to account for
    the new created variables. Variables are named in the form [anf_num]. *)
val to_anf : Lambda_lift.t -> t
