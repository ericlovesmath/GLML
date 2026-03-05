type atom =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
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
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

and anf_desc =
  | Let of string * term * anf
  | Return of term
[@@deriving sexp_of]

and anf =
  { desc : anf_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type top_desc =
  | Define of
      { name : string
      ; args : (string * Stlc.ty) list
      ; body : anf
      ; ret_ty : Stlc.ty
      }
  | Const of string * anf
  | Extern of string
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }
[@@deriving sexp_of]

type t = Program of top list [@@deriving sexp_of]

(** Converts [t] to A-normal form, updating the [type map] to account for
    the new created variables. Variables are named in the form [anf_num]. *)
val to_anf : Lambda_lift.t -> t
