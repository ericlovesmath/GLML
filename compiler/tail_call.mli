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

(** Removes recursive functions and replaces them with while loops,
    with a provided hardcap on the number of iterations so that the
    shader doesn't decide to explode your computer *)
val remove_rec : Anf.t -> t
