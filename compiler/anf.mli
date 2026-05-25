type atom_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Temp
[@@deriving sexp_of]

type atom =
  { desc : atom_desc
  ; ty : Lower_tuples.ty
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
  | Variant of string * atom list
  | Match of atom * (Frontend.pat * anf) list
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
      ; recur : Frontend.recur
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

(** Converts [t] to A-normal form, updating the [type map] to account for
    the new created variables. Variables are named in the form [anf_num]. *)
val to_anf : Lambda_lift.t -> t Compiler_error.t
