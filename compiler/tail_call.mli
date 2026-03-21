open Core
open Anf

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
  | Match of atom * (string * string list * anf) list
  | Switch of atom * (int * anf) list
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

(** Removes recursive functions and replaces them with while loops,
    with a provided hardcap on the number of iterations so that the
    shader doesn't decide to explode your computer.

    Example pseudocode:

    let fib n =
      let rec fib n acc =
        if n = 0 then acc else fib (n - 1) (acc * n)
      in
      fib n 1

    After ANF:

    let (rec 1000) fib_lift (n, acc) =
      let anf_1 = n = 0 in
      if anf_1
        then return acc
        else
          let anf_2 = n - 1 in
          let anf_3 = acc * n in
          return (fib (anf_2, anf_3))

    After Tail Call:

    // Note TC fails if there are any [fib] in non-return postion
    let fib_lift (n, acc) =
      let iters = 0 in
      while (iters < 1000) {
        let anf_1 = n = 0 in
        if anf_1
          then return acc
          else
            let anf_2 = n - 1 in
            let anf_3 = acc * n in
            set n = anf_2;
            set acc = anf_3;
            set iters = iters + 1;
      }
      return <placeholder_value_for_ret_ty>

  NOTE: The actual conversion occurs in [translate], but this step provides
  the information where relevant to do this
*)
val remove_rec : Anf.t -> t Or_error.t
