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
  | Loop of (string * atom) list * anf
  | Continue of atom list
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

(** Removes recursive functions and replaces them with structured loops,
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

      let fib_lift (n, acc) =
        loop (_iter, 0) (n, n) (acc, acc) {
          let _lim = _iter < 1000 in
          return
            (if _lim
              then
                let anf_1 = n = 0 in
                if anf_1
                  then return acc
                  else
                    let anf_2 = n - 1 in
                    let anf_3 = acc * n in
                    let _iter_inc = _iter + 1 in
                    continue (_iter_inc, anf_2, anf_3)
              else return <placeholder>)
        }
*)
val remove_rec : Anf.t -> t Compiler_error.t
