open Core
open Sexplib.Sexp

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int
  | TyMat of int * int
  | TyArrow of ty * ty
[@@deriving equal]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec i -> List [ Atom "vec"; Atom (Int.to_string i) ]
  | TyMat (x, y) -> List [ Atom "mat"; Atom (Int.to_string x); Atom (Int.to_string y) ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
;;

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty * term
  | App of term * term
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list

and term =
  { desc : term_desc
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_term)
  | Mat (x, y, ts) ->
    List
      (Atom ("mat" ^ Int.to_string x ^ "x" ^ Int.to_string y)
       :: List.map ts ~f:sexp_of_term)
  | Lam (v, ty, body) ->
    List [ Atom "lambda"; List [ Atom v; sexp_of_ty ty ]; sexp_of_term body ]
  | App (f, x) -> List [ Atom "app"; sexp_of_term f; sexp_of_term x ]
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_term body ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_term)

and sexp_of_term t = sexp_of_term_desc t.desc

type top_desc =
  | Define of string * term
  | Extern of ty * string
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; loc : Lexer.loc
  }

let sexp_of_top t = sexp_of_top_desc t.desc

type t = Program of top list [@@deriving sexp_of]
