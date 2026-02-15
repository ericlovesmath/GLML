open Core
open Sexplib.Sexp

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec3
  | TyArrow of ty * ty
[@@deriving sexp_of, equal]

type term =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec3 of term * term * term
  | Lam of string * ty * term
  | App of term * term
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
[@@deriving sexp_of]

type t = Program of term list [@@deriving sexp_of]

let rec ty_of_sexp = function
  | Atom "float" -> TyFloat
  | Atom "int" -> TyInt
  | Atom "bool" -> TyBool
  | Atom "vec3" -> TyVec3
  | List [ ty; Atom "->"; ty' ] -> TyArrow (ty_of_sexp ty, ty_of_sexp ty')
  | sexp -> raise_s [%message "ty_of_sexp: unexpected format" (sexp : Sexp.t)]
;;

let is_ident = String.for_all ~f:Char.is_alpha

let rec term_of_sexp = function
  | Atom i when Option.is_some (Int.of_string_opt i) -> Int (Int.of_string i)
  | Atom f when Option.is_some (Float.of_string_opt f) -> Float (Float.of_string f)
  | Atom "#t" -> Bool true
  | Atom "#f" -> Bool false
  | Atom v when is_ident v -> Var v
  | List [ Atom "vec3"; x; y; z ] -> Vec3 (term_of_sexp x, term_of_sexp y, term_of_sexp z)
  | List [ Atom "fun"; Atom v; Atom ":"; ty; Atom "->"; t ] when is_ident v ->
    Lam (v, ty_of_sexp ty, term_of_sexp t)
  | List [ Atom "let"; Atom v; Atom "="; bind; Atom "in"; body ] when is_ident v ->
    Let (v, term_of_sexp bind, term_of_sexp body)
  | List [ Atom "if"; c; t; e ] -> If (term_of_sexp c, term_of_sexp t, term_of_sexp e)
  | List [ Atom "+"; t; t' ] -> Bop (Add, term_of_sexp t, term_of_sexp t')
  | List [ Atom "-"; t; t' ] -> Bop (Sub, term_of_sexp t, term_of_sexp t')
  | List [ Atom "*"; t; t' ] -> Bop (Mul, term_of_sexp t, term_of_sexp t')
  | List [ Atom "/"; t; t' ] -> Bop (Div, term_of_sexp t, term_of_sexp t')
  | List [ Atom "%"; t; t' ] -> Bop (Mod, term_of_sexp t, term_of_sexp t')
  | List [ Atom "="; t; t' ] -> Bop (Eq, term_of_sexp t, term_of_sexp t')
  | List [ Atom "<"; t; t' ] -> Bop (Lt, term_of_sexp t, term_of_sexp t')
  | List [ Atom ">"; t; t' ] -> Bop (Gt, term_of_sexp t, term_of_sexp t')
  | List [ Atom "<="; t; t' ] -> Bop (Leq, term_of_sexp t, term_of_sexp t')
  | List [ Atom ">="; t; t' ] -> Bop (Geq, term_of_sexp t, term_of_sexp t')
  | List [ Atom "&&"; t; t' ] -> Bop (And, term_of_sexp t, term_of_sexp t')
  | List [ Atom "||"; t; t' ] -> Bop (Or, term_of_sexp t, term_of_sexp t')
  | List [ f; x ] -> App (term_of_sexp f, term_of_sexp x)
  | sexp -> raise_s [%message "[t_of_sexp] unexpected format" (sexp : Sexp.t)]
;;

let t_of_sexp s = Program (List.t_of_sexp term_of_sexp (Sexp.of_string s))

(** Reads string sexp for simple STLC representation, intended to be temporary
    until a real parser will be written *)
let of_string s = Or_error.try_with (fun () -> t_of_sexp s)
