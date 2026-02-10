open Core
open Sexplib.Sexp

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec3
  | TyArrow of ty * ty
[@@deriving sexp_of, equal]

type t =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec3 of t * t * t
  | Lam of string * ty * t
  | App of t * t
  | Let of string * t * t
  | If of t * t * t
  | Bop of Glsl.binary_op * t * t
[@@deriving sexp_of]

let rec ty_of_sexp = function
  | Atom "float" -> TyFloat
  | Atom "int" -> TyInt
  | Atom "bool" -> TyBool
  | Atom "vec3" -> TyVec3
  | List [ ty; Atom "->"; ty' ] -> TyArrow (ty_of_sexp ty, ty_of_sexp ty')
  | sexp -> raise_s [%message "ty_of_sexp: unexpected format" (sexp : Sexp.t)]
;;

let is_ident = String.for_all ~f:Char.is_alpha

let rec t_of_sexp = function
  | Atom i when Option.is_some (Int.of_string_opt i) -> Int (Int.of_string i)
  | Atom f when Option.is_some (Float.of_string_opt f) -> Float (Float.of_string f)
  | Atom "#t" -> Bool true
  | Atom "#f" -> Bool false
  | Atom v when is_ident v -> Var v
  | List [ Atom "vec3"; x; y; z ] -> Vec3 (t_of_sexp x, t_of_sexp y, t_of_sexp z)
  | List [ Atom "fun"; Atom v; Atom ":"; ty; Atom "->"; t ] when is_ident v ->
    Lam (v, ty_of_sexp ty, t_of_sexp t)
  | List [ Atom "let"; Atom v; Atom "="; bind; Atom "in"; body ] when is_ident v ->
    Let (v, t_of_sexp bind, t_of_sexp body)
  | List [ Atom "if"; c; t; e ] -> If (t_of_sexp c, t_of_sexp t, t_of_sexp e)
  | List [ Atom "+"; t; t' ] -> Bop (Add, t_of_sexp t, t_of_sexp t')
  | List [ Atom "-"; t; t' ] -> Bop (Sub, t_of_sexp t, t_of_sexp t')
  | List [ Atom "*"; t; t' ] -> Bop (Mul, t_of_sexp t, t_of_sexp t')
  | List [ Atom "/"; t; t' ] -> Bop (Div, t_of_sexp t, t_of_sexp t')
  | List [ Atom "%"; t; t' ] -> Bop (Mod, t_of_sexp t, t_of_sexp t')
  | List [ Atom "="; t; t' ] -> Bop (Eq, t_of_sexp t, t_of_sexp t')
  | List [ Atom "<"; t; t' ] -> Bop (Lt, t_of_sexp t, t_of_sexp t')
  | List [ Atom ">"; t; t' ] -> Bop (Gt, t_of_sexp t, t_of_sexp t')
  | List [ Atom "<="; t; t' ] -> Bop (Leq, t_of_sexp t, t_of_sexp t')
  | List [ Atom ">="; t; t' ] -> Bop (Geq, t_of_sexp t, t_of_sexp t')
  | List [ Atom "&&"; t; t' ] -> Bop (And, t_of_sexp t, t_of_sexp t')
  | List [ Atom "||"; t; t' ] -> Bop (Or, t_of_sexp t, t_of_sexp t')
  | List [ f; x ] -> App (t_of_sexp f, t_of_sexp x)
  | sexp -> raise_s [%message "[t_of_sexp] unexpected format" (sexp : Sexp.t)]
;;

(** Reads string sexp for simple STLC representation, intended to be temporary
    until a real parser will be written *)
let of_string s = t_of_sexp (Sexp.of_string s)
