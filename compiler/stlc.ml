open Core
open Sexplib.Sexp

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int
  | TyMat of int * int
  | TyArrow of ty * ty
  | TyName of string
  | TyVar of string
[@@deriving equal]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec i -> List [ Atom "vec"; Atom (Int.to_string i) ]
  | TyMat (x, y) -> List [ Atom "mat"; Atom (Int.to_string x); Atom (Int.to_string y) ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
  | TyName s -> Atom s
  | TyVar v -> Atom ("'" ^ v)
;;

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
[@@deriving sexp_of]

type recur =
  | Rec of int
  | Nonrec
[@@deriving sexp_of]

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty option * term
  | App of term * term
  | Let of recur * string * ty option * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of (string * term) list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (string * string list * term) list

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
  | Lam (v, ty_opt, body) ->
    let ty = Option.sexp_of_t sexp_of_ty ty_opt in
    List [ Atom "lambda"; List [ Atom v; ty ]; sexp_of_term body ]
  | App (f, x) -> List [ Atom "app"; sexp_of_term f; sexp_of_term x ]
  | Let (Rec n, v, None, bind, body) ->
    let rec_tag = List [ Atom "rec"; Atom (Int.to_string n) ] in
    List [ Atom "let"; rec_tag; Atom v; sexp_of_term bind; sexp_of_term body ]
  | Let (Rec n, v, Some ret_ty, bind, body) ->
    let rec_tag = List [ Atom "rec"; Atom (Int.to_string n) ] in
    List
      [ Atom "let"
      ; rec_tag
      ; Atom v
      ; List [ Atom ":"; sexp_of_ty ret_ty ]
      ; sexp_of_term bind
      ; sexp_of_term body
      ]
  | Let (Nonrec, v, None, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_term body ]
  | Let (Nonrec, v, Some ret_ty, bind, body) ->
    List
      [ Atom "let"
      ; Atom v
      ; List [ Atom ":"; sexp_of_ty ret_ty ]
      ; sexp_of_term bind
      ; sexp_of_term body
      ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_term)
  | Record fields ->
    let sexp_of_field (f, t) = List [ Atom f; sexp_of_term t ] in
    List (Atom "record" :: List.map fields ~f:sexp_of_field)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ctor, args) ->
    List (Atom "Variant" :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (ctor, vars, body) =
      List [ Atom ctor; List (List.map vars ~f:(fun v -> Atom v)); sexp_of_term body ]
    in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

type top_desc =
  | Define of recur * string * ty option * term
  | Extern of ty * string
  | TypeDef of string * type_decl

type top =
  { desc : top_desc
  ; loc : Lexer.loc
  }

let sexp_of_top_desc = function
  | Define (recur, v, ret_ty_opt, term) ->
    let recur_sexp = sexp_of_recur recur in
    let parts = [ Atom "Define"; recur_sexp; Atom v ] in
    let parts =
      match ret_ty_opt with
      | None -> parts
      | Some ret_ty -> parts @ [ List [ Atom ":"; sexp_of_ty ret_ty ] ]
    in
    List (parts @ [ sexp_of_term term ])
  | Extern (ty, v) -> List [ Atom "Extern"; sexp_of_ty ty; Atom v ]
  | TypeDef (name, decl) -> List [ Atom "TypeDef"; Atom name; sexp_of_type_decl decl ]
;;

let sexp_of_top t = sexp_of_top_desc t.desc

type t = Program of top list [@@deriving sexp_of]
