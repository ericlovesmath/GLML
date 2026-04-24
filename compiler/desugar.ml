open Core
open Sexplib.Sexp
open Frontend
open Compiler_error.Let_syntax

module Err = Compiler_error.Pass (struct
    let name = "desugar"
  end)

(* ===== Types ===== *)

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
  | AliasDecl of ty
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
  | Let of Frontend.recur * string * ty option * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of (string * term) list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (Frontend.pat * term) list

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
    let sexp_of_case (pat, body) = List [ Frontend.sexp_of_pat pat; sexp_of_term body ] in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

type top_desc =
  | Define of recur * string * ty option * term
  | Extern of ty * string
  | TypeDef of string * string list * type_decl

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
  | TypeDef (name, [], decl) -> List [ Atom "TypeDef"; Atom name; sexp_of_type_decl decl ]
  | TypeDef (name, params, decl) ->
    let ty = name ^ "[" ^ String.concat ~sep:", " params ^ "]" in
    List [ Atom "TypeDef"; Atom ty; sexp_of_type_decl decl ]
;;

let sexp_of_top t = sexp_of_top_desc t.desc

type t = Program of top list [@@deriving sexp_of]

(* ===== Desugaring Logic ===== *)

let desugar_type_decl (td : Frontend.type_decl) : type_decl =
  match td with
  | RecordDecl fields -> RecordDecl fields
  | VariantDecl ctors -> VariantDecl ctors
  | AliasDecl t -> AliasDecl t
;;

let rec desugar_term_desc (td : Frontend.term_desc) : term_desc Compiler_error.t =
  match td with
  | Var v -> Ok (Var v)
  | Float f -> Ok (Float f)
  | Int n -> Ok (Int n)
  | Bool b -> Ok (Bool b)
  | Vec (n, ts) ->
    let%map ts = Compiler_error.all (List.map ~f:desugar_term ts) in
    Vec (n, ts)
  | Mat (x, y, ts) ->
    let%map ts = Compiler_error.all (List.map ~f:desugar_term ts) in
    Mat (x, y, ts)
  | Lam (v, ty_opt, body) ->
    let%map body = desugar_term body in
    Lam (v, ty_opt, body)
  | App (f, x) ->
    let%bind f = desugar_term f in
    let%bind x = desugar_term x in
    return (App (f, x))
  | Let (r, v, ty_opt, bind, body) ->
    let%bind bind = desugar_term bind in
    let%bind body = desugar_term body in
    return (Let (r, v, ty_opt, bind, body))
  | If (c, t, e) ->
    let%bind c = desugar_term c in
    let%bind t = desugar_term t in
    let%bind e = desugar_term e in
    return (If (c, t, e))
  | Bop (op, l, r) ->
    let%bind l = desugar_term l in
    let%bind r = desugar_term r in
    return (Bop (op, l, r))
  | Index (t, i) ->
    let%map t = desugar_term t in
    Index (t, i)
  | Builtin (b, ts) ->
    let%map ts = Compiler_error.all (List.map ~f:desugar_term ts) in
    Builtin (b, ts)
  | Record fields ->
    let%map fields =
      Compiler_error.all
        (List.map fields ~f:(fun (s, t) ->
           let%map t = desugar_term t in
           s, t))
    in
    Record fields
  | Field (t, f) ->
    let%map t = desugar_term t in
    Field (t, f)
  | Variant (ctor, args) ->
    let%map args = Compiler_error.all (List.map ~f:desugar_term args) in
    Variant (ctor, args)
  | Match (scrutinee, cases) ->
    let%bind scrutinee = desugar_term scrutinee in
    let%bind cases =
      Compiler_error.all
        (List.map cases ~f:(fun (p, t) ->
           let%map t = desugar_term t in
           p, t))
    in
    Ok (Match (scrutinee, cases))

and desugar_term (t : Frontend.term) : term Compiler_error.t =
  let%map desc = desugar_term_desc t.desc in
  ({ desc; loc = t.loc } : term)
;;

let desugar_top_desc (td : Frontend.top_desc) : top_desc Compiler_error.t =
  match td with
  | Define (r, v, ty_opt, t) ->
    let%map t = desugar_term t in
    Define (r, v, ty_opt, t)
  | Extern (ty, v) -> Ok (Extern (ty, v))
  | TypeDef (name, params, decl) -> Ok (TypeDef (name, params, desugar_type_decl decl))
;;

let desugar_top (t : Frontend.top) : top Compiler_error.t =
  let%map desc = desugar_top_desc t.desc in
  { desc; loc = t.loc }
;;

let desugar (Program tops : Frontend.t) : t Compiler_error.t =
  let%map tops = Compiler_error.all (List.map ~f:desugar_top tops) in
  Program tops
;;
