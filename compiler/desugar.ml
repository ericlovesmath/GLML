open Core
open Sexplib.Sexp
open Frontend

include Compiler_error.Pass (struct
    let name = "desugar"
  end)

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
  | Lam of string * ty option * term
  | App of term * term
  | Let of recur * string * ty option * constr list * term * term
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
  | Lam (v, ty_opt, body) ->
    let ty = Option.sexp_of_t sexp_of_ty ty_opt in
    List [ Atom "lambda"; List [ Atom v; ty ]; sexp_of_term body ]
  | App (f, x) -> List [ Atom "app"; sexp_of_term f; sexp_of_term x ]
  | Let (recur, v, ret_ty, constrs, bind, body) ->
    let parts = [ Atom "let" ] in
    let parts =
      match recur with
      | Rec n -> parts @ [ List [ Atom "rec"; Atom (Int.to_string n) ] ]
      | Nonrec -> parts
    in
    let parts = parts @ [ Atom v ] in
    let parts =
      match ret_ty with
      | None -> parts
      | Some ret_ty -> parts @ [ List [ Atom ":"; sexp_of_ty ret_ty ] ]
    in
    let parts =
      match constrs with
      | [] -> parts
      | _ -> parts @ [ List [ Atom "where"; List.sexp_of_t sexp_of_constr constrs ] ]
    in
    List (parts @ [ sexp_of_term bind; sexp_of_term body ])
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
  | Define of recur * string * ty option * constr list * term
  | Extern of ty * string
  | TypeDef of string * string list * type_decl

type top =
  { desc : top_desc
  ; loc : Lexer.loc
  }

let sexp_of_top_desc = function
  | Define (recur, v, ret_ty, constrs, term) ->
    let recur_sexp = sexp_of_recur recur in
    let parts = [ Atom "Define"; recur_sexp; Atom v ] in
    let parts =
      match ret_ty with
      | None -> parts
      | Some ret_ty -> parts @ [ List [ Atom ":"; sexp_of_ty ret_ty ] ]
    in
    let parts =
      match constrs with
      | [] -> parts
      | _ -> parts @ [ List [ Atom "where"; List.sexp_of_t sexp_of_constr constrs ] ]
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

let rec desugar_term_desc ~loc (td : Frontend.term_desc) : term_desc =
  match td with
  | Var v -> Var v
  | Float f -> Float f
  | Int n -> Int n
  | Bool b -> Bool b
  | Vec (n, ts) -> Vec (n, List.map ~f:desugar_term ts)
  | Lam (v, ty_opt, body) -> Lam (v, ty_opt, desugar_term body)
  | App (f, x) -> App (desugar_term f, desugar_term x)
  | Pipe (l, r) ->
    (* x |> f   =>   f x *)
    App (desugar_term r, desugar_term l)
  | Let (r, PatVar v, ty_opt, where, bind, body) ->
    Let (r, v, ty_opt, where, desugar_term bind, desugar_term body)
  | Let (Rec _, _, _, _, _, _) ->
    raise "recursive let binding requires a variable pattern"
  | Let (Nonrec, pat, _, [], bind, body) ->
    Match (desugar_term bind, [ pat, desugar_term body ])
  | Let (Nonrec, _, _, _ :: _, _, _) ->
    raise "where-clause requires a variable pattern on the let binding"
  | If (c, t, e) -> If (desugar_term c, desugar_term t, desugar_term e)
  | Bop (op, l, r) -> Bop (op, desugar_term l, desugar_term r)
  | Index (t, i) -> Index (desugar_term t, i)
  | Builtin (b, ts) -> Builtin (b, List.map ~f:desugar_term ts)
  | Record fields -> Record (List.map fields ~f:(fun (s, t) -> s, desugar_term t))
  | Field (t, f) -> Field (desugar_term t, f)
  | Variant (ctor, args) -> Variant (ctor, List.map ~f:desugar_term args)
  | Match (scrutinee, cases) ->
    let cases = List.map cases ~f:(fun (p, t) -> p, desugar_term t) in
    Match (desugar_term scrutinee, cases)
  | Function cases ->
    (* function | pat -> e | pat' -> e'
       => fun _v -> match _v with | pat -> e | pat' -> e' *)
    let fresh_var = "_fn_arg" in
    let cases = List.map cases ~f:(fun (p, t) -> p, desugar_term t) in
    let match_term : term =
      { desc = Match ({ desc = Var fresh_var; loc }, cases); loc }
    in
    Lam (fresh_var, None, match_term)

and desugar_term (t : Frontend.term) : term =
  ({ desc = desugar_term_desc ~loc:t.loc t.desc; loc = t.loc } : term)
;;

let desugar_top_desc (td : Frontend.top_desc) : top_desc =
  match td with
  | Define (r, v, ty_opt, where, t) -> Define (r, v, ty_opt, where, desugar_term t)
  | Extern (ty, v) -> Extern (ty, v)
  | TypeDef (name, params, decl) -> TypeDef (name, params, desugar_type_decl decl)
;;

let desugar_top (t : Frontend.top) : top = { desc = desugar_top_desc t.desc; loc = t.loc }

let desugar (Program tops : Frontend.t) : t Compiler_error.t =
  try_with (fun () -> Program (List.map ~f:desugar_top tops))
;;
