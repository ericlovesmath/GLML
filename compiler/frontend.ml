open Core
open Sexplib.Sexp

type pat =
  | PatCtor of string * pat list
  | PatLitBool of bool
  | PatLitInt of int
  | PatLitFloat of float
  | PatWildcard
  | PatVar of string
  | PatBracket of pat list
  | PatRecord of (string * pat) list * bool
  | PatTuple of pat list
[@@deriving equal]

let rec sexp_of_pat = function
  | PatCtor (ctor, args) -> List (Atom ctor :: List.map args ~f:sexp_of_pat)
  | PatLitBool b -> Atom (Bool.to_string b)
  | PatLitInt n -> Atom (Int.to_string n)
  | PatLitFloat f -> Atom (Float.to_string f)
  | PatWildcard -> Atom "_"
  | PatVar v -> Atom v
  | PatBracket pats -> List (Atom "bracket" :: List.map pats ~f:sexp_of_pat)
  | PatRecord (fields, _) ->
    List
      (Atom "record" :: List.map fields ~f:(fun (f, p) -> List [ Atom f; sexp_of_pat p ]))
  | PatTuple pats -> List (Atom "tuple" :: List.map pats ~f:sexp_of_pat)
;;

let rec pat_fold_vars p ~init ~f =
  match p with
  | PatWildcard | PatLitBool _ | PatLitInt _ | PatLitFloat _ -> init
  | PatVar v -> f init v
  | PatCtor (_, ps) | PatBracket ps | PatTuple ps ->
    List.fold ps ~init ~f:(fun acc p -> pat_fold_vars p ~init:acc ~f)
  | PatRecord (fields, _) ->
    List.fold fields ~init ~f:(fun acc (_, p) -> pat_fold_vars p ~init:acc ~f)
;;

let rec pat_map_vars p ~f =
  match p with
  | PatWildcard | PatLitBool _ | PatLitInt _ | PatLitFloat _ -> p
  | PatVar v -> PatVar (f v)
  | PatCtor (c, ps) -> PatCtor (c, List.map ps ~f:(pat_map_vars ~f))
  | PatBracket ps -> PatBracket (List.map ps ~f:(pat_map_vars ~f))
  | PatTuple ps -> PatTuple (List.map ps ~f:(pat_map_vars ~f))
  | PatRecord (fields, partial) ->
    PatRecord (List.map fields ~f:(fun (n, p) -> n, pat_map_vars p ~f), partial)
;;

let pat_bound_vars p = pat_fold_vars p ~init:[] ~f:(Fn.flip List.cons) |> List.rev

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyName of string
  | TyVar of string
  | TyApp of string * ty list
  | TyTuple of ty list
[@@deriving equal]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec (i, t) -> List [ Atom "vec"; Atom (Int.to_string i); sexp_of_ty t ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
  | TyName s -> Atom s
  | TyVar v -> Atom ("'" ^ v)
  | TyApp (s, args) -> List (Atom s :: List.map args ~f:sexp_of_ty)
  | TyTuple ts -> List (Atom "tuple" :: List.map ts ~f:sexp_of_ty)
;;

type constr_desc =
  | CNumeric of ty
  | CBroadcast of ty * ty * ty
  | CMulBroadcast of ty * ty * ty

let sexp_of_constr_desc = function
  | CNumeric t -> List [ Atom "Numeric"; sexp_of_ty t ]
  | CBroadcast (a, b, r) ->
    List [ Atom "Broadcast"; sexp_of_ty a; sexp_of_ty b; sexp_of_ty r ]
  | CMulBroadcast (a, b, r) ->
    List [ Atom "MulBroadcast"; sexp_of_ty a; sexp_of_ty b; sexp_of_ty r ]
;;

type constr =
  { desc : constr_desc
  ; loc : Lexer.loc
  }

let sexp_of_constr (c : constr) = sexp_of_constr_desc c.desc

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
  | AliasDecl of ty
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
  | Lam of string * ty option * term
  | App of term * term
  | Pipe of term * term
  | Let of recur * pat * ty option * constr list * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin
  | BopSection of Glsl.binary_op
  | PipeSection
  | Record of (string * term) list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (pat * term) list
  | Function of (pat * term) list
  | Tuple of term list

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
  | Pipe (l, r) -> List [ sexp_of_term l; Atom "|>"; sexp_of_term r ]
  | Let (recur, v, ret_ty, constrs, bind, body) ->
    let parts = [ Atom "let" ] in
    let parts =
      match recur with
      | Rec n -> parts @ [ List [ Atom "rec"; Atom (Int.to_string n) ] ]
      | Nonrec -> parts
    in
    let parts = parts @ [ sexp_of_pat v ] in
    let parts =
      match ret_ty with
      | None -> parts
      | Some ret_ty -> parts @ [ List [ Atom ":"; sexp_of_ty ret_ty ] ]
    in
    let parts =
      match constrs with
      | [] -> parts
      | _ -> parts @ [ Atom "where"; List.sexp_of_t sexp_of_constr constrs ]
    in
    List (parts @ [ sexp_of_term bind; sexp_of_term body ])
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin b -> Atom (Glsl.string_of_builtin b)
  | BopSection op -> List [ Atom (Glsl.string_of_binary_op op) ]
  | PipeSection -> List [ Atom "|>" ]
  | Record fields ->
    let sexp_of_field (f, t) = List [ Atom f; sexp_of_term t ] in
    List (Atom "record" :: List.map fields ~f:sexp_of_field)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ctor, args) ->
    List (Atom "Variant" :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (pat, body) = List [ sexp_of_pat pat; sexp_of_term body ] in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)
  | Function cases ->
    let sexp_of_case (pat, body) = List [ sexp_of_pat pat; sexp_of_term body ] in
    List (Atom "function" :: List.map cases ~f:sexp_of_case)
  | Tuple ts -> List (Atom "tuple" :: List.map ts ~f:sexp_of_term)

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
  | Define (recur, v, ret_ty_opt, constrs, term) ->
    let recur_sexp = sexp_of_recur recur in
    let parts = [ Atom "Define"; recur_sexp; Atom v ] in
    let parts =
      match ret_ty_opt with
      | None -> parts
      | Some ret_ty -> parts @ [ List [ Atom ":"; sexp_of_ty ret_ty ] ]
    in
    let parts =
      match constrs with
      | [] -> parts
      | _ -> parts @ [ Atom "where"; List.sexp_of_t sexp_of_constr constrs ]
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
