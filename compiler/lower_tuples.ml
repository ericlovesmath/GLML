open Core
open Sexplib.Sexp

include Compiler_error.Pass (struct
    let name = "lower_tuples"
  end)

(* ===== Types ===== *)

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyRecord of string
  | TyVariant of string
[@@deriving equal]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec (i, t) -> List [ Atom "vec"; Atom (Int.to_string i); sexp_of_ty t ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
  | TyRecord s -> Atom s
  | TyVariant s -> Atom s
;;

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Lam of string * term
  | App of term * term
  | Let of Frontend.recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of term list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (Frontend.pat * term) list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc : term_desc -> Sexp.t = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_term)
  | Lam (v, body) -> List [ Atom "lambda"; Atom v; sexp_of_term body ]
  | App (f, x) -> List [ Atom "app"; sexp_of_term f; sexp_of_term x ]
  | Let (Rec n, v, bind, body) ->
    let rec_tag = List [ Atom "rec"; Atom (Int.to_string n) ] in
    List [ Atom "let"; rec_tag; Atom v; sexp_of_term bind; sexp_of_term body ]
  | Let (Nonrec, v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_term body ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_term)
  | Record ts -> List (Atom "record" :: List.map ts ~f:sexp_of_term)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ctor, args) ->
    List (Atom "Variant" :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (pat, body) = List [ Frontend.sexp_of_pat pat; sexp_of_term body ] in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = List [ sexp_of_term_desc t.desc ]

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
[@@deriving sexp_of]

type top_desc =
  | Define of Frontend.recur * string * term
  | Extern of string
  | TypeDef of string * type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let sexp_of_top t = List [ sexp_of_top_desc t.desc; Atom ":"; sexp_of_ty t.ty ]

type t = Program of top list [@@deriving sexp_of]

(* ===== Tuple Lowering ===== *)

let tuple_field_name i = "_" ^ Int.to_string i

(** Fresh record name to each distinct [TyTuple] shape *)
type env = (Monomorphize.ty list * string) list

let equal_shape = List.equal Monomorphize.equal_ty

let collect (tops : Monomorphize.top list) : env =
  let upsert acc ts =
    if List.exists acc ~f:(fun (k, _) -> equal_shape k ts)
    then acc
    else (ts, Utils.fresh "tuple") :: acc
  in
  let rec walk_ty acc (ty : Monomorphize.ty) =
    match ty with
    | TyFloat | TyInt | TyBool | TyRecord _ | TyVariant _ -> acc
    | TyVec (_, t) -> walk_ty acc t
    | TyArrow (a, b) -> walk_ty (walk_ty acc a) b
    | TyTuple ts -> upsert (List.fold ts ~init:acc ~f:walk_ty) ts
  in
  let rec walk_term acc (t : Monomorphize.term) =
    let acc = walk_ty acc t.ty in
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> acc
    | Vec (_, ts) | Builtin (_, ts) | Record ts | Variant (_, ts) | Tuple ts ->
      List.fold ts ~init:acc ~f:walk_term
    | Lam (_, body) -> walk_term acc body
    | App (f, x) -> walk_term (walk_term acc f) x
    | Let (_, _, bind, body) -> walk_term (walk_term acc bind) body
    | If (c, t, e) -> walk_term (walk_term (walk_term acc c) t) e
    | Bop (_, l, r) -> walk_term (walk_term acc l) r
    | Index (t, _) | Field (t, _) -> walk_term acc t
    | Match (scrut, cases) ->
      List.fold cases ~init:(walk_term acc scrut) ~f:(fun a (_, b) -> walk_term a b)
  in
  let walk_top acc (top : Monomorphize.top) =
    let acc = walk_ty acc top.ty in
    match top.desc with
    | Define (_, _, bind) -> walk_term acc bind
    | Extern _ -> acc
    | TypeDef (_, RecordDecl fields) ->
      List.fold fields ~init:acc ~f:(fun a (_, t) -> walk_ty a t)
    | TypeDef (_, VariantDecl ctors) ->
      List.fold ctors ~init:acc ~f:(fun a (_, ts) -> List.fold ts ~init:a ~f:walk_ty)
  in
  List.fold tops ~init:[] ~f:walk_top |> List.rev
;;

let rec ty_of (env : env) (ty : Monomorphize.ty) : ty =
  match ty with
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec (n, t) -> TyVec (n, ty_of env t)
  | TyArrow (a, b) -> TyArrow (ty_of env a, ty_of env b)
  | TyRecord s -> TyRecord s
  | TyVariant s -> TyVariant s
  | TyTuple ts ->
    (match List.Assoc.find env ts ~equal:equal_shape with
     | Some n -> TyRecord n
     | None -> raise "unassigned TyTuple shape" ~d:[%message (ty : Monomorphize.ty)])
;;

let rec pat_of (pat : Frontend.pat) : Frontend.pat =
  match pat with
  | PatWildcard | PatLitBool _ | PatLitInt _ | PatLitFloat _ | PatVar _ -> pat
  | PatCtor (c, ps) -> PatCtor (c, List.map ps ~f:pat_of)
  | PatBracket ps -> PatBracket (List.map ps ~f:pat_of)
  | PatRecord (fields, partial) ->
    PatRecord (List.map fields ~f:(fun (n, p) -> n, pat_of p), partial)
  | PatTuple ps ->
    let fields = List.mapi ps ~f:(fun i p -> tuple_field_name i, pat_of p) in
    PatRecord (fields, false)
;;

let rec term_of (env : env) (t : Monomorphize.term) : term =
  let ty = ty_of env t.ty in
  let desc : term_desc =
    match t.desc with
    | Var v -> Var v
    | Float f -> Float f
    | Int i -> Int i
    | Bool b -> Bool b
    | Vec (n, ts) -> Vec (n, List.map ts ~f:(term_of env))
    | Lam (v, body) -> Lam (v, term_of env body)
    | App (f, x) -> App (term_of env f, term_of env x)
    | Let (r, v, bind, body) -> Let (r, v, term_of env bind, term_of env body)
    | If (c, t, e) -> If (term_of env c, term_of env t, term_of env e)
    | Bop (op, l, r) -> Bop (op, term_of env l, term_of env r)
    | Index (t, i) -> Index (term_of env t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:(term_of env))
    | Record ts -> Record (List.map ts ~f:(term_of env))
    | Field (t, f) -> Field (term_of env t, f)
    | Variant (ctor, args) -> Variant (ctor, List.map args ~f:(term_of env))
    | Match (scrut, cases) ->
      let cases = List.map cases ~f:(fun (pat, body) -> pat_of pat, term_of env body) in
      Match (term_of env scrut, cases)
    | Tuple ts -> Record (List.map ts ~f:(term_of env))
  in
  { desc; ty; loc = t.loc }
;;

let type_decl_of (asg : env) (d : Monomorphize.type_decl) : type_decl =
  match d with
  | RecordDecl fields -> RecordDecl (List.map fields ~f:(fun (n, t) -> n, ty_of asg t))
  | VariantDecl ctors ->
    VariantDecl (List.map ctors ~f:(fun (n, ts) -> n, List.map ts ~f:(ty_of asg)))
;;

let top_of (env : env) (top : Monomorphize.top) : top =
  let desc : top_desc =
    match top.desc with
    | Define (r, v, bind) -> Define (r, v, term_of env bind)
    | Extern v -> Extern v
    | TypeDef (name, decl) -> TypeDef (name, type_decl_of env decl)
  in
  { desc; ty = ty_of env top.ty; loc = top.loc }
;;

let lower_exn (Program tops : Monomorphize.t) : t =
  let env = collect tops in
  let loc =
    match List.hd tops with
    | Some t -> t.loc
    | None -> raise "empty program"
  in
  let typedef_tops =
    List.map env ~f:(fun (ts, name) ->
      let fields = List.mapi ts ~f:(fun i t -> tuple_field_name i, ty_of env t) in
      let desc = TypeDef (name, RecordDecl fields) in
      { desc; ty = TyRecord name; loc })
  in
  Program (typedef_tops @ List.map tops ~f:(top_of env))
;;

let lower t = try_with (fun () -> lower_exn t)
