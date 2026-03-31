open Core
open Sexplib.Sexp

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of (string * Monomorphize.ty) list * term
  | App of term * term list
  | Let of Stlc.recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of string * term list
  | Field of term * string
  | Variant of string * string * term list
  | Match of term * (Stlc.pat * term) list

and term =
  { desc : term_desc
  ; ty : Monomorphize.ty
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
  | Lam (args, body) ->
    let args =
      List.map args ~f:(fun (v, ty) -> List [ Atom v; Monomorphize.sexp_of_ty ty ])
    in
    List [ Atom "lambda"; List args; sexp_of_term body ]
  | App (f, args) -> List (Atom "app" :: sexp_of_term f :: List.map args ~f:sexp_of_term)
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
  | Record (s, ts) -> List (Atom s :: List.map ts ~f:sexp_of_term)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ty_name, ctor, args) ->
    List (Atom "Variant" :: Atom ty_name :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (pat, body) = List [ Stlc.sexp_of_pat pat; sexp_of_term body ] in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

type top_desc =
  | Define of Stlc.recur * string * term
  | Extern of string
  | TypeDef of string * Monomorphize.type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : Monomorphize.ty
  ; loc : Lexer.loc
  }

let sexp_of_top t =
  List [ sexp_of_top_desc t.desc; Atom ":"; Monomorphize.sexp_of_ty t.ty ]
;;

type t = Program of top list [@@deriving sexp_of]

let arg_ty (t : Monomorphize.term) =
  match t.ty with
  | TyArrow (a, _) -> a
  | _ -> failwith "applying a non function type"
;;

let rec collect_lams (t : Monomorphize.term) : (string * Monomorphize.ty) list * term =
  match t.desc with
  | Lam (v, body) ->
    let args, body = collect_lams body in
    (v, arg_ty t) :: args, body
  | _ -> [], uncurry_term t

and uncurry_term (t : Monomorphize.term) : term =
  let desc =
    match t.desc with
    | Var v -> Var v
    | Float f -> Float f
    | Int i -> Int i
    | Bool b -> Bool b
    | Vec (n, ts) -> Vec (n, List.map ts ~f:uncurry_term)
    | Mat (x, y, ts) -> Mat (x, y, List.map ts ~f:uncurry_term)
    | Lam (v, body) ->
      let args, body = collect_lams body in
      Lam ((v, arg_ty t) :: args, body)
    | App _ ->
      let rec collect (acc : term list) (t : Monomorphize.term) =
        match t.desc with
        | App (f, x) -> collect (uncurry_term x :: acc) f
        | _ -> uncurry_term t, acc
      in
      let f, args = collect [] t in
      App (f, args)
    | Let (recur, v, bind, body) -> Let (recur, v, uncurry_term bind, uncurry_term body)
    | If (c, t_true, e) -> If (uncurry_term c, uncurry_term t_true, uncurry_term e)
    | Bop (op, l, r) -> Bop (op, uncurry_term l, uncurry_term r)
    | Index (t_sub, i) -> Index (uncurry_term t_sub, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:uncurry_term)
    | Record (s, ts) -> Record (s, List.map ts ~f:uncurry_term)
    | Field (t, f) -> Field (uncurry_term t, f)
    | Variant (ty_name, ctor, args) ->
      Variant (ty_name, ctor, List.map args ~f:uncurry_term)
    | Match (scrutinee, cases) ->
      Match (uncurry_term scrutinee, List.map cases ~f:(Tuple2.map_snd ~f:uncurry_term))
  in
  { desc; ty = t.ty; loc = t.loc }
;;

let uncurry_top (t : Monomorphize.top) : top =
  let desc =
    match t.desc with
    | Define (recur, v, term) -> Define (recur, v, uncurry_term term)
    | Extern v -> Extern v
    | TypeDef (name, decl) -> TypeDef (name, decl)
  in
  { desc; ty = t.ty; loc = t.loc }
;;

let uncurry (Monomorphize.Program tops) : t = Program (List.map tops ~f:uncurry_top)
