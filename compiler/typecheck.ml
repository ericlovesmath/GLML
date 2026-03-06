open Core
open Sexplib.Sexp
open Stlc

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  (* TODO: Vec, Math, Lam all don't need to store the size/ty now *)
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty * term
  | App of term * term
  | Let of recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list

and term =
  { desc : term_desc
  ; ty : ty
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
  | Let (Rec (n, ty), v, bind, body) ->
    let rec_tag = List [ Atom "rec"; Atom (Int.to_string n); sexp_of_ty ty ] in
    List [ Atom "let"; rec_tag; Atom v; sexp_of_term bind; sexp_of_term body ]
  | Let (Nonrec, v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_term body ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_term)

and sexp_of_term t = List [ sexp_of_term_desc t.desc; Atom ":"; Stlc.sexp_of_ty t.ty ]

type top_desc =
  | Define of recur * string * term
  | Extern of string
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let sexp_of_top t = List [ sexp_of_top_desc t.desc; Atom ":"; Stlc.sexp_of_ty t.ty ]

type t = Program of top list [@@deriving sexp_of]

let rec update (map : ty String.Map.t) (t : Stlc.term)
  : (ty String.Map.t * term) Or_error.t
  =
  let open Or_error.Let_syntax in
  let make ?(map = map) term ty = Ok (map, ({ desc = term; ty; loc = t.loc } : term)) in
  let error_s sexp =
    let sexps =
      match sexp with
      | Atom sexp -> [ Atom sexp ]
      | List sexps -> sexps
    in
    let tag =
      [ Atom "typecheck:" ] @ sexps @ [ List [ Atom "loc"; Lexer.sexp_of_loc t.loc ] ]
    in
    error_s (List tag)
  in
  match t.desc with
  | Var v ->
    (match Map.find map v with
     | Some ty -> make (Var v) ty
     | None ->
       error_s [%message "var not found in type map" (v : string) (map : ty String.Map.t)])
  | Float f -> make (Float f) TyFloat
  | Int i -> make (Int i) TyInt
  | Bool b -> make (Bool b) TyBool
  | Vec (n, ts) ->
    let%bind map, terms =
      List.fold_result ts ~init:(map, []) ~f:(fun (map, acc) t_elem ->
        let%bind map, term = update map t_elem in
        match term.ty with
        | TyFloat -> Ok (map, term :: acc)
        | _ -> error_s [%message "vec expected all floats" (ts : Stlc.term list)])
    in
    let size = List.length terms in
    if size = n
    then make ~map (Vec (n, List.rev terms)) (TyVec n)
    else error_s [%message "vec size mismatch" (n : int) (size : int)]
  | Mat (n, m, ts) ->
    let%bind map, terms =
      List.fold_result ts ~init:(map, []) ~f:(fun (map, acc) t_elem ->
        let%bind map, term = update map t_elem in
        match term.ty with
        | TyFloat -> Ok (map, term :: acc)
        | _ -> error_s [%message "mat expected all floats" (ts : Stlc.term list)])
    in
    let size = List.length terms in
    if size = n * m
    then make ~map (Mat (n, m, List.rev terms)) (TyMat (n, m))
    else error_s [%message "mat size mismatch" (n : int) (m : int) (size : int)]
  | Lam (v, ty_v, body) ->
    let map = Map.set map ~key:v ~data:ty_v in
    let%bind map, t = update map body in
    make ~map (Lam (v, ty_v, t)) (TyArrow (ty_v, t.ty))
  | App (f, x) ->
    (* Pure functions with all unique variable names, so passing maps
       like this should be fine to collect them *)
    let%bind map, f = update map f in
    let%bind map, x = update map x in
    (match f.ty with
     | TyArrow (l, r) when equal_ty x.ty l -> make ~map (App (f, x)) r
     | _ -> error_s [%message "invalid app" (f.ty : ty) (x.ty : ty)])
  | Let (Rec (n, ann_ty), v, bind, body) ->
    let map = Map.set map ~key:v ~data:ann_ty in
    let%bind map, bind = update map bind in
    if equal_ty ann_ty bind.ty
    then (
      let%bind map, body = update map body in
      make ~map (Let (Rec (n, ann_ty), v, bind, body)) body.ty)
    else
      error_s
        [%message "typecheck: unexpected type on letrec" (ann_ty : ty) (bind.ty : ty)]
  | Let (Nonrec, v, bind, body) ->
    let%bind map, bind = update map bind in
    let map = Map.set map ~key:v ~data:bind.ty in
    let%bind map, body = update map body in
    make ~map (Let (Nonrec, v, bind, body)) body.ty
  | If (c, t, e) ->
    (* Pure functions with all unique variable names, so passing maps
       like this should be fine to collect them *)
    let%bind map, c = update map c in
    let%bind map, t = update map t in
    let%bind map, e = update map e in
    if not (equal_ty c.ty TyBool)
    then error_s [%message "if cond is not bool" (c.ty : ty)]
    else if not (equal_ty t.ty e.ty)
    then error_s [%message "if/else differs" (t.ty : ty) (e.ty : ty)]
    else make ~map (If (c, t, e)) t.ty
  | Bop (op, l, r) ->
    (* Pure functions with all unique variable names, so passing maps
       like this should be fine to collect them *)
    let%bind map, l = update map l in
    let%bind map, r = update map r in
    let make ty = make ~map (Bop (op, l, r)) ty in
    (match op, l.ty, r.ty with
     | (Add | Sub | Mul | Div | Mod), TyFloat, TyFloat -> make TyFloat
     | (Add | Sub | Mul | Div | Mod), TyInt, TyInt -> make TyInt
     | (Add | Sub | Mul | Div | Mod), TyVec n, TyVec n' when n = n' -> make (TyVec n)
     | (Mul | Div), TyVec n, TyFloat | (Mul | Div), TyFloat, TyVec n -> make (TyVec n)
     | (Add | Sub | Mul | Div | Mod), TyMat (x, y), TyMat (x', y') when x = x' && y = y'
       -> make (TyMat (x, y))
     | (Mul | Div), TyMat (x, y), TyFloat | (Mul | Div), TyFloat, TyMat (x, y) ->
       make (TyMat (x, y))
     | (Mul | Div), TyMat (x, y), TyVec n when y = n -> make (TyVec x)
     | (Add | Sub | Mul | Div | Mod), _, _ ->
       error_s [%message "bop expected int/float" (l.ty : ty) (r.ty : ty)]
     | Eq, TyFloat, TyFloat
     | Eq, TyInt, TyInt
     | Eq, TyBool, TyBool
     | Eq, TyVec _, TyVec _
     | Eq, TyMat _, TyMat _ -> make TyBool
     | Eq, _, _ -> error_s [%message "unsupported eq" (l.ty : ty) (r.ty : ty)]
     | (Lt | Gt | Leq | Geq), TyFloat, TyFloat | (Lt | Gt | Leq | Geq), TyInt, TyInt ->
       make TyBool
     | (Lt | Gt | Leq | Geq), _, _ ->
       error_s [%message "bop expected int/float" (l.ty : ty) (r.ty : ty)]
     | (And | Or), TyBool, TyBool -> make TyBool
     | (And | Or), _, _ ->
       error_s [%message "and/or expected bools" (l.ty : ty) (r.ty : ty)])
  | Index (t, i) ->
    let%bind map, t = update map t in
    let make = make ~map (Index (t, i)) in
    (match t.ty with
     | TyVec n ->
       if 0 <= i && i < n
       then make TyFloat
       else error_s [%message "vec index out of bounds" (n : int) (i : int)]
     | TyMat (x, y) ->
       if 0 <= i && i < x
       then make (TyVec y)
       else error_s [%message "mat index out of bounds" (x : int) (i : int)]
     | ty -> error_s [%message "expected vec or mat" (ty : ty)])
  | Builtin (name, args) ->
    let%bind map, args =
      List.fold_result args ~init:(map, []) ~f:(fun (map, acc) t_arg ->
        let%bind map, term = update map t_arg in
        Ok (map, term :: acc))
    in
    let args = List.rev args in
    let tys = List.map ~f:(fun arg -> arg.ty) args in
    let make = make ~map (Builtin (name, args)) in
    (* TODO: "Row Polymorphism" behavior implemented like you expect in GLSL? *)
    let check_unary_math () =
      match tys with
      | [ TyFloat ] -> make TyFloat
      | [ TyVec n ] -> make (TyVec n)
      | _ ->
        error_s [%message "expected float or vec" (name : Glsl.builtin) (tys : ty list)]
    in
    let check_binary_math () =
      match tys with
      | [ TyFloat; TyFloat ] -> make TyFloat
      | [ TyVec n; TyVec n' ] when n = n' -> make (TyVec n)
      | [ TyVec n; TyFloat ] | [ TyFloat; TyVec n ] -> make (TyVec n)
      | _ ->
        error_s [%message "expected floats or vecs" (name : Glsl.builtin) (tys : ty list)]
    in
    let check_geometric () =
      match name, tys with
      | Length, [ TyVec _ ] | Length, [ TyFloat ] -> make TyFloat
      | Distance, [ TyVec n; TyVec n' ] when n = n' -> make TyFloat
      | Distance, [ TyFloat; TyFloat ] -> make TyFloat
      | Dot, [ TyVec n; TyVec n' ] when n = n' -> make TyFloat
      | Dot, [ TyFloat; TyFloat ] -> make TyFloat
      | Cross, [ TyVec 3; TyVec 3 ] -> make (TyVec 3)
      | Normalize, [ TyVec n ] -> make (TyVec n)
      | Normalize, [ TyFloat ] -> make TyFloat
      | _ ->
        error_s [%message "invalid geometric call" (name : Glsl.builtin) (tys : ty list)]
    in
    let check_common () =
      match name, tys with
      | (Abs | Sign | Floor | Ceil), _ -> check_unary_math ()
      | (Min | Max), _ -> check_binary_math ()
      | Clamp, [ TyFloat; TyFloat; TyFloat ] -> make TyFloat
      | Clamp, [ TyVec n; TyFloat; TyFloat ] -> make (TyVec n)
      | Clamp, [ TyVec n; TyVec n'; TyVec n'' ] when n = n' && n' = n'' -> make (TyVec n)
      | Mix, [ TyFloat; TyFloat; TyFloat ] -> make TyFloat
      | Mix, [ TyVec n; TyVec n'; TyFloat ] when n = n' -> make (TyVec n)
      | Mix, [ TyVec n; TyVec n'; TyVec n'' ] when n = n' && n' = n'' -> make (TyVec n)
      | _ ->
        error_s [%message "invalid common call" (name : Glsl.builtin) (tys : ty list)]
    in
    (match name with
     | Sin | Cos | Tan | Asin | Acos | Atan | Exp | Log | Exp2 | Log2 | Sqrt ->
       check_unary_math ()
     | Pow -> check_binary_math ()
     | Length | Distance | Dot | Cross | Normalize -> check_geometric ()
     | Abs | Sign | Floor | Ceil | Min | Max | Clamp | Mix -> check_common ())
;;

let typecheck (Program terms : Stlc.t) : t Or_error.t =
  let open Or_error.Let_syntax in
  let%map _, tops =
    List.fold_result terms ~init:(String.Map.empty, []) ~f:(fun (map, acc) top ->
      match top.desc with
      | Define (Rec (n, ann_ty), v, bind) ->
        let map = Map.set map ~key:v ~data:ann_ty in
        let%bind map, t = update map bind in
        if equal_ty ann_ty t.ty
        then (
          let desc = Define (Rec (n, ann_ty), v, t) in
          Ok (map, { desc; ty = t.ty; loc = top.loc } :: acc))
        else
          error_s
            [%message "typecheck: unexpected type on letrec" (ann_ty : ty) (t.ty : ty)]
      | Define (Nonrec, v, bind) ->
        let%bind map, t = update map bind in
        let map = Map.set map ~key:v ~data:t.ty in
        Ok (map, { desc = Define (Nonrec, v, t); ty = t.ty; loc = top.loc } :: acc)
      | Extern (ty, v) ->
        let map = Map.set map ~key:v ~data:ty in
        Ok (map, { desc = Extern v; ty; loc = top.loc } :: acc))
  in
  Program (List.rev tops)
;;
