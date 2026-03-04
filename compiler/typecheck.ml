open Core
open Stlc

type t = Program of ty String.Map.t * top list [@@deriving sexp_of]

let rec update (map : ty String.Map.t) (t : term) : (ty String.Map.t * ty) Or_error.t =
  let open Or_error.Let_syntax in
  match t.desc with
  | Var v ->
    (match Map.find map v with
     | Some ty -> Ok (map, ty)
     | None ->
       error_s
         [%message
           "typecheck: var not found in type map"
             (v : string)
             (map : ty String.Map.t)
             (t.loc : Lexer.loc)])
  | Float _ -> Ok (map, TyFloat)
  | Int _ -> Ok (map, TyInt)
  | Bool _ -> Ok (map, TyBool)
  | Vec (n, ts) ->
    let%bind map, tys =
      List.fold_result ts ~init:(map, []) ~f:(fun (map, acc) t_elem ->
        let%bind map, ty = update map t_elem in
        match ty with
        | TyFloat -> Ok (map, ty :: acc)
        | _ ->
          error_s
            [%message
              "typecheck: vec expected all floats" (ts : term list) (t.loc : Lexer.loc)])
    in
    let size = List.length tys in
    if size = n
    then Ok (map, TyVec n)
    else
      error_s
        [%message
          "typecheck: vec size mismatch" (n : int) (size : int) (t.loc : Lexer.loc)]
  | Mat (x, y, ts) ->
    let%bind map, tys =
      List.fold_result ts ~init:(map, []) ~f:(fun (map, acc) t_elem ->
        let%bind map, ty = update map t_elem in
        match ty with
        | TyFloat -> Ok (map, ty :: acc)
        | _ ->
          error_s
            [%message
              "typecheck: mat expected all floats" (ts : term list) (t.loc : Lexer.loc)])
    in
    let size = List.length tys in
    if size = x * y
    then Ok (map, TyMat (x, y))
    else
      error_s
        [%message
          "typecheck: mat size mismatch"
            (x : int)
            (y : int)
            (size : int)
            (t.loc : Lexer.loc)]
  | Lam (v, ty_v, body) ->
    let map = Map.set map ~key:v ~data:ty_v in
    let%bind map, ty_t = update map body in
    Ok (map, TyArrow (ty_v, ty_t))
  | App (f, x) ->
    (* Pure functions with all unique variable names, so passing maps
       like this should be fine to collect them *)
    let%bind map, ty_f = update map f in
    let%bind map, ty_x = update map x in
    (match ty_f with
     | TyArrow (l, r) when equal_ty ty_x l -> Ok (map, r)
     | _ ->
       error_s
         [%message "typecheck: invalid app" (ty_f : ty) (ty_x : ty) (t.loc : Lexer.loc)])
  | Let (v, bind, body) ->
    let%bind map, ty_v = update map bind in
    let map = Map.set map ~key:v ~data:ty_v in
    update map body
  | If (c, t_true, e) ->
    (* Pure functions with all unique variable names, so passing maps
       like this should be fine to collect them *)
    let%bind map, ty_c = update map c in
    let%bind map, ty_t = update map t_true in
    let%bind map, ty_e = update map e in
    if not (equal_ty ty_c TyBool)
    then
      error_s [%message "typecheck: if cond is not bool" (ty_c : ty) (t.loc : Lexer.loc)]
    else if not (equal_ty ty_t ty_e)
    then
      error_s
        [%message
          "typecheck: if/else differs" (ty_t : ty) (ty_e : ty) (t.loc : Lexer.loc)]
    else Ok (map, ty_t)
  | Bop (op, l, r) ->
    (* Pure functions with all unique variable names, so passing maps
       like this should be fine to collect them *)
    let%bind map, ty_l = update map l in
    let%bind map, ty_r = update map r in
    (match op, ty_l, ty_r with
     | (Add | Sub | Mul | Div | Mod), TyFloat, TyFloat -> Ok (map, TyFloat)
     | (Add | Sub | Mul | Div | Mod), TyInt, TyInt -> Ok (map, TyInt)
     | (Add | Sub | Mul | Div | Mod), TyVec n, TyVec n' when n = n' -> Ok (map, TyVec n)
     | (Mul | Div), TyVec n, TyFloat | (Mul | Div), TyFloat, TyVec n -> Ok (map, TyVec n)
     | (Add | Sub | Mul | Div | Mod), TyMat (x, y), TyMat (x', y') when x = x' && y = y'
       -> Ok (map, TyMat (x, y))
     | (Mul | Div), TyMat (x, y), TyFloat | (Mul | Div), TyFloat, TyMat (x, y) ->
       Ok (map, TyMat (x, y))
     | (Mul | Div), TyMat (x, y), TyVec n when y = n -> Ok (map, TyVec x)
     | (Add | Sub | Mul | Div | Mod), _, _ ->
       error_s
         [%message
           "typecheck: bop expected int/float" (ty_l : ty) (ty_r : ty) (t.loc : Lexer.loc)]
     | Eq, TyFloat, TyFloat
     | Eq, TyInt, TyInt
     | Eq, TyBool, TyBool
     | Eq, TyVec _, TyVec _
     | Eq, TyMat _, TyMat _
     | Eq, _, _ ->
       error_s
         [%message "typecheck: unsupport eq" (ty_l : ty) (ty_r : ty) (t.loc : Lexer.loc)]
     | (Lt | Gt | Leq | Geq), TyFloat, TyFloat | (Lt | Gt | Leq | Geq), TyInt, TyInt ->
       Ok (map, TyBool)
     | (Lt | Gt | Leq | Geq), _, _ ->
       error_s
         [%message
           "typecheck: bop expected int/float" (ty_l : ty) (ty_r : ty) (t.loc : Lexer.loc)]
     | (And | Or), TyBool, TyBool -> Ok (map, TyBool)
     | (And | Or), _, _ ->
       error_s
         [%message
           "typecheck: and/or expected bools" (ty_l : ty) (ty_r : ty) (t.loc : Lexer.loc)])
  | Index (t_sub, i) ->
    let%bind map, ty = update map t_sub in
    (match ty with
     | TyVec n ->
       if 0 <= i && i < n
       then Ok (map, TyFloat)
       else
         error_s
           [%message
             "typecheck: vec index out of bounds" (n : int) (i : int) (t.loc : Lexer.loc)]
     | TyMat (x, y) ->
       if 0 <= i && i < x
       then Ok (map, TyVec y)
       else
         error_s
           [%message
             "typecheck: mat index out of bounds" (x : int) (i : int) (t.loc : Lexer.loc)]
     | _ ->
       error_s [%message "typecheck: expected vec or mat" (ty : ty) (t.loc : Lexer.loc)])
  | Builtin (name, args) ->
    let%bind map, tys =
      List.fold_result args ~init:(map, []) ~f:(fun (map, acc) t_arg ->
        let%bind map, ty = update map t_arg in
        Ok (map, ty :: acc))
    in
    let tys = List.rev tys in
    let check_unary_math () =
      match tys with
      | [ TyFloat ] -> Ok (map, TyFloat)
      | [ TyVec n ] -> Ok (map, TyVec n)
      | _ ->
        error_s
          [%message
            "typecheck: expected float or vec"
              (name : Glsl.builtin)
              (tys : ty list)
              (t.loc : Lexer.loc)]
    in
    let check_binary_math () =
      match tys with
      | [ TyFloat; TyFloat ] -> Ok (map, TyFloat)
      | [ TyVec n; TyVec n' ] when n = n' -> Ok (map, TyVec n)
      | [ TyVec n; TyFloat ] | [ TyFloat; TyVec n ] -> Ok (map, TyVec n)
      | _ ->
        error_s
          [%message
            "typecheck: expected floats or vecs"
              (name : Glsl.builtin)
              (tys : ty list)
              (t.loc : Lexer.loc)]
    in
    let check_geometric () =
      match name, tys with
      | Length, [ TyVec _ ] | Length, [ TyFloat ] -> Ok (map, TyFloat)
      | Distance, [ TyVec n; TyVec n' ] when n = n' -> Ok (map, TyFloat)
      | Distance, [ TyFloat; TyFloat ] -> Ok (map, TyFloat)
      | Dot, [ TyVec n; TyVec n' ] when n = n' -> Ok (map, TyFloat)
      | Dot, [ TyFloat; TyFloat ] -> Ok (map, TyFloat)
      | Cross, [ TyVec 3; TyVec 3 ] -> Ok (map, TyVec 3)
      | Normalize, [ TyVec n ] -> Ok (map, TyVec n)
      | Normalize, [ TyFloat ] -> Ok (map, TyFloat)
      | _ ->
        error_s
          [%message
            "typecheck: invalid geometric call"
              (name : Glsl.builtin)
              (tys : ty list)
              (t.loc : Lexer.loc)]
    in
    let check_common () =
      match name, tys with
      | (Abs | Sign | Floor | Ceil), _ -> check_unary_math ()
      | (Min | Max), _ -> check_binary_math ()
      | Clamp, [ TyFloat; TyFloat; TyFloat ] -> Ok (map, TyFloat)
      | Clamp, [ TyVec n; TyFloat; TyFloat ] -> Ok (map, TyVec n)
      | Clamp, [ TyVec n; TyVec n'; TyVec n'' ] when n = n' && n' = n'' ->
        Ok (map, TyVec n)
      | Mix, [ TyFloat; TyFloat; TyFloat ] -> Ok (map, TyFloat)
      | Mix, [ TyVec n; TyVec n'; TyFloat ] when n = n' -> Ok (map, TyVec n)
      | Mix, [ TyVec n; TyVec n'; TyVec n'' ] when n = n' && n' = n'' -> Ok (map, TyVec n)
      | _ ->
        error_s
          [%message
            "typecheck: invalid common call"
              (name : Glsl.builtin)
              (tys : ty list)
              (t.loc : Lexer.loc)]
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
  let%map map, _ =
    List.fold_result terms ~init:(String.Map.empty, []) ~f:(fun (map, acc) t ->
      match t.desc with
      | Define (v, bind) ->
        let%bind map, ty = update map bind in
        let map = Map.set map ~key:v ~data:ty in
        Ok (map, Define (v, bind) :: acc)
      | Extern (ty, v) ->
        let map = Map.set map ~key:v ~data:ty in
        Ok (map, Extern (ty, v) :: acc))
  in
  Program (map, terms)
;;
