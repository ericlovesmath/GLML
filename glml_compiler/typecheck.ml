open Core
open Stlc

type t = Program of ty String.Map.t * top list [@@deriving sexp_of]

let rec update (map : ty String.Map.t) (t : term) : (ty String.Map.t * ty) Or_error.t =
  let open Or_error.Let_syntax in
  match t with
  | Var v ->
    (match Map.find map v with
     | Some ty -> Ok (map, ty)
     | None ->
       error_s
         [%message
           "typecheck: var not found in type map" (v : string) (map : ty String.Map.t)])
  | Float _ -> Ok (map, TyFloat)
  | Int _ -> Ok (map, TyInt)
  | Bool _ -> Ok (map, TyBool)
  | Unit -> Ok (map, TyUnit)
  | Vec (n, ts) ->
    let%bind map, tys =
      List.fold_result ts ~init:(map, []) ~f:(fun (map, acc) t ->
        let%bind map, ty = update map t in
        match ty with
        | TyFloat -> Ok (map, ty :: acc)
        | _ -> error_s [%message "typecheck: vec expected all floats" (ts : term list)])
    in
    let size = List.length tys in
    if size = n
    then Ok (map, TyVec n)
    else error_s [%message "typecheck: vec size mismatch" (n : int) (size : int)]
  | Mat (x, y, ts) ->
    let%bind map, tys =
      List.fold_result ts ~init:(map, []) ~f:(fun (map, acc) t ->
        let%bind map, ty = update map t in
        match ty with
        | TyFloat -> Ok (map, ty :: acc)
        | _ -> error_s [%message "typecheck: mat expected all floats" (ts : term list)])
    in
    let size = List.length tys in
    if size = x * y
    then Ok (map, TyMat (x, y))
    else
      error_s [%message "typecheck: mat size mismatch" (x : int) (y : int) (size : int)]
  | Lam (v, ty_v, t) ->
    let map = Map.set map ~key:v ~data:ty_v in
    let%bind map, ty_t = update map t in
    Ok (map, TyArrow (ty_v, ty_t))
  | App (f, x) ->
    (* Pure functions with all unique variable names, so passing maps
       like this should be fine to collect them *)
    let%bind map, ty_f = update map f in
    let%bind map, ty_x = update map x in
    (match ty_f with
     | TyArrow (l, r) when equal_ty ty_x l -> Ok (map, r)
     | _ -> error_s [%message "typecheck: invalid app" (ty_f : ty) (ty_x : ty)])
  | Let (v, bind, body) ->
    let%bind map, ty_v = update map bind in
    let map = Map.set map ~key:v ~data:ty_v in
    update map body
  | If (c, t, e) ->
    (* Pure functions with all unique variable names, so passing maps
       like this should be fine to collect them *)
    let%bind map, ty_c = update map c in
    let%bind map, ty_t = update map t in
    let%bind map, ty_e = update map e in
    if not (equal_ty ty_c TyBool)
    then error_s [%message "typecheck: if cond is not bool" (ty_c : ty)]
    else if not (equal_ty ty_t ty_e)
    then error_s [%message "typecheck: if/else differs" (ty_t : ty) (ty_e : ty)]
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
       error_s [%message "typecheck: bop expected int/float" (ty_l : ty) (ty_r : ty)]
     | Eq, TyFloat, TyFloat
     | Eq, TyInt, TyInt
     | Eq, TyBool, TyBool
     | Eq, TyVec _, TyVec _
     | Eq, TyMat _, TyMat _
     | Eq, _, _ -> error_s [%message "typecheck: unsupport eq" (ty_l : ty) (ty_r : ty)]
     | (Lt | Gt | Leq | Geq), TyFloat, TyFloat | (Lt | Gt | Leq | Geq), TyInt, TyInt ->
       Ok (map, TyBool)
     | (Lt | Gt | Leq | Geq), _, _ ->
       error_s [%message "typecheck: bop expected int/float" (ty_l : ty) (ty_r : ty)]
     | (And | Or), TyBool, TyBool -> Ok (map, TyBool)
     | (And | Or), _, _ ->
       error_s [%message "typecheck: and/or expected bools" (ty_l : ty) (ty_r : ty)])
;;

let typecheck (Program terms : Stlc.t) : t Or_error.t =
  let open Or_error.Let_syntax in
  let%map map, _ =
    List.fold_result terms ~init:(String.Map.empty, []) ~f:(fun (map, acc) t ->
      match t with
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
