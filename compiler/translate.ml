open Core
open Glsl
open Or_error.Let_syntax

type record_env = (string * Stlc.ty) list String.Map.t

let to_glsl_ty (ty : Stlc.ty) : ty Or_error.t =
  match ty with
  | TyFloat -> Ok TyFloat
  | TyInt -> Ok TyInt
  | TyBool -> Ok TyBool
  | TyVec n -> Ok (TyVec n)
  | TyMat (x, y) -> Ok (TyMat (x, y))
  | TyRecord s -> Ok (TyStruct s)
  | TyArrow _ ->
    error_s [%message "translate: arrow types should not be translated" (ty : Stlc.ty)]
  | TyVar _ ->
    error_s [%message "translate: type variables should not be translated" (ty : Stlc.ty)]
;;

let to_glsl_atom (a : Anf.atom) : term =
  match a with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
;;

let to_glsl_term (t : Tail_call.term) : term Or_error.t =
  match t.desc with
  | Atom a -> Ok (to_glsl_atom a)
  | Bop (op, l, r) -> Ok (Bop (op, to_glsl_atom l, to_glsl_atom r))
  | Vec (n, ts) ->
    let args = List.map ts ~f:to_glsl_atom in
    Ok (App ([%string "vec%{n#Int}"], args))
  | Mat (x, y, ts) ->
    let args = List.map ts ~f:to_glsl_atom in
    let ty =
      if x = y then [%string "mat%{x#Int}"] else [%string "mat%{x#Int}x%{y#Int}"]
    in
    Ok (App (ty, args))
  | Index (t, i) -> Ok (Index (to_glsl_atom t, i))
  | Builtin (f, args) -> Ok (Builtin (f, List.map args ~f:to_glsl_atom))
  | Record (s, args) -> Ok (App (s, List.map args ~f:to_glsl_atom))
  | Field (a, f) -> Ok (Swizzle (to_glsl_atom a, f))
  | App (f, args) -> Ok (App (f, List.map args ~f:to_glsl_atom))
  | If _ ->
    error_s
      [%message "to_glsl_term: should be handled in [tr_block]" (t : Tail_call.term)]
;;

let rec placeholder_value_for_ty (env : record_env) (ty : Stlc.ty) : term Or_error.t =
  match ty with
  | TyFloat -> Ok (Float 0.0)
  | TyInt -> Ok (Int 0)
  | TyBool -> Ok (Bool false)
  | TyVec n -> Ok (App ([%string "vec%{n#Int}"], [ Float 0.0 ]))
  | TyMat (x, y) ->
    let ty =
      if x = y then [%string "mat%{x#Int}"] else [%string "mat%{x#Int}x%{y#Int}"]
    in
    Ok (App (ty, [ Float 0.0 ]))
  | TyRecord s ->
    let%bind fields = Map.find_or_error env s in
    let%bind fields =
      fields
      |> List.map ~f:snd
      |> List.map ~f:(placeholder_value_for_ty env)
      |> Or_error.all
    in
    Ok (App (s, fields))
  | TyArrow _ ->
    error_s [%message "translate: arrow types should not be in tail" (ty : Stlc.ty)]
  | TyVar _ ->
    error_s [%message "translate: type variables should not be in tail" (ty : Stlc.ty)]
;;

let rec translate_set (env : record_env) (var : string) (anf : Tail_call.anf)
  : stmt list Or_error.t
  =
  match anf.desc with
  | Let (v, term, body) ->
    let%bind ty = to_glsl_ty term.ty in
    let%bind term = to_glsl_term term in
    let%bind tail = translate_set env var body in
    Ok (Decl (None, ty, v, term) :: tail)
  | Return t ->
    (match t.desc with
     | If (c, t, e) ->
       let%bind t = translate_set env var t in
       let%bind e = translate_set env var e in
       Ok [ IfStmt (to_glsl_atom c, Block t, Some (Block e)) ]
     | _ ->
       let%map t = to_glsl_term t in
       [ Set (Var var, t) ])
  | While (cond, body, tail) ->
    let%bind cond = to_glsl_term cond in
    let%bind body = translate_block env body in
    let%bind tail = translate_set env var tail in
    Ok ([ WhileStmt (cond, Block body) ] @ tail)
  | Set (v, a, tail) ->
    let%map tail = translate_set env var tail in
    Set (Var v, to_glsl_atom a) :: tail
  | Continue -> Ok [ Continue ]

and translate_block (env : record_env) (anf : Tail_call.anf) : stmt list Or_error.t =
  match anf.desc with
  | Let (v, term, body) ->
    let%bind ty = to_glsl_ty term.ty in
    let%bind body = translate_block env body in
    (match term.desc with
     | If (c, t, e) ->
       let%bind placeholder = placeholder_value_for_ty env term.ty in
       let%bind t = translate_set env v t in
       let%bind e = translate_set env v e in
       Ok
         (Decl (None, ty, v, placeholder)
          :: IfStmt (to_glsl_atom c, Block t, Some (Block e))
          :: body)
     | _ ->
       let%map term = to_glsl_term term in
       Decl (None, ty, v, term) :: body)
  | Return t ->
    (match t.desc with
     | If (c, t, e) ->
       let%bind t = translate_block env t in
       let%bind e = translate_block env e in
       Ok [ IfStmt (to_glsl_atom c, Block t, Some (Block e)) ]
     | _ ->
       let%map t = to_glsl_term t in
       [ Return (Some t) ])
  | While (cond, body, tail) ->
    let%bind cond = to_glsl_term cond in
    let%bind body = translate_block env body in
    let%bind tail = translate_block env tail in
    Ok ([ WhileStmt (cond, Block body) ] @ tail)
  | Set (v, a, tail) ->
    let%map tail = translate_block env tail in
    Set (Var v, to_glsl_atom a) :: tail
  | Continue -> Ok [ Continue ]
;;

let translate (Program tops : Tail_call.t) : t Or_error.t =
  let%bind env =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | RecordDef (s, fields) -> Some (s, fields)
      | Define _ | Extern _ | Const _ -> None)
    |> String.Map.of_alist_or_error
  in
  let%bind tops =
    tops
    |> List.map ~f:(fun (top : Tail_call.top) ->
      match top.desc with
      | Define { name; args; body; ret_ty } ->
        let%bind ret_type = to_glsl_ty ret_ty in
        let%bind params =
          args
          |> List.map ~f:(fun (arg, arg_ty) ->
            let%map arg_ty = to_glsl_ty arg_ty in
            arg_ty, arg)
          |> Or_error.all
        in
        let%bind body = translate_block env body in
        Ok (Function { name; desc = None; params; ret_type; body })
      (* TODO: We need to have constant folding and inlining before this works *)
      | Const _ -> error_s [%message "translate: toplevel const is unsupported"]
      | Extern v ->
        let%map ty = to_glsl_ty top.ty in
        Global (Uniform, ty, v)
      | RecordDef (s, fields) ->
        let%map fields =
          fields
          |> List.map ~f:(fun (arg, arg_ty) ->
            let%map arg_ty = to_glsl_ty arg_ty in
            arg_ty, arg)
          |> Or_error.all
        in
        Struct (s, fields))
    |> Or_error.all
  in
  Ok (Program tops)
;;
