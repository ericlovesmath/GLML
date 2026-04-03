open Core
open Glsl
open Compiler_error.Let_syntax

module Err = Compiler_error.Pass (struct
    let name = "translate"
  end)

type record_env = (string * Monomorphize.ty) list String.Map.t

let to_glsl_ty (ty : Monomorphize.ty) : ty Compiler_error.t =
  match ty with
  | TyFloat -> Ok TyFloat
  | TyInt -> Ok TyInt
  | TyBool -> Ok TyBool
  | TyVec n -> Ok (TyVec n)
  | TyMat (x, y) -> Ok (TyMat (x, y))
  | TyRecord s -> Ok (TyStruct s)
  | TyVariant _ ->
    Err.fail "variant types shouldn't exist" ~d:[%message (ty : Monomorphize.ty)]
  | TyArrow _ ->
    Err.fail "arrow types should not be translated" ~d:[%message (ty : Monomorphize.ty)]
;;

let to_glsl_atom (a : Anf.atom) : term =
  match a.desc with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
;;

let to_glsl_term (t : Lower_variants.term) : term Compiler_error.t =
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
    Err.fail "should be handled in [tr_block]" ~d:[%message (t : Lower_variants.term)]
  | Switch _ ->
    Err.fail
      "should be handled in [translate_block]"
      ~d:[%message (t : Lower_variants.term)]
;;

let rec placeholder_value_for_ty (env : record_env) (ty : Monomorphize.ty)
  : term Compiler_error.t
  =
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
    let%bind fields = Map.find_or_error env s |> Err.of_or_error in
    let%bind fields =
      fields
      |> List.map ~f:snd
      |> List.map ~f:(placeholder_value_for_ty env)
      |> Compiler_error.all
    in
    Ok (App (s, fields))
  | TyVariant _ ->
    Err.fail "variant types should not exist" ~d:[%message (ty : Monomorphize.ty)]
  | TyArrow _ ->
    Err.fail "arrow types should not be in tail" ~d:[%message (ty : Monomorphize.ty)]
;;

let translate_return
      (translate_sub : Lower_variants.anf -> stmt list Compiler_error.t)
      (k : term -> stmt)
      (t : Lower_variants.term)
  : stmt list Compiler_error.t
  =
  match t.desc with
  | If (c, t, e) ->
    let%bind t = translate_sub t in
    let%bind e = translate_sub e in
    Ok [ IfStmt (to_glsl_atom c, Block t, Some (Block e)) ]
  | Switch (tag, cases) ->
    let%map cases =
      cases
      |> List.map ~f:(fun (label, case_anf) ->
        let%map stmts = translate_sub case_anf in
        label, stmts @ [ Break ])
      |> Compiler_error.all
    in
    [ SwitchStmt (to_glsl_atom tag, cases) ]
  | _ ->
    let%map t = to_glsl_term t in
    [ k t ]
;;

let rec translate_let
          (env : record_env)
          (v : string)
          (bind : Lower_variants.term)
          (ty : ty)
          (tail : stmt list)
  : stmt list Compiler_error.t
  =
  match bind.desc with
  | If (c, t, e) ->
    let%bind placeholder = placeholder_value_for_ty env bind.ty in
    let%bind t = translate_set env v t in
    let%bind e = translate_set env v e in
    Ok
      (Decl (None, ty, v, placeholder)
       :: IfStmt (to_glsl_atom c, Block t, Some (Block e))
       :: tail)
  | Switch (tag, cases) ->
    let%bind placeholder = placeholder_value_for_ty env bind.ty in
    let%bind cases =
      cases
      |> List.map ~f:(fun (label, case) ->
        let%map stmts = translate_set env v case in
        label, stmts @ [ Break ])
      |> Compiler_error.all
    in
    Ok (Decl (None, ty, v, placeholder) :: SwitchStmt (to_glsl_atom tag, cases) :: tail)
  | _ ->
    let%map bind = to_glsl_term bind in
    Decl (None, ty, v, bind) :: tail

and translate_set (env : record_env) (var : string) (anf : Lower_variants.anf)
  : stmt list Compiler_error.t
  =
  match anf.desc with
  | Let (v, term, body) ->
    let%bind ty = to_glsl_ty term.ty in
    let%bind tail = translate_set env var body in
    translate_let env v term ty tail
  | Return t -> translate_return (translate_set env var) (fun t -> Set (Var var, t)) t
  | While (cond, body, tail) ->
    let%bind cond = to_glsl_term cond in
    let%bind body = translate_block env body in
    let%bind tail = translate_set env var tail in
    Ok ([ WhileStmt (cond, Block body) ] @ tail)
  | Set (v, a, tail) ->
    let%map tail = translate_set env var tail in
    Set (Var v, to_glsl_atom a) :: tail
  | Continue -> Ok [ Continue ]

and translate_block (env : record_env) (anf : Lower_variants.anf)
  : stmt list Compiler_error.t
  =
  match anf.desc with
  | Let (v, bind, body) ->
    let%bind ty = to_glsl_ty bind.ty in
    let%bind tail = translate_block env body in
    translate_let env v bind ty tail
  | Return t -> translate_return (translate_block env) (fun t -> Return (Some t)) t
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

let translate (Program tops : Lower_variants.t) : Glsl.t Compiler_error.t =
  let%bind env =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | TypeDef (s, RecordDecl fields) -> Some (s, fields)
      | TypeDef (_, VariantDecl _) -> None
      | Define _ | Extern _ | Const _ -> None)
    |> String.Map.of_alist_or_error
    |> Err.of_or_error
  in
  let%bind tops =
    tops
    |> List.map ~f:(fun (top : Lower_variants.top) ->
      let loc = top.loc in
      match top.desc with
      | Define { name; args; body; ret_ty } ->
        let%bind ret_type = to_glsl_ty ret_ty in
        let%bind params =
          args
          |> List.map ~f:(fun (arg, arg_ty) ->
            let%map arg_ty = to_glsl_ty arg_ty in
            arg_ty, arg)
          |> Compiler_error.all
        in
        let%bind body = translate_block env body in
        Ok (Function { name; desc = None; params; ret_type; body })
      | Const (name, body) ->
        (match body.desc with
         | Return { desc = Atom a; _ } ->
           let%map ty = to_glsl_ty top.ty in
           Global (Const, ty, name, Some (to_glsl_atom a))
         | _ ->
           Err.fail "top-level constant must be atomic" ~loc ~d:[%message (name : string)])
      | Extern v ->
        let%map ty = to_glsl_ty top.ty in
        Global (Uniform, ty, v, None)
      | TypeDef (s, RecordDecl fields) ->
        let%map fields =
          fields
          |> List.map ~f:(fun (arg, arg_ty) ->
            let%map arg_ty = to_glsl_ty arg_ty in
            arg_ty, arg)
          |> Compiler_error.all
        in
        Struct (s, fields)
      | TypeDef (_, VariantDecl _) ->
        Err.fail
          "VariantDecl should have been lowered"
          ~loc
          ~d:[%message (top : Lower_variants.top)])
    |> Compiler_error.all
  in
  Ok (Program tops)
;;
