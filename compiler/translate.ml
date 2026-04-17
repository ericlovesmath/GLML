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
  | TyRecord s | TyVariant s -> Ok (TyStruct s)
  | TyArrow _ ->
    Err.fail "arrow types should not be translated" ~d:[%message (ty : Monomorphize.ty)]
;;

let to_glsl_atom (a : Remove_placeholder.atom) : term =
  match a.desc with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
;;

let to_glsl_term (t : Remove_placeholder.term) : term Compiler_error.t =
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
    Err.fail "should be handled in [tr_block]" ~d:[%message (t : Remove_placeholder.term)]
  | Switch _ ->
    Err.fail
      "should be handled in [translate_block]"
      ~d:[%message (t : Remove_placeholder.term)]
;;

let translate_return
      (translate_sub : Remove_placeholder.anf -> stmt list Compiler_error.t)
      (k : term -> stmt)
      (t : Remove_placeholder.term)
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
          (bind : Remove_placeholder.term)
          (ty : ty)
          (tail : stmt list)
  : stmt list Compiler_error.t
  =
  match bind.desc with
  | If (c, t, e) ->
    let%bind t = translate_set env v t in
    let%bind e = translate_set env v e in
    Ok
      (Decl (None, ty, v, None)
       :: IfStmt (to_glsl_atom c, Block t, Some (Block e))
       :: tail)
  | Switch (tag, cases) ->
    let%bind cases =
      cases
      |> List.map ~f:(fun (label, case) ->
        let%map stmts = translate_set env v case in
        label, stmts @ [ Break ])
      |> Compiler_error.all
    in
    Ok (Decl (None, ty, v, None) :: SwitchStmt (to_glsl_atom tag, cases) :: tail)
  | _ ->
    let%map bind = to_glsl_term bind in
    Decl (None, ty, v, Some bind) :: tail

and translate_set (env : record_env) (var : string) (anf : Remove_placeholder.anf)
  : stmt list Compiler_error.t
  =
  match anf.desc with
  | Let (v, term, body) ->
    let%bind ty = to_glsl_ty term.ty in
    let%bind tail = translate_set env var body in
    translate_let env v term ty tail
  | Placeholder (v, body) ->
    let%bind ty = to_glsl_ty anf.ty in
    let placeholder = Decl (None, ty, v, None) in
    let%bind tail = translate_set env var body in
    Ok (placeholder :: tail)
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

and translate_block (env : record_env) (anf : Remove_placeholder.anf)
  : stmt list Compiler_error.t
  =
  match anf.desc with
  | Let (v, bind, body) ->
    let%bind ty = to_glsl_ty bind.ty in
    let%bind tail = translate_block env body in
    translate_let env v bind ty tail
  | Placeholder (v, body) ->
    let%bind ty = to_glsl_ty anf.ty in
    let placeholder = Decl (None, ty, v, None) in
    let%bind tail = translate_block env body in
    Ok (placeholder :: tail)
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

(* TODO: Replace this sad generation logic to some special type in [lift_consts.ml].
   Right now we have to explicitly fold and expand, but that shouldn't be necessary
   if we just make a special kind of term that doesn't need inlining.

   But maybe this is just fine...? *)
let build_const_term body =
  let subst_atom (subs : (string * term) list) (a : Remove_placeholder.atom) : term =
    match a.desc with
    | Var v ->
      (match List.Assoc.find subs v ~equal:String.equal with
       | Some t -> t
       | None -> Var v)
    | Float f -> Float f
    | Int i -> Int i
    | Bool b -> Bool b
  in
  let translate_const_term subs (t : Remove_placeholder.term) : term option =
    let sa = subst_atom subs in
    match t.desc with
    | Atom a -> Some (sa a)
    | Bop (op, l, r) -> Some (Bop (op, sa l, sa r))
    | Vec (n, ts) -> Some (App ([%string "vec%{n#Int}"], List.map ts ~f:sa))
    | Mat (x, y, ts) ->
      let tname =
        if x = y then [%string "mat%{x#Int}"] else [%string "mat%{x#Int}x%{y#Int}"]
      in
      Some (App (tname, List.map ts ~f:sa))
    | Builtin (f, ts) -> Some (Builtin (f, List.map ts ~f:sa))
    | Record (s, ts) -> Some (App (s, List.map ts ~f:sa))
    | Index (a, i) -> Some (Index (sa a, i))
    | Field (a, f) -> Some (Swizzle (sa a, f))
    | App _ | If _ | Switch _ -> None
  in
  let rec eval_const subs (anf : Remove_placeholder.anf) : term option =
    match anf.desc with
    | Return t -> translate_const_term subs t
    | Let (v, term, rest) ->
      (match translate_const_term subs term with
       | Some glsl_t -> eval_const ((v, glsl_t) :: subs) rest
       | None -> None)
    | _ -> None
  in
  eval_const [] body
;;

let translate (Program tops : Remove_placeholder.t) : Glsl.t Compiler_error.t =
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
    |> List.map ~f:(fun (top : Remove_placeholder.top) ->
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
        (match build_const_term body with
         | Some glsl_t ->
           let%bind ty = to_glsl_ty top.ty in
           Ok (Global (Const, ty, name, Some glsl_t))
         | None ->
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
          ~d:[%message (top : Remove_placeholder.top)])
    |> Compiler_error.all
  in
  Ok (Program tops)
;;
