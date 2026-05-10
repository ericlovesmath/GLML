open Core
open Glsl

include Compiler_error.Pass (struct
    let name = "translate"
  end)

type record_env = (string * Monomorphize.ty) list String.Map.t

let to_glsl_ty (loc : Lexer.loc) (ty : Monomorphize.ty) : ty =
  match ty with
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec (n, TyFloat) -> TyVec n
  | TyVec (n, TyVec (m, TyFloat)) -> TyMat (n, m)
  | TyRecord s | TyVariant s -> TyStruct s
  | TyArrow _ | TyVec _ ->
    raise "unexpected type" ~loc ~d:[%message (ty : Monomorphize.ty)]
;;

let to_glsl_atom (a : Remove_placeholder.atom) : term =
  match a.desc with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
;;

let to_glsl_term (t : Remove_placeholder.term) : term =
  match t.desc with
  | Atom a -> to_glsl_atom a
  | Bop (op, l, r) -> Bop (op, to_glsl_atom l, to_glsl_atom r)
  | Vec (n, ts) ->
    let ctor =
      match t.ty with
      | TyVec (_, TyVec (m, _)) when n = m -> [%string "mat%{n#Int}"]
      | TyVec (_, TyVec (m, _)) -> [%string "mat%{n#Int}x%{m#Int}"]
      | _ -> [%string "vec%{n#Int}"]
    in
    App (ctor, List.map ~f:to_glsl_atom ts)
  | Index (t, i) -> Index (to_glsl_atom t, i)
  | Builtin (f, args) -> Builtin (f, List.map args ~f:to_glsl_atom)
  | Record args ->
    (match t.ty with
     | TyRecord s -> App (s, List.map args ~f:to_glsl_atom)
     | _ -> raise "Record term does not have type [record]")
  | Field (a, f) -> Swizzle (to_glsl_atom a, f)
  | App (f, args) -> App (f, List.map args ~f:to_glsl_atom)
  | If _ ->
    raise "should be handled in [tr_block]" ~d:[%message (t : Remove_placeholder.term)]
  | Switch _ ->
    raise
      "should be handled in [translate_block]"
      ~d:[%message (t : Remove_placeholder.term)]
;;

let translate_return
      (translate_sub : Remove_placeholder.anf -> stmt list)
      (k : term -> stmt)
      (t : Remove_placeholder.term)
  : stmt list
  =
  match t.desc with
  | If (c, t, e) ->
    let t = translate_sub t in
    let e = translate_sub e in
    [ IfStmt (to_glsl_atom c, Block t, Some (Block e)) ]
  | Switch (tag, cases) ->
    let cases =
      List.map cases ~f:(fun (label, case_anf) ->
        let stmts = translate_sub case_anf in
        label, stmts @ [ Break ])
    in
    [ SwitchStmt (to_glsl_atom tag, cases) ]
  | _ -> [ k (to_glsl_term t) ]
;;

let rec translate_let
          (env : record_env)
          (v : string)
          (bind : Remove_placeholder.term)
          (ty : ty)
          (tail : stmt list)
  : stmt list
  =
  match bind.desc with
  | If (c, t, e) ->
    let t = translate_set env v t in
    let e = translate_set env v e in
    Decl (None, ty, v, None) :: IfStmt (to_glsl_atom c, Block t, Some (Block e)) :: tail
  | Switch (tag, cases) ->
    let cases =
      List.map cases ~f:(fun (label, case) ->
        let stmts = translate_set env v case in
        label, stmts @ [ Break ])
    in
    Decl (None, ty, v, None) :: SwitchStmt (to_glsl_atom tag, cases) :: tail
  | _ -> Decl (None, ty, v, Some (to_glsl_term bind)) :: tail

and translate_set (env : record_env) (var : string) (anf : Remove_placeholder.anf)
  : stmt list
  =
  match anf.desc with
  | Let (v, term, body) ->
    let ty = to_glsl_ty term.loc term.ty in
    let tail = translate_set env var body in
    translate_let env v term ty tail
  | Placeholder (v, body) ->
    let ty = to_glsl_ty anf.loc anf.ty in
    let placeholder = Decl (None, ty, v, None) in
    placeholder :: translate_set env var body
  | Return t -> translate_return (translate_set env var) (fun t -> Set (Var var, t)) t
  | While (cond, body, tail) ->
    let cond = to_glsl_term cond in
    let body = translate_block env body in
    let tail = translate_set env var tail in
    [ WhileStmt (cond, Block body) ] @ tail
  | Set (v, a, tail) ->
    let tail = translate_set env var tail in
    Set (Var v, to_glsl_atom a) :: tail
  | Continue -> [ Continue ]

and translate_block (env : record_env) (anf : Remove_placeholder.anf) : stmt list =
  match anf.desc with
  | Let (v, bind, body) ->
    let ty = to_glsl_ty bind.loc bind.ty in
    let tail = translate_block env body in
    translate_let env v bind ty tail
  | Placeholder (v, body) ->
    let ty = to_glsl_ty anf.loc anf.ty in
    let placeholder = Decl (None, ty, v, None) in
    placeholder :: translate_block env body
  | Return t -> translate_return (translate_block env) (fun t -> Return (Some t)) t
  | While (cond, body, tail) ->
    let cond = to_glsl_term cond in
    let body = translate_block env body in
    let tail = translate_block env tail in
    [ WhileStmt (cond, Block body) ] @ tail
  | Set (v, a, tail) ->
    let tail = translate_block env tail in
    Set (Var v, to_glsl_atom a) :: tail
  | Continue -> [ Continue ]
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
    | Vec (n, ts) ->
      let ctor =
        match t.ty with
        | TyVec (_, TyVec (m, _)) when n = m -> [%string "mat%{n#Int}"]
        | TyVec (_, TyVec (m, _)) -> [%string "mat%{n#Int}x%{m#Int}"]
        | _ -> [%string "vec%{n#Int}"]
      in
      Some (App (ctor, List.map ~f:sa ts))
    | Builtin (f, ts) -> Some (Builtin (f, List.map ts ~f:sa))
    | Record ts ->
      (match t.ty with
       | TyRecord s -> Some (App (s, List.map ts ~f:sa))
       | _ -> raise "Record term does not have type [record]")
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

let translate_exn (Program tops : Remove_placeholder.t) : Glsl.t =
  let env =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | TypeDef (s, RecordDecl fields) -> Some (s, fields)
      | TypeDef (_, VariantDecl _) -> None
      | Define _ | Extern _ | Const _ -> None)
    |> String.Map.of_alist_or_error
    |> of_or_error
    |> ok_exn
  in
  let tops =
    List.map tops ~f:(fun (top : Remove_placeholder.top) ->
      let loc = top.loc in
      match top.desc with
      | Define { name; args; body; ret_ty } ->
        let ret_type = to_glsl_ty top.loc ret_ty in
        let params =
          List.map args ~f:(fun (arg, arg_ty) -> to_glsl_ty top.loc arg_ty, arg)
        in
        let body = translate_block env body in
        Function { name; desc = None; params; ret_type; body }
      | Const (name, body) ->
        (match build_const_term body with
         | Some glsl_t ->
           let ty = to_glsl_ty top.loc top.ty in
           Global (Const, ty, name, Some glsl_t)
         | None ->
           raise "top-level constant must be atomic" ~loc ~d:[%message (name : string)])
      | Extern v ->
        let ty = to_glsl_ty top.loc top.ty in
        Global (Uniform, ty, v, None)
      | TypeDef (s, RecordDecl fields) ->
        let fields =
          List.map fields ~f:(fun (arg, arg_ty) -> to_glsl_ty top.loc arg_ty, arg)
        in
        Struct (s, fields)
      | TypeDef (_, VariantDecl _) ->
        raise
          "VariantDecl should have been lowered"
          ~loc
          ~d:[%message (top : Remove_placeholder.top)])
  in
  Program tops
;;

let translate t = try_with (fun () -> translate_exn t)
