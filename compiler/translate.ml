open Core
open Glsl

type record_env = (string * Stlc.ty) list String.Map.t

let to_glsl_ty (ty : Stlc.ty) : ty =
  match ty with
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec n -> TyVec n
  | TyMat (x, y) -> TyMat (x, y)
  | TyRecord s -> TyStruct s
  (* TODO: Patch failwiths and exn in this file for [Or_error.t] *)
  | TyArrow _ -> failwith "translate: arrow types should not be translated"
;;

let to_glsl_atom (a : Anf.atom) : term =
  match a with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
;;

let to_glsl_term (t : Tail_call.term) : term =
  match t.desc with
  | Atom a -> to_glsl_atom a
  | Bop (op, l, r) -> Bop (op, to_glsl_atom l, to_glsl_atom r)
  | Vec (n, ts) ->
    let args = List.map ts ~f:to_glsl_atom in
    App ([%string "vec%{n#Int}"], args)
  | Mat (x, y, ts) ->
    let args = List.map ts ~f:to_glsl_atom in
    let ty =
      if x = y then [%string "mat%{x#Int}"] else [%string "mat%{x#Int}x%{y#Int}"]
    in
    App (ty, args)
  | Index (t, i) -> Index (to_glsl_atom t, i)
  | Builtin (f, args) -> Builtin (f, List.map args ~f:to_glsl_atom)
  | Record (s, args) -> App (s, List.map args ~f:to_glsl_atom)
  | Field (a, f) -> Swizzle (to_glsl_atom a, f)
  | App (f, args) -> App (f, List.map args ~f:to_glsl_atom)
  | If (_, _, _) ->
    failwith
      [%string "to_glsl_term: complex If with condition should be handled in tr_block"]
;;

let rec placeholder_value_for_ty (env : record_env) (ty : Stlc.ty) : term =
  match ty with
  | TyFloat -> Float 0.0
  | TyInt -> Int 0
  | TyBool -> Bool false
  | TyVec n -> App ([%string "vec%{n#Int}"], [ Float 0.0 ])
  | TyMat (x, y) ->
    let ty =
      if x = y then [%string "mat%{x#Int}"] else [%string "mat%{x#Int}x%{y#Int}"]
    in
    App (ty, [ Float 0.0 ])
  | TyRecord s ->
    let fields = Map.find_exn env s in
    App (s, List.map fields ~f:(fun (_, f_ty) -> placeholder_value_for_ty env f_ty))
  | TyArrow _ -> failwith "arrow"
;;

let rec translate_set (env : record_env) (var : string) (anf : Tail_call.anf) : stmt list =
  match anf.desc with
  | Let (v, bind, body) ->
    Decl (None, to_glsl_ty anf.ty, v, to_glsl_term bind) :: translate_set env var body
  | Return t ->
    (match t.desc with
     | If (c, t, e) ->
       [ IfStmt
           ( to_glsl_atom c
           , Block (translate_set env var t)
           , Some (Block (translate_set env var e)) )
       ]
     | _ -> [ Set (Var var, to_glsl_term t) ])
  | While (cond, body, tail) ->
    [ WhileStmt (to_glsl_term cond, Block (translate_block env body)) ]
    @ translate_set env var tail
  | Set (v, a, tail) -> Set (Var v, to_glsl_atom a) :: translate_set env var tail
  | Continue -> [ Continue ]

and translate_block (env : record_env) (anf : Tail_call.anf) : stmt list =
  match anf.desc with
  | Let (v, term, body) ->
    let ty = to_glsl_ty term.ty in
    let term_ty = term.ty in
    (match term.desc with
     | If (c, t, e) ->
       Decl (None, ty, v, placeholder_value_for_ty env term_ty)
       :: IfStmt
            ( to_glsl_atom c
            , Block (translate_set env v t)
            , Some (Block (translate_set env v e)) )
       :: translate_block env body
     | _ -> Decl (None, ty, v, to_glsl_term term) :: translate_block env body)
  | Return t ->
    (match t.desc with
     | If (c, t, e) ->
       [ IfStmt
           ( to_glsl_atom c
           , Block (translate_block env t)
           , Some (Block (translate_block env e)) )
       ]
     | _ -> [ Return (Some (to_glsl_term t)) ])
  | While (cond, body, tail) ->
    [ WhileStmt (to_glsl_term cond, Block (translate_block env body)) ]
    @ translate_block env tail
  | Set (v, a, tail) -> Set (Var v, to_glsl_atom a) :: translate_block env tail
  | Continue -> [ Continue ]
;;

let translate (Program tops : Tail_call.t) : t =
  let env =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | RecordDef (s, fields) -> Some (s, fields)
      | Define _ | Extern _ | Const _ -> None)
    |> String.Map.of_alist_exn
  in
  let globals =
    List.map tops ~f:(fun (top : Tail_call.top) ->
      match top.desc with
      | Define { name; args; body; ret_ty } ->
        let ret_type = to_glsl_ty ret_ty in
        let params = List.map args ~f:(fun (arg, arg_ty) -> to_glsl_ty arg_ty, arg) in
        let body = translate_block env body in
        Function { name; desc = None; params; ret_type; body }
      (* TODO: We need to have constant folding and inlining before this works *)
      | Const _ -> failwith "translate: toplevel constants are unsupported"
      | Extern v -> Global (Uniform, to_glsl_ty top.ty, v)
      | RecordDef (s, fields) ->
        Struct (s, List.map fields ~f:(fun (f, ty) -> to_glsl_ty ty, f)))
  in
  Program globals
;;
