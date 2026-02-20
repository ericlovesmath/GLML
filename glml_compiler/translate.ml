open Core
open Glsl

let to_glsl_ty (ty : Stlc.ty) : ty =
  match ty with
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec n -> TyVec n
  | TyMat (x, y) -> TyMat (x, y)
  | TyUnit -> failwith "translate: no unit should be left"
  | TyArrow _ -> failwith "translate: arrow types should not be translated"
;;

let to_glsl_atom (a : Anf.atom) : term =
  match a with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
  | Unit -> Int 0
;;

let to_glsl_term (t : Anf.term) : term =
  match t with
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
  | App (f, x) ->
    (match f with
     | Var v -> App (v, [ to_glsl_atom x ])
     | _ -> failwith "to_glsl_term: expected function name in app")
  | If (_, _, _) ->
    failwith
      [%string "to_glsl_term: complex If with condition should be handled in tr_block"]
  | Lam _ -> failwith "to_glsl_term: lambdas not yet supported"
;;

let placeholder_value_for_ty (ty : ty) : term =
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
  | TyVoid -> failwith "translate: void is unsupported"
;;

let rec translate_set (map : Stlc.ty String.Map.t) (var : string) (anf : Anf.anf)
  : stmt list
  =
  match anf with
  | Let (v, bind, body) ->
    let ty = to_glsl_ty (Map.find_exn map v) in
    let stmt = Decl (None, ty, v, to_glsl_term bind) in
    stmt :: translate_set map var body
  | Return t ->
    (match t with
     | If (c, t, e) ->
       [ IfStmt
           ( to_glsl_atom c
           , Block (translate_set map var t)
           , Some (Block (translate_set map var e)) )
       ]
     | _ -> [ Set (Var var, to_glsl_term t) ])
;;

let rec translate_block (map : Stlc.ty String.Map.t) (anf : Anf.anf) : stmt list =
  match anf with
  | Let (v, term, body) ->
    (match term with
     | If (c, t, e) ->
       let ty = to_glsl_ty (Map.find_exn map v) in
       let decl = Decl (None, ty, v, placeholder_value_for_ty ty) in
       let if_stmt =
         IfStmt
           ( to_glsl_atom c
           , Block (translate_set map v t)
           , Some (Block (translate_set map v e)) )
       in
       decl :: if_stmt :: translate_block map body
     | _ ->
       let ty = to_glsl_ty (Map.find_exn map v) in
       let stmt = Decl (None, ty, v, to_glsl_term term) in
       stmt :: translate_block map body)
  | Return t ->
    (match t with
     | If (c, t, e) ->
       [ IfStmt
           ( to_glsl_atom c
           , Block (translate_block map t)
           , Some (Block (translate_block map e)) )
       ]
     | _ -> [ Return (Some (to_glsl_term t)) ])
;;

let rec replace_returns_with_set (var : string) (stmts : stmt list) : stmt list =
  List.concat_map stmts ~f:(fun stmt ->
    match stmt with
    | Return (Some t) -> [ Set (Var var, t); Return None ]
    | Return None -> [ stmt ]
    | IfStmt (c, t, e) ->
      let t =
        match t with
        | Block ss -> Block (replace_returns_with_set var ss)
        | s -> Block (replace_returns_with_set var [ s ])
      in
      let e =
        Option.map e ~f:(fun e ->
          match e with
          | Block ss -> Block (replace_returns_with_set var ss)
          | s -> Block (replace_returns_with_set var [ s ]))
      in
      [ IfStmt (c, t, e) ]
    | Block ss -> [ Block (replace_returns_with_set var ss) ]
    | _ -> [ stmt ])
;;

let translate (Program (map, tops) : Anf.t) : t =
  let globals =
    List.map tops ~f:(fun top ->
      match top with
      | Define ("main", Return (Lam (_, _, body))) ->
        (* TODO: Validate typechecking special case for main *)
        let stmts = translate_block map body in
        let body = replace_returns_with_set "fragColor" stmts in
        Function { name = "main"; desc = None; params = []; ret_type = TyVoid; body }
      | Define (name, Return (Lam (arg, arg_ty, body))) ->
        let ret_type =
          match Map.find_exn map name with
          | TyArrow (_, r) -> to_glsl_ty r
          | _ -> failwith "translate: expected arrow type for function"
        in
        let params = [ to_glsl_ty arg_ty, arg ] in
        let body = translate_block map body in
        Function { name; desc = None; params; ret_type; body }
      | Define (_, Return _) ->
        raise_s [%message "translate: expected lam form at toplevel" (top : Anf.top)]
      | Define (_, Let _) ->
        raise_s [%message "translate: expected return toplevel" (top : Anf.top)]
      | Extern (ty, v) -> Global (Uniform, to_glsl_ty ty, v))
  in
  Program ([ Global (Out, TyVec 3, "fragColor") ] @ globals)
;;
