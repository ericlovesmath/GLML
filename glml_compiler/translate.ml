open Core
open Glsl

let to_glsl_ty (ty : Stlc.ty) : ty =
  match ty with
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec3 -> TyVec 3
  | TyArrow _ -> failwith "translate: arrow types should not be translated"
;;

let to_glsl_atom (a : Anf.atom) : term =
  match a with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
;;

let to_glsl_term (t : Anf.term) : term =
  match t with
  | Atom a -> to_glsl_atom a
  | Bop (op, l, r) -> Bop (op, to_glsl_atom l, to_glsl_atom r)
  | Vec3 (x, y, z) -> App ("vec3", [ to_glsl_atom x; to_glsl_atom y; to_glsl_atom z ])
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
  | TyVec 3 -> App ("vec3", [ Float 0.0 ])
  | TyVec _ -> failwith "translate: non vec3 is unsupported"
  | TyMat _ -> failwith "translate: mat is unsupported"
  | TyVoid -> failwith "translate: if statement shouldn't return void"
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

let translate (Program (map, terms) : Anf.t) : t Or_error.t =
  Ok
    (Program
       [ Global (Out, TyVec 4, "fragColor")
       ; Function
           { name = "main"
           ; desc = None
           ; params = []
           ; ret_type = TyVoid
           ; body =
               translate_block
                 map
                 (* TODO: Clearly do not just want to do that *)
                 (List.hd_exn terms)
           }
       ])
;;
