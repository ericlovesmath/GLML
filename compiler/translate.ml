open Core
open Glsl

include Compiler_error.Pass (struct
    let name = "translate"
  end)

let to_glsl_ty (loc : Lexer.loc) (ty : Lower_variants.ty) : ty =
  match ty with
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec (n, TyFloat) -> TyVec n
  | TyVec (n, TyVec (m, TyFloat)) -> TyMat (n, m)
  | TyRecord s -> TyStruct s
  | TyArrow _ | TyVec _ ->
    raise "unexpected type" ~loc ~d:[%message (ty : Lower_variants.ty)]
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
    raise "should be in [translate_anf]" ~d:[%message (t : Remove_placeholder.term)]
  | Switch _ ->
    raise "should be in [translate_anf]" ~d:[%message (t : Remove_placeholder.term)]
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

(** For [assign_to], [None] means that the tail of the block returns from the
    enclosing functions and [Some v] means it initialized [let v] *)
type ctx =
  { assign_to : string option
  ; loop_params : string list option
  }

let emit_return ctx t =
  match ctx.assign_to with
  | None -> Return (Some t)
  | Some v -> Set (Var v, t)
;;

let translate_continue
      ~(loc : Lexer.loc)
      (params : string list)
      (args : Remove_placeholder.atom list)
  : stmt list
  =
  let writes =
    match List.zip params args with
    | Unequal_lengths -> raise "continue arity mismatch with loop params" ~loc
    | Ok pairs ->
      List.filter pairs ~f:(fun (name, a) ->
        match a.desc with
        | Var v -> not (String.equal v name)
        | _ -> true)
  in
  let targets = String.Set.of_list (List.map writes ~f:fst) in
  let needs_temps =
    List.exists writes ~f:(fun (_, a) ->
      match a.desc with
      | Var v -> Set.mem targets v
      | _ -> false)
  in
  let stmts =
    if needs_temps
    then
      (* TODO: Hm. Not optimizable? *)
      writes
      |> List.map ~f:(fun (n, a) ->
        let tmp = Utils.fresh "_tmp" in
        let decl = Decl (None, to_glsl_ty a.loc a.ty, tmp, Some (to_glsl_atom a)) in
        let set = Set (Var n, Var tmp) in
        decl, set)
      |> List.unzip
      |> Tuple2.uncurry List.append
    else List.map writes ~f:(fun (n, a) -> Set (Var n, to_glsl_atom a))
  in
  stmts @ [ Continue ]
;;

let rec translate_let
          (ctx : ctx)
          (v : string)
          (bind : Remove_placeholder.term)
          (ty : ty)
          (tail : stmt list)
  : stmt list
  =
  match bind.desc with
  | If _ | Switch _ ->
    let sub_ctx = { ctx with assign_to = Some v } in
    let sub = translate_return (translate_anf sub_ctx) (emit_return sub_ctx) bind in
    (Decl (None, ty, v, None) :: sub) @ tail
  | _ -> Decl (None, ty, v, Some (to_glsl_term bind)) :: tail

and translate_anf (ctx : ctx) (anf : Remove_placeholder.anf) : stmt list =
  match anf.desc with
  | Let (v, bind, body) ->
    let ty = to_glsl_ty bind.loc bind.ty in
    translate_let ctx v bind ty (translate_anf ctx body)
  | Placeholder (v, body) ->
    let ty = to_glsl_ty anf.loc anf.ty in
    Decl (None, ty, v, None) :: translate_anf ctx body
  | Return t -> translate_return (translate_anf ctx) (emit_return ctx) t
  | Loop (params, body) -> translate_loop params body
  | Continue args ->
    (match ctx.loop_params with
     | Some names -> translate_continue ~loc:anf.loc names args
     | None -> raise "Continue outside of Loop" ~loc:anf.loc)

and translate_loop
      (params : (string * Remove_placeholder.atom) list)
      (body : Remove_placeholder.anf)
  : stmt list
  =
  let decls =
    List.filter_map params ~f:(fun (name, init) ->
      match init.desc with
      | Var v when String.equal v name -> None
      | _ ->
        Some (Decl (None, to_glsl_ty init.loc init.ty, name, Some (to_glsl_atom init))))
  in
  let ctx = { assign_to = None; loop_params = Some (List.map params ~f:fst) } in
  decls @ [ WhileStmt (Bool true, Block (translate_anf ctx body)) ]
;;

let translate_function_body body =
  let ctx = { assign_to = None; loop_params = None } in
  translate_anf ctx body
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
  let tops =
    List.map tops ~f:(fun (top : Remove_placeholder.top) ->
      let loc = top.loc in
      match top.desc with
      | Define { name; args; body; ret_ty } ->
        let ret_type = to_glsl_ty top.loc ret_ty in
        let params =
          List.map args ~f:(fun (arg, arg_ty) -> to_glsl_ty top.loc arg_ty, arg)
        in
        let body = translate_function_body body in
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
