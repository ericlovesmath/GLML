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
  | TySampler -> TySampler
  | TyArrow _ | TyVec _ ->
    raise "unexpected type" ~loc ~d:[%message (ty : Lower_variants.ty)]
;;

let to_glsl_atom (a : Anf.atom) : term =
  match a.desc with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
;;

let to_glsl_value (ty : Lower_variants.ty) (vd : Tail_call.value_desc) : term =
  match vd with
  | Atom a -> to_glsl_atom a
  | Bop (op, l, r) -> Bop (op, to_glsl_atom l, to_glsl_atom r)
  | Vec (n, ts) ->
    let ctor =
      match ty with
      | TyVec (_, TyVec (m, _)) when n = m -> [%string "mat%{n#Int}"]
      | TyVec (_, TyVec (m, _)) -> [%string "mat%{n#Int}x%{m#Int}"]
      | _ -> [%string "vec%{n#Int}"]
    in
    App (ctor, List.map ~f:to_glsl_atom ts)
  | Index (t, i) -> Index (to_glsl_atom t, i)
  | Builtin (f, args) -> Builtin (f, List.map args ~f:to_glsl_atom)
  | Record args ->
    (match ty with
     | TyRecord s -> App (s, List.map args ~f:to_glsl_atom)
     | _ -> raise "Record term does not have type [record]")
  | Init_struct _ -> raise "Init_struct must be emitted as statements"
  | Field (a, f) -> Swizzle (to_glsl_atom a, f)
  | App (f, args) -> App (f, List.map args ~f:to_glsl_atom)
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

(** Write only the named fields of [v], leaving the rest GLSL-default *)
let set_fields (v : string) (fields : (string * Anf.atom) list) : stmt list =
  List.map fields ~f:(fun (f, a) -> Set (Swizzle (Var v, f), to_glsl_atom a))
;;

let translate_continue ~(loc : Lexer.loc) (params : string list) (args : Anf.atom list)
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

let rec translate_term (ctx : ctx) (t : Tail_call.term) : stmt list =
  let translate = translate_anf ctx in
  match t.desc with
  | Value (Init_struct fields) ->
    (match ctx.assign_to with
     | Some v -> set_fields v fields
     | None ->
       let tmp = Utils.fresh "_mk" in
       (Decl (None, to_glsl_ty t.loc t.ty, tmp, None) :: set_fields tmp fields)
       @ [ Return (Some (Var tmp)) ])
  | Value v -> [ emit_return ctx (to_glsl_value t.ty v) ]
  | If (c, t, e) ->
    [ IfStmt (to_glsl_atom c, Block (translate t), Some (Block (translate e))) ]
  | Switch (tag, cases) ->
    let cases = List.map cases ~f:(fun (l, case) -> l, translate case @ [ Break ]) in
    [ SwitchStmt (to_glsl_atom tag, cases) ]

and translate_anf (ctx : ctx) (anf : Tail_call.anf) : stmt list =
  match anf.desc with
  | Let (v, bind, body) ->
    let ty = to_glsl_ty bind.loc bind.ty in
    let tail = translate_anf ctx body in
    (match bind.desc with
     | Value (Init_struct fields) ->
       (Decl (None, ty, v, None) :: set_fields v fields) @ tail
     | Value value -> Decl (None, ty, v, Some (to_glsl_value bind.ty value)) :: tail
     | If _ | Switch _ ->
       let sub = translate_term { ctx with assign_to = Some v } bind in
       (Decl (None, ty, v, None) :: sub) @ tail)
  | Return t -> translate_term ctx t
  | Loop { counter; limit; params; body; on_exceed } ->
    let init = Decl (None, TyInt, counter, Some (Int 0)) in
    let cond = Bop (Lt, Var counter, Int limit) in
    let step = Set (Var counter, Bop (Add, Var counter, Int 1)) in
    let ctx = { assign_to = None; loop_params = Some params } in
    let after = translate_anf { assign_to = None; loop_params = None } on_exceed in
    For (init, cond, step, Block (translate_anf ctx body)) :: after
  | Continue args ->
    (match ctx.loop_params with
     | Some names -> translate_continue ~loc:anf.loc names args
     | None -> raise "Continue outside of Loop" ~loc:anf.loc)
;;

let translate_function_body body =
  let ctx = { assign_to = None; loop_params = None } in
  translate_anf ctx body
;;

let build_const_term body =
  let rec subst subs (t : term) : term =
    match t with
    | Var v -> Map.find subs v |> Option.value ~default:t
    | Float _ | Int _ | Bool _ -> t
    | Bop (op, l, r) -> Bop (op, subst subs l, subst subs r)
    | If (c, a, b) -> If (subst subs c, subst subs a, subst subs b)
    | App (f, ts) -> App (f, List.map ts ~f:(subst subs))
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:(subst subs))
    | Swizzle (t, s) -> Swizzle (subst subs t, s)
    | Index (t, i) -> Index (subst subs t, i)
  in
  let rec go subs (anf : Tail_call.anf) : term option =
    match anf.desc with
    | Return { desc = Value vd; ty; _ } -> Some (subst subs (to_glsl_value ty vd))
    | Let (v, { desc = Value vd; ty; _ }, rest) ->
      let bind = subst subs (to_glsl_value ty vd) in
      go (Map.set subs ~key:v ~data:bind) rest
    | Return { desc = If _ | Switch _; _ } | Let (_, { desc = If _ | Switch _; _ }, _) ->
      None
    | Loop _ | Continue _ -> None
  in
  go String.Map.empty body
;;

let translate_exn (Program tops : Tail_call.t) : Glsl.t =
  let tops =
    List.map tops ~f:(fun (top : Tail_call.top) ->
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
        Struct (s, fields))
  in
  Program tops
;;

let translate t = try_with (fun () -> translate_exn t)
