open Core
open Frontend
open Desugar

include Compiler_error.Pass (struct
    let name = "uniquify"
  end)

(** Map to module's exported fresh names *)
type member_map = string String.Map.t

(** Flattened module's exported value and type members *)
type module_entry =
  { values : member_map
  ; types : member_map
  }

type env =
  { names : member_map (* value scope *)
  ; tys : member_map (* type scope *)
  ; modules : module_entry String.Map.t
  }

(** Bind [v] to a fresh global, returning the new name and extended env *)
let bind_fresh env v =
  let v' = Utils.fresh v in
  v', { env with names = Map.set env.names ~key:v ~data:v' }
;;

let resolve env v ~loc =
  Map.find env.names v
  |> of_option "unbound variable" ~loc ~d:[%message (v : string)]
  |> ok_exn
;;

let find_module ~loc env m =
  Map.find env.modules m
  |> of_option "unknown module" ~loc ~d:[%message (m : string)]
  |> ok_exn
;;

(** Resolve type-name references, avoiding typechecker like real OCaml compiler *)
let rec rewrite_ty ~loc env (t : ty) : ty =
  let recur = rewrite_ty ~loc env in
  let rename n = Map.find env.tys n |> Option.value ~default:n in
  match t with
  | TyName n -> TyName (rename n)
  | TyApp (n, args) -> TyApp (rename n, List.map args ~f:recur)
  | TyArrow (l, r) -> TyArrow (recur l, recur r)
  | TyVec (i, t) -> TyVec (i, recur t)
  | TyTuple ts -> TyTuple (List.map ts ~f:recur)
  | TyQual (m, tn) ->
    Map.find (find_module ~loc env m).types tn
    |> of_option "unknown module type" ~loc ~d:[%message (m : string) (tn : string)]
    |> ok_exn
    |> fun fresh -> TyName fresh
  | TyFloat | TyInt | TyBool | TyVar _ | TySampler -> t
;;

let rewrite_constr ~loc env (c : constr) : constr =
  let r = rewrite_ty ~loc env in
  let desc =
    match c.desc with
    | CNumeric t -> CNumeric (r t)
    | CBroadcast (a, b, c) -> CBroadcast (r a, r b, r c)
    | CMulBroadcast (a, b, c) -> CMulBroadcast (r a, r b, r c)
  in
  { c with desc }
;;

let rewrite_type_decl ~loc env (d : type_decl) : type_decl =
  let r = rewrite_ty ~loc env in
  match d with
  | AliasDecl t -> AliasDecl (r t)
  | RecordDecl fields -> RecordDecl (List.map fields ~f:(fun (n, t) -> n, r t))
  | VariantDecl ctors ->
    VariantDecl (List.map ctors ~f:(fun (c, ts) -> c, List.map ts ~f:r))
;;

let rewrite_sig ~loc env return_ty constrs =
  ( Option.map return_ty ~f:(rewrite_ty ~loc env)
  , List.map constrs ~f:(rewrite_constr ~loc env) )
;;

let rec uniquify_term (env : env) (t : term) : term =
  let pure desc : term = { desc; loc = t.loc } in
  let aux = uniquify_term env in
  let aux_list ts = List.map ~f:aux ts in
  let rewrite_ty = rewrite_ty ~loc:t.loc env in
  match t.desc with
  | Float _ | Int _ | Bool _ -> pure t.desc
  | Var v -> pure (Var (resolve env v ~loc:t.loc))
  | Qual (m, x) ->
    Map.find (find_module ~loc:t.loc env m).values x
    |> of_option "unknown module mem" ~loc:t.loc ~d:[%message (m : string) (x : string)]
    |> ok_exn
    |> fun v -> pure (Var v)
  | Lam (v, ty, body) ->
    let ty = Option.map ty ~f:rewrite_ty in
    let v, env = bind_fresh env v in
    pure (Lam (v, ty, uniquify_term env body))
  | App (f, x) -> pure (App (aux f, aux x))
  | Let (recur, v, return_ty, constrs, bind, body) ->
    let return_ty, constrs = rewrite_sig ~loc:t.loc env return_ty constrs in
    let v, env, bind = bind_def env recur v bind in
    pure (Let (recur, v, return_ty, constrs, bind, uniquify_term env body))
  | If (c, t, f) -> pure (If (aux c, aux t, aux f))
  | Vec (n, ts) -> pure (Vec (n, aux_list ts))
  | Bop (op, t, t') -> pure (Bop (op, aux t, aux t'))
  | Index (t, i) -> pure (Index (aux t, i))
  | Builtin (f, args) -> pure (Builtin (f, aux_list args))
  | Sample (s, coord) -> pure (Sample (resolve env s ~loc:t.loc, aux coord))
  | Record fields -> pure (Record (List.map fields ~f:(fun (f, t) -> f, aux t)))
  | Field (t, f) -> pure (Field (aux t, f))
  | Variant (ctor, args) -> pure (Variant (ctor, aux_list args))
  | Tuple ts -> pure (Tuple (aux_list ts))
  | Match (scrutinee, cases) ->
    let scrutinee = aux scrutinee in
    let cases =
      List.map cases ~f:(fun (pat, body) ->
        let env =
          List.fold (Frontend.pat_bound_vars pat) ~init:env ~f:(fun env v ->
            snd (bind_fresh env v))
        in
        let pat =
          Frontend.pat_map_vars pat ~f:(fun v ->
            Map.find env.names v |> Option.value ~default:v)
        in
        pat, uniquify_term env body)
    in
    pure (Match (scrutinee, cases))

(** Rename a [let]/[Define] binder to a fresh global and uniquify its body *)
and bind_def env recur v bind =
  let v', env' = bind_fresh env v in
  let bind =
    match recur with
    | Nonrec -> uniquify_term env bind
    | Rec _ -> uniquify_term env' bind
  in
  v', env', bind
;;

(** Flatten module body to top-level defs, members are sequential.
    Returns module's exported members and the emitted tops *)
let flatten_module ~loc env mname body =
  (* Each member emits one flat [top] and a [First value | Second member] *)
  let member env (m : top) =
    let loc = m.loc in
    match m.desc with
    | Define (_, "main", _, _, _) ->
      (* TODO: Handle this when we have files be modules *)
      raise "main may not be defined inside a module" ~loc
    | Define (recur, v, return_ty, constrs, bind) ->
      let core, env, bind = bind_def env recur v bind in
      let return_ty, constrs = rewrite_sig ~loc env return_ty constrs in
      ( env
      , (First (v, core), { m with desc = Define (recur, core, return_ty, constrs, bind) })
      )
    | TypeDef (name, params, AliasDecl ty) ->
      let core = Utils.fresh (mname ^ "_" ^ name) in
      let ty = rewrite_ty ~loc env ty in
      let env = { env with tys = Map.set env.tys ~key:name ~data:core } in
      env, (Second (name, core), { m with desc = TypeDef (core, params, AliasDecl ty) })
    | TypeDef (_, _, (RecordDecl _ | VariantDecl _)) ->
      raise "in-module record/variant declarations are not yet supported" ~loc
    | _ -> raise "only let and type-alias members are allowed in modules" ~loc
  in
  let exports, tops = List.fold_map body ~init:env ~f:member |> snd |> List.unzip in
  let value_exports, type_exports = List.partition_map exports ~f:Fn.id in
  let to_map ~what xs =
    match String.Map.of_alist xs with
    | `Duplicate_key dup -> raise what ~loc ~d:[%message (mname : string) (dup : string)]
    | `Ok m -> m
  in
  let entry =
    { values = to_map ~what:"duplicate module member" value_exports
    ; types = to_map ~what:"duplicate module type member" type_exports
    }
  in
  entry, tops
;;

let uniquify_top (env : env) (t : top) : env * top list =
  let loc = t.loc in
  match t.desc with
  | Define (recur, v, return_ty, constrs, bind) ->
    let core, env, bind = bind_def env recur v bind in
    let return_ty, constrs = rewrite_sig ~loc env return_ty constrs in
    env, [ { t with desc = Define (recur, core, return_ty, constrs, bind) } ]
  | Extern (ty, v) ->
    let desc = Extern (rewrite_ty ~loc env ty, v) in
    { env with names = Map.set env.names ~key:v ~data:v }, [ { t with desc } ]
  | TypeDef (name, params, decl) ->
    env, [ { t with desc = TypeDef (name, params, rewrite_type_decl ~loc env decl) } ]
  | Open m ->
    let entry = find_module ~loc env m in
    let merge a b = Map.merge_skewed a b ~combine:(fun ~key:_ _ m -> m) in
    { env with names = merge env.names entry.values; tys = merge env.tys entry.types }, []
  | Module (mname, body) ->
    if Map.mem env.modules mname
    then raise "duplicate module" ~loc ~d:[%message (mname : string)];
    let entry, tops = flatten_module ~loc env mname body in
    { env with modules = Map.set env.modules ~key:mname ~data:entry }, tops
;;

let uniquify (Program tops) =
  try_with (fun () ->
    let empty = String.Map.empty in
    let init = { names = empty; tys = empty; modules = empty } in
    let _, tops = List.fold_map tops ~init ~f:uniquify_top in
    Program (List.concat tops))
;;
