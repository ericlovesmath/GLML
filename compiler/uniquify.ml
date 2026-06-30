open Core
open Frontend
open Desugar

include Compiler_error.Pass (struct
    let name = "uniquify"
  end)

(** Surface value name resolves to (for modules) *)
type vbind =
  | Local of string
  | Member of string

(** A module's exported interface *)
type iface =
  { vals : String.Set.t
  ; tys : String.Set.t
  }

type env =
  { names : vbind String.Map.t (* value scope *)
  ; tys : (string * string) String.Map.t (* opened module types *)
  ; modules : iface String.Map.t
  ; sigs : iface String.Map.t (* named [module type] to interface *)
  }

let bind_local env v =
  let v' = Utils.fresh v in
  v', { env with names = Map.set env.names ~key:v ~data:(Local v') }
;;

let find_module ~loc env m =
  Map.find env.modules m
  |> of_option "unknown module" ~loc ~d:[%message (m : string)]
  |> ok_exn
;;

(** Resolve type-name references to paths for type checker *)
let rec rewrite_ty ~loc env (t : ty) : ty =
  let recur = rewrite_ty ~loc env in
  match t with
  | TyName n ->
    (match Map.find env.tys n with
     | Some (m, tn) -> TyQual (m, tn)
     | None -> TyName n)
  | TyApp (n, args) ->
    if Map.mem env.tys n
    then
      raise
        "parametric type members of modules are not supported"
        ~loc
        ~d:[%message (n : string)];
    TyApp (n, List.map args ~f:recur)
  | TyArrow (l, r) -> TyArrow (recur l, recur r)
  | TyVec (i, t) -> TyVec (i, recur t)
  | TyTuple ts -> TyTuple (List.map ts ~f:recur)
  | TyQual (m, tn) ->
    let iface = find_module ~loc env m in
    if not (Set.mem iface.tys tn)
    then raise "unknown module type" ~loc ~d:[%message (m : string) (tn : string)];
    TyQual (m, tn)
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

let rewrite_spec ~loc env (s : Frontend.spec) : Frontend.spec =
  let r = rewrite_ty ~loc env in
  match s with
  | SpecVal (n, t) -> SpecVal (n, r t)
  | SpecManifestType (n, t) -> SpecManifestType (n, r t)
  | SpecAbstractType n -> SpecAbstractType n
;;

(** A signature's exported name sets, rejecting duplicate components. *)
let spec_names ~loc specs : iface =
  let dedup ~what ns =
    match List.find_a_dup ns ~compare:String.compare with
    | Some dup -> raise what ~loc ~d:[%message (dup : string)]
    | None -> String.Set.of_list ns
  in
  let vals, tys =
    List.partition_map specs ~f:(function
      | Frontend.SpecVal (n, _) -> First n
      | SpecManifestType (n, _) | SpecAbstractType n -> Second n)
  in
  { vals = dedup ~what:"duplicate signature val" vals
  ; tys = dedup ~what:"duplicate signature type" tys
  }
;;

(** Resolve an ascription's signature reference to its name-rewritten form *)
let resolve_sigref ~loc env (sigref : sig_ref) : sig_ref * iface =
  match sigref with
  | SigInline specs ->
    let specs = List.map specs ~f:(rewrite_spec ~loc env) in
    SigInline specs, spec_names ~loc specs
  | SigName sname ->
    ( SigName sname
    , Map.find env.sigs sname
      |> of_option "unknown signature" ~loc ~d:[%message (sname : string)]
      |> ok_exn )
;;

let resolve_sampler env s ~loc =
  match Map.find env.names s with
  | Some (Local f) -> f
  | Some (Member _) ->
    raise "sampler cannot be a module member" ~loc ~d:[%message (s : string)]
  | None -> raise "unbound variable" ~loc ~d:[%message (s : string)]
;;

let rec uniquify_term (env : env) (t : term) : term =
  let pure desc : term = { desc; loc = t.loc } in
  let aux = uniquify_term env in
  let aux_list ts = List.map ~f:aux ts in
  let rewrite_ty = rewrite_ty ~loc:t.loc env in
  match t.desc with
  | Float _ | Int _ | Bool _ -> pure t.desc
  | Var v ->
    (match Map.find env.names v with
     | Some (Local f) -> pure (Var f)
     | Some (Member m) -> pure (Qual (m, v))
     | None -> raise "unbound variable" ~loc:t.loc ~d:[%message (v : string)])
  | Qual (m, x) ->
    let iface = find_module ~loc:t.loc env m in
    if not (Set.mem iface.vals x)
    then raise "unknown module mem" ~loc:t.loc ~d:[%message (m : string) (x : string)];
    pure (Qual (m, x))
  | Lam (v, ty, body) ->
    let ty = Option.map ty ~f:rewrite_ty in
    let v, env = bind_local env v in
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
  | Sample (s, coord) -> pure (Sample (resolve_sampler env s ~loc:t.loc, aux coord))
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
            snd (bind_local env v))
        in
        let pat =
          Frontend.pat_map_vars pat ~f:(fun v ->
            match Map.find env.names v with
            | Some (Local f) -> f
            | _ -> v)
        in
        pat, uniquify_term env body)
    in
    pure (Match (scrutinee, cases))

(** Rename a [let]/[Define] binder to a fresh global and uniquify its body *)
and bind_def env recur v bind =
  let v', env' = bind_local env v in
  let bind =
    match recur with
    | Nonrec -> uniquify_term env bind
    | Rec _ -> uniquify_term env' bind
  in
  v', env', bind

(** Uniquify a module body in place, no flatten *)
and uniquify_module env ~loc mname sig_opt body =
  if Map.mem env.modules mname
  then raise "duplicate module" ~loc ~d:[%message (mname : string)];
  (* NOTE: Members are sequential *)
  let expose env surface =
    { env with names = Map.set env.names ~key:surface ~data:(Member mname) }
  in
  let member env (m : top) : env * top =
    let loc = m.loc in
    match m.desc with
    | Define (_, "main", _, _, _) -> raise "main may not be defined inside a module" ~loc
    | Define (recur, surface, return_ty, constrs, bind) ->
      let exposed = expose env surface in
      (* a recursive member sees itself *)
      let body_env =
        match recur with
        | Rec _ -> exposed
        | Nonrec -> env
      in
      let bind = uniquify_term body_env bind in
      let return_ty, constrs = rewrite_sig ~loc env return_ty constrs in
      exposed, { m with desc = Define (recur, surface, return_ty, constrs, bind) }
    | TypeDef (surface, params, AliasDecl ty) ->
      env, { m with desc = TypeDef (surface, params, AliasDecl (rewrite_ty ~loc env ty)) }
    | TypeDef (_, _, (RecordDecl _ | VariantDecl _)) ->
      raise "in-module record/variant declarations are not yet supported" ~loc
    | _ -> raise "only let and type-alias members are allowed in modules" ~loc
  in
  let _, members = List.fold_map body ~init:env ~f:member in
  (* tTe module's exported interface, derived from the renamed members *)
  let dedup ~what ns =
    match List.find_a_dup ns ~compare:String.compare with
    | Some dup -> raise what ~loc ~d:[%message (mname : string) (dup : string)]
    | None -> String.Set.of_list ns
  in
  let vals, tys =
    List.partition_map members ~f:(fun m ->
      match m.desc with
      | Define (_, n, _, _, _) -> First n
      | TypeDef (n, _, _) -> Second n
      | _ -> raise "unreachable: non-member survived uniquify" ~loc:m.loc)
  in
  let iface =
    { vals = dedup ~what:"duplicate module member" vals
    ; tys = dedup ~what:"duplicate module type member" tys
    }
  in
  let sig_opt, exported =
    match sig_opt with
    | None -> None, iface
    | Some sigref ->
      let sigref, s = resolve_sigref ~loc env sigref in
      Some sigref, { vals = Set.inter iface.vals s.vals; tys = Set.inter iface.tys s.tys }
  in
  ( { env with modules = Map.set env.modules ~key:mname ~data:exported }
  , { desc = Module (mname, sig_opt, members); loc } )
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
    { env with names = Map.set env.names ~key:v ~data:(Local v) }, [ { t with desc } ]
  | TypeDef (name, params, decl) ->
    let env = { env with tys = Map.remove env.tys name } in
    env, [ { t with desc = TypeDef (name, params, rewrite_type_decl ~loc env decl) } ]
  | Open m ->
    let iface = find_module ~loc env m in
    let names =
      Set.fold iface.vals ~init:env.names ~f:(fun names x ->
        Map.set names ~key:x ~data:(Member m))
    in
    let tys =
      Set.fold iface.tys ~init:env.tys ~f:(fun tys t -> Map.set tys ~key:t ~data:(m, t))
    in
    { env with names; tys }, []
  | ModuleType (sname, specs) ->
    if Map.mem env.sigs sname
    then raise "duplicate signature" ~loc ~d:[%message (sname : string)];
    let specs = List.map specs ~f:(rewrite_spec ~loc env) in
    let env =
      { env with sigs = Map.set env.sigs ~key:sname ~data:(spec_names ~loc specs) }
    in
    env, [ { t with desc = ModuleType (sname, specs) } ]
  | Module (mname, sig_opt, body) ->
    let env, node = uniquify_module env ~loc mname sig_opt body in
    env, [ node ]
;;

let uniquify (Program tops) =
  try_with (fun () ->
    let empty = String.Map.empty in
    let init = { names = empty; tys = empty; modules = empty; sigs = empty } in
    let _, tops = List.fold_map tops ~init ~f:uniquify_top in
    Program (List.concat tops))
;;
