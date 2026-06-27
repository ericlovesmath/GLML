open Core
open Frontend
open Desugar

include Compiler_error.Pass (struct
    let name = "uniquify"
  end)

(** Module's fresh exported names *)
type member_map = string String.Map.t

type env =
  { names : string String.Map.t (* Map to fresh name *)
  ; modules : member_map String.Map.t
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

let rec uniquify_term (env : env) (t : term) : term =
  let pure desc : term = { desc; loc = t.loc } in
  let aux = uniquify_term env in
  let aux_list ts = List.map ~f:aux ts in
  match t.desc with
  | Float _ | Int _ | Bool _ -> pure t.desc
  | Var v -> pure (Var (resolve env v ~loc:t.loc))
  | Qual (m, x) ->
    Map.find env.modules m
    |> of_option "unknown module" ~loc:t.loc ~d:[%message (m : string)]
    |> ok_exn
    |> Fn.flip Map.find x
    |> of_option "unknown module mem" ~loc:t.loc ~d:[%message (m : string) (x : string)]
    |> ok_exn
    |> fun v -> pure (Var v)
  | Lam (v, ty, body) ->
    let v, env = bind_fresh env v in
    pure (Lam (v, ty, uniquify_term env body))
  | App (f, x) -> pure (App (aux f, aux x))
  | Let (recur, v, return_ty, constrs, bind, body) ->
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

let uniquify_top (env : env) (t : top) : env * top list =
  match t.desc with
  | Define (recur, v, return_ty, constrs, bind) ->
    let v, env, bind = bind_def env recur v bind in
    env, [ { t with desc = Define (recur, v, return_ty, constrs, bind) } ]
  | Extern (_, v) -> { env with names = Map.set env.names ~key:v ~data:v }, [ t ]
  | TypeDef _ -> env, [ t ]
  | Open mname ->
    (* Bring [M]'s members into scope *)
    Map.find env.modules mname
    |> of_option "unknown module" ~loc:t.loc ~d:[%message (mname : string)]
    |> ok_exn
    |> fun members ->
    let names = Map.merge_skewed env.names members ~combine:(fun ~key:_ _ m -> m) in
    { env with names }, []
  | Module (mname, body) ->
    (* Flatten members to fresh-named top-level [Define]s *)
    if Map.mem env.modules mname
    then raise "duplicate module" ~loc:t.loc ~d:[%message (mname : string)];
    let _, members =
      List.fold_map body ~init:env ~f:(fun env (member : top) ->
        match member.desc with
        | Define (_, "main", _, _, _) ->
          (* TODO: Handle this when we have files be modules *)
          raise "main may not be defined inside a module" ~loc:member.loc
        | Define (recur, v, return_ty, constrs, bind) ->
          let core, env, bind = bind_def env recur v bind in
          let desc = Define (recur, core, return_ty, constrs, bind) in
          env, ((v, core), { member with desc })
          (* TODO: more than let bindings in modules *)
        | _ -> raise "only let-bindings are in modules for now" ~loc:member.loc)
    in
    let exports, tops = List.unzip members in
    (match String.Map.of_alist exports with
     | `Duplicate_key dup ->
       raise
         "duplicate module member"
         ~loc:t.loc
         ~d:[%message (mname : string) (dup : string)]
     | `Ok member_map ->
       { env with modules = Map.set env.modules ~key:mname ~data:member_map }, tops)
;;

let uniquify (Program tops) =
  try_with (fun () ->
    let init = { names = String.Map.empty; modules = String.Map.empty } in
    let _, tops = List.fold_map tops ~init ~f:uniquify_top in
    Program (List.concat tops))
;;
