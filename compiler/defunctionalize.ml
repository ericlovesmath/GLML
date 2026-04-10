open Core
open Monomorphize
open Uncurry

module Err = Compiler_error.Pass (struct
    let name = "defunctionalize"
  end)

let is_fn_ty = function
  | TyArrow _ -> true
  | _ -> false
;;

let rec mangle_ty : ty -> string = function
  | TyFloat -> "float"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVec n -> Printf.sprintf "vec%d" n
  | TyMat (m, n) -> Printf.sprintf "mat%dx%d" m n
  | TyRecord s | TyVariant s -> s
  | TyArrow (a, b) -> mangle_ty a ^ "_" ^ mangle_ty b
;;

let flatten_arrow ty =
  let rec go acc = function
    | TyArrow (a, b) -> go (a :: acc) b
    | ret -> List.rev acc, ret
  in
  go [] ty
;;

let build_arrow_ty (arg_tys : ty list) (ret_ty : ty) : ty =
  List.fold_right arg_tys ~init:ret_ty ~f:(fun ty acc -> TyArrow (ty, acc))
;;

(* TODO: Maybe we can have a reader + writer monad to handle this... *)
type fn_entry =
  | LambdaEntry of
      { ctor_name : string
      ; params : (string * ty) list
      ; body : Uncurry.term
      ; captured : (string * ty) list
      ; loc : Lexer.loc
      }
  | GlobalEntry of
      { ctor_name : string
      ; fn_name : string
      ; loc : Lexer.loc
      }

type fn_type_info =
  { arrow_ty : ty
  ; arg_tys : ty list
  ; ret_ty : ty
  ; variant_name : string
  ; apply_name : string
  ; entries : fn_entry list
  }

type registry = fn_type_info String.Map.t

let entry_loc = function
  | LambdaEntry e -> e.loc
  | GlobalEntry e -> e.loc
;;

let collect_globals tops =
  List.filter_map tops ~f:(fun (top : Uncurry.top) ->
    match top.desc with
    | Define (_, v, _) -> Some v
    | Extern v -> Some v
    | TypeDef _ -> None)
  |> String.Set.of_list
;;

(* TODO: So ugly and so sad *)
let free_vars_of (globals : String.Set.t) (excluded : String.Set.t) (t : Uncurry.term)
  : (string * ty) list
  =
  let union m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_ -> function
      | `Left v | `Right v | `Both (v, _) -> Some v)
  in
  let union_list ms = List.fold ms ~init:String.Map.empty ~f:union in
  let rec fv (bound : String.Set.t) (t : Uncurry.term) : ty String.Map.t =
    match t.desc with
    | Var v ->
      if Set.mem globals v || Set.mem bound v
      then String.Map.empty
      else String.Map.singleton v t.ty
    | Float _ | Int _ | Bool _ -> String.Map.empty
    | Vec (_, ts) | Builtin (_, ts) -> union_list (List.map ts ~f:(fv bound))
    | Mat (_, _, ts) -> union_list (List.map ts ~f:(fv bound))
    | Lam (args, body) ->
      let bound = List.fold args ~init:bound ~f:(fun acc (v, _) -> Set.add acc v) in
      fv bound body
    | App (f, args) -> union_list (fv bound f :: List.map args ~f:(fv bound))
    | Let (_, v, bind, body) -> union (fv bound bind) (fv (Set.add bound v) body)
    | If (c, t, e) -> union_list [ fv bound c; fv bound t; fv bound e ]
    | Bop (_, l, r) -> union (fv bound l) (fv bound r)
    | Index (t, _) -> fv bound t
    | Record (_, ts) -> union_list (List.map ts ~f:(fv bound))
    | Field (t, _) -> fv bound t
    | Variant (_, _, args) -> union_list (List.map args ~f:(fv bound))
    | Match (scrut, cases) ->
      let fv_cases =
        List.map cases ~f:(fun (pat, body) ->
          let vars = Stlc.pat_bound_vars pat in
          let bound = List.fold vars ~init:bound ~f:Set.add in
          fv bound body)
      in
      union_list (fv bound scrut :: fv_cases)
  in
  fv excluded t |> Map.to_alist
;;

let rec subst_vars (subs : (string * string) list) (t : Uncurry.term) : Uncurry.term =
  let rw = subst_vars subs in
  let desc : Uncurry.term_desc =
    match t.desc with
    | Var v ->
      (match List.Assoc.find subs v ~equal:String.equal with
       | Some v' -> Var v'
       | None -> Var v)
    | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rw)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:rw)
    | Lam (params, body) ->
      let shadowed = List.map params ~f:fst in
      let subs =
        List.filter subs ~f:(fun (v, _) -> not (List.mem shadowed v ~equal:String.equal))
      in
      Lam (params, subst_vars subs body)
    | App (f, args) -> App (rw f, List.map args ~f:rw)
    | Let (recur, v, bind, body) ->
      let subs = List.filter subs ~f:(fun (k, _) -> not (String.equal k v)) in
      Let (recur, v, rw bind, subst_vars subs body)
    | If (c, t, e) -> If (rw c, rw t, rw e)
    | Bop (op, l, r) -> Bop (op, rw l, rw r)
    | Index (t, i) -> Index (rw t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rw)
    | Record (s, ts) -> Record (s, List.map ts ~f:rw)
    | Field (t, f) -> Field (rw t, f)
    | Variant (tn, c, args) -> Variant (tn, c, List.map args ~f:rw)
    | Match (scrut, cases) ->
      let rw_case (pat, body) =
        let bound = Stlc.pat_bound_vars pat in
        let subs =
          List.filter subs ~f:(fun (k, _) -> not (List.mem bound k ~equal:String.equal))
        in
        pat, subst_vars subs body
      in
      Match (rw scrut, List.map cases ~f:rw_case)
  in
  { t with desc }
;;

(* TODO: SO sad *)
let rec is_used_as_value (var_name : string) (t : Uncurry.term) : bool =
  match t.desc with
  | Var v -> String.equal v var_name
  | Float _ | Int _ | Bool _ -> false
  | App (f, args) ->
    let f_used =
      match f.desc with
      | Var v when String.equal v var_name ->
        false (* direct call head = not value position *)
      | _ -> is_used_as_value var_name f
    in
    f_used || List.exists args ~f:(is_used_as_value var_name)
  | Vec (_, ts) | Builtin (_, ts) -> List.exists ts ~f:(is_used_as_value var_name)
  | Mat (_, _, ts) -> List.exists ts ~f:(is_used_as_value var_name)
  | Lam (params, body) ->
    if List.exists params ~f:(fun (v, _) -> String.equal v var_name)
    then false
    else is_used_as_value var_name body
  | Let (_, v, bind, body) ->
    is_used_as_value var_name bind
    || if String.equal v var_name then false else is_used_as_value var_name body
  | If (c, t, e) ->
    is_used_as_value var_name c
    || is_used_as_value var_name t
    || is_used_as_value var_name e
  | Bop (_, l, r) -> is_used_as_value var_name l || is_used_as_value var_name r
  | Index (t, _) -> is_used_as_value var_name t
  | Record (_, ts) -> List.exists ts ~f:(is_used_as_value var_name)
  | Field (t, _) -> is_used_as_value var_name t
  | Variant (_, _, args) -> List.exists args ~f:(is_used_as_value var_name)
  | Match (scrut, cases) ->
    is_used_as_value var_name scrut
    || List.exists cases ~f:(fun (pat, body) ->
      (not (List.mem (Stlc.pat_bound_vars pat) var_name ~equal:String.equal))
      && is_used_as_value var_name body)
;;

(** Retype function-typed params to their corresponding variant types. Returns updated params
    and an env mapping changed param names to their new types. *)
let retype_params params =
  let params' =
    List.map params ~f:(fun (v, ty) ->
      v, if is_fn_ty ty then TyVariant ("DFn_" ^ mangle_ty ty) else ty)
  in
  let env =
    List.zip_exn params params'
    |> List.filter_map ~f:(fun ((v, old_ty), (_, new_ty)) ->
      if equal_ty old_ty new_ty then None else Some (v, new_ty))
    |> String.Map.of_alist_exn
  in
  params', env
;;

let get_or_create_info (reg : registry) (ty : ty) : registry * fn_type_info =
  let key = mangle_ty ty in
  match Map.find reg key with
  | Some info -> reg, info
  | None ->
    let arg_tys, ret_ty = flatten_arrow ty in
    let info =
      { arrow_ty = ty
      ; arg_tys
      ; ret_ty
      ; variant_name = "DFn_" ^ key
      ; apply_name = "dapply_" ^ key
      ; entries = []
      }
    in
    Map.set reg ~key ~data:info, info
;;

let add_lambda_entry (reg : registry) (ty : ty) params body captured loc
  : registry * string
  =
  let reg, info = get_or_create_info reg ty in
  let ctor_name = Utils.fresh "lctor" in
  let entry = LambdaEntry { ctor_name; params; body; captured; loc } in
  let info = { info with entries = info.entries @ [ entry ] } in
  Map.set reg ~key:(mangle_ty ty) ~data:info, ctor_name
;;

let add_global_entry (reg : registry) (ty : ty) (fn_name : string) (loc : Lexer.loc)
  : registry * string
  =
  let reg, info = get_or_create_info reg ty in
  match
    List.find info.entries ~f:(function
      | GlobalEntry e -> String.equal e.fn_name fn_name
      | _ -> false)
  with
  | Some (GlobalEntry e) -> reg, e.ctor_name
  | _ ->
    let ctor_name = Utils.fresh ("gctor_" ^ fn_name) in
    let entry = GlobalEntry { ctor_name; fn_name; loc } in
    let info = { info with entries = info.entries @ [ entry ] } in
    Map.set reg ~key:(mangle_ty ty) ~data:info, ctor_name
;;

let gen_typedef info : Uncurry.top =
  let loc = entry_loc (List.hd_exn info.entries) in
  let ctors =
    List.map info.entries ~f:(function
      | LambdaEntry e -> e.ctor_name, List.map e.captured ~f:snd
      | GlobalEntry e -> e.ctor_name, [])
  in
  { desc = TypeDef (info.variant_name, VariantDecl ctors)
  ; ty = TyVariant info.variant_name
  ; loc
  }
;;

let gen_apply_fn info : Uncurry.top =
  let first_loc = entry_loc (List.hd_exn info.entries) in
  let fn_var = Utils.fresh "dfn" in
  let arg_vars = List.map info.arg_tys ~f:(fun _ -> Utils.fresh "da") in
  let apply_params =
    (fn_var, TyVariant info.variant_name) :: List.zip_exn arg_vars info.arg_tys
  in
  let match_cases =
    List.filter_map info.entries ~f:(fun entry ->
      match entry with
      | LambdaEntry e ->
        let pat = Stlc.PatCtor (e.ctor_name, List.map e.captured ~f:fst) in
        let subs =
          List.map2_exn (List.map e.params ~f:fst) arg_vars ~f:(fun old_v new_v ->
            old_v, new_v)
        in
        let body = subst_vars subs e.body in
        Some (pat, body)
      | GlobalEntry e ->
        let pat = Stlc.PatCtor (e.ctor_name, []) in
        let arg_terms =
          List.map2_exn arg_vars info.arg_tys ~f:(fun v ty ->
            ({ desc = Var v; ty; loc = e.loc } : Uncurry.term))
        in
        let body : Uncurry.term =
          { desc =
              App ({ desc = Var e.fn_name; ty = info.arrow_ty; loc = e.loc }, arg_terms)
          ; ty = info.ret_ty
          ; loc = e.loc
          }
        in
        Some (pat, body))
  in
  let match_term : Uncurry.term =
    { desc =
        Match
          ( { desc = Var fn_var; ty = TyVariant info.variant_name; loc = first_loc }
          , match_cases )
    ; ty = info.ret_ty
    ; loc = first_loc
    }
  in
  let apply_ty = build_arrow_ty (List.map apply_params ~f:snd) info.ret_ty in
  let fn_body : Uncurry.term =
    { desc = Lam (apply_params, match_term); ty = apply_ty; loc = first_loc }
  in
  { desc = Define (Stlc.Nonrec, info.apply_name, fn_body)
  ; ty = apply_ty
  ; loc = first_loc
  }
;;

let rec rewrite_term
          (globals : String.Set.t)
          (lift_only : String.Set.t)
          (env : ty String.Map.t)
          (call_head : bool)
          (reg : registry)
          (t : Uncurry.term)
  : registry * Uncurry.term
  =
  let rw reg t = rewrite_term globals lift_only env false reg t in
  let rw_call reg t = rewrite_term globals lift_only env true reg t in
  let rw_list reg ts = List.fold_map ts ~init:reg ~f:rw in
  match t.desc with
  | Var v ->
    if (not call_head) && is_fn_ty t.ty
    then (
      match Map.find env v with
      | Some new_ty -> reg, { t with ty = new_ty }
      | None when Set.mem globals v ->
        let reg, ctor_name = add_global_entry reg t.ty v t.loc in
        let variant_name = "DFn_" ^ mangle_ty t.ty in
        ( reg
        , ({ desc = Variant (variant_name, ctor_name, [])
           ; ty = TyVariant variant_name
           ; loc = t.loc
           }
           : term) )
      | None -> reg, t)
    else reg, t
  | App (f, args) ->
    let reg, args = rw_list reg args in
    (match f.desc with
     | Var v when Set.mem globals v || Set.mem lift_only v ->
       let reg, f = rw_call reg f in
       reg, { t with desc = App (f, args) }
     | _ when is_fn_ty f.ty ->
       let arg_tys, ret_ty = flatten_arrow f.ty in
       let variant_name = "DFn_" ^ mangle_ty f.ty in
       let apply_name = "dapply_" ^ mangle_ty f.ty in
       let reg, f = rw reg f in
       let apply_ty = build_arrow_ty (TyVariant variant_name :: arg_tys) ret_ty in
       let apply_var : Uncurry.term =
         { desc = Var apply_name; ty = apply_ty; loc = f.loc }
       in
       reg, { t with desc = App (apply_var, f :: args) }
     | _ ->
       let reg, f = rw_call reg f in
       reg, { t with desc = App (f, args) })
  | Lam (params, body) ->
    (* Lam in value position (defunctionalize) *)
    let param_names = List.map params ~f:fst in
    let excluded = Set.union globals (String.Set.of_list param_names) in
    let captured_raw = free_vars_of globals excluded body in
    let captured =
      List.map captured_raw ~f:(fun (name, ty) ->
        name, Map.find env name |> Option.value ~default:ty)
    in
    let env_for_body = List.fold param_names ~init:env ~f:Map.remove in
    let lift_only_for_body = Set.diff lift_only (String.Set.of_list param_names) in
    let reg, body = rewrite_term globals lift_only_for_body env_for_body false reg body in
    let reg, ctor_name = add_lambda_entry reg t.ty params body captured t.loc in
    let variant_name = "DFn_" ^ mangle_ty t.ty in
    let payload =
      List.map captured ~f:(fun (name, ty) ->
        ({ desc = Var name; ty; loc = t.loc } : term))
    in
    ( reg
    , ({ desc = Variant (variant_name, ctor_name, payload)
       ; ty = TyVariant variant_name
       ; loc = t.loc
       }
       : term) )
  | Let (recur, v, ({ desc = Lam (lparams, lbody); _ } as lam_term), rest) ->
    let is_value =
      match recur with
      | Rec _ -> false
      | Nonrec -> is_used_as_value v rest
    in
    if is_value
    then (
      (* Lambda used as a value (defunctionalize) *)
      let reg, variant_term = rewrite_term globals lift_only env false reg lam_term in
      let env = Map.set env ~key:v ~data:variant_term.ty in
      let reg, rest = rewrite_term globals lift_only env false reg rest in
      reg, { t with desc = Let (recur, v, variant_term, rest) })
    else (
      (* Lambda only directly called (rewrite params/body, leave for Lambda_lift) *)
      let params, param_env = retype_params lparams in
      let env_for_body =
        Map.merge_skewed
          param_env
          (List.fold (List.map lparams ~f:fst) ~init:env ~f:Map.remove)
          ~combine:(fun ~key:_ v1 _ -> v1)
      in
      let lift_only_for_body =
        let lo = Set.diff lift_only (String.Set.of_list (List.map lparams ~f:fst)) in
        (* Recursive bindings can call themselves (treat as direct call) *)
        match recur with
        | Rec _ -> Set.add lo v
        | Nonrec -> lo
      in
      let reg, lbody' =
        rewrite_term globals lift_only_for_body env_for_body false reg lbody
      in
      let lam_term = { lam_term with desc = Lam (params, lbody') } in
      let lift_only = Set.add lift_only v in
      let reg, rest = rewrite_term globals lift_only env false reg rest in
      reg, { t with desc = Let (recur, v, lam_term, rest) })
  | Let (recur, v, bind, body) ->
    let reg, bind = rw reg bind in
    let reg, body = rw reg body in
    reg, { t with desc = Let (recur, v, bind, body) }
  | If (c, t, e) ->
    let reg, c = rw reg c in
    let reg, t = rw reg t in
    let reg, e = rw reg e in
    reg, { t with desc = If (c, t, e) }
  | Bop (op, l, r) ->
    let reg, l = rw reg l in
    let reg, r = rw reg r in
    reg, { t with desc = Bop (op, l, r) }
  | Vec (n, ts) ->
    let reg, ts = rw_list reg ts in
    reg, { t with desc = Vec (n, ts) }
  | Mat (n, m, ts) ->
    let reg, ts = rw_list reg ts in
    reg, { t with desc = Mat (n, m, ts) }
  | Index (tt, i) ->
    let reg, tt = rw reg tt in
    reg, { t with desc = Index (tt, i) }
  | Builtin (b, ts) ->
    let reg, ts = rw_list reg ts in
    reg, { t with desc = Builtin (b, ts) }
  | Record (s, ts) ->
    let reg, ts = rw_list reg ts in
    reg, { t with desc = Record (s, ts) }
  | Field (tt, f) ->
    let reg, tt = rw reg tt in
    reg, { t with desc = Field (tt, f) }
  | Variant (tn, c, args) ->
    let reg, args = rw_list reg args in
    reg, { t with desc = Variant (tn, c, args) }
  | Match (scrut, cases) ->
    let reg, scrut = rw reg scrut in
    let reg, cases =
      List.fold_map cases ~init:reg ~f:(fun reg (pat, body) ->
        let bound = Stlc.pat_bound_vars pat in
        let env = List.fold bound ~init:env ~f:Map.remove in
        let lift_only = Set.diff lift_only (String.Set.of_list bound) in
        let reg, body = rewrite_term globals lift_only env false reg body in
        reg, (pat, body))
    in
    reg, { t with desc = Match (scrut, cases) }
  | Float _ | Int _ | Bool _ -> reg, t
;;

let rewrite_top (globals : String.Set.t) (reg : registry) (top : Uncurry.top)
  : registry * Uncurry.top * bool
  =
  match top.desc with
  | Define (recur, name, ({ desc = Lam (params, body); _ } as lam)) ->
    let params, env = retype_params params in
    let is_hof_consumer = not (Map.is_empty env) in
    let reg, body = rewrite_term globals String.Set.empty env false reg body in
    ( reg
    , { top with desc = Define (recur, name, { lam with desc = Lam (params, body) }) }
    , is_hof_consumer )
  | Define (recur, name, term) ->
    let reg, term =
      rewrite_term globals String.Set.empty String.Map.empty false reg term
    in
    reg, { top with desc = Define (recur, name, term) }, false
  | TypeDef (name, RecordDecl fields) ->
    let fields =
      List.map fields ~f:(fun (field_name, ty) ->
        field_name, if is_fn_ty ty then TyVariant ("DFn_" ^ mangle_ty ty) else ty)
    in
    reg, { top with desc = TypeDef (name, RecordDecl fields) }, false
  | TypeDef (name, VariantDecl ctors) ->
    let ctors =
      List.map ctors ~f:(fun (ctor_name, arg_tys) ->
        ( ctor_name
        , List.map arg_tys ~f:(fun ty ->
            if is_fn_ty ty then TyVariant ("DFn_" ^ mangle_ty ty) else ty) ))
    in
    reg, { top with desc = TypeDef (name, VariantDecl ctors) }, false
  | Extern _ -> reg, top, false
;;

let defunctionalize (Program tops : Uncurry.t) : Uncurry.t Compiler_error.t =
  let globals = collect_globals tops in
  let reg, tagged_tops =
    List.fold_map tops ~init:String.Map.empty ~f:(fun reg top ->
      let reg, top, is_hof = rewrite_top globals reg top in
      reg, (top, is_hof))
  in
  (* Names of functions referenced as HOF values, dapply_ calls them directly,
     so they must be emitted before dapply_. *)
  let before_apply_names =
    Map.data reg
    |> List.concat_map ~f:(fun info ->
      List.filter_map info.entries ~f:(function
        | GlobalEntry e -> Some e.fn_name
        | LambdaEntry _ -> None))
    |> String.Set.of_list
  in
  let typedef_tops = Map.data reg |> List.map ~f:gen_typedef in
  let apply_fn_tops = Map.data reg |> List.map ~f:gen_apply_fn in
  (* Non-HOF functions referenced as HOF values, must precede dapply_ *)
  let before_tops =
    List.filter_map tagged_tops ~f:(fun (top, _) ->
      match top.desc with
      | Define (_, name, _) when Set.mem before_apply_names name -> Some top
      | _ -> None)
  in
  (* HOF consumers (have DFn_ params, call dapply_), must follow dapply_ *)
  let after_tops =
    List.filter_map tagged_tops ~f:(fun (top, is_hof) ->
      if is_hof then Some top else None)
  in
  (* Everything else (typedefs, externs, main, non-HOF non-referenced funcs) *)
  let rest_tops =
    List.filter_map tagged_tops ~f:(fun (top, is_hof) ->
      let in_before =
        match top.desc with
        | Define (_, name, _) -> Set.mem before_apply_names name
        | _ -> false
      in
      if in_before || is_hof then None else Some top)
  in
  Ok (Program (typedef_tops @ before_tops @ apply_fn_tops @ after_tops @ rest_tops))
;;
