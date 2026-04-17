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
  ; level : int
  }

(** [by_arrow]: [mangle_ty ty] => level-0 canonical DFn
    [by_variant]: [variant_name] => all DFns *)
type registry =
  { by_arrow : fn_type_info String.Map.t
  ; by_variant : fn_type_info String.Map.t
  }

let empty_registry = { by_arrow = String.Map.empty; by_variant = String.Map.empty }

type ctx =
  { globals : String.Set.t
  ; global_arities : int String.Map.t
  ; closure_globals : String.Set.t
  ; closure_global_types : ty String.Map.t
  ; lift_only : String.Set.t
  ; lift_only_bodies : ((string * ty) list * Uncurry.term) String.Map.t
  ; env : ty String.Map.t
  }

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
  let is_used = is_used_as_value var_name in
  match t.desc with
  | Var v -> String.equal v var_name
  | Float _ | Int _ | Bool _ -> false
  | App (f, args) ->
    let f_used =
      match f.desc with
      (* direct call head = not value position *)
      | Var v when String.equal v var_name -> false
      | _ -> is_used f
    in
    f_used || List.exists args ~f:is_used
  | Vec (_, ts) | Builtin (_, ts) -> List.exists ts ~f:is_used
  | Mat (_, _, ts) -> List.exists ts ~f:is_used
  | Lam (params, body) ->
    if List.exists params ~f:(fun (v, _) -> String.equal v var_name)
    then false
    else is_used body
  | Let (_, v, bind, body) ->
    is_used bind || if String.equal v var_name then false else is_used body
  | If (c, t, e) -> is_used c || is_used t || is_used e
  | Bop (_, l, r) -> is_used l || is_used r
  | Index (t, _) -> is_used t
  | Record (_, ts) -> List.exists ts ~f:is_used
  | Field (t, _) -> is_used t
  | Variant (_, _, args) -> List.exists args ~f:is_used
  | Match (scrut, cases) ->
    is_used scrut
    || List.exists cases ~f:(fun (pat, body) ->
      (not (List.mem (Stlc.pat_bound_vars pat) var_name ~equal:String.equal))
      && is_used body)
;;

let get_or_create_info (reg : registry) (ty : ty) : registry * fn_type_info =
  let key = mangle_ty ty in
  match Map.find reg.by_arrow key with
  | Some info -> reg, info
  | None ->
    let arg_tys, ret_ty = flatten_arrow ty in
    let info =
      { arrow_ty = ty
      ; arg_tys
      ; ret_ty
      ; variant_name = Utils.fresh "DFn"
      ; apply_name = Utils.fresh "dapply"
      ; entries = []
      ; level = 0
      }
    in
    ( { by_arrow = Map.set reg.by_arrow ~key ~data:info
      ; by_variant = Map.set reg.by_variant ~key:info.variant_name ~data:info
      }
    , info )
;;

(** Retype function-typed params to their corresponding variant types. Returns updated
    registry, updated params, and an env mapping changed param names to their new types. *)
let retype_params (reg : registry) params =
  let reg, params' =
    List.fold_map params ~init:reg ~f:(fun reg (v, ty) ->
      if is_fn_ty ty
      then (
        let reg, info = get_or_create_info reg ty in
        reg, (v, TyVariant info.variant_name))
      else reg, (v, ty))
  in
  let env =
    List.zip_exn params params'
    |> List.filter_map ~f:(fun ((v, old_ty), (_, new_ty)) ->
      if equal_ty old_ty new_ty then None else Some (v, new_ty))
    |> String.Map.of_alist_exn
  in
  reg, params', env
;;

let add_lambda_entry (reg : registry) (ty : ty) params body captured loc
  : registry * fn_type_info * string
  =
  let reg, canonical_info = get_or_create_info reg ty in
  let canonical_key = mangle_ty ty in
  let max_captured_level =
    List.fold captured ~init:(-1) ~f:(fun acc (_, cty) ->
      match cty with
      | TyVariant vname ->
        (match Map.find reg.by_variant vname with
         | Some info when String.equal (mangle_ty info.arrow_ty) canonical_key ->
           max acc info.level
         | _ -> acc)
      | _ -> acc)
  in
  let target_level = max_captured_level + 1 in
  let reg, info =
    if target_level = 0
    then reg, canonical_info
    else (
      (* Create a fresh DFn type for this level (prevent recursive DFn) *)
      let info =
        { canonical_info with
          variant_name = Utils.fresh "DFn"
        ; apply_name = Utils.fresh "dapply"
        ; entries = []
        ; level = target_level
        }
      in
      ( { reg with by_variant = Map.set reg.by_variant ~key:info.variant_name ~data:info }
      , info ))
  in
  let ctor_name = Utils.fresh "lctor" in
  let entry = LambdaEntry { ctor_name; params; body; captured; loc } in
  let info = { info with entries = info.entries @ [ entry ] } in
  let reg =
    if target_level = 0
    then
      (* Update both maps so future get_or_create_info sees accumulated entries *)
      { by_arrow = Map.set reg.by_arrow ~key:(mangle_ty ty) ~data:info
      ; by_variant = Map.set reg.by_variant ~key:info.variant_name ~data:info
      }
    else
      { reg with by_variant = Map.set reg.by_variant ~key:info.variant_name ~data:info }
  in
  reg, info, ctor_name
;;

let add_global_entry (reg : registry) (ty : ty) (fn_name : string) (loc : Lexer.loc)
  : registry * fn_type_info * string
  =
  let reg, info = get_or_create_info reg ty in
  match
    List.find info.entries ~f:(function
      | GlobalEntry e -> String.equal e.fn_name fn_name
      | _ -> false)
  with
  | Some (GlobalEntry e) -> reg, info, e.ctor_name
  | _ ->
    let ctor_name = Utils.fresh ("gctor_" ^ fn_name) in
    let entry = GlobalEntry { ctor_name; fn_name; loc } in
    let info = { info with entries = info.entries @ [ entry ] } in
    let key = mangle_ty ty in
    ( { by_arrow = Map.set reg.by_arrow ~key ~data:info
      ; by_variant = Map.set reg.by_variant ~key:info.variant_name ~data:info
      }
    , info
    , ctor_name )
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

let rec rewrite_term (ctx : ctx) (call_head : bool) (reg : registry) (t : Uncurry.term)
  : registry * Uncurry.term
  =
  let rw reg t = rewrite_term ctx false reg t in
  let rw_call reg t = rewrite_term ctx true reg t in
  let rw_list reg ts = List.fold_map ts ~init:reg ~f:rw in
  match t.desc with
  | Var v ->
    if (not call_head) && is_fn_ty t.ty
    then (
      match Map.find ctx.env v with
      | Some new_ty -> reg, { t with ty = new_ty }
      | None when Set.mem ctx.globals v ->
        let reg, info, ctor_name = add_global_entry reg t.ty v t.loc in
        ( reg
        , ({ desc = Variant (info.variant_name, ctor_name, [])
           ; ty = TyVariant info.variant_name
           ; loc = t.loc
           }
           : term) )
      | None -> reg, t)
    else reg, t
  | App (f, args) ->
    let reg, args = rw_list reg args in
    (match f.desc with
     | Var v when Set.mem ctx.globals v ->
       (* Global function: call by name, globals retain their name through lambda_lift *)
       if Set.mem ctx.closure_globals v
       then (
         (* Evaluates to a closure value, so we route through [dapply] instead of calling by name *)
         let reg, info = get_or_create_info reg f.ty in
         (* Use the actual post-defunctionalization DFn type if known *)
         let actual_info =
           match Map.find ctx.closure_global_types v with
           | Some (TyVariant vname) ->
             Map.find reg.by_variant vname |> Option.value ~default:info
           | _ -> info
         in
         let apply_ty =
           build_arrow_ty
             (TyVariant actual_info.variant_name :: actual_info.arg_tys)
             actual_info.ret_ty
         in
         let apply_var : term =
           { desc = Var actual_info.apply_name; ty = apply_ty; loc = f.loc }
         in
         let f = { f with ty = TyVariant actual_info.variant_name } in
         reg, { t with desc = App (apply_var, f :: args); ty = actual_info.ret_ty })
       else if is_fn_ty t.ty
       then (
         let f_arity =
           Map.find ctx.global_arities v
           |> Option.value ~default:(List.length (fst (flatten_arrow f.ty)))
         in
         let n_provided = List.length args in
         if n_provided < f_arity
         then (
           (* True partial application: create a closure that calls f with all args *)
           let remaining_arg_tys, final_ret_ty = flatten_arrow t.ty in
           let remaining_arg_vars =
             List.map remaining_arg_tys ~f:(fun _ -> Utils.fresh "ra")
           in
           let remaining_params = List.zip_exn remaining_arg_vars remaining_arg_tys in
           let captured_arg_vars =
             List.map args ~f:(fun arg -> Utils.fresh "ca", arg.ty)
           in
           let all_call_args =
             List.map captured_arg_vars ~f:(fun (name, ty) ->
               ({ desc = Var name; ty; loc = t.loc } : Uncurry.term))
             @ List.map remaining_params ~f:(fun (name, ty) ->
               ({ desc = Var name; ty; loc = t.loc } : Uncurry.term))
           in
           let body : Uncurry.term =
             { desc = App (f, all_call_args); ty = final_ret_ty; loc = t.loc }
           in
           let reg, info, ctor_name =
             add_lambda_entry reg t.ty remaining_params body captured_arg_vars t.loc
           in
           ( reg
           , { desc = Variant (info.variant_name, ctor_name, args)
             ; ty = TyVariant info.variant_name
             ; loc = t.loc
             } ))
         else (
           (* Full application of a HOF: the result is a function value,
              Call the global directly, ensure the result type reflects the DFn variant. *)
           let reg, info = get_or_create_info reg t.ty in
           reg, { t with desc = App (f, args); ty = TyVariant info.variant_name }))
       else reg, { t with desc = App (f, args) }
     | Var v when Set.mem ctx.lift_only v ->
       (* lift_only function: lambda_lift will rename it, so inline the body instead of
          calling by name. We stored the params/body when we processed the let-binding. *)
       if is_fn_ty t.ty
       then (
         match Map.find ctx.lift_only_bodies v with
         | Some (fn_params, fn_body) ->
           let n_applied = List.length args in
           let applied_params = List.take fn_params n_applied in
           let remaining_params_orig = List.drop fn_params n_applied in
           let captured_arg_vars =
             List.map args ~f:(fun arg -> Utils.fresh "ca", arg.ty)
           in
           let remaining_arg_vars =
             List.map remaining_params_orig ~f:(fun _ -> Utils.fresh "ra")
           in
           let remaining_params =
             List.zip_exn remaining_arg_vars (List.map remaining_params_orig ~f:snd)
           in
           let subs =
             List.zip_exn
               (List.map applied_params ~f:fst)
               (List.map captured_arg_vars ~f:fst)
             @ List.zip_exn (List.map remaining_params_orig ~f:fst) remaining_arg_vars
           in
           let body = subst_vars subs fn_body in
           let reg, info, ctor_name =
             add_lambda_entry reg t.ty remaining_params body captured_arg_vars t.loc
           in
           ( reg
           , { desc = Variant (info.variant_name, ctor_name, args)
             ; ty = TyVariant info.variant_name
             ; loc = t.loc
             } )
         | None -> reg, { t with desc = App (f, args) })
       else reg, { t with desc = App (f, args) }
     | _ when is_fn_ty f.ty ->
       let reg, info = get_or_create_info reg f.ty in
       let reg, f = rw reg f in
       (* Resolve actual DFn info based on the rewritten type of [f] *)
       let actual_info =
         match f.ty with
         | TyVariant vname -> Map.find reg.by_variant vname |> Option.value ~default:info
         | _ -> info
       in
       if is_fn_ty t.ty
       then (
         (* Partial application of a first-class function value, create a new closure *)
         (* TODO: Clean up this arity analysis logic in a different pass? *)
         let n_provided = List.length args in
         let remaining_arg_tys = List.drop info.arg_tys n_provided in
         let cap_fn_var = Utils.fresh "ca" in
         let cap_fn = cap_fn_var, f.ty in
         let cap_arg_vars = List.map args ~f:(fun arg -> Utils.fresh "ca", arg.ty) in
         let rem_arg_vars = List.map remaining_arg_tys ~f:(fun _ -> Utils.fresh "ra") in
         let rem_params = List.zip_exn rem_arg_vars remaining_arg_tys in
         let apply_ty = build_arrow_ty (f.ty :: info.arg_tys) info.ret_ty in
         let apply_var : term =
           { desc = Var actual_info.apply_name; ty = apply_ty; loc = f.loc }
         in
         let cap_fn_term : term = { desc = Var cap_fn_var; ty = f.ty; loc = t.loc } in
         let cap_arg_terms : term list =
           List.map cap_arg_vars ~f:(fun (name, ty) ->
             ({ desc = Var name; ty; loc = t.loc } : term))
         in
         let rem_arg_terms : term list =
           List.map rem_params ~f:(fun (name, ty) ->
             ({ desc = Var name; ty; loc = t.loc } : term))
         in
         let body : term =
           { desc = App (apply_var, (cap_fn_term :: cap_arg_terms) @ rem_arg_terms)
           ; ty = info.ret_ty
           ; loc = t.loc
           }
         in
         let captured_all = cap_fn :: cap_arg_vars in
         let reg, result_info, ctor_name =
           add_lambda_entry reg t.ty rem_params body captured_all t.loc
         in
         let payload = f :: args in
         ( reg
         , { desc = Variant (result_info.variant_name, ctor_name, payload)
           ; ty = TyVariant result_info.variant_name
           ; loc = t.loc
           } ))
       else (
         let apply_ty = build_arrow_ty (f.ty :: info.arg_tys) info.ret_ty in
         let apply_var : Uncurry.term =
           { desc = Var actual_info.apply_name; ty = apply_ty; loc = f.loc }
         in
         reg, { t with desc = App (apply_var, f :: args) })
     | _ ->
       let reg, f = rw_call reg f in
       reg, { t with desc = App (f, args) })
  | Lam (params, body) ->
    (* Lam in value position (defunctionalize) *)
    let param_names = List.map params ~f:fst in
    let excluded = Set.union ctx.globals (String.Set.of_list param_names) in
    let captured_raw = free_vars_of ctx.globals excluded body in
    let captured =
      List.map captured_raw ~f:(fun (name, ty) ->
        name, Map.find ctx.env name |> Option.value ~default:ty)
    in
    let ctx_for_body =
      { ctx with
        env = List.fold param_names ~init:ctx.env ~f:Map.remove
      ; lift_only = Set.diff ctx.lift_only (String.Set.of_list param_names)
      }
    in
    let reg, body = rewrite_term ctx_for_body false reg body in
    let reg, info, ctor_name = add_lambda_entry reg t.ty params body captured t.loc in
    let payload =
      List.map captured ~f:(fun (name, ty) ->
        ({ desc = Var name; ty; loc = t.loc } : term))
    in
    ( reg
    , ({ desc = Variant (info.variant_name, ctor_name, payload)
       ; ty = TyVariant info.variant_name
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
      let reg, variant_term = rewrite_term ctx false reg lam_term in
      let ctx = { ctx with env = Map.set ctx.env ~key:v ~data:variant_term.ty } in
      let reg, rest = rewrite_term ctx false reg rest in
      reg, { t with desc = Let (recur, v, variant_term, rest); ty = rest.ty })
    else (
      (* Lambda only directly called (rewrite params/body, leave for Lambda_lift) *)
      let reg, params, param_env = retype_params reg lparams in
      let env_for_body =
        Map.merge_skewed
          param_env
          (List.fold (List.map lparams ~f:fst) ~init:ctx.env ~f:Map.remove)
          ~combine:(fun ~key:_ v1 _ -> v1)
      in
      let lift_only_for_body =
        let lo = Set.diff ctx.lift_only (String.Set.of_list (List.map lparams ~f:fst)) in
        (* Recursive bindings can call themselves (treat as direct call) *)
        match recur with
        | Rec _ -> Set.add lo v
        | Nonrec -> lo
      in
      let ctx_for_body =
        { ctx with env = env_for_body; lift_only = lift_only_for_body }
      in
      let reg, lbody' = rewrite_term ctx_for_body false reg lbody in
      let lam_ty = build_arrow_ty (List.map params ~f:snd) lbody'.ty in
      let lam_term = { lam_term with desc = Lam (params, lbody'); ty = lam_ty } in
      let ctx =
        { ctx with
          lift_only = Set.add ctx.lift_only v
        ; lift_only_bodies = Map.set ctx.lift_only_bodies ~key:v ~data:(params, lbody')
        }
      in
      let reg, rest = rewrite_term ctx false reg rest in
      reg, { t with desc = Let (recur, v, lam_term, rest); ty = rest.ty })
  | Let (recur, v, bind, body) ->
    let orig_bind_ty = bind.ty in
    let reg, bind = rw reg bind in
    let ctx =
      if is_fn_ty orig_bind_ty && not (is_fn_ty bind.ty)
      then { ctx with env = Map.set ctx.env ~key:v ~data:bind.ty }
      else ctx
    in
    let reg, body = rewrite_term ctx false reg body in
    reg, { t with desc = Let (recur, v, bind, body); ty = body.ty }
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
        let case_ctx =
          { ctx with
            env = List.fold bound ~init:ctx.env ~f:Map.remove
          ; lift_only = Set.diff ctx.lift_only (String.Set.of_list bound)
          }
        in
        let reg, body = rewrite_term case_ctx false reg body in
        reg, (pat, body))
    in
    reg, { t with desc = Match (scrut, cases) }
  | Float _ | Int _ | Bool _ -> reg, t
;;

let rewrite_top
      (globals : String.Set.t)
      (global_arities : int String.Map.t)
      (closure_globals : String.Set.t)
      (closure_global_types : ty String.Map.t)
      (reg : registry)
      (top : Uncurry.top)
  : registry * Uncurry.top
  =
  let ctx_base =
    { globals
    ; global_arities
    ; closure_globals
    ; closure_global_types
    ; lift_only = String.Set.empty
    ; lift_only_bodies = String.Map.empty
    ; env = String.Map.empty
    }
  in
  match top.desc with
  | Define (recur, name, ({ desc = Lam (params, body); _ } as lam)) ->
    let reg, params, env = retype_params reg params in
    let ctx = { ctx_base with env } in
    let reg, body = rewrite_term ctx false reg body in
    let lam_ty = build_arrow_ty (List.map params ~f:snd) body.ty in
    let lam = { lam with desc = Lam (params, body); ty = lam_ty } in
    let top = { top with desc = Define (recur, name, lam); ty = lam_ty } in
    reg, top
  | Define (recur, name, term) ->
    let reg, term = rewrite_term ctx_base false reg term in
    reg, { top with desc = Define (recur, name, term); ty = term.ty }
  | TypeDef (name, RecordDecl fields) ->
    let reg, fields =
      List.fold_map fields ~init:reg ~f:(fun reg (field_name, ty) ->
        if is_fn_ty ty
        then (
          let reg, info = get_or_create_info reg ty in
          reg, (field_name, TyVariant info.variant_name))
        else reg, (field_name, ty))
    in
    reg, { top with desc = TypeDef (name, RecordDecl fields) }
  | TypeDef (name, VariantDecl ctors) ->
    let reg, ctors =
      List.fold_map ctors ~init:reg ~f:(fun reg (ctor_name, arg_tys) ->
        let reg, arg_tys =
          List.fold_map arg_tys ~init:reg ~f:(fun reg ty ->
            if is_fn_ty ty
            then (
              let reg, info = get_or_create_info reg ty in
              reg, TyVariant info.variant_name)
            else reg, ty)
        in
        reg, (ctor_name, arg_tys))
    in
    reg, { top with desc = TypeDef (name, VariantDecl ctors) }
  | Extern _ -> reg, top
;;

(******************************************************************************)
(* Topological Sort Logic for Generated Types/Functions                       *)
(******************************************************************************)

let rec global_refs_of (globals : String.Set.t) (term : Uncurry.term) : String.Set.t =
  let go = global_refs_of globals in
  let union_many ts = List.fold ts ~init:String.Set.empty ~f:Set.union in
  match term.desc with
  | Var v -> if Set.mem globals v then String.Set.singleton v else String.Set.empty
  | Float _ | Int _ | Bool _ -> String.Set.empty
  | App (f, args) -> union_many (go f :: List.map args ~f:go)
  | Lam (_, t) | Index (t, _) | Field (t, _) -> go t
  | Let (_, _, bind, body) -> Set.union (go bind) (go body)
  | If (c, t, e) -> union_many [ go c; go t; go e ]
  | Bop (_, l, r) -> Set.union (go l) (go r)
  | Vec (_, ts) | Builtin (_, ts) | Record (_, ts) | Variant (_, _, ts) | Mat (_, _, ts)
    -> union_many (List.map ts ~f:go)
  | Match (scrut, cases) ->
    union_many (go scrut :: List.map cases ~f:(fun (_, body) -> go body))
;;

let rec ty_struct_deps (ty : ty) : String.Set.t =
  match ty with
  | TyRecord s | TyVariant s -> String.Set.singleton s
  | TyArrow (a, b) -> Set.union (ty_struct_deps a) (ty_struct_deps b)
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ -> String.Set.empty
;;

let rec term_ty_deps (t : term) : String.Set.t =
  let self = ty_struct_deps t.ty in
  let from_desc =
    match t.desc with
    | Lam (params, body) ->
      List.fold params ~init:(term_ty_deps body) ~f:(fun acc (_, ty) ->
        Set.union acc (ty_struct_deps ty))
    | Var _ | Float _ | Int _ | Bool _ -> String.Set.empty
    | App (f, args) -> String.Set.union_list (List.map ~f:term_ty_deps (f :: args))
    | Let (_, _, b, e) -> Set.union (term_ty_deps b) (term_ty_deps e)
    | If (c, t, e) ->
      Set.union (Set.union (term_ty_deps c) (term_ty_deps t)) (term_ty_deps e)
    | Bop (_, t, t') -> Set.union (term_ty_deps t) (term_ty_deps t')
    | Vec (_, ts) | Mat (_, _, ts) | Builtin (_, ts) | Record (_, ts) | Variant (_, _, ts)
      -> String.Set.union_list (List.map ~f:term_ty_deps ts)
    | Index (t, _) | Field (t, _) -> term_ty_deps t
    | Match (scrut, cases) ->
      List.fold cases ~init:(term_ty_deps scrut) ~f:(fun acc (_, body) ->
        Set.union acc (term_ty_deps body))
  in
  Set.union self from_desc
;;

let typedef_decl_deps = function
  | RecordDecl fields ->
    List.fold fields ~init:String.Set.empty ~f:(fun acc (_, ty) ->
      Set.union acc (ty_struct_deps ty))
  | VariantDecl ctors ->
    List.fold ctors ~init:String.Set.empty ~f:(fun acc (_, tys) ->
      List.fold tys ~init:acc ~f:(fun acc ty -> Set.union acc (ty_struct_deps ty)))
;;

(* TODO: Use the [topological_sort] library from Jane Street *)
let topo_sort (all_tops : top list) : top list =
  let all_globals =
    List.filter_map all_tops ~f:(fun top ->
      match top.desc with
      | Define (_, name, _) | Extern name | TypeDef (name, _) -> Some name)
    |> String.Set.of_list
  in
  let key_of (top : top) =
    match top.desc with
    | Define (_, name, _) | Extern name | TypeDef (name, _) -> name
  in
  let deps_of (top : top) =
    match top.desc with
    | Extern _ -> String.Set.empty
    | TypeDef (_, decl) -> typedef_decl_deps decl
    | Define (_, _, body) ->
      Set.union (global_refs_of all_globals body) (term_ty_deps body)
  in
  let by_key = List.map all_tops ~f:(fun i -> key_of i, i) |> String.Map.of_alist_exn in
  let visited = ref String.Set.empty in
  let result = ref [] in
  let rec visit k =
    if not (Set.mem !visited k)
    then (
      visited := Set.add !visited k;
      match Map.find by_key k with
      | None -> ()
      | Some item ->
        Set.iter (deps_of item) ~f:visit;
        result := item :: !result)
  in
  List.iter all_tops ~f:(fun i -> visit (key_of i));
  List.rev !result
;;

let defunctionalize (Program tops : Uncurry.t) : Uncurry.t Compiler_error.t =
  let globals = collect_globals tops in
  (* Arity of each global function at the GLSL level, needed to distinguish partial
     application from a full call to a function that returns a function value. *)
  let global_arities =
    List.filter_map tops ~f:(fun (top : Uncurry.top) ->
      match top.desc with
      | Define (_, name, { desc = Lam (params, _); _ }) -> Some (name, List.length params)
      | Define (_, name, _) -> Some (name, 0)
      | _ -> None)
    |> String.Map.of_alist_exn
  in
  (* Globals whose RHS is not a direct lambda but evaluates to a function value.
     These are now closure values. *)
  let closure_globals =
    List.filter_map tops ~f:(fun (top : Uncurry.top) ->
      match top.desc with
      | Define (_, _, { desc = Lam _; _ }) -> None
      | Define (_, name, term) when is_fn_ty term.ty -> Some name
      | _ -> None)
    |> String.Set.of_list
  in
  let (reg, _), rewritten_tops =
    List.fold_map tops ~init:(empty_registry, String.Map.empty) ~f:(fun (reg, cgt) top ->
      let reg, top = rewrite_top globals global_arities closure_globals cgt reg top in
      let cgt =
        match top.desc with
        | Define (_, name, term) when Set.mem closure_globals name ->
          Map.set cgt ~key:name ~data:term.ty
        | _ -> cgt
      in
      (reg, cgt), top)
  in
  let all_tops =
    let dfn_infos =
      Map.data reg.by_variant |> List.filter ~f:(fun i -> not (List.is_empty i.entries))
    in
    rewritten_tops
    @ List.map ~f:gen_typedef dfn_infos
    @ List.map ~f:gen_apply_fn dfn_infos
  in
  Ok (Program (topo_sort all_tops))
;;
