open Core
open Monomorphize
open Lambda_lift

include Compiler_error.Pass (struct
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
  | TyVec (n, TyFloat) -> Printf.sprintf "vec%d" n
  | TyVec (n, TyVec (m, TyFloat)) ->
    if n = m then Printf.sprintf "mat%d" n else Printf.sprintf "mat%dx%d" n m
  | TyVec (n, t) -> Printf.sprintf "vec%d_%s" n (mangle_ty t)
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
      ; body : Lambda_lift.term
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

type global_kind =
  | GlobalFn of { arity : int } (** top-level Define with a direct Lam *)
  | ClosureVal (** top-level Define with fn type but not a direct Lam *)
  | GlobalNonFn (** extern or non-fn define *)

type ctx =
  { globals : global_kind String.Map.t
  ; global_tys : ty String.Map.t
  ; env : ty String.Map.t
  }

let entry_loc = function
  | LambdaEntry e -> e.loc
  | GlobalEntry e -> e.loc
;;

let collect_globals tops =
  List.filter_map tops ~f:(fun (top : Lambda_lift.top) ->
    match top.desc with
    | Define { name; args; _ } -> Some (name, GlobalFn { arity = List.length args })
    | Const (name, term) when is_fn_ty term.ty -> Some (name, ClosureVal)
    | Const (name, _) -> Some (name, GlobalNonFn)
    | Extern name -> Some (name, GlobalNonFn)
    | TypeDef _ -> None)
  |> String.Map.of_alist_or_error
  |> of_or_error
  |> ok_exn
;;

let rec subst_vars (subs : (string * string) list) (t : Lambda_lift.term)
  : Lambda_lift.term
  =
  let rw = subst_vars subs in
  let desc : Lambda_lift.term_desc =
    match t.desc with
    | Var v ->
      (match List.Assoc.find subs v ~equal:String.equal with
       | Some v' -> Var v'
       | None -> Var v)
    | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rw)
    | App (f, args) -> App (rw f, List.map args ~f:rw)
    | Let (v, bind, body) ->
      let subs = List.filter subs ~f:(fun (k, _) -> not (String.equal k v)) in
      Let (v, rw bind, subst_vars subs body)
    | If (c, t, e) -> If (rw c, rw t, rw e)
    | Bop (op, l, r) -> Bop (op, rw l, rw r)
    | Index (t, i) -> Index (rw t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rw)
    | Record ts -> Record (List.map ts ~f:rw)
    | Field (t, f) -> Field (rw t, f)
    | Variant (c, args) -> Variant (c, List.map args ~f:rw)
    | Match (scrut, cases) ->
      let rw_case (pat, body) =
        let bound = Frontend.pat_bound_vars pat in
        let subs =
          List.filter subs ~f:(fun (k, _) -> not (List.mem bound k ~equal:String.equal))
        in
        pat, subst_vars subs body
      in
      Match (rw scrut, List.map cases ~f:rw_case)
  in
  { t with desc }
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
    (match List.zip params params' with
     | Ok r -> r
     | Unequal_lengths -> raise "(unreachable) retype_params length mismatch")
    |> List.filter_map ~f:(fun ((v, old_ty), (_, new_ty)) ->
      if equal_ty old_ty new_ty then None else Some (v, new_ty))
    |> String.Map.of_alist_or_error
    |> of_or_error
    |> ok_exn
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
  (* Always update [by_arrow] so subsequent [retype_params] calls see the highest
     level for this arrow type. Otherwise a Define processed after a higher level
     was created (e.g. [eval_material] after [scene_mat]'s partial app) would
     retype its fn-typed params against the stale lower variant. *)
  let reg =
    { by_arrow = Map.set reg.by_arrow ~key:(mangle_ty ty) ~data:info
    ; by_variant = Map.set reg.by_variant ~key:info.variant_name ~data:info
    }
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

let gen_typedef info : Lambda_lift.top =
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

let gen_apply_fn (reg : registry) info : Lambda_lift.top =
  let first_loc = entry_loc (List.hd_exn info.entries) in
  let fn_var = Utils.fresh "dfn" in
  let arg_vars = List.map info.arg_tys ~f:(fun _ -> Utils.fresh "da") in
  let apply_arg_tys =
    List.map info.arg_tys ~f:(fun ty ->
      if is_fn_ty ty
      then (
        let i =
          Map.find reg.by_arrow (mangle_ty ty)
          |> of_option "fn type not registered in dfn registry" ~d:[%message (ty : ty)]
          |> ok_exn
        in
        TyVariant i.variant_name)
      else ty)
  in
  let apply_params =
    (fn_var, TyVariant info.variant_name)
    ::
    (match List.zip arg_vars apply_arg_tys with
     | Ok r -> r
     | Unequal_lengths -> raise "(unreachable) gen_apply_fn arg/type length mismatch")
  in
  let match_cases =
    List.filter_map info.entries ~f:(fun entry ->
      match entry with
      | LambdaEntry e ->
        let pat =
          Frontend.PatCtor
            (e.ctor_name, List.map e.captured ~f:(fun (v, _) -> Frontend.PatVar v))
        in
        let subs =
          match
            List.map2 (List.map e.params ~f:fst) arg_vars ~f:(fun old_v new_v ->
              old_v, new_v)
          with
          | Ok r -> r
          | Unequal_lengths ->
            raise "(unreachable) gen_apply_fn params/arg_vars length mismatch"
        in
        let body = subst_vars subs e.body in
        Some (pat, body)
      | GlobalEntry e ->
        let pat = Frontend.PatCtor (e.ctor_name, []) in
        let arg_terms =
          match
            List.map2 arg_vars apply_arg_tys ~f:(fun v ty ->
              ({ desc = Var v; ty; loc = e.loc } : Lambda_lift.term))
          with
          | Ok r -> r
          | Unequal_lengths ->
            raise "(unreachable) gen_apply_fn arg_vars/types length mismatch"
        in
        let body : Lambda_lift.term =
          { desc =
              App ({ desc = Var e.fn_name; ty = info.arrow_ty; loc = e.loc }, arg_terms)
          ; ty = info.ret_ty
          ; loc = e.loc
          }
        in
        Some (pat, body))
  in
  let match_term : Lambda_lift.term =
    { desc =
        Match
          ( { desc = Var fn_var; ty = TyVariant info.variant_name; loc = first_loc }
          , match_cases )
    ; ty = info.ret_ty
    ; loc = first_loc
    }
  in
  let apply_ty = build_arrow_ty (List.map apply_params ~f:snd) info.ret_ty in
  { desc =
      Define
        { name = info.apply_name
        ; recur = Nonrec
        ; args = apply_params
        ; body = match_term
        ; ret_ty = info.ret_ty
        }
  ; ty = apply_ty
  ; loc = first_loc
  }
;;

let rec rewrite_term
          (ctx : ctx)
          (call_head : bool)
          (reg : registry)
          (t : Lambda_lift.term)
  : registry * Lambda_lift.term
  =
  let rw reg t = rewrite_term ctx false reg t in
  let rw_list reg ts = List.fold_map ts ~init:reg ~f:rw in
  match t.desc with
  | Var v ->
    if (not call_head) && is_fn_ty t.ty
    then (
      match Map.find ctx.env v with
      | Some new_ty -> reg, { t with ty = new_ty }
      | None when Map.mem ctx.globals v ->
        let reg, info, ctor_name = add_global_entry reg t.ty v t.loc in
        ( reg
        , ({ desc = Variant (ctor_name, [])
           ; ty = TyVariant info.variant_name
           ; loc = t.loc
           }
           : Lambda_lift.term) )
      | None -> reg, t)
    else reg, t
  | App (f, args) ->
    let reg, args = rw_list reg args in
    (match f.desc with
     | Var v when Map.mem ctx.globals v ->
       (match Map.find ctx.globals v with
        | Some ClosureVal ->
          (* Evaluates to a closure value, route through dapply instead of calling by name *)
          let reg, info = get_or_create_info reg f.ty in
          let actual_info =
            match Map.find ctx.global_tys v with
            | Some (TyVariant vname) ->
              Map.find reg.by_variant vname |> Option.value ~default:info
            | _ -> info
          in
          let apply_ty =
            build_arrow_ty
              (TyVariant actual_info.variant_name :: actual_info.arg_tys)
              actual_info.ret_ty
          in
          let apply_var : Lambda_lift.term =
            { desc = Var actual_info.apply_name; ty = apply_ty; loc = f.loc }
          in
          let f = { f with ty = TyVariant actual_info.variant_name } in
          reg, { t with desc = App (apply_var, f :: args); ty = actual_info.ret_ty }
        | Some (GlobalFn { arity = f_arity }) when is_fn_ty t.ty ->
          let n_provided = List.length args in
          if n_provided < f_arity
          then (
            (* True partial application: create a closure that calls f with all args *)
            let remaining_arg_tys, final_ret_ty = flatten_arrow t.ty in
            let remaining_arg_vars =
              List.map remaining_arg_tys ~f:(fun _ -> Utils.fresh "ra")
            in
            let remaining_params =
              match List.zip remaining_arg_vars remaining_arg_tys with
              | Ok r -> r
              | Unequal_lengths ->
                raise "(unreachable) partial app remaining params length mismatch"
            in
            let captured_arg_vars =
              List.map args ~f:(fun arg -> Utils.fresh "ca", arg.ty)
            in
            let all_call_args =
              List.map captured_arg_vars ~f:(fun (name, ty) ->
                ({ desc = Var name; ty; loc = t.loc } : Lambda_lift.term))
              @ List.map remaining_params ~f:(fun (name, ty) ->
                ({ desc = Var name; ty; loc = t.loc } : Lambda_lift.term))
            in
            let body : Lambda_lift.term =
              { desc = App (f, all_call_args); ty = final_ret_ty; loc = t.loc }
            in
            let reg, info, ctor_name =
              add_lambda_entry reg t.ty remaining_params body captured_arg_vars t.loc
            in
            ( reg
            , { desc = Variant (ctor_name, args)
              ; ty = TyVariant info.variant_name
              ; loc = t.loc
              } ))
          else (
            (* Full application of a HOF: result is a function value *)
            let reg, info = get_or_create_info reg t.ty in
            let ty =
              Map.find ctx.global_tys v
              |> Option.value ~default:(TyVariant info.variant_name)
            in
            reg, { t with desc = App (f, args); ty })
        | _ -> reg, { t with desc = App (f, args) })
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
         let n_provided = List.length args in
         let remaining_arg_tys = List.drop info.arg_tys n_provided in
         let cap_fn_var = Utils.fresh "ca" in
         let cap_fn = cap_fn_var, f.ty in
         let cap_arg_vars = List.map args ~f:(fun arg -> Utils.fresh "ca", arg.ty) in
         let rem_arg_vars = List.map remaining_arg_tys ~f:(fun _ -> Utils.fresh "ra") in
         let rem_params =
           match List.zip rem_arg_vars remaining_arg_tys with
           | Ok r -> r
           | Unequal_lengths ->
             raise "(unreachable) partial app closure rem_params length mismatch"
         in
         let apply_ty = build_arrow_ty (f.ty :: info.arg_tys) info.ret_ty in
         let apply_var : Lambda_lift.term =
           { desc = Var actual_info.apply_name; ty = apply_ty; loc = f.loc }
         in
         let cap_fn_term : Lambda_lift.term =
           { desc = Var cap_fn_var; ty = f.ty; loc = t.loc }
         in
         let cap_arg_terms : Lambda_lift.term list =
           List.map cap_arg_vars ~f:(fun (name, ty) ->
             ({ desc = Var name; ty; loc = t.loc } : Lambda_lift.term))
         in
         let rem_arg_terms : Lambda_lift.term list =
           List.map rem_params ~f:(fun (name, ty) ->
             ({ desc = Var name; ty; loc = t.loc } : Lambda_lift.term))
         in
         let body : Lambda_lift.term =
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
         , { desc = Variant (ctor_name, payload)
           ; ty = TyVariant result_info.variant_name
           ; loc = t.loc
           } ))
       else (
         let apply_ty = build_arrow_ty (f.ty :: info.arg_tys) info.ret_ty in
         let apply_var : term =
           { desc = Var actual_info.apply_name; ty = apply_ty; loc = f.loc }
         in
         reg, { t with desc = App (apply_var, f :: args) })
     | _ ->
       let reg, f = rewrite_term ctx true reg f in
       reg, { t with desc = App (f, args) })
  | Let (v, bind, body) ->
    let orig_bind_ty = bind.ty in
    let reg, bind = rw reg bind in
    let ctx =
      if is_fn_ty orig_bind_ty && not (is_fn_ty bind.ty)
      then { ctx with env = Map.set ctx.env ~key:v ~data:bind.ty }
      else ctx
    in
    let reg, body = rewrite_term ctx false reg body in
    reg, { t with desc = Let (v, bind, body); ty = body.ty }
  | If (c, tt, e) ->
    let reg, c = rw reg c in
    let reg, tt = rw reg tt in
    let reg, e = rw reg e in
    let ty = if is_fn_ty t.ty then tt.ty else t.ty in
    reg, { t with desc = If (c, tt, e); ty }
  | Bop (op, l, r) ->
    let reg, l = rw reg l in
    let reg, r = rw reg r in
    reg, { t with desc = Bop (op, l, r) }
  | Vec (n, ts) ->
    let reg, ts = rw_list reg ts in
    reg, { t with desc = Vec (n, ts) }
  | Index (tt, i) ->
    let reg, tt = rw reg tt in
    reg, { t with desc = Index (tt, i) }
  | Builtin (b, ts) ->
    let reg, ts = rw_list reg ts in
    reg, { t with desc = Builtin (b, ts) }
  | Record ts ->
    let reg, ts = rw_list reg ts in
    reg, { t with desc = Record ts }
  | Field (tt, f) ->
    let reg, tt = rw reg tt in
    if (not call_head) && is_fn_ty t.ty
    then (
      let reg, info = get_or_create_info reg t.ty in
      reg, { t with desc = Field (tt, f); ty = TyVariant info.variant_name })
    else reg, { t with desc = Field (tt, f) }
  | Variant (c, args) ->
    let reg, args = rw_list reg args in
    reg, { t with desc = Variant (c, args) }
  | Match (scrut, cases) ->
    let reg, scrut = rw reg scrut in
    let reg, cases =
      List.fold_map cases ~init:reg ~f:(fun reg (pat, body) ->
        let bound = Frontend.pat_bound_vars pat in
        let case_ctx = { ctx with env = List.fold bound ~init:ctx.env ~f:Map.remove } in
        let reg, body = rewrite_term case_ctx false reg body in
        reg, (pat, body))
    in
    reg, { t with desc = Match (scrut, cases) }
  | Float _ | Int _ | Bool _ -> reg, t
;;

let rewrite_top (ctx : ctx) (reg : registry) (top : top) : registry * top =
  match top.desc with
  | Define { name; recur; args; body; ret_ty = _ } ->
    let reg, args, env = retype_params reg args in
    let ctx = { ctx with env } in
    let reg, body = rewrite_term ctx false reg body in
    let lam_ty = build_arrow_ty (List.map args ~f:snd) body.ty in
    ( reg
    , { top with
        desc = Define { name; recur; args; body; ret_ty = body.ty }
      ; ty = lam_ty
      } )
  | Const (name, term) ->
    let reg, term = rewrite_term ctx false reg term in
    reg, { top with desc = Const (name, term); ty = term.ty }
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

let rec global_refs_of (globals : String.Set.t) (term : Lambda_lift.term) : String.Set.t =
  let go = global_refs_of globals in
  let union_many ts = List.fold ts ~init:String.Set.empty ~f:Set.union in
  match term.desc with
  | Var v -> if Set.mem globals v then String.Set.singleton v else String.Set.empty
  | Float _ | Int _ | Bool _ -> String.Set.empty
  | App (f, args) -> union_many (go f :: List.map args ~f:go)
  | Index (t, _) | Field (t, _) -> go t
  | Let (_, bind, body) -> Set.union (go bind) (go body)
  | If (c, t, e) -> union_many [ go c; go t; go e ]
  | Bop (_, l, r) -> Set.union (go l) (go r)
  | Vec (_, ts) | Builtin (_, ts) | Record ts | Variant (_, ts) ->
    union_many (List.map ts ~f:go)
  | Match (scrut, cases) ->
    union_many (go scrut :: List.map cases ~f:(fun (_, body) -> go body))
;;

let rec ty_struct_deps (ty : ty) : String.Set.t =
  match ty with
  | TyRecord s | TyVariant s -> String.Set.singleton s
  | TyArrow (a, b) -> Set.union (ty_struct_deps a) (ty_struct_deps b)
  | TyFloat | TyInt | TyBool -> String.Set.empty
  | TyVec (_, t) -> ty_struct_deps t
;;

let rec term_ty_deps (t : Lambda_lift.term) : String.Set.t =
  let self = ty_struct_deps t.ty in
  let from_desc =
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> String.Set.empty
    | App (f, args) -> String.Set.union_list (List.map ~f:term_ty_deps (f :: args))
    | Let (_, b, e) -> Set.union (term_ty_deps b) (term_ty_deps e)
    | If (c, t, e) ->
      Set.union (Set.union (term_ty_deps c) (term_ty_deps t)) (term_ty_deps e)
    | Bop (_, t, t') -> Set.union (term_ty_deps t) (term_ty_deps t')
    | Vec (_, ts) | Builtin (_, ts) | Record ts | Variant (_, ts) ->
      String.Set.union_list (List.map ~f:term_ty_deps ts)
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

let topo_sort (all_tops : Lambda_lift.top list) : Lambda_lift.t =
  let key_of (top : Lambda_lift.top) =
    match top.desc with
    | Define { name; _ } | Const (name, _) | Extern name | TypeDef (name, _) -> name
  in
  let nodes = List.map all_tops ~f:key_of in
  let globals = String.Set.of_list nodes in
  let deps_of (top : Lambda_lift.top) =
    match top.desc with
    | Extern _ -> String.Set.empty
    | TypeDef (_, decl) -> typedef_decl_deps decl
    | Define { body; args; _ } ->
      let body_refs = Set.union (global_refs_of globals body) (term_ty_deps body) in
      let arg_deps =
        List.fold args ~init:String.Set.empty ~f:(fun acc (_, ty) ->
          Set.union acc (ty_struct_deps ty))
      in
      Set.union body_refs arg_deps
    | Const (_, term) -> Set.union (global_refs_of globals term) (term_ty_deps term)
  in
  let edges =
    List.concat_map all_tops ~f:(fun top ->
      let name = key_of top in
      Set.to_list (deps_of top)
      |> List.filter_map ~f:(fun dep ->
        if String.equal dep name || not (Set.mem globals dep)
        then None
        else Some { Topological_sort.Edge.from = dep; to_ = name }))
  in
  let by_key =
    List.map all_tops ~f:(fun top -> key_of top, top)
    |> String.Map.of_alist_or_error
    |> of_or_error
    |> ok_exn
  in
  let labels =
    Topological_sort.sort (module String) ~what:Nodes ~nodes ~edges
    |> of_or_error
    |> ok_exn
  in
  (* NOTE: DFS to drop unreachable params from [main] *)
  let rec visit acc name =
    if Set.mem acc name
    then acc
    else (
      let acc = Set.add acc name in
      match Map.find by_key name with
      | None -> acc
      | Some top -> Set.fold (deps_of top) ~init:acc ~f:visit)
  in
  let reachable =
    List.fold all_tops ~init:String.Set.empty ~f:(fun acc top ->
      match top.desc with
      | Define { name = "main"; _ } | Extern _ -> visit acc (key_of top)
      | _ -> acc)
  in
  Program
    (List.filter_map labels ~f:(fun n ->
       Map.find by_key n |> Option.filter ~f:(fun _ -> Set.mem reachable n)))
;;

(** Re-resolve each variant-typed Define param against the latest [by_arrow]
    entry, needed when [add_lambda_entry] forces a higher level after a [Define]
    has already been retyped against the lower variant *)
let promote_define_params (reg : registry) (tops : Lambda_lift.top list) =
  let rec uses v (t : Lambda_lift.term) =
    match t.desc with
    | Var n -> String.equal n v
    | App (f, ts) -> uses v f || List.exists ts ~f:(uses v)
    | Vec (_, ts) | Builtin (_, ts) | Record ts | Variant (_, ts) ->
      List.exists ts ~f:(uses v)
    | Let (_, a, b) | Bop (_, a, b) -> uses v a || uses v b
    | If (a, b, c) -> uses v a || uses v b || uses v c
    | Index (t, _) | Field (t, _) -> uses v t
    | Match (s, cs) -> uses v s || List.exists cs ~f:(fun (_, b) -> uses v b)
    | Float _ | Int _ | Bool _ -> false
  in
  let promote ty =
    match ty with
    | TyVariant n ->
      Map.find reg.by_variant n
      |> Option.value_map ~default:ty ~f:(fun i ->
        mangle_ty i.arrow_ty
        |> Map.find reg.by_arrow
        |> Option.value_map ~default:ty ~f:(fun w -> TyVariant w.variant_name))
    | _ -> ty
  in
  List.map tops ~f:(fun (top : Lambda_lift.top) ->
    match top.desc with
    | Define ({ args; body; ret_ty; _ } as d) ->
      let args =
        List.map args ~f:(fun (v, ty) -> v, if uses v body then ty else promote ty)
      in
      { top with
        desc = Define { d with args }
      ; ty = build_arrow_ty (List.map args ~f:snd) ret_ty
      }
    | _ -> top)
;;

let defunctionalize (Program tops : Lambda_lift.t) : Lambda_lift.t Compiler_error.t =
  let globals = collect_globals tops in
  let (reg, _), rewritten_tops =
    List.fold_map
      tops
      ~init:(empty_registry, String.Map.empty)
      ~f:(fun (reg, global_tys) top ->
        let ctx = { globals; global_tys; env = String.Map.empty } in
        let reg, top = rewrite_top ctx reg top in
        let global_tys =
          match top.desc with
          | Const (name, term) when Map.mem globals name ->
            Map.set global_tys ~key:name ~data:term.ty
          | Define { name; body; _ } -> Map.set global_tys ~key:name ~data:body.ty
          | _ -> global_tys
        in
        (reg, global_tys), top)
  in
  let rewritten_tops = promote_define_params reg rewritten_tops in
  let all_dfn_infos = Map.data reg.by_variant in
  let nonempty_dfn_infos =
    List.filter all_dfn_infos ~f:(fun i -> not (List.is_empty i.entries))
  in
  let all_tops =
    rewritten_tops
    @ List.map ~f:gen_typedef nonempty_dfn_infos
    @ List.map nonempty_dfn_infos ~f:(gen_apply_fn reg)
  in
  Compiler_error.try_with (fun () -> topo_sort all_tops)
;;
