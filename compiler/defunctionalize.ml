open Core
open Lower_tuples
open Lambda_lift

include Compiler_error.Pass (struct
    let name = "defunctionalize"
  end)

(* ======= Generic AST ======= *)

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
  | TySampler -> "sampler"
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

let mk ~loc desc ty : Lambda_lift.term = { desc; ty; loc }

let map_children (f : Lambda_lift.term -> Lambda_lift.term) (t : Lambda_lift.term)
  : Lambda_lift.term
  =
  let desc : Lambda_lift.term_desc =
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f)
    | App (g, xs) -> App (f g, List.map xs ~f)
    | Let (v, a, b) -> Let (v, f a, f b)
    | If (a, b, c) -> If (f a, f b, f c)
    | Bop (op, a, b) -> Bop (op, f a, f b)
    | Index (a, i) -> Index (f a, i)
    | Builtin (bi, ts) -> Builtin (bi, List.map ts ~f)
    | Record ts -> Record (List.map ts ~f)
    | Field (a, fl) -> Field (f a, fl)
    | Variant (c, ts) -> Variant (c, List.map ts ~f)
    | Match (s, cs) -> Match (f s, List.map cs ~f:(fun (p, b) -> p, f b))
  in
  { t with desc }
;;

let fold_children (f : 'a -> Lambda_lift.term -> 'a) (acc : 'a) (t : Lambda_lift.term)
  : 'a
  =
  match t.desc with
  | Var _ | Float _ | Int _ | Bool _ -> acc
  | Vec (_, ts) | Builtin (_, ts) | Record ts | Variant (_, ts) ->
    List.fold ts ~init:acc ~f
  | App (g, xs) -> List.fold (g :: xs) ~init:acc ~f
  | Let (_, a, b) | Bop (_, a, b) -> f (f acc a) b
  | If (a, b, c) -> f (f (f acc a) b) c
  | Index (a, _) | Field (a, _) -> f acc a
  | Match (s, cs) -> List.fold cs ~init:(f acc s) ~f:(fun acc (_, b) -> f acc b)
;;

(* Substitute variables by terms. Binders are globally unique after [uniquify],
   so substitution never captures and binder scopes need no special handling. *)
let rec subst (subs : Lambda_lift.term String.Map.t) (t : Lambda_lift.term)
  : Lambda_lift.term
  =
  match t.desc with
  | Var v -> Map.find subs v |> Option.value ~default:t
  | _ -> map_children (subst subs) t
;;

let rec ty_struct_deps (ty : ty) : String.Set.t =
  match ty with
  | TyRecord s | TyVariant s -> String.Set.singleton s
  | TyArrow (a, b) -> Set.union (ty_struct_deps a) (ty_struct_deps b)
  | TyFloat | TyInt | TyBool | TySampler -> String.Set.empty
  | TyVec (_, t) -> ty_struct_deps t
;;

let rec global_refs_of (globals : String.Set.t) (term : Lambda_lift.term) : String.Set.t =
  match term.desc with
  | Var v -> if Set.mem globals v then String.Set.singleton v else String.Set.empty
  | _ ->
    fold_children
      (fun acc t -> Set.union acc (global_refs_of globals t))
      String.Set.empty
      term
;;

let rec term_ty_deps (t : Lambda_lift.term) : String.Set.t =
  fold_children (fun acc t -> Set.union acc (term_ty_deps t)) (ty_struct_deps t.ty) t
;;

let typedef_decl_deps = function
  | RecordDecl fields ->
    List.fold fields ~init:String.Set.empty ~f:(fun acc (_, ty) ->
      Set.union acc (ty_struct_deps ty))
  | VariantDecl ctors ->
    List.fold ctors ~init:String.Set.empty ~f:(fun acc (_, tys) ->
      List.fold tys ~init:acc ~f:(fun acc ty -> Set.union acc (ty_struct_deps ty)))
;;

(* ======= Per-arrow registry ======= *)
(*                                                                            *)
(******************************************************************************)

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

(** The fold below registers one [fn_type_info] per arrow type, and every closure
    of that arrow becomes a constructor [entry]. Every var/app refers to a placeholder
    [variant_name]/[apply_name]. Leveled family is later [stratify]'d. *)
type fn_type_info =
  { arrow_ty : ty
  ; arg_tys : ty list
  ; ret_ty : ty
  ; variant_name : string
  ; apply_name : string
  ; entries : fn_entry list
  }

(* [by_arrow]: [mangle_ty ty] => the arrow's canonical info
   [by_variant]: [variant_name] => same info, keyed by placeholder variant *)
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
      }
    in
    ( { by_arrow = Map.set reg.by_arrow ~key ~data:info
      ; by_variant = Map.set reg.by_variant ~key:info.variant_name ~data:info
      }
    , info )
;;

(* Retype function-typed params to their corresponding variant types *)
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
    |> String.Map.of_alist_or_error
    |> of_or_error
    |> ok_exn
  in
  reg, params', env
;;

let register_info (reg : registry) (info : fn_type_info) : registry =
  { by_arrow = Map.set reg.by_arrow ~key:(mangle_ty info.arrow_ty) ~data:info
  ; by_variant = Map.set reg.by_variant ~key:info.variant_name ~data:info
  }
;;

let add_lambda_entry (reg : registry) (ty : ty) params body captured loc
  : registry * fn_type_info * string
  =
  let reg, info = get_or_create_info reg ty in
  let ctor_name = Utils.fresh "lctor" in
  let entry = LambdaEntry { ctor_name; params; body; captured; loc } in
  let info = { info with entries = info.entries @ [ entry ] } in
  register_info reg info, info, ctor_name
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
    register_info reg info, info, ctor_name
;;

(* ======= Read-only views of the registry ======= *)

let info_by_key (reg : registry) (key : string) : fn_type_info =
  Map.find_exn reg.by_arrow key
;;

let is_dfn_ty (reg : registry) = function
  | TyVariant v -> Map.mem reg.by_variant v
  | _ -> false
;;

let key_of_dfn_ty (reg : registry) = function
  | TyVariant v ->
    (match Map.find reg.by_variant v with
     | Some info -> mangle_ty info.arrow_ty
     | None -> raise "(unreachable) key_of_dfn_ty on non-closure")
  | _ -> raise "(unreachable) key_of_dfn_ty on non-closure"
;;

(** Placeholder apply name => owning arrow key *)
let apply_to_key (reg : registry) : string String.Map.t =
  Map.data reg.by_variant
  |> List.map ~f:(fun info -> info.apply_name, mangle_ty info.arrow_ty)
  |> String.Map.of_alist_reduce ~f:Fn.const
;;

(** Constructor name => (entry, owning arrow key) *)
let entry_of_ctor (reg : registry) : (fn_entry * string) String.Map.t =
  Map.data reg.by_variant
  |> List.concat_map ~f:(fun info ->
    List.map info.entries ~f:(fun e ->
      let name =
        match e with
        | LambdaEntry le -> le.ctor_name
        | GlobalEntry ge -> ge.ctor_name
      in
      name, (e, mangle_ty info.arrow_ty)))
  |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
;;

let is_closure_ctor (entries : (fn_entry * string) String.Map.t) c = Map.mem entries c

let key_of_ctor (entries : (fn_entry * string) String.Map.t) c =
  snd (Map.find_exn entries c)
;;

let captured_of (entries : (fn_entry * string) String.Map.t) c =
  match fst (Map.find_exn entries c) with
  | LambdaEntry e -> e.captured
  | GlobalEntry _ -> []
;;

(* for each payload position of [c], whether it stores a closure of the SAME arrow *)
let same_arrow_flags (reg : registry) (entries : (fn_entry * string) String.Map.t) c =
  let key = key_of_ctor entries c in
  List.map (captured_of entries c) ~f:(fun (_, ty) ->
    match ty with
    | TyVariant v ->
      (match Map.find reg.by_variant v with
       | Some info -> String.equal (mangle_ty info.arrow_ty) key
       | None -> false)
    | _ -> false)
;;

(* ======= LEVEL AGNOSTIC Reynolds fold ======= *)

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
        reg, mk ~loc:t.loc (Variant (ctor_name, [])) (TyVariant info.variant_name)
      | None -> reg, t)
    else reg, t
  | App (f, args) ->
    let reg, args = rw_list reg args in
    (match f.desc with
     | Var v when Map.mem ctx.globals v -> rewrite_global_app ctx reg t f v args
     | _ when is_fn_ty f.ty -> rewrite_value_app ctx reg t f args
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

(** Application whose head is a known global name. *)
and rewrite_global_app
      (ctx : ctx)
      (reg : registry)
      (t : Lambda_lift.term)
      (f : Lambda_lift.term)
      (v : string)
      (args : Lambda_lift.term list)
  : registry * Lambda_lift.term
  =
  match Map.find ctx.globals v with
  | Some ClosureVal ->
    (* Evaluates to a closure value: route through dapply instead of calling by name *)
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
    let apply_var = mk ~loc:f.loc (Var actual_info.apply_name) apply_ty in
    let f = { f with ty = TyVariant actual_info.variant_name } in
    reg, { t with desc = App (apply_var, f :: args); ty = actual_info.ret_ty }
  | Some (GlobalFn { arity = f_arity }) when is_fn_ty t.ty ->
    if List.length args < f_arity
    then (
      (* True partial application *)
      let remaining_arg_tys, final_ret_ty = flatten_arrow t.ty in
      let remaining_params =
        List.map remaining_arg_tys ~f:(fun ty -> Utils.fresh "ra", ty)
      in
      let captured = List.map args ~f:(fun arg -> Utils.fresh "ca", arg.ty) in
      let all_call_args =
        List.map (captured @ remaining_params) ~f:(fun (name, ty) ->
          mk ~loc:t.loc (Var name) ty)
      in
      let body = mk ~loc:t.loc (App (f, all_call_args)) final_ret_ty in
      make_closure
        reg
        ~loc:t.loc
        ~arrow_ty:t.ty
        ~params:remaining_params
        ~body
        ~captured
        ~payload:args)
    else (
      (* Full application of a HOF *)
      let reg, info = get_or_create_info reg t.ty in
      let ty =
        Map.find ctx.global_tys v |> Option.value ~default:(TyVariant info.variant_name)
      in
      reg, { t with desc = App (f, args); ty })
  | _ -> reg, { t with desc = App (f, args) }

(** Application whose head is a first-class function value *)
and rewrite_value_app
      (ctx : ctx)
      (reg : registry)
      (t : Lambda_lift.term)
      (f : Lambda_lift.term)
      (args : Lambda_lift.term list)
  : registry * Lambda_lift.term
  =
  let reg, info = get_or_create_info reg f.ty in
  let reg, f = rewrite_term ctx false reg f in
  (* Resolve actual DFn info based on the rewritten type of [f] *)
  let actual_info =
    match f.ty with
    | TyVariant vname -> Map.find reg.by_variant vname |> Option.value ~default:info
    | _ -> info
  in
  let apply_ty = build_arrow_ty (f.ty :: info.arg_tys) info.ret_ty in
  let apply_var = mk ~loc:f.loc (Var actual_info.apply_name) apply_ty in
  if is_fn_ty t.ty
  then (
    (* Partial application of a function value: capture it and the provided args *)
    let remaining_arg_tys = List.drop info.arg_tys (List.length args) in
    let cap_fn = Utils.fresh "ca", f.ty in
    let captured = List.map args ~f:(fun arg -> Utils.fresh "ca", arg.ty) in
    let remaining_params =
      List.map remaining_arg_tys ~f:(fun ty -> Utils.fresh "ra", ty)
    in
    let call_args =
      List.map
        ((cap_fn :: captured) @ remaining_params)
        ~f:(fun (name, ty) -> mk ~loc:t.loc (Var name) ty)
    in
    let body = mk ~loc:t.loc (App (apply_var, call_args)) info.ret_ty in
    make_closure
      reg
      ~loc:t.loc
      ~arrow_ty:t.ty
      ~params:remaining_params
      ~body
      ~captured:(cap_fn :: captured)
      ~payload:(f :: args))
  else reg, { t with desc = App (apply_var, f :: args) }

(** Register a fresh closure constructor and return its [Variant] value *)
and make_closure reg ~loc ~arrow_ty ~params ~body ~captured ~payload
  : registry * Lambda_lift.term
  =
  let reg, info, ctor_name = add_lambda_entry reg arrow_ty params body captured loc in
  reg, mk ~loc (Variant (ctor_name, payload)) (TyVariant info.variant_name)
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

(* ======= Stratified finalization ======= *)

(* Reynolds defunctionalization wants one variant + apply per arrow, but since we
   have no recursive types, we do finite unrolling by inling every closeure producing
   call and let binding so every clsure value is a concrete variant tree. Then, we
   emit a stratified family [C_i] for each arrow [k] of depth [i], so no struct is
   recrusive, then [apply_j] dispatches to all depth [j] constructors *)

let level_key key lvl = Printf.sprintf "%s#%d" key lvl
let pair_key orig d = Printf.sprintf "%s#%d" orig d

let map_top_term ~f (top : Lambda_lift.top) : Lambda_lift.top =
  match top.desc with
  | Define d -> { top with desc = Define { d with body = f d.body } }
  | Const (n, t) -> { top with desc = Const (n, f t) }
  | Extern _ | TypeDef _ -> top
;;

(* Beta-reduce closure-returning calls and closure-typed lets so that each
   closure value becomes a concrete [Variant] *)
let rec inline (reg : registry) defines ?(fuel = 100000) (t : Lambda_lift.term)
  : Lambda_lift.term
  =
  if fuel <= 0 then raise "closure inlining did not terminate (recursive closure?)";
  let beta params args body =
    match List.zip params args with
    | Ok pairs ->
      Some
        (inline reg defines ~fuel:(fuel - 1) (subst (String.Map.of_alist_exn pairs) body))
    | Unequal_lengths -> None
  in
  match t.desc with
  | App ({ desc = Var name; _ }, args) when is_dfn_ty reg t.ty && Map.mem defines name ->
    let params, body = Map.find_exn defines name in
    let args = List.map args ~f:(inline reg defines ~fuel) in
    Option.value
      (beta params args body)
      ~default:(map_children (inline reg defines ~fuel) t)
  | Let (v, b, body) when is_dfn_ty reg b.ty ->
    inline
      reg
      defines
      ~fuel:(fuel - 1)
      (subst (String.Map.singleton v (inline reg defines ~fuel b)) body)
  | _ -> map_children (inline reg defines ~fuel) t
;;

(** Structural depth of a concrete closure value *)
let rec depth (reg : registry) entries (t : Lambda_lift.term) : int =
  match t.desc with
  | Variant (c, payload) when is_closure_ctor entries c ->
    List.zip_exn (same_arrow_flags reg entries c) payload
    |> List.filter_map ~f:(fun (s, p) -> if s then Some (depth reg entries p) else None)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_map ~default:0 ~f:(( + ) 1)
  | _ -> 0
;;

(** Every (ctor, depth) occurring in the program *)
let collect_occurrences (reg : registry) entries (tops : Lambda_lift.top list)
  : (string * int) list
  =
  let rec collect acc (t : Lambda_lift.term) =
    let acc =
      match t.desc with
      | Variant (c, _) when is_closure_ctor entries c ->
        Map.set acc ~key:(pair_key c (depth reg entries t)) ~data:(c, depth reg entries t)
      | _ -> acc
    in
    fold_children collect acc t
  in
  List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
    match top.desc with
    | Define { body; _ } -> collect acc body
    | Const (_, t) -> collect acc t
    | _ -> acc)
  |> Map.data
;;

(** The generated stratified naming scheme, derived once from the occurrence set. *)
type levels =
  { big_d : string -> int (* max capture depth per arrow key *)
  ; lv : string -> int -> string (* variant name at (arrow key, level) *)
  ; la : string -> int -> string (* apply name at (arrow key, level) *)
  ; cn : string -> int -> string (* ctor name at (orig ctor, depth) *)
  ; occ_by_key : (string * int) list String.Map.t
  }

let build_levels (reg : registry) entries (occ : (string * int) list) : levels =
  let big_d_tbl =
    List.fold occ ~init:String.Map.empty ~f:(fun acc (c, d) ->
      let key = key_of_ctor entries c in
      Map.update acc key ~f:(function
        | None -> d
        | Some m -> Int.max m d))
  in
  let big_d key = Map.find big_d_tbl key |> Option.value ~default:0 in
  let level_names =
    Map.keys reg.by_arrow
    |> List.concat_map ~f:(fun key ->
      List.init
        (big_d key + 1)
        ~f:(fun lvl -> level_key key lvl, (Utils.fresh "DFn", Utils.fresh "dapply")))
    |> String.Map.of_alist_exn
  in
  let lv key lvl = fst (Map.find_exn level_names (level_key key lvl)) in
  let la key lvl = snd (Map.find_exn level_names (level_key key lvl)) in
  let ctor_at =
    List.map occ ~f:(fun (c, d) -> pair_key c d, Utils.fresh "lctor")
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  in
  let cn c d = Map.find_exn ctor_at (pair_key c d) in
  let occ_by_key =
    List.fold occ ~init:String.Map.empty ~f:(fun acc (c, d) ->
      Map.add_multi acc ~key:(key_of_ctor entries c) ~data:(c, d))
    |> Map.map ~f:List.rev
  in
  { big_d; lv; la; cn; occ_by_key }
;;

(* The read-only environment threaded through the stratification rewrite *)
type st =
  { reg : registry
  ; entries : (fn_entry * string) String.Map.t
  ; levels : levels
  ; apply_to_key : string String.Map.t (* placeholder apply name => arrow key *)
  ; defines : (string list * Lambda_lift.term) String.Map.t
  ; fallback : Lexer.loc
    (* loc for synthesized nodes with no natural source loc, maybe pointless *)
  }

let rec resolve_ty (st : st) (ty : ty) : ty =
  match ty with
  | TyVariant v ->
    (match Map.find st.reg.by_variant v with
     | Some info ->
       let key = mangle_ty info.arrow_ty in
       TyVariant (st.levels.lv key (st.levels.big_d key))
     | None -> ty)
  | TyArrow (a, b) -> TyArrow (resolve_ty st a, resolve_ty st b)
  | TyVec (n, t) -> TyVec (n, resolve_ty st t)
  | TyFloat | TyInt | TyBool | TyRecord _ | TySampler -> ty
;;

(** Rewrite the program to the stratified representation *)
let resolve (st : st) =
  let rec go ~env ~slot (t : Lambda_lift.term) : Lambda_lift.term =
    let here desc ty = mk ~loc:t.loc desc ty in
    let top = go ~env ~slot:None in
    match t.desc with
    | Variant (c, payload) when is_closure_ctor st.entries c ->
      let key = key_of_ctor st.entries c in
      let d = depth st.reg st.entries t in
      let this_lvl = Option.value slot ~default:(st.levels.big_d key) in
      let payload =
        List.zip_exn (same_arrow_flags st.reg st.entries c) payload
        |> List.map ~f:(fun (sa, p) ->
          go ~env ~slot:(if sa then Some (d - 1) else None) p)
      in
      here (Variant (st.levels.cn c d, payload)) (TyVariant (st.levels.lv key this_lvl))
    | App ({ desc = Var aph; _ }, args) when Map.mem st.apply_to_key aph ->
      let key = Map.find_exn st.apply_to_key aph in
      let head = List.hd_exn args in
      let lvl =
        match head.desc with
        | Var v -> Map.find env v |> Option.value ~default:(st.levels.big_d key)
        | Variant (c, _) when is_closure_ctor st.entries c -> depth st.reg st.entries head
        | _ -> st.levels.big_d key
      in
      let args =
        List.mapi args ~f:(fun i a -> if i = 0 then go ~env ~slot:(Some lvl) a else top a)
      in
      let f =
        mk
          ~loc:t.loc
          (Var (st.levels.la key lvl))
          (build_arrow_ty (List.map args ~f:(fun a -> a.ty)) (resolve_ty st t.ty))
      in
      here (App (f, args)) (resolve_ty st t.ty)
    | Var v when is_dfn_ty st.reg t.ty ->
      let key = key_of_dfn_ty st.reg t.ty in
      let lvl = Map.find env v |> Option.value ~default:(st.levels.big_d key) in
      here (Var v) (TyVariant (st.levels.lv key lvl))
    | _ -> { (map_children top t) with ty = resolve_ty st t.ty }
  in
  go
;;

let resolve_top (st : st) (top : Lambda_lift.top) : Lambda_lift.top =
  let r t = resolve st ~env:String.Map.empty ~slot:None t in
  match top.desc with
  | Define { name; recur; args; body; ret_ty } ->
    let args = List.map args ~f:(fun (v, ty) -> v, resolve_ty st ty) in
    let body = r body in
    { top with
      desc = Define { name; recur; args; body; ret_ty = resolve_ty st ret_ty }
    ; ty = build_arrow_ty (List.map args ~f:snd) body.ty
    }
  | Const (n, t) ->
    let t = r t in
    { top with desc = Const (n, t); ty = t.ty }
  | TypeDef (n, RecordDecl fs) ->
    { top with
      desc = TypeDef (n, RecordDecl (List.map fs ~f:(Tuple2.map_snd ~f:(resolve_ty st))))
    }
  | TypeDef (n, VariantDecl cs) ->
    { top with
      desc =
        TypeDef
          ( n
          , VariantDecl (List.map cs ~f:(Tuple2.map_snd ~f:(List.map ~f:(resolve_ty st))))
          )
    }
  | Extern _ -> top
;;

let loc_of_key (st : st) key =
  match (info_by_key st.reg key).entries with
  | e :: _ -> entry_loc e
  | [] -> st.fallback
;;

let ctor_field_tys (st : st) c d =
  List.zip_exn (same_arrow_flags st.reg st.entries c) (captured_of st.entries c)
  |> List.map ~f:(fun (sa, (_, ty)) ->
    if sa
    then TyVariant (st.levels.lv (key_of_ctor st.entries c) (d - 1))
    else resolve_ty st ty)
;;

(** C_lvl = constructors of depth <= lvl *)
let gen_typedef (st : st) key lvl : Lambda_lift.top =
  let ctors =
    Map.find st.levels.occ_by_key key
    |> Option.value ~default:[]
    |> List.filter ~f:(fun (_, d) -> d <= lvl)
    |> List.map ~f:(fun (c, d) -> st.levels.cn c d, ctor_field_tys st c d)
  in
  { desc = TypeDef (st.levels.lv key lvl, VariantDecl ctors)
  ; ty = TyVariant (st.levels.lv key lvl)
  ; loc = loc_of_key st key
  }
;;

(** C_lvl -> args -> ret *)
let gen_apply (st : st) key lvl : Lambda_lift.top option =
  let info = info_by_key st.reg key in
  let loc = loc_of_key st key in
  let ctors =
    Map.find st.levels.occ_by_key key
    |> Option.value ~default:[]
    |> List.filter ~f:(fun (_, d) -> d <= lvl)
  in
  if List.is_empty ctors
  then None
  else (
    let fn_var = Utils.fresh "dfn" in
    let arg_vars =
      List.map info.arg_tys ~f:(fun ty -> Utils.fresh "da", resolve_ty st ty)
    in
    let cases =
      List.map ctors ~f:(fun (c, d) ->
        let captured = captured_of st.entries c in
        let pat =
          Frontend.PatCtor
            (st.levels.cn c d, List.map captured ~f:(fun (v, _) -> Frontend.PatVar v))
        in
        (* Same-arrow captured vars live one level down *)
        let env =
          List.zip_exn (same_arrow_flags st.reg st.entries c) captured
          |> List.filter_map ~f:(fun (sa, (v, _)) -> if sa then Some (v, d - 1) else None)
          |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
        in
        let entry, _ = Map.find_exn st.entries c in
        let body =
          match entry with
          | LambdaEntry e ->
            let arg_terms = List.map arg_vars ~f:(fun (v, ty) -> mk ~loc (Var v) ty) in
            let bound =
              subst
                (String.Map.of_alist_exn
                   (List.zip_exn (List.map e.params ~f:fst) arg_terms))
                e.body
            in
            (* Expose the underlying HOF body so captured-base applications
               become visible and can target apply_{d-1} *)
            let bound =
              match bound.desc with
              | App ({ desc = Var g; _ }, gargs) when Map.mem st.defines g ->
                let gparams, gbody = Map.find_exn st.defines g in
                (match List.zip gparams gargs with
                 | Ok ps -> subst (String.Map.of_alist_reduce ps ~f:(fun a _ -> a)) gbody
                 | Unequal_lengths -> bound)
              | _ -> bound
            in
            resolve st ~env ~slot:None bound
          | GlobalEntry e ->
            let args = List.map arg_vars ~f:(fun (v, ty) -> mk ~loc (Var v) ty) in
            let call =
              mk ~loc (App (mk ~loc (Var e.fn_name) info.arrow_ty, args)) info.ret_ty
            in
            resolve st ~env:String.Map.empty ~slot:None call
        in
        pat, body)
    in
    let scrut = mk ~loc (Var fn_var) (TyVariant (st.levels.lv key lvl)) in
    let match_term = mk ~loc (Match (scrut, cases)) (resolve_ty st info.ret_ty) in
    let params = (fn_var, TyVariant (st.levels.lv key lvl)) :: arg_vars in
    Some
      { desc =
          Define
            { name = st.levels.la key lvl
            ; recur = Frontend.Nonrec
            ; args = params
            ; body = match_term
            ; ret_ty = resolve_ty st info.ret_ty
            }
      ; ty = build_arrow_ty (List.map params ~f:snd) (resolve_ty st info.ret_ty)
      ; loc
      })
;;

let stratify (reg : registry) (tops : Lambda_lift.top list) : Lambda_lift.top list =
  let entries = entry_of_ctor reg in
  let defines =
    List.filter_map tops ~f:(fun (top : Lambda_lift.top) ->
      match top.desc with
      | Define { name; args; body; _ } -> Some (name, (List.map args ~f:fst, body))
      | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  in
  (* Make closures concrete, then read off the occurring (ctor, depth) pairs *)
  let inlined_tops = List.map tops ~f:(map_top_term ~f:(inline reg defines ?fuel:None)) in
  let occ = collect_occurrences reg entries inlined_tops in
  let levels = build_levels reg entries occ in
  let fallback =
    match tops with
    | top :: _ -> top.loc
    | [] -> raise "unreachable, defunctionalize on empty program"
  in
  let st = { reg; entries; levels; apply_to_key = apply_to_key reg; defines; fallback } in
  let program = List.map inlined_tops ~f:(resolve_top st) in
  let generated =
    Map.keys reg.by_arrow
    |> List.concat_map ~f:(fun key ->
      List.init (levels.big_d key + 1) ~f:(fun lvl -> key, lvl))
    |> List.concat_map ~f:(fun (key, lvl) ->
      gen_typedef st key lvl :: Option.to_list (gen_apply st key lvl))
  in
  program @ generated
;;

(* ======= Topo sort and Reachability ======= *)

let topo_sort (all_tops : Lambda_lift.top list) : Lambda_lift.t =
  let key_of (top : Lambda_lift.top) =
    match top.desc with
    | Define { name; _ } | Const (name, _) | Extern name | TypeDef (name, _) -> name
  in
  let globals = String.Set.of_list (List.map all_tops ~f:key_of) in
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
  let by_key =
    List.map all_tops ~f:(fun top -> key_of top, top)
    |> String.Map.of_alist_or_error
    |> of_or_error
    |> ok_exn
  in
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
  let live = List.filter all_tops ~f:(fun top -> Set.mem reachable (key_of top)) in
  let nodes = List.map live ~f:key_of in
  let edges =
    List.concat_map live ~f:(fun top ->
      let name = key_of top in
      Set.to_list (deps_of top)
      |> List.filter_map ~f:(fun dep ->
        if String.equal dep name || not (Set.mem reachable dep)
        then None
        else Some { Topological_sort.Edge.from = dep; to_ = name }))
  in
  let labels =
    Topological_sort.sort (module String) ~what:Nodes ~nodes ~edges
    |> of_or_error
    |> ok_exn
  in
  Program (List.filter_map labels ~f:(Map.find by_key))
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
  Compiler_error.try_with (fun () -> topo_sort (stratify reg rewritten_tops))
;;
