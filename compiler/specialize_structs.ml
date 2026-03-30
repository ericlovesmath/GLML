open Core
open Typecheck

(** Maps parametrized struct names to (params, fields) *)
type struct_poly_env = (string list * (string * ty) list) String.Map.t

(** String suffix to add to generated functions *)
let rec mangle_ty (ty : ty) : string =
  match ty with
  | TyFloat -> "float"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVec n -> "vec" ^ Int.to_string n
  | TyMat (x, y) -> "mat" ^ Int.to_string x ^ "x" ^ Int.to_string y
  | TyRecord (s, []) -> s
  | TyRecord (s, args) -> String.concat ~sep:"_" (s :: List.map args ~f:mangle_ty)
  | TyVariant s -> s
  | TyVar v -> "tv" ^ v
  | TyArrow (a, b) -> mangle_ty a ^ "_to_" ^ mangle_ty b
;;

let rec is_concrete (ty : ty) : bool =
  match ty with
  | TyVar _ -> false
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVariant _ -> true
  | TyRecord (_, args) -> List.for_all args ~f:is_concrete
  | TyArrow (a, b) -> is_concrete a && is_concrete b
;;

(** Collect (name, args) instantiation pairs in topological dependency order.
    Recursively expands field types to find transitive dependencies. *)
let rec collect_from_ty ~(struct_poly_env : struct_poly_env) (ty : ty)
  : (string * ty list) list
  =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVariant _ | TyVar _ -> []
  | TyArrow (a, b) ->
    collect_from_ty ~struct_poly_env a @ collect_from_ty ~struct_poly_env b
  | TyRecord (_, []) -> []
  | TyRecord (name, args) when not (is_concrete (TyRecord (name, args))) -> []
  | TyRecord (name, args) ->
    let deps_from_args = List.concat_map args ~f:(collect_from_ty ~struct_poly_env) in
    let deps_from_fields =
      match Map.find struct_poly_env name with
      | None -> []
      | Some (params, fields) ->
        let sub = List.zip_exn params args in
        List.concat_map fields ~f:(fun (_, fty) ->
          let fty' = subst_ty sub fty in
          collect_from_ty ~struct_poly_env fty')
    in
    deps_from_args @ deps_from_fields @ [ name, args ]
;;

(* TODO: Does... Let bindings even need the type now? In that case do I even need this? *)
let collect_from_term ~(struct_poly_env : struct_poly_env) (t : term)
  : (string * ty list) list
  =
  let rec walk (t : term) =
    let from_ty = collect_from_ty ~struct_poly_env t.ty in
    let from_desc =
      match t.desc with
      | Var _ | Float _ | Int _ | Bool _ -> []
      | Vec (_, ts) | Mat (_, _, ts) | Builtin (_, ts) | Record (_, ts) ->
        List.concat_map ts ~f:walk
      | Lam (_, lam_ty, body) -> collect_from_ty ~struct_poly_env lam_ty @ walk body
      | App (f, x) -> walk f @ walk x
      | Let (_, _, _, bind, body) -> walk bind @ walk body
      | If (c, t, e) -> walk c @ walk t @ walk e
      | Bop (_, l, r) -> walk l @ walk r
      | Index (t, _) | Field (t, _) -> walk t
      | Variant (_, _, args) -> List.concat_map args ~f:walk
      | Match (scrutinee, cases) ->
        walk scrutinee @ List.concat_map cases ~f:(fun (_, body) -> walk body)
    in
    from_ty @ from_desc
  in
  walk t
;;

let collect_from_top ~(struct_poly_env : struct_poly_env) (top : top)
  : (string * ty list) list
  =
  match top.desc with
  | TypeDef (_, RecordDecl (params, fields)) ->
    if List.is_empty params
    then List.concat_map fields ~f:(fun (_, fty) -> collect_from_ty ~struct_poly_env fty)
    else []
  | TypeDef (_, VariantDecl ctors) ->
    List.concat_map ctors ~f:(fun (_, tys) ->
      List.concat_map tys ~f:(collect_from_ty ~struct_poly_env))
  | Extern _ -> []
  | Define (_, _, bind) ->
    collect_from_ty ~struct_poly_env top.ty @ collect_from_term ~struct_poly_env bind
;;

(** Stable dedup using structural equality, keeping first occurrence. *)
let equal_instance (n1, args1) (n2, args2) =
  String.equal n1 n2 && List.equal equal_ty args1 args2
;;

let dedup_instances (instances : (string * ty list) list) : (string * ty list) list =
  List.fold instances ~init:[] ~f:(fun acc inst ->
    if List.exists acc ~f:(equal_instance inst) then acc else inst :: acc)
  |> List.rev
;;

let specialize_struct
      ~(struct_poly_env : struct_poly_env)
      ~(loc : Lexer.loc)
      (name : string)
      (args : ty list)
  : top
  =
  let params, fields = Map.find_exn struct_poly_env name in
  let sub = List.zip_exn params args in
  let specialized_fields =
    List.map fields ~f:(fun (field_name, fty) -> field_name, subst_ty sub fty)
  in
  let mangled = mangle_ty (TyRecord (name, args)) in
  { desc = TypeDef (mangled, RecordDecl ([], specialized_fields))
  ; ty = TyRecord (mangled, [])
  ; loc
  ; scheme_constrs = []
  }
;;

let rec rewrite_ty ~(struct_poly_env : struct_poly_env) (ty : ty) : ty =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVariant _ | TyVar _ -> ty
  | TyArrow (a, b) ->
    TyArrow (rewrite_ty ~struct_poly_env a, rewrite_ty ~struct_poly_env b)
  | TyRecord (_, []) -> ty
  | TyRecord (name, args)
    when is_concrete (TyRecord (name, args)) && Map.mem struct_poly_env name ->
    TyRecord (mangle_ty (TyRecord (name, args)), [])
  | TyRecord (name, args) ->
    TyRecord (name, List.map args ~f:(rewrite_ty ~struct_poly_env))
;;

let rec rewrite_term ~(struct_poly_env : struct_poly_env) (t : term) : term =
  let rewrite = rewrite_term ~struct_poly_env in
  let ty = rewrite_ty ~struct_poly_env t.ty in
  let desc : term_desc =
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rewrite)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:rewrite)
    | Lam (v, lam_ty, body) -> Lam (v, rewrite_ty ~struct_poly_env lam_ty, rewrite body)
    | App (f, x) -> App (rewrite f, rewrite x)
    | Let (recur, v, constrs, bind, body) ->
      Let (recur, v, constrs, rewrite bind, rewrite body)
    | If (c, t, e) -> If (rewrite c, rewrite t, rewrite e)
    | Bop (op, l, r) -> Bop (op, rewrite l, rewrite r)
    | Index (t, i) -> Index (rewrite t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rewrite)
    | Record (name, ts) ->
      let new_name =
        match t.ty with
        | TyRecord (n, args)
          when (not (List.is_empty args))
               && is_concrete (TyRecord (n, args))
               && Map.mem struct_poly_env n -> mangle_ty (TyRecord (n, args))
        | _ -> name
      in
      Record (new_name, List.map ts ~f:rewrite)
    | Field (t, f) -> Field (rewrite t, f)
    | Variant (ty_name, ctor, args) -> Variant (ty_name, ctor, List.map args ~f:rewrite)
    | Match (scrutinee, cases) ->
      Match (rewrite scrutinee, List.map cases ~f:(fun (pat, body) -> pat, rewrite body))
  in
  { t with ty; desc }
;;

let rewrite_top ~(struct_poly_env : struct_poly_env) (top : top) : top =
  let ty = rewrite_ty ~struct_poly_env top.ty in
  let desc : top_desc =
    match top.desc with
    | Define (r, v, bind) -> Define (r, v, rewrite_term ~struct_poly_env bind)
    | Extern _ -> top.desc
    | TypeDef (name, RecordDecl (params, fields)) ->
      let fields =
        List.map fields ~f:(fun (fn, fty) -> fn, rewrite_ty ~struct_poly_env fty)
      in
      TypeDef (name, RecordDecl (params, fields))
    | TypeDef (name, VariantDecl ctors) ->
      let ctors =
        List.map ctors ~f:(fun (cn, tys) ->
          cn, List.map tys ~f:(rewrite_ty ~struct_poly_env))
      in
      TypeDef (name, VariantDecl ctors)
  in
  { top with ty; desc }
;;

let specialize (Program tops : t) : t =
  let poly_tops =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, RecordDecl (params, fields)) when not (List.is_empty params) ->
        Some (name, params, fields, top.loc)
      | _ -> None)
  in
  let struct_poly_env =
    poly_tops
    |> List.map ~f:(fun (name, params, fields, _) -> name, (params, fields))
    |> String.Map.of_alist_exn
  in
  let struct_locs =
    poly_tops
    |> List.map ~f:(fun (name, _, _, loc) -> name, loc)
    |> String.Map.of_alist_exn
  in
  let instances =
    List.concat_map tops ~f:(collect_from_top ~struct_poly_env) |> dedup_instances
  in
  let struct_def_tops =
    List.map instances ~f:(fun (name, args) ->
      let loc = Map.find_exn struct_locs name in
      specialize_struct ~struct_poly_env ~loc name args)
  in
  let filtered_tops =
    List.filter tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, RecordDecl _) when Map.mem struct_poly_env name -> false
      | _ -> true)
  in
  let rewritten =
    List.map (struct_def_tops @ filtered_tops) ~f:(rewrite_top ~struct_poly_env)
  in
  Program rewritten
;;
