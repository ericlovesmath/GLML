open Core
open Typecheck

(* TODO: Combine the poly envs into one, some refactoring? Or at least remove [poly] name *)

(** Maps parametrized struct names to (params, fields) *)
type struct_poly_env = (string list * (string * ty) list) String.Map.t

(** Maps parametrized variant names to (params, ctors) *)
type variant_poly_env = (string list * (string * ty list) list) String.Map.t

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
  | TyVariant (s, []) -> s
  (* TODO: This makes name mangling for records and variants potentially clash,
     maybe I should somehow generate the unique names to account for this in the
     future. Right now, even something like nested variants in different ways
     might be indistinguishable. *)
  | TyVariant (s, args) -> String.concat ~sep:"_" (s :: List.map args ~f:mangle_ty)
  | TyVar v -> "tv" ^ v
  | TyArrow (a, b) -> mangle_ty a ^ "_to_" ^ mangle_ty b
;;

let rec is_concrete (ty : ty) : bool =
  match ty with
  | TyVar _ -> false
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ -> true
  | TyVariant (_, args) -> List.for_all args ~f:is_concrete
  | TyRecord (_, args) -> List.for_all args ~f:is_concrete
  | TyArrow (a, b) -> is_concrete a && is_concrete b
;;

(** Collect (name, args) instantiation pairs in topological dependency order.
    Recursively expands field/ctor types to find transitive dependencies. *)
let rec collect_record_from_ty
          ~(struct_poly_env : struct_poly_env)
          ~(variant_poly_env : variant_poly_env)
          (ty : ty)
  : (string * ty list) list
  =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVar _ -> []
  | TyVariant (_, args) ->
    List.concat_map args ~f:(collect_record_from_ty ~struct_poly_env ~variant_poly_env)
  | TyArrow (a, b) ->
    collect_record_from_ty ~struct_poly_env ~variant_poly_env a
    @ collect_record_from_ty ~struct_poly_env ~variant_poly_env b
  | TyRecord (_, []) -> []
  | TyRecord (name, args) when not (is_concrete (TyRecord (name, args))) -> []
  | TyRecord (name, args) ->
    let deps_from_args =
      List.concat_map args ~f:(collect_record_from_ty ~struct_poly_env ~variant_poly_env)
    in
    let deps_from_fields =
      match Map.find struct_poly_env name with
      | None -> []
      | Some (params, fields) ->
        let sub = List.zip_exn params args in
        List.concat_map fields ~f:(fun (_, fty) ->
          let fty = subst_ty sub fty in
          collect_record_from_ty ~struct_poly_env ~variant_poly_env fty)
    in
    deps_from_args @ deps_from_fields @ [ name, args ]
;;

let rec collect_variant_from_ty
          ~(struct_poly_env : struct_poly_env)
          ~(variant_poly_env : variant_poly_env)
          (ty : ty)
  : (string * ty list) list
  =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVar _ -> []
  | TyRecord (_, args) ->
    List.concat_map args ~f:(collect_variant_from_ty ~struct_poly_env ~variant_poly_env)
  | TyArrow (a, b) ->
    collect_variant_from_ty ~struct_poly_env ~variant_poly_env a
    @ collect_variant_from_ty ~struct_poly_env ~variant_poly_env b
  | TyVariant (_, []) -> []
  | TyVariant (name, args) when not (is_concrete (TyVariant (name, args))) -> []
  | TyVariant (name, args) ->
    let deps_from_args =
      List.concat_map args ~f:(collect_variant_from_ty ~struct_poly_env ~variant_poly_env)
    in
    let deps_from_ctors =
      match Map.find variant_poly_env name with
      | None -> []
      | Some (params, ctors) ->
        let sub = List.zip_exn params args in
        List.concat_map ctors ~f:(fun (_, tys) ->
          List.concat_map tys ~f:(fun fty ->
            let fty = subst_ty sub fty in
            collect_variant_from_ty ~struct_poly_env ~variant_poly_env fty))
    in
    deps_from_args @ deps_from_ctors @ [ name, args ]
;;

let collect_from_ty
      ~(struct_poly_env : struct_poly_env)
      ~(variant_poly_env : variant_poly_env)
      (ty : ty)
  : [ `Record of string * ty list | `Variant of string * ty list ] list
  =
  let records = collect_record_from_ty ~struct_poly_env ~variant_poly_env ty in
  let variants = collect_variant_from_ty ~struct_poly_env ~variant_poly_env ty in
  List.map records ~f:(fun x -> `Record x) @ List.map variants ~f:(fun x -> `Variant x)
;;

let collect_from_term
      ~(struct_poly_env : struct_poly_env)
      ~(variant_poly_env : variant_poly_env)
      (t : term)
  : [ `Record of string * ty list | `Variant of string * ty list ] list
  =
  let rec walk (t : term) =
    let from_ty = collect_from_ty ~struct_poly_env ~variant_poly_env t.ty in
    let from_desc =
      match t.desc with
      | Var _ | Float _ | Int _ | Bool _ -> []
      | Vec (_, ts) | Mat (_, _, ts) | Builtin (_, ts) | Record (_, ts) ->
        List.concat_map ts ~f:walk
      | Lam (_, body) -> walk body
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

(* TODO: I'm too lazy I need to refactor this, is there a native [List.concat_map] for
   [Either] types, maybe? That seems reasonable. *)
let collect_from_top
      ~(struct_poly_env : struct_poly_env)
      ~(variant_poly_env : variant_poly_env)
      (top : top)
  : [ `Record of string * ty list | `Variant of string * ty list ] list
  =
  match top.desc with
  | TypeDef (_, RecordDecl (params, fields)) ->
    if List.is_empty params
    then
      List.concat_map fields ~f:(fun (_, fty) ->
        collect_from_ty ~struct_poly_env ~variant_poly_env fty)
    else []
  | TypeDef (_, VariantDecl (params, ctors)) ->
    if List.is_empty params
    then
      List.concat_map ctors ~f:(fun (_, tys) ->
        List.concat_map tys ~f:(collect_from_ty ~struct_poly_env ~variant_poly_env))
    else []
  | Extern _ -> []
  | Define (_, _, bind) ->
    collect_from_ty ~struct_poly_env ~variant_poly_env top.ty
    @ collect_from_term ~struct_poly_env ~variant_poly_env bind
;;

let dedup_tagged_instances
      (instances : [ `Record of string * ty list | `Variant of string * ty list ] list)
  : [ `Record of string * ty list | `Variant of string * ty list ] list
  =
  let eq a b =
    match a, b with
    | `Record (n, args), `Record (n', args') ->
      String.equal n n' && List.equal equal_ty args args'
    | `Variant (n, args), `Variant (n', args') ->
      String.equal n n' && List.equal equal_ty args args'
    | _, _ -> false
  in
  List.fold instances ~init:[] ~f:(fun acc inst ->
    if List.exists acc ~f:(eq inst) then acc else inst :: acc)
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

let specialize_variant
      ~(variant_poly_env : variant_poly_env)
      ~(loc : Lexer.loc)
      (name : string)
      (args : ty list)
  : top
  =
  let params, ctors = Map.find_exn variant_poly_env name in
  let sub = List.zip_exn params args in
  let specialized_ctors =
    List.map ctors ~f:(fun (ctor_name, tys) -> ctor_name, List.map tys ~f:(subst_ty sub))
  in
  let mangled = mangle_ty (TyVariant (name, args)) in
  { desc = TypeDef (mangled, VariantDecl ([], specialized_ctors))
  ; ty = TyVariant (mangled, [])
  ; loc
  ; scheme_constrs = []
  }
;;

let rec rewrite_ty
          ~(struct_poly_env : struct_poly_env)
          ~(variant_poly_env : variant_poly_env)
          (ty : ty)
  : ty
  =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVar _ -> ty
  | TyArrow (a, b) ->
    TyArrow
      ( rewrite_ty ~struct_poly_env ~variant_poly_env a
      , rewrite_ty ~struct_poly_env ~variant_poly_env b )
  | TyRecord (_, []) -> ty
  | TyRecord (name, args)
    when is_concrete (TyRecord (name, args)) && Map.mem struct_poly_env name ->
    TyRecord (mangle_ty (TyRecord (name, args)), [])
  | TyRecord (name, args) ->
    TyRecord (name, List.map args ~f:(rewrite_ty ~struct_poly_env ~variant_poly_env))
  | TyVariant (_, []) -> ty
  | TyVariant (name, args)
    when is_concrete (TyVariant (name, args)) && Map.mem variant_poly_env name ->
    TyVariant (mangle_ty (TyVariant (name, args)), [])
  | TyVariant (name, args) ->
    TyVariant (name, List.map args ~f:(rewrite_ty ~struct_poly_env ~variant_poly_env))
;;

let rec rewrite_term
          ~(struct_poly_env : struct_poly_env)
          ~(variant_poly_env : variant_poly_env)
          (t : term)
  : term
  =
  let rewrite = rewrite_term ~struct_poly_env ~variant_poly_env in
  let ty = rewrite_ty ~struct_poly_env ~variant_poly_env t.ty in
  let desc : term_desc =
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rewrite)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:rewrite)
    | Lam (v, body) -> Lam (v, rewrite body)
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
    | Variant (ty_name, ctor, args) ->
      (* TODO: Duplicated from above mostly... *)
      let new_ty_name =
        match t.ty with
        | TyVariant (n, vargs)
          when (not (List.is_empty vargs))
               && is_concrete (TyVariant (n, vargs))
               && Map.mem variant_poly_env n -> mangle_ty (TyVariant (n, vargs))
        | _ -> ty_name
      in
      Variant (new_ty_name, ctor, List.map args ~f:rewrite)
    | Match (scrutinee, cases) ->
      Match (rewrite scrutinee, List.map cases ~f:(fun (pat, body) -> pat, rewrite body))
  in
  { t with ty; desc }
;;

let rewrite_top
      ~(struct_poly_env : struct_poly_env)
      ~(variant_poly_env : variant_poly_env)
      (top : top)
  : top
  =
  let ty = rewrite_ty ~struct_poly_env ~variant_poly_env top.ty in
  let desc : top_desc =
    match top.desc with
    | Define (r, v, bind) ->
      Define (r, v, rewrite_term ~struct_poly_env ~variant_poly_env bind)
    | Extern _ -> top.desc
    | TypeDef (name, RecordDecl (params, fields)) ->
      let fields =
        List.map fields ~f:(fun (fn, fty) ->
          fn, rewrite_ty ~struct_poly_env ~variant_poly_env fty)
      in
      TypeDef (name, RecordDecl (params, fields))
    | TypeDef (name, VariantDecl (params, ctors)) ->
      let ctors =
        List.map ctors ~f:(fun (cn, tys) ->
          cn, List.map tys ~f:(rewrite_ty ~struct_poly_env ~variant_poly_env))
      in
      TypeDef (name, VariantDecl (params, ctors))
  in
  { top with ty; desc }
;;

let specialize (Program tops : t) : t =
  let struct_poly_tops =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, RecordDecl (params, fields)) when not (List.is_empty params) ->
        Some (name, params, fields, top.loc)
      | _ -> None)
  in
  let struct_poly_env =
    struct_poly_tops
    |> List.map ~f:(fun (name, params, fields, _) -> name, (params, fields))
    |> String.Map.of_alist_exn
  in
  let struct_locs =
    struct_poly_tops
    |> List.map ~f:(fun (name, _, _, loc) -> name, loc)
    |> String.Map.of_alist_exn
  in
  let variant_poly_tops =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, VariantDecl (params, ctors)) when not (List.is_empty params) ->
        Some (name, params, ctors, top.loc)
      | _ -> None)
  in
  let variant_poly_env =
    variant_poly_tops
    |> List.map ~f:(fun (name, params, ctors, _) -> name, (params, ctors))
    |> String.Map.of_alist_exn
  in
  let variant_locs =
    variant_poly_tops
    |> List.map ~f:(fun (name, _, _, loc) -> name, loc)
    |> String.Map.of_alist_exn
  in
  let all_instances =
    List.concat_map tops ~f:(collect_from_top ~struct_poly_env ~variant_poly_env)
    |> dedup_tagged_instances
  in
  let struct_def_tops =
    List.filter_map all_instances ~f:(function
      | `Record (name, args) when Map.mem struct_poly_env name ->
        let loc = Map.find_exn struct_locs name in
        Some (specialize_struct ~struct_poly_env ~loc name args)
      | _ -> None)
  in
  let variant_def_tops =
    List.filter_map all_instances ~f:(function
      | `Variant (name, args) when Map.mem variant_poly_env name ->
        let loc = Map.find_exn variant_locs name in
        Some (specialize_variant ~variant_poly_env ~loc name args)
      | _ -> None)
  in
  let filtered_tops =
    List.filter tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, RecordDecl _) when Map.mem struct_poly_env name -> false
      | TypeDef (name, VariantDecl _) when Map.mem variant_poly_env name -> false
      | _ -> true)
  in
  let rewritten =
    List.map
      (struct_def_tops @ variant_def_tops @ filtered_tops)
      ~f:(rewrite_top ~struct_poly_env ~variant_poly_env)
  in
  Program rewritten
;;
