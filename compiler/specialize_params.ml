open Core
open Typecheck

(** Maps parametrized type names to their declarations *)
type env = type_decl String.Map.t

(* TODO: We should not assume that we can look up again by mangling the type again and using that
   as the key, that feels like it could clash easily and be potentially very fragile *)

(** String suffix to add to generated functions *)
let rec mangle_ty (ty : ty) : string =
  match ty with
  | TyFloat -> "float"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVec n -> "vec" ^ Int.to_string n
  | TyMat (x, y) -> "mat" ^ Int.to_string x ^ "x" ^ Int.to_string y
  | TyRecord (s, []) -> s
  | TyRecord (s, args) -> "r_" ^ String.concat ~sep:"_" (s :: List.map args ~f:mangle_ty)
  | TyVariant (s, []) -> s
  | TyVariant (s, args) -> "v_" ^ String.concat ~sep:"_" (s :: List.map args ~f:mangle_ty)
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
let rec collect_from_ty ~(env : env) (ty : ty)
  : [ `Record of string * ty list | `Variant of string * ty list ] list
  =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVar _ -> []
  | TyArrow (a, b) -> collect_from_ty ~env a @ collect_from_ty ~env b
  | TyRecord (_, []) | TyVariant (_, []) -> []
  | TyRecord (name, args) when not (is_concrete (TyRecord (name, args))) -> []
  | TyVariant (name, args) when not (is_concrete (TyVariant (name, args))) -> []
  | TyRecord (name, args) ->
    (* TODO: This makes name mangling for records and variants potentially clash,
     maybe I should somehow generate the unique names to account for this in the
     future. Right now, even something like nested variants in different ways
     might be indistinguishable. *)
    let deps_from_args = List.concat_map args ~f:(collect_from_ty ~env) in
    let deps_from_fields =
      match Map.find env name with
      | Some (RecordDecl (params, fields)) ->
        let sub = List.zip_exn params args in
        List.concat_map fields ~f:(fun (_, fty) ->
          collect_from_ty ~env (subst_ty sub fty))
      | _ -> []
    in
    deps_from_args @ deps_from_fields @ [ `Record (name, args) ]
  | TyVariant (name, args) ->
    let deps_from_args = List.concat_map args ~f:(collect_from_ty ~env) in
    let deps_from_ctors =
      match Map.find env name with
      | Some (VariantDecl (params, ctors)) ->
        let sub = List.zip_exn params args in
        List.concat_map ctors ~f:(fun (_, tys) ->
          List.concat_map tys ~f:(fun fty -> collect_from_ty ~env (subst_ty sub fty)))
      | _ -> []
    in
    deps_from_args @ deps_from_ctors @ [ `Variant (name, args) ]
;;

(* TODO: I'm too lazy I need to refactor this, is there a native [List.concat_map] for
   [Either] types, maybe? That seems reasonable. *)
let collect_from_term ~(env : env) (t : term)
  : [ `Record of string * ty list | `Variant of string * ty list ] list
  =
  let rec walk (t : term) =
    let from_ty = collect_from_ty ~env t.ty in
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

let collect_from_top ~(env : env) (top : top)
  : [ `Record of string * ty list | `Variant of string * ty list ] list
  =
  match top.desc with
  | TypeDef (_, RecordDecl (params, _)) when not (List.is_empty params) -> []
  | TypeDef (_, VariantDecl (params, _)) when not (List.is_empty params) -> []
  | TypeDef (_, RecordDecl (_, fields)) ->
    List.concat_map fields ~f:(fun (_, fty) -> collect_from_ty ~env fty)
  | TypeDef (_, VariantDecl (_, ctors)) ->
    List.concat_map ctors ~f:(fun (_, tys) ->
      List.concat_map tys ~f:(collect_from_ty ~env))
  | Extern _ -> []
  | Define (_, _, bind) -> collect_from_ty ~env top.ty @ collect_from_term ~env bind
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

let specialize_struct ~(env : env) ~(loc : Lexer.loc) (name : string) (args : ty list)
  : top
  =
  let params, fields =
    match Map.find_exn env name with
    | RecordDecl (params, fields) -> params, fields
    | _ -> failwith "specialze_struct on non record"
  in
  let sub = List.zip_exn params args in
  let specialized_fields = List.map fields ~f:(Tuple2.map_snd ~f:(subst_ty sub)) in
  let mangled = mangle_ty (TyRecord (name, args)) in
  { desc = TypeDef (mangled, RecordDecl ([], specialized_fields))
  ; ty = TyRecord (mangled, [])
  ; loc
  ; scheme_constrs = []
  }
;;

let specialize_variant ~(env : env) ~(loc : Lexer.loc) (name : string) (args : ty list)
  : top
  =
  let params, ctors =
    match Map.find_exn env name with
    | VariantDecl (params, ctors) -> params, ctors
    | _ -> assert false
  in
  let sub = List.zip_exn params args in
  let specialized_ctors =
    List.map ctors ~f:(Tuple2.map_snd ~f:(List.map ~f:(subst_ty sub)))
  in
  let mangled = mangle_ty (TyVariant (name, args)) in
  { desc = TypeDef (mangled, VariantDecl ([], specialized_ctors))
  ; ty = TyVariant (mangled, [])
  ; loc
  ; scheme_constrs = []
  }
;;

let rec rewrite_ty ~(env : env) (ty : ty) : ty =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVar _ -> ty
  | TyArrow (a, b) -> TyArrow (rewrite_ty ~env a, rewrite_ty ~env b)
  | TyRecord (_, []) -> ty
  | TyRecord (name, args)
    when is_concrete (TyRecord (name, args))
         &&
         match Map.find env name with
         | Some (RecordDecl _) -> true
         | _ -> false -> TyRecord (mangle_ty (TyRecord (name, args)), [])
  | TyRecord (name, args) -> TyRecord (name, List.map args ~f:(rewrite_ty ~env))
  | TyVariant (_, []) -> ty
  | TyVariant (name, args)
    when is_concrete (TyVariant (name, args))
         &&
         match Map.find env name with
         | Some (VariantDecl _) -> true
         | _ -> false -> TyVariant (mangle_ty (TyVariant (name, args)), [])
  | TyVariant (name, args) -> TyVariant (name, List.map args ~f:(rewrite_ty ~env))
;;

(* TODO: hmm... weird code duplicating from above *)
(* TODO: hmm... a lot of checking if instance of RecordDevl or TyVariant, but there's also just a lot
   of generally checking of struct vs record in this code that can probably be improved *)
let maybe_mangle_name ~(env : env) ~fallback (ty : ty) : string =
  match ty with
  | TyRecord (n, args)
    when (not (List.is_empty args))
         && is_concrete ty
         &&
         match Map.find env n with
         | Some (RecordDecl _) -> true
         | _ -> false -> mangle_ty (TyRecord (n, args))
  | TyVariant (n, args)
    when (not (List.is_empty args))
         && is_concrete ty
         &&
         match Map.find env n with
         | Some (VariantDecl _) -> true
         | _ -> false -> mangle_ty (TyVariant (n, args))
  | _ -> fallback
;;

let rec rewrite_term ~(env : env) (t : term) : term =
  let rewrite = rewrite_term ~env in
  let ty = rewrite_ty ~env t.ty in
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
      let new_name = maybe_mangle_name ~env ~fallback:name t.ty in
      Record (new_name, List.map ts ~f:rewrite)
    | Field (t, f) -> Field (rewrite t, f)
    | Variant (ty_name, ctor, args) ->
      let new_ty_name = maybe_mangle_name ~env ~fallback:ty_name t.ty in
      Variant (new_ty_name, ctor, List.map args ~f:rewrite)
    | Match (scrutinee, cases) ->
      Match (rewrite scrutinee, List.map cases ~f:(fun (pat, body) -> pat, rewrite body))
  in
  { t with ty; desc }
;;

let rewrite_top ~(env : env) (top : top) : top =
  let ty = rewrite_ty ~env top.ty in
  let desc : top_desc =
    match top.desc with
    | Define (r, v, bind) -> Define (r, v, rewrite_term ~env bind)
    | Extern _ -> top.desc
    | TypeDef (name, RecordDecl (params, fields)) ->
      let fields = List.map fields ~f:(fun (fn, fty) -> fn, rewrite_ty ~env fty) in
      TypeDef (name, RecordDecl (params, fields))
    | TypeDef (name, VariantDecl (params, ctors)) ->
      let ctors =
        List.map ctors ~f:(fun (cn, tys) -> cn, List.map tys ~f:(rewrite_ty ~env))
      in
      TypeDef (name, VariantDecl (params, ctors))
  in
  { top with ty; desc }
;;

let specialize (Program tops : t) : t =
  let poly_tops =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, (RecordDecl (params, _) as decl)) when not (List.is_empty params)
        -> Some (name, decl, top.loc)
      | TypeDef (name, (VariantDecl (params, _) as decl)) when not (List.is_empty params)
        -> Some (name, decl, top.loc)
      | _ -> None)
  in
  let env =
    poly_tops
    |> List.map ~f:(fun (name, decl, _) -> name, decl)
    |> String.Map.of_alist_exn
  in
  let poly_locs =
    poly_tops |> List.map ~f:(fun (name, _, loc) -> name, loc) |> String.Map.of_alist_exn
  in
  let new_def_tops =
    tops
    |> List.concat_map ~f:(collect_from_top ~env)
    |> dedup_tagged_instances
    |> List.filter_map ~f:(function
      | `Record (name, args) when Map.mem env name ->
        let loc = Map.find_exn poly_locs name in
        Some (specialize_struct ~env ~loc name args)
      | `Variant (name, args) when Map.mem env name ->
        let loc = Map.find_exn poly_locs name in
        Some (specialize_variant ~env ~loc name args)
      | _ -> None)
  in
  let filtered_tops =
    List.filter tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, (RecordDecl _ | VariantDecl _)) when Map.mem env name -> false
      | _ -> true)
  in
  let rewritten = List.map (new_def_tops @ filtered_tops) ~f:(rewrite_top ~env) in
  Program rewritten
;;
