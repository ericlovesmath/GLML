open Core
open Sexplib.Sexp
open Stlc
open Compiler_error.Let_syntax

module Err = Compiler_error.Pass (struct
    let name = "monomorphize"
  end)

(* ===== Helpers absorbed from specialize_params ===== *)

let rec mangle_ty (ty : Typecheck.ty) : string =
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

let rec is_concrete (ty : Typecheck.ty) : bool =
  match ty with
  | TyVar _ -> false
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ -> true
  | TyVariant (_, args) -> List.for_all args ~f:is_concrete
  | TyRecord (_, args) -> List.for_all args ~f:is_concrete
  | TyArrow (a, b) -> is_concrete a && is_concrete b
;;

let rec collect_from_ty
          ~(type_poly_env : Typecheck.type_decl String.Map.t)
          (ty : Typecheck.ty)
  : [ `Record of string * Typecheck.ty list | `Variant of string * Typecheck.ty list ]
      list
  =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVar _ -> []
  | TyArrow (a, b) -> collect_from_ty ~type_poly_env a @ collect_from_ty ~type_poly_env b
  | TyRecord (_, []) | TyVariant (_, []) -> []
  | TyRecord (name, args) when not (is_concrete (TyRecord (name, args))) -> []
  | TyVariant (name, args) when not (is_concrete (TyVariant (name, args))) -> []
  | TyRecord (name, args) ->
    let deps_from_args = List.concat_map args ~f:(collect_from_ty ~type_poly_env) in
    let deps_from_fields =
      match Map.find type_poly_env name with
      | Some (RecordDecl (params, fields)) ->
        let sub = List.zip_exn params args in
        List.concat_map fields ~f:(fun (_, fty) ->
          collect_from_ty ~type_poly_env (Typecheck.subst_ty sub fty))
      | _ -> []
    in
    deps_from_args @ deps_from_fields @ [ `Record (name, args) ]
  | TyVariant (name, args) ->
    let deps_from_args = List.concat_map args ~f:(collect_from_ty ~type_poly_env) in
    let deps_from_ctors =
      match Map.find type_poly_env name with
      | Some (VariantDecl (params, ctors)) ->
        let sub = List.zip_exn params args in
        List.concat_map ctors ~f:(fun (_, tys) ->
          List.concat_map tys ~f:(fun fty ->
            collect_from_ty ~type_poly_env (Typecheck.subst_ty sub fty)))
      | _ -> []
    in
    deps_from_args @ deps_from_ctors @ [ `Variant (name, args) ]
;;

let collect_from_term
      ~(type_poly_env : Typecheck.type_decl String.Map.t)
      (t : Typecheck.term)
  : [ `Record of string * Typecheck.ty list | `Variant of string * Typecheck.ty list ]
      list
  =
  let rec walk (t : Typecheck.term) =
    let from_ty = collect_from_ty ~type_poly_env t.ty in
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

let collect_from_top
      ~(type_poly_env : Typecheck.type_decl String.Map.t)
      (top : Typecheck.top)
  : [ `Record of string * Typecheck.ty list | `Variant of string * Typecheck.ty list ]
      list
  =
  match top.desc with
  | TypeDef (_, RecordDecl (params, _)) when not (List.is_empty params) -> []
  | TypeDef (_, VariantDecl (params, _)) when not (List.is_empty params) -> []
  | TypeDef (_, RecordDecl (_, fields)) ->
    List.concat_map fields ~f:(fun (_, fty) -> collect_from_ty ~type_poly_env fty)
  | TypeDef (_, VariantDecl (_, ctors)) ->
    List.concat_map ctors ~f:(fun (_, tys) ->
      List.concat_map tys ~f:(collect_from_ty ~type_poly_env))
  | Extern _ -> []
  | Define (_, _, bind) ->
    collect_from_ty ~type_poly_env top.ty @ collect_from_term ~type_poly_env bind
;;

let dedup_tagged_instances
      (instances :
        [ `Record of string * Typecheck.ty list | `Variant of string * Typecheck.ty list ]
          list)
  : [ `Record of string * Typecheck.ty list | `Variant of string * Typecheck.ty list ]
      list
  =
  let eq a b =
    match a, b with
    | `Record (n, args), `Record (n', args') ->
      String.equal n n' && List.equal Typecheck.equal_ty args args'
    | `Variant (n, args), `Variant (n', args') ->
      String.equal n n' && List.equal Typecheck.equal_ty args args'
    | _, _ -> false
  in
  List.fold instances ~init:[] ~f:(fun acc inst ->
    if List.exists acc ~f:(eq inst) then acc else inst :: acc)
  |> List.rev
;;

let specialize_struct
      ~(type_poly_env : Typecheck.type_decl String.Map.t)
      ~(loc : Lexer.loc)
      (name : string)
      (args : Typecheck.ty list)
  : Typecheck.top
  =
  let params, fields =
    match Map.find_exn type_poly_env name with
    | RecordDecl (params, fields) -> params, fields
    | _ -> failwith "specialize_struct on non record"
  in
  let sub = List.zip_exn params args in
  let specialized_fields =
    List.map fields ~f:(Tuple2.map_snd ~f:(Typecheck.subst_ty sub))
  in
  let mangled = mangle_ty (TyRecord (name, args)) in
  { desc = TypeDef (mangled, RecordDecl ([], specialized_fields))
  ; ty = TyRecord (mangled, [])
  ; loc
  ; scheme_constrs = []
  }
;;

let specialize_variant
      ~(type_poly_env : Typecheck.type_decl String.Map.t)
      ~(loc : Lexer.loc)
      (name : string)
      (args : Typecheck.ty list)
  : Typecheck.top
  =
  let params, ctors =
    match Map.find_exn type_poly_env name with
    | VariantDecl (params, ctors) -> params, ctors
    | _ -> assert false
  in
  let sub = List.zip_exn params args in
  let specialized_ctors =
    List.map ctors ~f:(Tuple2.map_snd ~f:(List.map ~f:(Typecheck.subst_ty sub)))
  in
  let mangled = mangle_ty (TyVariant (name, args)) in
  { desc = TypeDef (mangled, VariantDecl ([], specialized_ctors))
  ; ty = TyVariant (mangled, [])
  ; loc
  ; scheme_constrs = []
  }
;;

let rec rewrite_ty ~(type_poly_env : Typecheck.type_decl String.Map.t) (ty : Typecheck.ty)
  : Typecheck.ty
  =
  match ty with
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyVar _ -> ty
  | TyArrow (a, b) -> TyArrow (rewrite_ty ~type_poly_env a, rewrite_ty ~type_poly_env b)
  | TyRecord (_, []) -> ty
  | TyRecord (name, args)
    when is_concrete (TyRecord (name, args))
         &&
         match Map.find type_poly_env name with
         | Some (RecordDecl _) -> true
         | _ -> false -> TyRecord (mangle_ty (TyRecord (name, args)), [])
  | TyRecord (name, args) -> TyRecord (name, List.map args ~f:(rewrite_ty ~type_poly_env))
  | TyVariant (_, []) -> ty
  | TyVariant (name, args)
    when is_concrete (TyVariant (name, args))
         &&
         match Map.find type_poly_env name with
         | Some (VariantDecl _) -> true
         | _ -> false -> TyVariant (mangle_ty (TyVariant (name, args)), [])
  | TyVariant (name, args) ->
    TyVariant (name, List.map args ~f:(rewrite_ty ~type_poly_env))
;;

let maybe_mangle_name
      ~(type_poly_env : Typecheck.type_decl String.Map.t)
      ~fallback
      (ty : Typecheck.ty)
  : string
  =
  match ty with
  | TyRecord (n, args)
    when (not (List.is_empty args))
         && is_concrete ty
         &&
         match Map.find type_poly_env n with
         | Some (RecordDecl _) -> true
         | _ -> false -> mangle_ty (TyRecord (n, args))
  | TyVariant (n, args)
    when (not (List.is_empty args))
         && is_concrete ty
         &&
         match Map.find type_poly_env n with
         | Some (VariantDecl _) -> true
         | _ -> false -> mangle_ty (TyVariant (n, args))
  | _ -> fallback
;;

let rec rewrite_term
          ~(type_poly_env : Typecheck.type_decl String.Map.t)
          ?(poly_names = String.Set.empty)
          (t : Typecheck.term)
  : Typecheck.term
  =
  let rewrite = rewrite_term ~type_poly_env ~poly_names in
  let ty =
    match t.desc with
    | Var v when Set.mem poly_names v -> t.ty
    | _ -> rewrite_ty ~type_poly_env t.ty
  in
  let desc : Typecheck.term_desc =
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
      let new_name = maybe_mangle_name ~type_poly_env ~fallback:name t.ty in
      Record (new_name, List.map ts ~f:rewrite)
    | Field (t, f) -> Field (rewrite t, f)
    | Variant (ty_name, ctor, args) ->
      let new_ty_name = maybe_mangle_name ~type_poly_env ~fallback:ty_name t.ty in
      Variant (new_ty_name, ctor, List.map args ~f:rewrite)
    | Match (scrutinee, cases) ->
      Match (rewrite scrutinee, List.map cases ~f:(fun (pat, body) -> pat, rewrite body))
  in
  { t with ty; desc }
;;

(* ===== Monomorphize-specific helpers ===== *)

let rec subst ~(poly : Typecheck.ty) ~(concrete : Typecheck.ty)
  : (string * Typecheck.ty) list
  =
  match poly, concrete with
  | TyVar v, _ -> [ v, concrete ]
  | TyArrow (l, r), TyArrow (l', r') ->
    subst ~poly:l ~concrete:l' @ subst ~poly:r ~concrete:r'
  | TyRecord (n, args), TyRecord (n', args')
    when String.equal n n' && List.length args = List.length args' ->
    List.concat_map (List.zip_exn args args') ~f:(fun (a, a') ->
      subst ~poly:a ~concrete:a')
  | TyVariant (n, args), TyVariant (n', args')
    when String.equal n n' && List.length args = List.length args' ->
    List.concat_map (List.zip_exn args args') ~f:(fun (a, a') ->
      subst ~poly:a ~concrete:a')
  | _, _ -> []
;;

let rec fold_term ~(f : 'a -> Typecheck.term -> 'a) (acc : 'a) (t : Typecheck.term) : 'a =
  let acc = f acc t in
  match t.desc with
  | Var _ | Float _ | Int _ | Bool _ -> acc
  | Vec (_, ts) | Mat (_, _, ts) | Builtin (_, ts) | Record (_, ts) ->
    List.fold ts ~init:acc ~f:(fold_term ~f)
  | Lam (_, body) -> fold_term ~f acc body
  | App (fn, x) -> fold_term ~f (fold_term ~f acc fn) x
  | Let (_, _, _, bind, body) -> fold_term ~f (fold_term ~f acc bind) body
  | If (c, t, e) -> fold_term ~f (fold_term ~f (fold_term ~f acc c) t) e
  | Bop (_, l, r) -> fold_term ~f (fold_term ~f acc l) r
  | Index (t, _) | Field (t, _) -> fold_term ~f acc t
  | Variant (_, _, args) -> List.fold args ~init:acc ~f:(fold_term ~f)
  | Match (scrutinee, cases) ->
    let acc = fold_term ~f acc scrutinee in
    List.fold cases ~init:acc ~f:(fun acc (_, body) -> fold_term ~f acc body)
;;

let collect_var_usages (name : string) (t : Typecheck.term) : Typecheck.ty list =
  fold_term
    ~f:(fun acc t ->
      match t.desc with
      | Var v when String.equal v name -> t.ty :: acc
      | _ -> acc)
    []
    t
  |> List.stable_dedup ~compare:(fun a b -> if Typecheck.equal_ty a b then 0 else 1)
;;

let map_cases_capture_avoiding
      ~(var : string)
      ~(f : Typecheck.term -> Typecheck.term)
      (cases : (Stlc.pat * Typecheck.term) list)
  : (Stlc.pat * Typecheck.term) list
  =
  List.map cases ~f:(fun (pat, body) ->
    if List.mem (Stlc.pat_bound_vars pat) var ~equal:String.equal
    then pat, body
    else pat, f body)
;;

let rec subst_var
          ~(name : string)
          ~(new_name : string)
          ~(pred : Typecheck.term -> bool)
          (t : Typecheck.term)
  : Typecheck.term
  =
  let subst = subst_var ~name ~new_name ~pred in
  let desc : Typecheck.term_desc =
    match t.desc with
    | Var v when String.equal v name && pred t -> Var new_name
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:subst)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:subst)
    | Lam (v, body) -> if String.equal v name then t.desc else Lam (v, subst body)
    | App (f, x) -> App (subst f, subst x)
    | Let (recur, v, constrs, bind, body) ->
      let bind = subst bind in
      let body = if String.equal v name then body else subst body in
      Let (recur, v, constrs, bind, body)
    | If (c, t, e) -> If (subst c, subst t, subst e)
    | Bop (op, l, r) -> Bop (op, subst l, subst r)
    | Index (t, i) -> Index (subst t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:subst)
    | Record (s, ts) -> Record (s, List.map ts ~f:subst)
    | Field (t, f) -> Field (subst t, f)
    | Variant (ty_name, ctor, args) -> Variant (ty_name, ctor, List.map args ~f:subst)
    | Match (scrutinee, cases) ->
      Match (subst scrutinee, map_cases_capture_avoiding ~var:name ~f:subst cases)
  in
  { t with desc }
;;

let rename_var (src : string) (dst : string) =
  subst_var ~name:src ~new_name:dst ~pred:(Fun.const true)
;;

type poly_def =
  { poly_type : Typecheck.ty
  ; poly_bind : Typecheck.term
  ; poly_recur : recur
  ; poly_loc : Lexer.loc
  ; poly_constrs : Typecheck.constr list
  }

type spec_map = (Typecheck.ty * string) list String.Map.t

type state =
  { type_poly_env : Typecheck.type_decl String.Map.t
  ; structs_for_constrs : (string list * (string * Typecheck.ty) list) String.Map.t
  ; poly_names : String.Set.t
  ; poly_fn_env : poly_def String.Map.t ref
  ; spec_map : spec_map ref
  ; output_tops : Typecheck.top list ref
  }

let collect_poly_refs (poly_fn_env : poly_def String.Map.t) (t : Typecheck.term)
  : (string * Typecheck.ty) list
  =
  let refs =
    fold_term
      ~f:(fun acc t ->
        match t.desc with
        | Var v when Map.mem poly_fn_env v && is_concrete t.ty -> (v, t.ty) :: acc
        | _ -> acc)
      []
      t
  in
  List.stable_dedup refs ~compare:(fun (n1, t1) (n2, t2) ->
    let c = String.compare n1 n2 in
    if c <> 0 then c else if Typecheck.equal_ty t1 t2 then 0 else 1)
;;

let find_spec (env : spec_map) (name : string) (ty : Typecheck.ty) : string option =
  Map.find env name
  |> Option.bind
       ~f:
         (List.find_map ~f:(fun (t, n) ->
            if Typecheck.equal_ty t ty then Some n else None))
;;

let add_spec (env : spec_map) (name : string) (ty : Typecheck.ty) (spec_name : string)
  : spec_map
  =
  let specs = Option.value (Map.find env name) ~default:[] in
  Map.set env ~key:name ~data:((ty, spec_name) :: specs)
;;

let rec resolve_spec (st : state) (name : string) (concrete_ty : Typecheck.ty) : string =
  match find_spec !(st.spec_map) name concrete_ty with
  | Some spec_name -> spec_name
  | None ->
    let entry = Map.find_exn !(st.poly_fn_env) name in
    let spec_name = Utils.fresh (name ^ "_" ^ mangle_ty concrete_ty) in
    st.spec_map := add_spec !(st.spec_map) name concrete_ty spec_name;
    let sub = subst ~poly:entry.poly_type ~concrete:concrete_ty in
    let sub =
      Typecheck.solve_scheme_constrs
        ~structs:st.structs_for_constrs
        entry.poly_constrs
        sub
      |> Compiler_error.ok_exn
    in
    let body = Typecheck.subst_term sub entry.poly_bind in
    let body =
      rewrite_term ~type_poly_env:st.type_poly_env ~poly_names:st.poly_names body
    in
    let body =
      match entry.poly_recur with
      | Rec _ -> rename_var name spec_name body
      | Nonrec -> body
    in
    let refs = collect_poly_refs !(st.poly_fn_env) body in
    List.iter refs ~f:(fun (dep_name, dep_ty) -> ignore (resolve_spec st dep_name dep_ty));
    let body = rewrite_refs st body in
    let concrete_ty = rewrite_ty ~type_poly_env:st.type_poly_env concrete_ty in
    let top : Typecheck.top =
      { desc = Define (entry.poly_recur, spec_name, body)
      ; ty = concrete_ty
      ; loc = entry.poly_loc
      ; scheme_constrs = []
      }
    in
    st.output_tops := !(st.output_tops) @ [ top ];
    spec_name

and rewrite_refs (st : state) (t : Typecheck.term) : Typecheck.term =
  let desc : Typecheck.term_desc =
    match t.desc with
    | Var v ->
      (match Map.find !(st.poly_fn_env) v with
       | Some _ when is_concrete t.ty ->
         let spec_name = resolve_spec st v t.ty in
         Var spec_name
       | _ -> t.desc)
    | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:(rewrite_refs st))
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:(rewrite_refs st))
    | Lam (v, body) -> Lam (v, rewrite_refs st body)
    | App (f, x) -> App (rewrite_refs st f, rewrite_refs st x)
    | Let (recur, v, constrs, bind, body) when not (is_concrete bind.ty) ->
      (* Specialization for inner polymorphic lets *)
      let usages = collect_var_usages v body in
      if List.is_empty usages
      then (rewrite_refs st body).desc
      else (
        let specs =
          List.map usages ~f:(fun concrete_ty ->
            let sub = subst ~poly:bind.ty ~concrete:concrete_ty in
            let sub =
              Typecheck.solve_scheme_constrs ~structs:st.structs_for_constrs constrs sub
              |> Compiler_error.ok_exn
            in
            let spec_bind = Typecheck.subst_term sub bind in
            let spec_bind =
              rewrite_term
                ~type_poly_env:st.type_poly_env
                ~poly_names:st.poly_names
                spec_bind
            in
            let spec_name = Utils.fresh (v ^ "_" ^ mangle_ty concrete_ty) in
            let spec_bind = rewrite_refs st spec_bind in
            let spec_bind =
              match recur with
              | Rec _ -> rename_var v spec_name spec_bind
              | Nonrec -> spec_bind
            in
            spec_name, spec_bind, concrete_ty)
        in
        let body' =
          List.fold specs ~init:body ~f:(fun b (spec_name, _, concrete_ty) ->
            subst_var
              ~name:v
              ~new_name:spec_name
              ~pred:(fun t -> Typecheck.equal_ty t.ty concrete_ty)
              b)
        in
        let body' = rewrite_refs st body' in
        (List.fold_right specs ~init:body' ~f:(fun (spec_name, spec_bind, _) acc ->
           ({ desc = Let (recur, spec_name, [], spec_bind, acc)
            ; ty = acc.ty
            ; loc = t.loc
            }
            : Typecheck.term)))
          .desc)
    | Let (recur, v, constrs, bind, body) ->
      Let (recur, v, constrs, rewrite_refs st bind, rewrite_refs st body)
    | If (c, t, e) -> If (rewrite_refs st c, rewrite_refs st t, rewrite_refs st e)
    | Bop (op, l, r) -> Bop (op, rewrite_refs st l, rewrite_refs st r)
    | Index (t, i) -> Index (rewrite_refs st t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:(rewrite_refs st))
    | Record (s, ts) ->
      let new_name = maybe_mangle_name ~type_poly_env:st.type_poly_env ~fallback:s t.ty in
      Record (new_name, List.map ts ~f:(rewrite_refs st))
    | Field (t, f) -> Field (rewrite_refs st t, f)
    | Variant (ty_name, ctor, args) ->
      let new_ty_name =
        maybe_mangle_name ~type_poly_env:st.type_poly_env ~fallback:ty_name t.ty
      in
      Variant (new_ty_name, ctor, List.map args ~f:(rewrite_refs st))
    | Match (scrutinee, cases) ->
      Match
        ( rewrite_refs st scrutinee
        , List.map cases ~f:(fun (pat, body) -> pat, rewrite_refs st body) )
  in
  let ty = rewrite_ty ~type_poly_env:st.type_poly_env t.ty in
  { t with desc; ty }
;;

(* ===== Output types ===== *)

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int
  | TyMat of int * int
  | TyArrow of ty * ty
  | TyRecord of string
  | TyVariant of string
[@@deriving equal]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec i -> List [ Atom "vec"; Atom (Int.to_string i) ]
  | TyMat (x, y) -> List [ Atom "mat"; Atom (Int.to_string x); Atom (Int.to_string y) ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
  | TyRecord s -> Atom s
  | TyVariant s -> Atom s
;;

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
[@@deriving sexp_of]

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * term
  | App of term * term
  | Let of recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of string * term list
  | Field of term * string
  | Variant of string * string * term list
  | Match of term * (Stlc.pat * term) list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc : term_desc -> Sexp.t = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_term)
  | Mat (x, y, ts) ->
    List
      (Atom ("mat" ^ Int.to_string x ^ "x" ^ Int.to_string y)
       :: List.map ts ~f:sexp_of_term)
  | Lam (v, body) -> List [ Atom "lambda"; Atom v; sexp_of_term body ]
  | App (f, x) -> List [ Atom "app"; sexp_of_term f; sexp_of_term x ]
  | Let (Rec n, v, bind, body) ->
    let rec_tag = List [ Atom "rec"; Atom (Int.to_string n) ] in
    List [ Atom "let"; rec_tag; Atom v; sexp_of_term bind; sexp_of_term body ]
  | Let (Nonrec, v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_term body ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_term)
  | Record (s, ts) -> List (Atom s :: List.map ts ~f:sexp_of_term)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ty_name, ctor, args) ->
    List (Atom "Variant" :: Atom ty_name :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (pat, body) = List [ Stlc.sexp_of_pat pat; sexp_of_term body ] in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = List [ sexp_of_term_desc t.desc; Atom ":"; sexp_of_ty t.ty ]

type top_desc =
  | Define of recur * string * term
  | Extern of string
  | TypeDef of string * type_decl
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let sexp_of_top t = List [ sexp_of_top_desc t.desc; Atom ":"; sexp_of_ty t.ty ]

type t = Program of top list [@@deriving sexp_of]

(* ===== Conversion from Typecheck types ===== *)

let rec ty_of (t : Typecheck.ty) : ty Compiler_error.t =
  match t with
  | TyVar _ -> Err.fail "unexpected TyVar after monomorphization"
  | TyFloat -> Ok TyFloat
  | TyInt -> Ok TyInt
  | TyBool -> Ok TyBool
  | TyVec n -> Ok (TyVec n)
  | TyMat (x, y) -> Ok (TyMat (x, y))
  | TyRecord (s, []) -> Ok (TyRecord s)
  | TyRecord (_, _ :: _) ->
    Err.fail "unexpected parametrized TyRecord after [specialize_structs]"
  | TyVariant (s, []) -> Ok (TyVariant s)
  | TyVariant (_, _ :: _) ->
    Err.fail "unexpected parametrized TyVariant after [specialize_structs]"
  | TyArrow (a, b) ->
    let%bind a = ty_of a in
    let%bind b = ty_of b in
    Ok (TyArrow (a, b))
;;

let rec term_of_tc (t : Typecheck.term) : term Compiler_error.t =
  let%bind ty = ty_of t.ty in
  let%bind desc = term_desc_of_tc t.desc in
  Ok ({ desc; ty; loc = t.loc } : term)

and term_desc_of_tc (d : Typecheck.term_desc) : term_desc Compiler_error.t =
  match d with
  | Var v -> Ok (Var v)
  | Float f -> Ok (Float f)
  | Int i -> Ok (Int i)
  | Bool b -> Ok (Bool b)
  | Vec (n, ts) ->
    let%map ts = Compiler_error.all (List.map ts ~f:term_of_tc) in
    Vec (n, ts)
  | Mat (n, m, ts) ->
    let%map ts = Compiler_error.all (List.map ts ~f:term_of_tc) in
    Mat (n, m, ts)
  | Lam (v, body) ->
    let%map body = term_of_tc body in
    Lam (v, body)
  | App (f, x) ->
    let%bind f = term_of_tc f in
    let%bind x = term_of_tc x in
    Ok (App (f, x))
  | Let (r, v, constrs, bind, body) ->
    let%bind bind = term_of_tc bind in
    let%bind body = term_of_tc body in
    if List.is_empty constrs
    then Ok (Let (r, v, bind, body))
    else Err.fail "Let has constraints" ~d:[%message (d : Typecheck.term_desc)]
  | If (c, t, e) ->
    let%bind c = term_of_tc c in
    let%bind t = term_of_tc t in
    let%bind e = term_of_tc e in
    Ok (If (c, t, e))
  | Bop (op, l, r) ->
    let%bind l = term_of_tc l in
    let%bind r = term_of_tc r in
    Ok (Bop (op, l, r))
  | Index (t, i) ->
    let%map t = term_of_tc t in
    Index (t, i)
  | Builtin (b, ts) ->
    let%map ts = Compiler_error.all (List.map ts ~f:term_of_tc) in
    Builtin (b, ts)
  | Record (s, ts) ->
    let%map ts = Compiler_error.all (List.map ts ~f:term_of_tc) in
    Record (s, ts)
  | Field (t, f) ->
    let%map t = term_of_tc t in
    Field (t, f)
  | Variant (ty_name, ctor, args) ->
    let%map args = Compiler_error.all (List.map args ~f:term_of_tc) in
    Variant (ty_name, ctor, args)
  | Match (scrutinee, cases) ->
    let%bind scrutinee = term_of_tc scrutinee in
    let%bind cases =
      cases
      |> List.map ~f:(fun (pat, body) ->
        let%map body = term_of_tc body in
        pat, body)
      |> Compiler_error.all
    in
    Ok (Match (scrutinee, cases))
;;

let top_of_tc (t : Typecheck.top) : top Compiler_error.t =
  let%bind ty = ty_of t.ty in
  let%bind desc =
    match t.desc with
    | Define (r, v, bind) ->
      let%map bind = term_of_tc bind in
      Define (r, v, bind)
    | Extern v -> Ok (Extern v)
    | TypeDef (name, RecordDecl ([], fields)) ->
      let%map fields =
        List.map fields ~f:(fun (field_name, field_ty) ->
          let%map field_ty = ty_of field_ty in
          field_name, field_ty)
        |> Compiler_error.all
      in
      TypeDef (name, RecordDecl fields)
    | TypeDef (_, RecordDecl (_ :: _, _)) ->
      Err.fail "unexpected parametrized TypeDef after SpecializeStructs"
    | TypeDef (_, VariantDecl (_ :: _, _)) ->
      Err.fail "unexpected parametrized VariantDecl after SpecializeStructs"
    | TypeDef (name, VariantDecl ([], ctors)) ->
      let%map ctors =
        ctors
        |> List.map ~f:(fun (ctor_name, tys) ->
          let%map tys = Compiler_error.all (List.map tys ~f:ty_of) in
          ctor_name, tys)
        |> Compiler_error.all
      in
      TypeDef (name, VariantDecl ctors)
  in
  Ok { desc; ty; loc = t.loc }
;;

let monomorphize (program : Typecheck.t) : t Compiler_error.t =
  let (Program tops) = program in
  (* Build state *)
  let poly_type_tops =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, (RecordDecl (params, _) as decl)) when not (List.is_empty params)
        -> Some (name, decl, top.loc)
      | TypeDef (name, (VariantDecl (params, _) as decl)) when not (List.is_empty params)
        -> Some (name, decl, top.loc)
      | _ -> None)
  in
  let type_poly_env =
    poly_type_tops
    |> List.map ~f:(fun (name, decl, _) -> name, decl)
    |> String.Map.of_alist_exn
  in
  let type_poly_locs =
    poly_type_tops
    |> List.map ~f:(fun (name, _, loc) -> name, loc)
    |> String.Map.of_alist_exn
  in
  let structs_for_constrs =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | TypeDef (name, RecordDecl (params, fields)) -> Some (name, (params, fields))
      | _ -> None)
    |> String.Map.of_alist_exn
  in
  let poly_names =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | Define (_, v, _) when not (is_concrete top.ty) -> Some v
      | _ -> None)
    |> String.Set.of_list
  in
  let st =
    { type_poly_env
    ; structs_for_constrs
    ; poly_names
    ; poly_fn_env = ref String.Map.empty
    ; spec_map = ref String.Map.empty
    ; output_tops = ref []
    }
  in
  (* Emit concrete TypeDef tops for all parametrized type instantiations *)
  let instances =
    List.concat_map tops ~f:(collect_from_top ~type_poly_env) |> dedup_tagged_instances
  in
  (* Rewrite field/ctor types in a specialized TypeDef top *)
  let rewrite_spec_typedef (top : Typecheck.top) : Typecheck.top =
    match top.desc with
    | TypeDef (name, RecordDecl (params, fields)) ->
      let fields =
        List.map fields ~f:(fun (fn, fty) -> fn, rewrite_ty ~type_poly_env fty)
      in
      { top with desc = TypeDef (name, RecordDecl (params, fields)) }
    | TypeDef (name, VariantDecl (params, ctors)) ->
      let ctors =
        List.map ctors ~f:(fun (cn, tys) ->
          cn, List.map tys ~f:(rewrite_ty ~type_poly_env))
      in
      { top with desc = TypeDef (name, VariantDecl (params, ctors)) }
    | _ -> top
  in
  let spec_type_tops =
    List.filter_map instances ~f:(function
      | `Record (name, args) when Map.mem type_poly_env name ->
        let loc = Map.find_exn type_poly_locs name in
        Some (specialize_struct ~type_poly_env ~loc name args |> rewrite_spec_typedef)
      | `Variant (name, args) when Map.mem type_poly_env name ->
        let loc = Map.find_exn type_poly_locs name in
        Some (specialize_variant ~type_poly_env ~loc name args |> rewrite_spec_typedef)
      | _ -> None)
  in
  st.output_tops := spec_type_tops;
  (* Process tops *)
  List.iter tops ~f:(fun top ->
    match top.desc with
    (* Skip poly TypeDef templates — already handled previous *)
    | TypeDef (name, _) when Map.mem type_poly_env name -> ()
    | Extern _ ->
      let top = { top with ty = rewrite_ty ~type_poly_env top.ty } in
      st.output_tops := !(st.output_tops) @ [ top ]
    (* Concrete TypeDef: rewrite field/ctor types *)
    | TypeDef (name, RecordDecl (params, fields)) ->
      let fields =
        List.map fields ~f:(fun (fn, fty) -> fn, rewrite_ty ~type_poly_env fty)
      in
      let ty = rewrite_ty ~type_poly_env top.ty in
      st.output_tops
      := !(st.output_tops)
         @ [ { top with ty; desc = TypeDef (name, RecordDecl (params, fields)) } ]
    | TypeDef (name, VariantDecl (params, ctors)) ->
      let ctors =
        List.map ctors ~f:(fun (cn, tys) ->
          cn, List.map tys ~f:(rewrite_ty ~type_poly_env))
      in
      let ty = rewrite_ty ~type_poly_env top.ty in
      st.output_tops
      := !(st.output_tops)
         @ [ { top with ty; desc = TypeDef (name, VariantDecl (params, ctors)) } ]
    (* Poly Define: register in poly_fn_env, don't emit *)
    | Define (recur, v, bind) when not (is_concrete top.ty) ->
      let entry =
        { poly_type = top.ty
        ; poly_bind = bind
        ; poly_recur = recur
        ; poly_loc = top.loc
        ; poly_constrs = top.scheme_constrs
        }
      in
      st.poly_fn_env := Map.set !(st.poly_fn_env) ~key:v ~data:entry
    (* Concrete Define: resolve poly refs, rewrite, emit *)
    | Define (recur, v, bind) ->
      let refs = collect_poly_refs !(st.poly_fn_env) bind in
      List.iter refs ~f:(fun (name, ty) -> ignore (resolve_spec st name ty));
      let bind = rewrite_refs st bind in
      let ty = rewrite_ty ~type_poly_env top.ty in
      st.output_tops
      := !(st.output_tops) @ [ { top with ty; desc = Define (recur, v, bind) } ]);
  (* Lower Typecheck types to Monomorphize types *)
  let%map tops = Compiler_error.all (List.map !(st.output_tops) ~f:top_of_tc) in
  Program tops
;;
