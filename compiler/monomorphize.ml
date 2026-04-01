open Core
open Sexplib.Sexp
open Stlc
open Compiler_error.Let_syntax

module Err = Compiler_error.Pass (struct
    let name = "monomorphize"
  end)

let rec subst ~(poly : Typecheck.ty) ~(concrete : Typecheck.ty)
  : (string * Typecheck.ty) list
  =
  match poly, concrete with
  | TyVar v, _ -> [ v, concrete ]
  | TyArrow (l, r), TyArrow (l', r') ->
    subst ~poly:l ~concrete:l' @ subst ~poly:r ~concrete:r'
  | _, _ -> []
;;

(** Collect all concrete types a variable is used at in a term *)
let collect_var_usages (name : string) (t : Typecheck.term) : Typecheck.ty list =
  let rec walk (acc : Typecheck.ty list) (t : Typecheck.term) : Typecheck.ty list =
    let acc =
      match t.desc with
      | Var v when String.equal v name -> t.ty :: acc
      | _ -> acc
    in
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> acc
    | Vec (_, ts) | Mat (_, _, ts) | Builtin (_, ts) | Record (_, ts) ->
      List.fold ts ~init:acc ~f:walk
    | Lam (_, body) -> walk acc body
    | App (f, x) -> walk (walk acc f) x
    | Let (_, _, _, bind, body) -> walk (walk acc bind) body
    | If (c, t, e) -> walk (walk (walk acc c) t) e
    | Bop (_, l, r) -> walk (walk acc l) r
    | Index (t, _) | Field (t, _) -> walk acc t
    | Variant (_, _, args) -> List.fold args ~init:acc ~f:walk
    | Match (scrutinee, cases) ->
      let acc = walk acc scrutinee in
      List.fold cases ~init:acc ~f:(fun acc (_, body) -> walk acc body)
  in
  let usages = walk [] t in
  List.stable_dedup usages ~compare:(fun a b -> if Typecheck.equal_ty a b then 0 else 1)
;;

(** Rename variable references from old_name to new_name *)
let rec rename_var (src : string) (dst : string) (t : Typecheck.term) : Typecheck.term =
  let rename = rename_var src dst in
  let desc : Typecheck.term_desc =
    match t.desc with
    | Var v when String.equal v src -> Var dst
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rename)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:rename)
    | Lam (v, body) -> if String.equal v src then t.desc else Lam (v, rename body)
    | App (f, x) -> App (rename f, rename x)
    | Let (recur, v, constrs, bind, body) ->
      let bind = rename bind in
      let body = if String.equal v src then body else rename body in
      Let (recur, v, constrs, bind, body)
    | If (c, t, e) -> If (rename c, rename t, rename e)
    | Bop (op, l, r) -> Bop (op, rename l, rename r)
    | Index (t, i) -> Index (rename t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rename)
    | Record (s, ts) -> Record (s, List.map ts ~f:rename)
    | Field (t, f) -> Field (rename t, f)
    | Variant (ty_name, ctor, args) -> Variant (ty_name, ctor, List.map args ~f:rename)
    | Match (scrutinee, cases) ->
      Match
        ( rename scrutinee
        , List.map cases ~f:(fun (pat, body) ->
            let vars = Stlc.pat_bound_vars pat in
            if List.mem vars src ~equal:String.equal then pat, body else pat, rename body)
        )
  in
  { t with desc }
;;

(** Replace references to [name] that have a specific type with [new_name] *)
let rec rewrite_var_at_type
          (name : string)
          (new_name : string)
          (target_ty : Typecheck.ty)
          (t : Typecheck.term)
  : Typecheck.term
  =
  let rewrite = rewrite_var_at_type name new_name target_ty in
  let desc : Typecheck.term_desc =
    match t.desc with
    | Var v when String.equal v name && Typecheck.equal_ty t.ty target_ty -> Var new_name
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rewrite)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:rewrite)
    | Lam (v, body) -> if String.equal v name then t.desc else Lam (v, rewrite body)
    | App (f, x) -> App (rewrite f, rewrite x)
    | Let (recur, v, constrs, bind, body) ->
      let bind = rewrite bind in
      let body = if String.equal v name then body else rewrite body in
      Let (recur, v, constrs, bind, body)
    | If (c, t, e) -> If (rewrite c, rewrite t, rewrite e)
    | Bop (op, l, r) -> Bop (op, rewrite l, rewrite r)
    | Index (t, i) -> Index (rewrite t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rewrite)
    | Record (s, ts) -> Record (s, List.map ts ~f:rewrite)
    | Field (t, f) -> Field (rewrite t, f)
    | Variant (ty_name, ctor, args) -> Variant (ty_name, ctor, List.map args ~f:rewrite)
    | Match (scrutinee, cases) ->
      (* TODO: This pattern on matches seems pretty common, maybe factor out *)
      Match
        ( rewrite scrutinee
        , List.map cases ~f:(fun (pat, body) ->
            let vars = Stlc.pat_bound_vars pat in
            if List.mem vars name ~equal:String.equal
            then pat, body
            else pat, rewrite body) )
  in
  { t with desc }
;;

(** Read-only info about a polymorphic definition *)
type poly_def =
  { poly_type : Typecheck.ty
  ; poly_bind : Typecheck.term
  ; poly_recur : recur
  ; poly_loc : Lexer.loc
  ; poly_constrs : Typecheck.constr list
  }

type poly_env = poly_def String.Map.t

(** Tracks which specializations have been generated: poly_name -> [(concrete_ty, spec_name)] *)
type spec_map = (Typecheck.ty * string) list String.Map.t

(** Collect all (poly_name, concrete_ty) pairs where name is in poly_env and ty has no tyvars *)
let collect_poly_refs (poly_env : poly_env) (t : Typecheck.term)
  : (string * Typecheck.ty) list
  =
  let rec walk acc (t : Typecheck.term) =
    (* TODO: Refactor this is kind of sad and foldy *)
    let acc =
      match t.desc with
      | Var v when Map.mem poly_env v && Specialize_structs.is_concrete t.ty ->
        (v, t.ty) :: acc
      | _ -> acc
    in
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> acc
    | Vec (_, ts) | Mat (_, _, ts) | Builtin (_, ts) | Record (_, ts) ->
      List.fold ts ~init:acc ~f:walk
    | Lam (_, body) -> walk acc body
    | App (f, x) -> walk (walk acc f) x
    | Let (_, _, _, bind, body) -> walk (walk acc bind) body
    | If (c, t, e) -> walk (walk (walk acc c) t) e
    | Bop (_, l, r) -> walk (walk acc l) r
    | Index (t, _) | Field (t, _) -> walk acc t
    | Variant (_, _, args) -> List.fold args ~init:acc ~f:walk
    | Match (scrutinee, cases) ->
      let acc = walk acc scrutinee in
      List.fold cases ~init:acc ~f:(fun acc (_, body) -> walk acc body)
  in
  let refs = walk [] t in
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

(** Resolve (or look up) a specialization for [name] at [concrete_ty].
    Returns updated spec_map, the specialized name, and any new top-level defs
    in dependency order. *)
let rec resolve_spec
          (poly_env : poly_env)
          (env : spec_map)
          (name : string)
          (concrete_ty : Typecheck.ty)
  : spec_map * string * Typecheck.top list
  =
  match find_spec env name concrete_ty with
  | Some spec_name -> env, spec_name, []
  | None ->
    let entry = Map.find_exn poly_env name in
    let spec_name = Utils.fresh (name ^ "_" ^ Specialize_structs.mangle_ty concrete_ty) in
    let spec_map = add_spec env name concrete_ty spec_name in
    let sub = subst ~poly:entry.poly_type ~concrete:concrete_ty in
    let sub =
      Typecheck.solve_scheme_constrs entry.poly_constrs sub |> Compiler_error.ok_exn
    in
    let body = Typecheck.subst_term sub entry.poly_bind in
    (* For recursive functions, rename self-references *)
    let body =
      match entry.poly_recur with
      | Rec _ -> rename_var name spec_name body
      | Nonrec -> body
    in
    (* Resolve transitive poly refs in the specialized body *)
    let refs = collect_poly_refs poly_env body in
    let env, dep_defs =
      List.fold refs ~init:(spec_map, []) ~f:(fun (env, defs) (dep_name, dep_ty) ->
        let spec_map, _, new_defs = resolve_spec poly_env env dep_name dep_ty in
        spec_map, defs @ new_defs)
    in
    (* Rewrite references in the body *)
    let env, body, inner_defs = rewrite_refs poly_env env body in
    let top : Typecheck.top =
      { desc = Define (entry.poly_recur, spec_name, body)
      ; ty = concrete_ty
      ; loc = entry.poly_loc
      ; scheme_constrs = []
      }
    in
    env, spec_name, dep_defs @ inner_defs @ [ top ]

(** Rewrite references to polymorphic functions with their specialized names.
    For top-level poly refs, delegates to [resolve_spec] if not yet resolved.
    For inner poly lets, specializes locally. *)
and rewrite_refs (poly_env : poly_env) (env : spec_map) (t : Typecheck.term)
  : spec_map * Typecheck.term * Typecheck.top list
  =
  let (desc : Typecheck.term_desc), env, defs =
    match t.desc with
    | Var v ->
      (match Map.find poly_env v with
       | Some _ when Specialize_structs.is_concrete t.ty ->
         let env, spec_name, defs = resolve_spec poly_env env v t.ty in
         Var spec_name, env, defs
       | _ -> t.desc, env, [])
    | Float _ | Int _ | Bool _ -> t.desc, env, []
    | Vec (n, ts) ->
      let env, ts, defs = rewrite_refs_list poly_env env ts in
      Vec (n, ts), env, defs
    | Mat (n, m, ts) ->
      let env, ts, defs = rewrite_refs_list poly_env env ts in
      Mat (n, m, ts), env, defs
    | Lam (v, body) ->
      let env, body, defs = rewrite_refs poly_env env body in
      Lam (v, body), env, defs
    | App (f, x) ->
      let env, f, defs1 = rewrite_refs poly_env env f in
      let env, x, defs2 = rewrite_refs poly_env env x in
      App (f, x), env, defs1 @ defs2
    | Let (recur, v, constrs, bind, body)
      when not (Specialize_structs.is_concrete bind.ty) ->
      (* Specialization for inner polymorphic lets *)
      let usages = collect_var_usages v body in
      if List.is_empty usages
      then (
        (* Dead code, just rewrite body *)
        let env, body, defs = rewrite_refs poly_env env body in
        body.desc, env, defs)
      else (
        let env, specs, defs1 =
          List.fold usages ~init:(env, [], []) ~f:(fun (env, specs, defs) concrete_ty ->
            let sub = subst ~poly:bind.ty ~concrete:concrete_ty in
            let sub =
              Typecheck.solve_scheme_constrs constrs sub |> Compiler_error.ok_exn
            in
            let spec_bind = Typecheck.subst_term sub bind in
            let spec_name =
              Utils.fresh (v ^ "_" ^ Specialize_structs.mangle_ty concrete_ty)
            in
            let env, spec_bind, new_defs = rewrite_refs poly_env env spec_bind in
            let spec_bind =
              match recur with
              | Rec _ -> rename_var v spec_name spec_bind
              | Nonrec -> spec_bind
            in
            env, specs @ [ spec_name, spec_bind, concrete_ty ], defs @ new_defs)
        in
        (* Rewrite body replacing [Var v] with appropriate spec name *)
        let body' =
          List.fold specs ~init:body ~f:(fun b (spec_name, _, concrete_ty) ->
            rewrite_var_at_type v spec_name concrete_ty b)
        in
        let env, body', defs2 = rewrite_refs poly_env env body' in
        (* Wrap in nested lets *)
        ( (List.fold_right specs ~init:body' ~f:(fun (spec_name, spec_bind, _) acc ->
             ({ desc = Let (recur, spec_name, [], spec_bind, acc)
              ; ty = acc.ty
              ; loc = t.loc
              }
              : Typecheck.term)))
            .desc
        , env
        , defs1 @ defs2 ))
    | Let (recur, v, constrs, bind, body) ->
      let env, bind, defs1 = rewrite_refs poly_env env bind in
      let env, body, defs2 = rewrite_refs poly_env env body in
      Let (recur, v, constrs, bind, body), env, defs1 @ defs2
    | If (c, t, e) ->
      let env, c, defs1 = rewrite_refs poly_env env c in
      let env, t, defs2 = rewrite_refs poly_env env t in
      let env, e, defs3 = rewrite_refs poly_env env e in
      If (c, t, e), env, defs1 @ defs2 @ defs3
    | Bop (op, l, r) ->
      let env, l, defs1 = rewrite_refs poly_env env l in
      let env, r, defs2 = rewrite_refs poly_env env r in
      Bop (op, l, r), env, defs1 @ defs2
    | Index (t, i) ->
      let env, t, defs = rewrite_refs poly_env env t in
      Index (t, i), env, defs
    | Builtin (b, ts) ->
      let env, ts, defs = rewrite_refs_list poly_env env ts in
      Builtin (b, ts), env, defs
    | Record (s, ts) ->
      let env, ts, defs = rewrite_refs_list poly_env env ts in
      Record (s, ts), env, defs
    | Field (t, f) ->
      let env, t, defs = rewrite_refs poly_env env t in
      Field (t, f), env, defs
    | Variant (ty_name, ctor, args) ->
      let env, args, defs = rewrite_refs_list poly_env env args in
      Variant (ty_name, ctor, args), env, defs
    | Match (scrutinee, cases) ->
      let env, scrutinee, defs1 = rewrite_refs poly_env env scrutinee in
      let env, cases_rev, defs2 =
        List.fold cases ~init:(env, [], []) ~f:(fun (env, acc, defs) (pat, body) ->
          let env, body, new_defs = rewrite_refs poly_env env body in
          env, (pat, body) :: acc, defs @ new_defs)
      in
      Match (scrutinee, List.rev cases_rev), env, defs1 @ defs2
  in
  env, { t with desc }, defs

and rewrite_refs_list (poly_env : poly_env) (env : spec_map) (ts : Typecheck.term list) =
  let spec_map, ts_rev, defs =
    List.fold ts ~init:(env, [], []) ~f:(fun (spec_map, acc, defs) t ->
      let spec_map, t, new_defs = rewrite_refs poly_env spec_map t in
      spec_map, t :: acc, defs @ new_defs)
  in
  spec_map, List.rev ts_rev, defs
;;

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

let monomorphize (Program tops : Typecheck.t) : t Compiler_error.t =
  let _, _, tops =
    List.fold
      tops
      ~init:(String.Map.empty, String.Map.empty, [])
      ~f:(fun (poly_env, env, acc) (top : Typecheck.top) ->
        match top.desc with
        (* Polymorphic case: register in poly_env, emit nothing *)
        | Define (recur, v, bind) when not (Specialize_structs.is_concrete top.ty) ->
          let entry =
            { poly_type = top.ty
            ; poly_bind = bind
            ; poly_recur = recur
            ; poly_loc = top.loc
            ; poly_constrs = top.scheme_constrs
            }
          in
          let poly_env = Map.set poly_env ~key:v ~data:entry in
          poly_env, env, acc
        (* Monomorphic case: resolve poly refs, rewrite, emit *)
        | Define (recur, v, bind) ->
          let refs = collect_poly_refs poly_env bind in
          let env, ref_defs =
            List.fold refs ~init:(env, []) ~f:(fun (env, defs) (name, ty) ->
              let env, _, new_defs = resolve_spec poly_env env name ty in
              env, defs @ new_defs)
          in
          let env, bind, inner_defs = rewrite_refs poly_env env bind in
          let top : Typecheck.top = { top with desc = Define (recur, v, bind) } in
          poly_env, env, acc @ ref_defs @ inner_defs @ [ top ]
        | Extern _ | TypeDef _ -> poly_env, env, acc @ [ top ])
  in
  let%map tops = Compiler_error.all (List.map tops ~f:top_of_tc) in
  Program tops
;;
