open Core
open Sexplib.Sexp
open Typecheck
open Compiler_error.Let_syntax

module Err = Compiler_error.Pass (struct
    let name = "monomorphize"
  end)

(* ===== Helper types ===== *)

type poly_def =
  { poly_type : Type_system.ty
  ; poly_bind : Typecheck.term
  ; poly_recur : Frontend.recur
  ; poly_loc : Lexer.loc
  ; poly_constrs : Type_system.constr list
  }

type spec_map = (Type_system.ty * string) list String.Map.t

(* TODO: We can probably redo the code so env/acc is merged now *)
(* Read-only context. Built once before processing *)
type env = { poly_fn_env : poly_def String.Map.t }

(* Write accumulator. [all_tops_rev] holds both specializations and
   concrete tops in reverse program order, so that [List.rev all_tops_rev]
   gives the correct declaration order. *)
type acc =
  { spec_map : spec_map
  ; all_tops_rev : Typecheck.top list
  }

(* ===== Output types ===== *)

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyRecord of string
  | TyVariant of string
[@@deriving equal]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec (i, t) -> List [ Atom "vec"; Atom (Int.to_string i); sexp_of_ty t ]
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
  | Lam of string * term
  | App of term * term
  | Let of Frontend.recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of term list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (Frontend.pat * term) list

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
  | Record ts -> List (Atom "record" :: List.map ts ~f:sexp_of_term)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ctor, args) ->
    List (Atom "Variant" :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (pat, body) = List [ Frontend.sexp_of_pat pat; sexp_of_term body ] in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = List [ sexp_of_term_desc t.desc; Atom ":"; sexp_of_ty t.ty ]

type top_desc =
  | Define of Frontend.recur * string * term
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

(* ===== Param Specialization Helpers ===== *)

(** Propose a [params -> concrete] substitution by structural recursion *)
let rec subst ~(poly : Type_system.ty) ~(concrete : Type_system.ty)
  : (string * Type_system.ty) list
  =
  let pairwise xs ys ~f =
    if List.length xs = List.length ys then List.concat (List.map2_exn xs ys ~f) else []
  in
  match poly, concrete with
  | TyVar v, _ -> [ v, concrete ]
  | TyArrow (l, r), TyArrow (l', r') ->
    subst ~poly:l ~concrete:l' @ subst ~poly:r ~concrete:r'
  | TyVec (n, t), TyVec (n', t') when n = n' -> subst ~poly:t ~concrete:t'
  | TyRecord (_, fs), TyRecord (_, fs') ->
    pairwise fs fs' ~f:(fun (_, a) (_, a') -> subst ~poly:a ~concrete:a')
  | TyVariant (_, cs), TyVariant (_, cs') ->
    pairwise cs cs' ~f:(fun (_, ts) (_, ts') ->
      pairwise ts ts' ~f:(fun a a' -> subst ~poly:a ~concrete:a'))
  | _ -> []
;;

let rec is_concrete (ty : Type_system.ty) : bool =
  match ty with
  | TyVar _ -> false
  | TyFloat | TyInt | TyBool -> true
  | TyVec (_, t) -> is_concrete t
  | TyVariant (_, ctors) ->
    List.for_all ctors ~f:(fun (_, ts) -> List.for_all ts ~f:is_concrete)
  | TyRecord (_, fields) -> List.for_all fields ~f:(fun (_, t) -> is_concrete t)
  | TyArrow (a, b) -> is_concrete a && is_concrete b
;;

(* ===== Monomorphize-specific helpers ===== *)

let collect_var_usages (name : string) (t : Typecheck.term) : Type_system.ty list =
  fold_term
    ~f:(fun acc t ->
      match t.desc with
      | Var v when String.equal v name -> t.ty :: acc
      | _ -> acc)
    []
    t
  |> List.stable_dedup ~compare:(fun a b -> if Type_system.equal_ty a b then 0 else 1)
;;

(** Solve scheme constraints under [sub], then apply the resulting substitution
    to the polymorphic term. Used by [Monomorphize] to specialize bindings. *)
let instantiate_scheme constrs term sub =
  let%map sub = Constraint_solver.solve_scheme constrs sub in
  Typecheck.subst_term sub term
;;

(** For each polymorphic Let binding in [t], collect Eq constraints between
    the binding's definition type and each usage type in the continuation.
    These encode equality information that was solved away inside infer_binding
    but not propagated to the outer term's type, allowing [instantiate_scheme] to
    resolve "orphan" constraint variables. *)
let collect_poly_let_eqs (t : Typecheck.term) : Type_system.constr list =
  fold_term
    ~f:(fun acc t ->
      match t.desc with
      | Let (_, v, inner_constrs, bind, body) when not (is_concrete bind.ty) ->
        let usages = collect_var_usages v body in
        let eq_constrs =
          List.filter_map usages ~f:(fun usage_ty ->
            if Type_system.equal_ty bind.ty usage_ty
            then None
            else Some { Type_system.desc = Eq (bind.ty, usage_ty); loc = t.loc })
        in
        (* Also include the inner let's scheme constraints so that any variables
           introduced by those constraints (like return-type vars) also get resolved
           once the Eq constraints fix the parameter types. *)
        eq_constrs @ inner_constrs @ acc
      | _ -> acc)
    []
    t
;;

let map_cases_capture_avoiding
      ~(var : string)
      ~(f : Typecheck.term -> Typecheck.term)
      (cases : (Frontend.pat * Typecheck.term) list)
  : (Frontend.pat * Typecheck.term) list
  =
  List.map cases ~f:(fun (pat, body) ->
    if List.mem (Frontend.pat_bound_vars pat) var ~equal:String.equal
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
    | Record args -> Record (List.map args ~f:subst)
    | Field (t, f) -> Field (subst t, f)
    | Variant (ctor, args) -> Variant (ctor, List.map args ~f:subst)
    | Match (scrutinee, cases) ->
      Match (subst scrutinee, map_cases_capture_avoiding ~var:name ~f:subst cases)
    | Coerce (target, inner) -> Coerce (target, subst inner)
  in
  { t with desc }
;;

let rename_var (src : string) (dst : string) =
  subst_var ~name:src ~new_name:dst ~pred:(Fun.const true)
;;

(* ==== Env and Acc ==== *)

let collect_poly_refs (poly_fn_env : poly_def String.Map.t) (t : Typecheck.term)
  : (string * Type_system.ty) list
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
    if c <> 0 then c else if Type_system.equal_ty t1 t2 then 0 else 1)
;;

let find_spec (env : spec_map) (name : string) (ty : Type_system.ty) : string option =
  Map.find env name
  |> Option.bind
       ~f:
         (List.find_map ~f:(fun (t, n) ->
            if Type_system.equal_ty t ty then Some n else None))
;;

let add_spec (env : spec_map) (name : string) (ty : Type_system.ty) (spec_name : string)
  : spec_map
  =
  let specs = Option.value (Map.find env name) ~default:[] in
  Map.set env ~key:name ~data:((ty, spec_name) :: specs)
;;

let rec resolve_spec
          (env : env)
          (acc : acc)
          (name : string)
          (concrete_ty : Type_system.ty)
  : (acc * string) Compiler_error.t
  =
  match find_spec acc.spec_map name concrete_ty with
  | Some spec_name -> Ok (acc, spec_name)
  | None ->
    let entry = Map.find_exn env.poly_fn_env name in
    let spec_name = Utils.fresh (name ^ "_m") in
    (* Register in spec_map FIRST as cycle guard for recursive functions *)
    let acc = { acc with spec_map = add_spec acc.spec_map name concrete_ty spec_name } in
    let sub = subst ~poly:entry.poly_type ~concrete:concrete_ty in
    let extra_constrs = collect_poly_let_eqs entry.poly_bind in
    let%bind body =
      instantiate_scheme (entry.poly_constrs @ extra_constrs) entry.poly_bind sub
    in
    let body =
      match entry.poly_recur with
      | Rec _ -> rename_var name spec_name body
      | Nonrec -> body
    in
    let refs = collect_poly_refs env.poly_fn_env body in
    let%bind acc =
      List.fold_left refs ~init:(Ok acc) ~f:(fun acc_r (dep_name, dep_ty) ->
        let%bind acc = acc_r in
        let%bind acc, _ = resolve_spec env acc dep_name dep_ty in
        return acc)
    in
    let%bind acc, body = rewrite_refs env acc body in
    (* [TyVar]s that blocked [Coerce] lowering at typecheck-time are now concrete *)
    let body = Promote_ints.rewrite body in
    let top : Typecheck.top =
      { desc = Define (entry.poly_recur, spec_name, body)
      ; ty = concrete_ty
      ; loc = entry.poly_loc
      ; scheme_constrs = []
      }
    in
    Ok ({ acc with all_tops_rev = top :: acc.all_tops_rev }, spec_name)

and rewrite_list (env : env) (acc : acc) (ts : Typecheck.term list)
  : (acc * Typecheck.term list) Compiler_error.t
  =
  let%map acc, rev =
    List.fold_left
      ts
      ~init:(Ok (acc, []))
      ~f:(fun acc_r t ->
        let%bind acc, rev = acc_r in
        let%bind acc, t = rewrite_refs env acc t in
        return (acc, t :: rev))
  in
  acc, List.rev rev

and rewrite_refs (env : env) (acc : acc) (t : Typecheck.term)
  : (acc * Typecheck.term) Compiler_error.t
  =
  let inner : (acc * Typecheck.term_desc) Compiler_error.t =
    let open Typecheck in
    match t.desc with
    | Var v ->
      (match Map.find env.poly_fn_env v with
       | Some _ when is_concrete t.ty ->
         let%map acc, spec_name = resolve_spec env acc v t.ty in
         acc, Var spec_name
       | _ -> Ok (acc, t.desc))
    | Float _ | Int _ | Bool _ -> Ok (acc, t.desc)
    | Vec (n, ts) ->
      let%map acc, ts = rewrite_list env acc ts in
      acc, Vec (n, ts)
    | Lam (v, body) ->
      let%map acc, body = rewrite_refs env acc body in
      acc, Lam (v, body)
    | App (f, x) ->
      let%bind acc, f = rewrite_refs env acc f in
      let%bind acc, x = rewrite_refs env acc x in
      Ok (acc, App (f, x))
    | Let (recur, v, constrs, bind, body) when not (is_concrete bind.ty) ->
      (* Specialization for inner polymorphic lets *)
      let usages = collect_var_usages v body in
      if List.is_empty usages
      then (
        let%map acc, body = rewrite_refs env acc body in
        acc, body.desc)
      else (
        let%bind acc, specs_rev =
          List.fold_left
            usages
            ~init:(Ok (acc, []))
            ~f:(fun acc_r concrete_ty ->
              let%bind acc, specs_rev = acc_r in
              let sub = subst ~poly:bind.ty ~concrete:concrete_ty in
              let%bind spec_bind = instantiate_scheme constrs bind sub in
              let spec_name = Utils.fresh (v ^ "_m") in
              let%map acc, spec_bind = rewrite_refs env acc spec_bind in
              let spec_bind =
                match recur with
                | Rec _ -> rename_var v spec_name spec_bind
                | Nonrec -> spec_bind
              in
              let spec_bind = Promote_ints.rewrite spec_bind in
              acc, (spec_name, spec_bind, concrete_ty) :: specs_rev)
        in
        let specs = List.rev specs_rev in
        let body =
          List.fold specs ~init:body ~f:(fun b (spec_name, _, concrete_ty) ->
            subst_var
              ~name:v
              ~new_name:spec_name
              ~pred:(fun t -> Type_system.equal_ty t.ty concrete_ty)
              b)
        in
        let%map acc, body = rewrite_refs env acc body in
        ( acc
        , (List.fold_right specs ~init:body ~f:(fun (spec_name, spec_bind, _) acc ->
             { desc = Let (recur, spec_name, [], spec_bind, acc)
             ; ty = acc.ty
             ; loc = t.loc
             }))
            .desc ))
    | Let (recur, v, _, bind, body) ->
      (* NOTE: Now that inner lets may be resolved early with concrete types,
         their scheme constraints are already consumed, so we can ignore them *)
      let%bind acc, bind = rewrite_refs env acc bind in
      let%bind acc, body = rewrite_refs env acc body in
      Ok (acc, Let (recur, v, [], bind, body))
    | If (c, t, e) ->
      let%bind acc, c = rewrite_refs env acc c in
      let%bind acc, t = rewrite_refs env acc t in
      let%bind acc, e = rewrite_refs env acc e in
      Ok (acc, If (c, t, e))
    | Bop (op, l, r) ->
      let%bind acc, l = rewrite_refs env acc l in
      let%bind acc, r = rewrite_refs env acc r in
      Ok (acc, Bop (op, l, r))
    | Index (t, i) ->
      let%map acc, t = rewrite_refs env acc t in
      acc, Index (t, i)
    | Builtin (b, ts) ->
      let%map acc, ts = rewrite_list env acc ts in
      acc, Builtin (b, ts)
    | Record ts ->
      let%map acc, ts = rewrite_list env acc ts in
      acc, Record ts
    | Field (t, f) ->
      let%map acc, t = rewrite_refs env acc t in
      acc, Field (t, f)
    | Variant (ctor, args) ->
      let%map acc, args = rewrite_list env acc args in
      acc, Variant (ctor, args)
    | Coerce (target, inner) ->
      let%map acc, inner = rewrite_refs env acc inner in
      acc, Coerce (target, inner)
    | Match (scrutinee, cases) ->
      let%bind acc, scrutinee = rewrite_refs env acc scrutinee in
      let%bind acc, cases_rev =
        List.fold_left
          cases
          ~init:(Ok (acc, []))
          ~f:(fun acc_r (pat, body) ->
            let%bind acc, cases_rev = acc_r in
            let%bind acc, body = rewrite_refs env acc body in
            Ok (acc, (pat, body) :: cases_rev))
      in
      Ok (acc, Match (scrutinee, List.rev cases_rev))
  in
  let%map acc, desc = inner in
  acc, { t with desc }
;;

(* ===== Conversion from Typecheck types =====
   [shape_to_name] is the single source of truth for nominal names. Both
   [shape_to_typedef] (emission) and [ty_of] / term-name rewriting go through
   it, so usage-site names always match the emitted struct names. *)

let rec ty_of (t : Type_system.ty) : ty Compiler_error.t =
  match t with
  | TyVar _ -> Err.fail "unexpected TyVar after monomorphization"
  | TyFloat -> Ok TyFloat
  | TyInt -> Ok TyInt
  | TyBool -> Ok TyBool
  | TyVec (n, t) ->
    let%map t = ty_of t in
    TyVec (n, t)
  | TyRecord (n, _) -> Ok (TyRecord n)
  | TyVariant (n, _) -> Ok (TyVariant n)
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
  | Record ts ->
    let%map ts = Compiler_error.all (List.map ts ~f:term_of_tc) in
    Record ts
  | Field (t, f) ->
    let%map t = term_of_tc t in
    Field (t, f)
  | Variant (ctor, args) ->
    let%map args = Compiler_error.all (List.map args ~f:term_of_tc) in
    Variant (ctor, args)
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
  | Coerce (target, inner) ->
    (* materialize_coerce re-runs after monomorphize, so the only Coerce that can
       reach here is a residual where both sides ended up equal. *)
    if Type_system.equal_ty target inner.ty
    then (
      let%map inner = term_of_tc inner in
      inner.desc)
    else
      Err.fail
        "unexpected unresolved Coerce after monomorphize"
        ~d:[%message (d : Typecheck.term_desc)]
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

(* GLSL has no ivec, so any [TyVe c(_, TyInt)] annotation that survived monomorphization
   has to be rewritten by the previous pass, this just makes the type annotations agree.

   TODO: This is here because is must run after specialization is complete since doing it
   earlier would mess with the [TyVar] case but I probably want to move it to the previous pass... *)
let promote_int_vecs : t -> t =
  let rec promote_ty = function
    | TyVec (n, TyInt) -> TyVec (n, TyFloat)
    | TyVec (n, inner) -> TyVec (n, promote_ty inner)
    | TyArrow (a, b) -> TyArrow (promote_ty a, promote_ty b)
    | (TyFloat | TyInt | TyBool | TyRecord _ | TyVariant _) as ty -> ty
  in
  let rec promote_term (t : term) : term =
    let ty = promote_ty t.ty in
    let desc =
      match t.desc with
      | Var _ | Float _ | Int _ | Bool _ -> t.desc
      | Vec (n, ts) -> Vec (n, List.map ts ~f:promote_term)
      | Lam (v, body) -> Lam (v, promote_term body)
      | App (f, x) -> App (promote_term f, promote_term x)
      | Let (recur, v, bind, body) -> Let (recur, v, promote_term bind, promote_term body)
      | If (c, tt, e) -> If (promote_term c, promote_term tt, promote_term e)
      | Bop (op, l, r) -> Bop (op, promote_term l, promote_term r)
      | Index (inner, i) -> Index (promote_term inner, i)
      | Builtin (b, ts) -> Builtin (b, List.map ts ~f:promote_term)
      | Record ts -> Record (List.map ts ~f:promote_term)
      | Field (inner, f) -> Field (promote_term inner, f)
      | Variant (ctor, args) -> Variant (ctor, List.map args ~f:promote_term)
      | Match (scrut, cases) ->
        Match (promote_term scrut, List.map cases ~f:(Tuple2.map_snd ~f:promote_term))
    in
    { t with desc; ty }
  in
  let promote_top (top : top) : top =
    let ty = promote_ty top.ty in
    let desc =
      match top.desc with
      | Define (recur, v, bind) -> Define (recur, v, promote_term bind)
      | Extern _ -> top.desc
      | TypeDef (name, RecordDecl fields) ->
        let fields = List.map fields ~f:(fun (f, ty) -> f, promote_ty ty) in
        TypeDef (name, RecordDecl fields)
      | TypeDef (name, VariantDecl ctors) ->
        let ctors = List.map ctors ~f:(fun (c, tys) -> c, List.map tys ~f:promote_ty) in
        TypeDef (name, VariantDecl ctors)
    in
    { top with desc; ty }
  in
  fun (Program tops) -> Program (List.map tops ~f:promote_top)
;;

(** Walk the value tops, assign a fresh name to each unique concrete
    [TyRecord]/[TyVariant] shape, rewrite every type occurrence in the program
    to carry that assigned name as its [hint], and prepend a [TypeDef] top per
    unique shape using [typedef_loc]. *)
let assign_names ~(typedef_loc : Lexer.loc) (tops : Typecheck.top list)
  : Typecheck.top list
  =
  let open Type_system in
  let hint_of = function
    | TyRecord (h, _) | TyVariant (h, _) -> h
    | _ -> ""
  in
  let canon = function
    | TyRecord (_, fs) -> TyRecord ("", fs)
    | TyVariant (_, cs) -> TyVariant ("", cs)
    | t -> t
  in
  let record acc ty =
    if not (is_concrete ty)
    then acc
    else (
      let key = canon ty in
      let h = hint_of ty in
      let found, updated =
        List.fold_map acc ~init:false ~f:(fun seen (k, eh) ->
          if equal_ty k key then true, (k, merge_hint eh h) else seen, (k, eh))
      in
      if found then updated else acc @ [ key, h ])
  in
  let rec walk_ty acc ty =
    match ty with
    | TyFloat | TyInt | TyBool | TyVar _ -> acc
    | TyVec (_, t) -> walk_ty acc t
    | TyArrow (a, b) -> walk_ty (walk_ty acc a) b
    | TyRecord (_, fields) ->
      record (List.fold fields ~init:acc ~f:(fun a (_, t) -> walk_ty a t)) ty
    | TyVariant (_, ctors) ->
      record
        (List.fold ctors ~init:acc ~f:(fun a (_, ts) -> List.fold ts ~init:a ~f:walk_ty))
        ty
  in
  let walk_term acc t = fold_term ~f:(fun a t -> walk_ty a t.ty) acc t in
  let walk_top acc (top : Typecheck.top) =
    let acc = walk_ty acc top.ty in
    match top.desc with
    | Define (_, _, bind) -> walk_term acc bind
    | Extern _ -> acc
    | TypeDef (_, RecordDecl (_, fields)) ->
      List.fold fields ~init:acc ~f:(fun a (_, t) -> walk_ty a t)
    | TypeDef (_, VariantDecl (_, ctors)) ->
      List.fold ctors ~init:acc ~f:(fun a (_, ts) -> List.fold ts ~init:a ~f:walk_ty)
  in
  let assignments =
    List.fold tops ~init:[] ~f:walk_top
    |> List.map ~f:(fun (ty, h) ->
      let prefix =
        match h, ty with
        | "", TyVariant _ -> "var"
        | "", _ -> "rec"
        | h, _ -> h
      in
      ty, Utils.fresh prefix)
  in
  let lookup ty = List.Assoc.find assignments ~equal:equal_ty (canon ty) in
  let rec rty ty =
    match ty with
    | TyFloat | TyInt | TyBool | TyVar _ -> ty
    | TyVec (n, t) -> TyVec (n, rty t)
    | TyArrow (a, b) -> TyArrow (rty a, rty b)
    | TyRecord (h, fields) ->
      let fields = List.map fields ~f:(fun (n, t) -> n, rty t) in
      TyRecord (Option.value (lookup (TyRecord ("", fields))) ~default:h, fields)
    | TyVariant (h, ctors) ->
      let ctors = List.map ctors ~f:(fun (n, ts) -> n, List.map ts ~f:rty) in
      TyVariant (Option.value (lookup (TyVariant ("", ctors))) ~default:h, ctors)
  in
  let rec rterm (t : Typecheck.term) : Typecheck.term =
    let desc : Typecheck.term_desc =
      match t.desc with
      | Var _ | Float _ | Int _ | Bool _ -> t.desc
      | Vec (n, ts) -> Vec (n, List.map ts ~f:rterm)
      | Lam (v, body) -> Lam (v, rterm body)
      | App (f, x) -> App (rterm f, rterm x)
      | Let (recur, v, constrs, bind, body) ->
        Let (recur, v, subst_constraints [] constrs, rterm bind, rterm body)
      | If (c, tt, e) -> If (rterm c, rterm tt, rterm e)
      | Bop (op, l, r) -> Bop (op, rterm l, rterm r)
      | Index (tt, i) -> Index (rterm tt, i)
      | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rterm)
      | Record ts -> Record (List.map ts ~f:rterm)
      | Field (tt, f) -> Field (rterm tt, f)
      | Variant (ctor, args) -> Variant (ctor, List.map args ~f:rterm)
      | Match (scrut, cases) ->
        Match (rterm scrut, List.map cases ~f:(fun (p, b) -> p, rterm b))
      | Coerce (target, inner) -> Coerce (rty target, rterm inner)
    in
    { t with desc; ty = rty t.ty }
  in
  let rfields fs = List.map fs ~f:(fun (n, t) -> n, rty t) in
  let rctors cs = List.map cs ~f:(fun (n, ts) -> n, List.map ts ~f:rty) in
  let rtop (top : Typecheck.top) : Typecheck.top =
    let desc : Typecheck.top_desc =
      match top.desc with
      | Define (recur, v, bind) -> Define (recur, v, rterm bind)
      | Extern v -> Extern v
      | TypeDef (name, RecordDecl (params, fields)) ->
        TypeDef (name, RecordDecl (params, rfields fields))
      | TypeDef (name, VariantDecl (params, ctors)) ->
        TypeDef (name, VariantDecl (params, rctors ctors))
    in
    { top with desc; ty = rty top.ty }
  in
  let typedef_top (key, name) : Typecheck.top =
    let desc, ty =
      match key with
      | TyRecord (_, fields) ->
        let fields = rfields fields in
        ( (TypeDef (name, RecordDecl ([], fields)) : Typecheck.top_desc)
        , TyRecord (name, fields) )
      | TyVariant (_, ctors) ->
        let ctors = rctors ctors in
        ( (TypeDef (name, VariantDecl ([], ctors)) : Typecheck.top_desc)
        , TyVariant (name, ctors) )
      | _ -> assert false
    in
    { desc; ty; loc = typedef_loc; scheme_constrs = [] }
  in
  List.map assignments ~f:typedef_top @ List.map tops ~f:rtop
;;

let monomorphize (program : Typecheck.t) : t Compiler_error.t =
  let (Program tops) = program in
  (* Pre-populate poly_fn_env eagerly (valid because poly defines precede concrete uses) *)
  let poly_fn_env =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | Define (recur, v, bind) when not (is_concrete top.ty) ->
        Some
          ( v
          , { poly_type = top.ty
            ; poly_bind = bind
            ; poly_recur = recur
            ; poly_loc = top.loc
            ; poly_constrs = top.scheme_constrs
            } )
      | _ -> None)
    |> String.Map.of_alist_exn
  in
  let env = { poly_fn_env } in
  (* Process Defines + Externs. TypeDefs are dropped here - they'll be
     re-emitted from collected shapes after specialization is complete. *)
  let%bind acc =
    let init = Ok { spec_map = String.Map.empty; all_tops_rev = [] } in
    List.fold_left tops ~init ~f:(fun acc_r top ->
      let%bind acc = acc_r in
      match top.desc with
      | TypeDef _ -> Ok acc
      | Extern _ -> Ok { acc with all_tops_rev = top :: acc.all_tops_rev }
      | Define _ when not (is_concrete top.ty) ->
        (* Skip poly defines (already in [poly_fn_env]) *)
        Ok acc
      | Define (recur, v, bind) ->
        let refs = collect_poly_refs env.poly_fn_env bind in
        let%bind acc =
          List.fold_left refs ~init:(Ok acc) ~f:(fun acc_r (name, ty) ->
            let%bind acc = acc_r in
            let%bind acc, _ = resolve_spec env acc name ty in
            Ok acc)
        in
        let%bind acc, bind = rewrite_refs env acc bind in
        let top = { top with desc = Define (recur, v, bind) } in
        Ok { acc with all_tops_rev = top :: acc.all_tops_rev })
  in
  let final_value_tops = List.rev acc.all_tops_rev in
  let typedef_loc = (List.hd_exn tops).loc in
  (* Every concrete TyRecord/TyVariant in the program gets a fresh, hint-derived name *)
  let all_tops_tc = assign_names ~typedef_loc final_value_tops in
  let%map tops = Compiler_error.all (List.map all_tops_tc ~f:top_of_tc) in
  promote_int_vecs (Program tops)
;;
