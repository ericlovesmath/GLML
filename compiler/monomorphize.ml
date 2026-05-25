open Core
open Sexplib.Sexp
open Typecheck

include Compiler_error.Pass (struct
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

(** NOTE: [poly_fn_env] is populated once up front and treated as read-only.

   [all_tops_rev] holds both specializations and concrete tops in reverse *)
type state =
  { poly_fn_env : poly_def String.Map.t
  ; spec_map : spec_map
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
  | TyTuple of ty list
[@@deriving equal]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec (i, t) -> List [ Atom "vec"; Atom (Int.to_string i); sexp_of_ty t ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
  | TyRecord s -> Atom s
  | TyVariant s -> Atom s
  | TyTuple ts -> List (Atom "tuple" :: List.map ts ~f:sexp_of_ty)
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
  | Tuple of term list

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
  | Tuple ts -> List (Atom "tuple" :: List.map ts ~f:sexp_of_term)

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
    match List.map2 xs ys ~f with
    | Ok results -> List.concat results
    | Unequal_lengths -> []
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
  | TyTuple ts -> List.for_all ts ~f:is_concrete
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
  let sub = Constraint_solver.solve_scheme constrs sub in
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
    | Tuple ts -> Tuple (List.map ts ~f:subst)
  in
  { t with desc }
;;

let rename_var (src : string) (dst : string) =
  subst_var ~name:src ~new_name:dst ~pred:(Fun.const true)
;;

(* ==== State threading ==== *)

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

let rec resolve_spec (st : state) (name : string) (concrete_ty : Type_system.ty)
  : state * string
  =
  match find_spec st.spec_map name concrete_ty with
  | Some spec_name -> st, spec_name
  | None ->
    let entry =
      Map.find st.poly_fn_env name
      |> of_option "(unreachable) poly fn not found" ~d:[%message (name : string)]
      |> ok_exn
    in
    let spec_name = Utils.fresh (name ^ "_m") in
    (* Register in spec_map FIRST as cycle guard for recursive functions *)
    let st = { st with spec_map = add_spec st.spec_map name concrete_ty spec_name } in
    let sub = subst ~poly:entry.poly_type ~concrete:concrete_ty in
    let extra_constrs = collect_poly_let_eqs entry.poly_bind in
    let body =
      instantiate_scheme (entry.poly_constrs @ extra_constrs) entry.poly_bind sub
    in
    let body =
      match entry.poly_recur with
      | Rec _ -> rename_var name spec_name body
      | Nonrec -> body
    in
    let refs = collect_poly_refs st.poly_fn_env body in
    let st =
      List.fold_left refs ~init:st ~f:(fun st (dep_name, dep_ty) ->
        fst (resolve_spec st dep_name dep_ty))
    in
    let st, body = rewrite_refs st body in
    (* [TyVar]s that blocked [Coerce] lowering at typecheck-time are now concrete *)
    let body = Promote_ints.rewrite body in
    let top : Typecheck.top =
      { desc = Define (entry.poly_recur, spec_name, body)
      ; ty = concrete_ty
      ; loc = entry.poly_loc
      ; scheme_constrs = []
      }
    in
    { st with all_tops_rev = top :: st.all_tops_rev }, spec_name

and rewrite_refs (st : state) (t : Typecheck.term) : state * Typecheck.term =
  let st, desc =
    let open Typecheck in
    match t.desc with
    | Var v ->
      (match Map.find st.poly_fn_env v with
       | Some _ when is_concrete t.ty ->
         let st, spec_name = resolve_spec st v t.ty in
         st, Var spec_name
       | _ -> st, t.desc)
    | Float _ | Int _ | Bool _ -> st, t.desc
    | Vec (n, ts) ->
      let st, ts = List.fold_map ~f:rewrite_refs ~init:st ts in
      st, Vec (n, ts)
    | Lam (v, body) ->
      let st, body = rewrite_refs st body in
      st, Lam (v, body)
    | App (f, x) ->
      let st, f = rewrite_refs st f in
      let st, x = rewrite_refs st x in
      st, App (f, x)
    | Let (recur, v, constrs, bind, body) when not (is_concrete bind.ty) ->
      (* Specialization for inner polymorphic lets *)
      let usages = collect_var_usages v body in
      if List.is_empty usages
      then (
        let st, body = rewrite_refs st body in
        st, body.desc)
      else (
        let st, specs =
          List.fold_map usages ~init:st ~f:(fun st concrete_ty ->
            let sub = subst ~poly:bind.ty ~concrete:concrete_ty in
            let spec_bind = instantiate_scheme constrs bind sub in
            let spec_name = Utils.fresh (v ^ "_m") in
            let st, spec_bind = rewrite_refs st spec_bind in
            let spec_bind =
              match recur with
              | Rec _ -> rename_var v spec_name spec_bind
              | Nonrec -> spec_bind
            in
            let spec_bind = Promote_ints.rewrite spec_bind in
            st, (spec_name, spec_bind, concrete_ty))
        in
        let body =
          List.fold specs ~init:body ~f:(fun b (spec_name, _, concrete_ty) ->
            subst_var
              ~name:v
              ~new_name:spec_name
              ~pred:(fun t -> Type_system.equal_ty t.ty concrete_ty)
              b)
        in
        let st, body = rewrite_refs st body in
        ( st
        , (List.fold_right specs ~init:body ~f:(fun (spec_name, spec_bind, _) acc ->
             { desc = Let (recur, spec_name, [], spec_bind, acc)
             ; ty = acc.ty
             ; loc = t.loc
             }))
            .desc ))
    | Let (recur, v, _, bind, body) ->
      let st, bind = rewrite_refs st bind in
      let st, body = rewrite_refs st body in
      st, Let (recur, v, [], bind, body)
    | If (c, t, e) ->
      let st, c = rewrite_refs st c in
      let st, t = rewrite_refs st t in
      let st, e = rewrite_refs st e in
      st, If (c, t, e)
    | Bop (op, l, r) ->
      let st, l = rewrite_refs st l in
      let st, r = rewrite_refs st r in
      st, Bop (op, l, r)
    | Index (t, i) ->
      let st, t = rewrite_refs st t in
      st, Index (t, i)
    | Builtin (b, ts) ->
      let st, ts = List.fold_map ~f:rewrite_refs ~init:st ts in
      st, Builtin (b, ts)
    | Record ts ->
      let st, ts = List.fold_map ~f:rewrite_refs ~init:st ts in
      st, Record ts
    | Field (t, f) ->
      let st, t = rewrite_refs st t in
      st, Field (t, f)
    | Variant (ctor, args) ->
      let st, args = List.fold_map ~f:rewrite_refs ~init:st args in
      st, Variant (ctor, args)
    | Coerce (target, inner) ->
      let st, inner = rewrite_refs st inner in
      st, Coerce (target, inner)
    | Match (scrutinee, cases) ->
      let st, scrutinee = rewrite_refs st scrutinee in
      let st, cases =
        List.fold_map cases ~init:st ~f:(fun st (pat, body) ->
          let st, body = rewrite_refs st body in
          st, (pat, body))
      in
      st, Match (scrutinee, cases)
    | Tuple ts ->
      let st, ts = List.fold_map ~f:rewrite_refs ~init:st ts in
      st, Tuple ts
  in
  st, { t with desc }
;;

(* ===== Conversion from Typecheck types ===== *)

let rec ty_of (t : Type_system.ty) : ty =
  match t with
  | TyVar _ -> raise "unexpected TyVar after monomorphization"
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec (n, t) -> TyVec (n, ty_of t)
  | TyRecord (n, _) -> TyRecord n
  | TyVariant (n, _) -> TyVariant n
  | TyArrow (a, b) ->
    let a = ty_of a in
    let b = ty_of b in
    TyArrow (a, b)
  | TyTuple ts -> TyTuple (List.map ts ~f:ty_of)
;;

let rec term_of_tc (t : Typecheck.term) : term =
  let ty = ty_of t.ty in
  let desc = term_desc_of_tc t.desc in
  ({ desc; ty; loc = t.loc } : term)

and term_desc_of_tc (d : Typecheck.term_desc) : term_desc =
  match d with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
  | Vec (n, ts) -> Vec (n, List.map ts ~f:term_of_tc)
  | Lam (v, body) -> Lam (v, term_of_tc body)
  | App (f, x) ->
    let f = term_of_tc f in
    let x = term_of_tc x in
    App (f, x)
  | Let (r, v, constrs, bind, body) ->
    let bind = term_of_tc bind in
    let body = term_of_tc body in
    if List.is_empty constrs
    then Let (r, v, bind, body)
    else raise "Let has constraints" ~d:[%message (d : Typecheck.term_desc)]
  | If (c, t, e) ->
    let c = term_of_tc c in
    let t = term_of_tc t in
    let e = term_of_tc e in
    If (c, t, e)
  | Bop (op, l, r) ->
    let l = term_of_tc l in
    let r = term_of_tc r in
    Bop (op, l, r)
  | Index (t, i) -> Index (term_of_tc t, i)
  | Builtin (b, ts) -> Builtin (b, List.map ts ~f:term_of_tc)
  | Record ts -> Record (List.map ts ~f:term_of_tc)
  | Field (t, f) -> Field (term_of_tc t, f)
  | Variant (ctor, args) -> Variant (ctor, List.map args ~f:term_of_tc)
  | Match (scrutinee, cases) ->
    let scrutinee = term_of_tc scrutinee in
    let cases = List.map cases ~f:(fun (pat, body) -> pat, term_of_tc body) in
    Match (scrutinee, cases)
  | Tuple ts -> Tuple (List.map ts ~f:term_of_tc)
  | Coerce (target, inner) ->
    (* materialize_coerce re-runs after monomorphize, so the only Coerce that can
       reach here is a residual where both sides ended up equal. *)
    if Type_system.equal_ty target inner.ty
    then (term_of_tc inner).desc
    else
      raise
        "unexpected unresolved Coerce after monomorphize"
        ~d:[%message (d : Typecheck.term_desc)]
;;

let top_of_tc (t : Typecheck.top) : top =
  let ty = ty_of t.ty in
  let desc =
    match t.desc with
    | Define (r, v, bind) -> Define (r, v, term_of_tc bind)
    | Extern v -> Extern v
    | TypeDef (name, RecordDecl ([], fields)) ->
      let fields =
        List.map fields ~f:(fun (field_name, field_ty) -> field_name, ty_of field_ty)
      in
      TypeDef (name, RecordDecl fields)
    | TypeDef (_, RecordDecl (_ :: _, _)) ->
      raise "unexpected parametrized TypeDef after SpecializeStructs"
    | TypeDef (_, VariantDecl (_ :: _, _)) ->
      raise "unexpected parametrized VariantDecl after SpecializeStructs"
    | TypeDef (name, VariantDecl ([], ctors)) ->
      let ctors =
        List.map ctors ~f:(fun (ctor_name, tys) -> ctor_name, List.map tys ~f:ty_of)
      in
      TypeDef (name, VariantDecl ctors)
  in
  { desc; ty; loc = t.loc }
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
    | TyTuple ts -> TyTuple (List.map ts ~f:promote_ty)
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
      | Tuple ts -> Tuple (List.map ts ~f:promote_term)
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
  (* [equal_ty] already ignores the hint string at every level (see
     [@equal.ignore] in [Type_system.ty]), so we don't have to canonicalize
     hints to compare shapes - we just thread hints alongside as metadata. *)
  let hint_of = function
    | (TyRecord (h, _) | TyVariant (h, _)) when not (String.is_empty h) -> Some h
    | _ -> None
  in
  let rec upsert_shape acc ty h =
    match acc with
    | [] -> [ ty, h ]
    | (k, eh) :: rest when equal_ty k ty -> (k, Option.first_some eh h) :: rest
    | entry :: rest -> entry :: upsert_shape rest ty h
  in
  let record acc ty = if is_concrete ty then upsert_shape acc ty (hint_of ty) else acc in
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
    | TyTuple ts -> List.fold ts ~init:acc ~f:walk_ty
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
  let prefix_for ty hint =
    match hint with
    | Some h -> h
    | None ->
      (match ty with
       | TyVariant _ -> "var"
       | _ -> "rec")
  in
  let assignments =
    List.fold tops ~init:[] ~f:walk_top
    |> List.map ~f:(fun (ty, h) -> ty, Utils.fresh (prefix_for ty h))
  in
  let lookup_ty ty = List.Assoc.find assignments ~equal:equal_ty ty in
  let rec rty ty =
    match ty with
    | TyFloat | TyInt | TyBool | TyVar _ -> ty
    | TyVec (n, t) -> TyVec (n, rty t)
    | TyArrow (a, b) -> TyArrow (rty a, rty b)
    | TyRecord (h, fields) ->
      let fields = List.map fields ~f:(fun (n, t) -> n, rty t) in
      TyRecord (Option.value (lookup_ty (TyRecord (h, fields))) ~default:h, fields)
    | TyVariant (h, ctors) ->
      let ctors = List.map ctors ~f:(fun (n, ts) -> n, List.map ts ~f:rty) in
      TyVariant (Option.value (lookup_ty (TyVariant (h, ctors))) ~default:h, ctors)
    | TyTuple ts -> TyTuple (List.map ts ~f:rty)
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
      | Tuple ts -> Tuple (List.map ts ~f:rterm)
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

let monomorphize_exn (program : Typecheck.t) : t =
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
    |> String.Map.of_alist_or_error
    |> of_or_error
    |> ok_exn
  in
  (* Process Defines + Externs. TypeDefs are dropped here - they'll be
     re-emitted from collected shapes after specialization is complete. *)
  let init_state = { poly_fn_env; spec_map = String.Map.empty; all_tops_rev = [] } in
  let st =
    List.fold_left tops ~init:init_state ~f:(fun st top ->
      match top.desc with
      | TypeDef _ -> st
      | Extern _ -> { st with all_tops_rev = top :: st.all_tops_rev }
      | Define _ when not (is_concrete top.ty) ->
        (* Skip poly defines (already in [poly_fn_env]) *)
        st
      | Define (recur, v, bind) ->
        let refs = collect_poly_refs st.poly_fn_env bind in
        let st =
          List.fold_left refs ~init:st ~f:(fun st (name, ty) ->
            fst (resolve_spec st name ty))
        in
        let st, bind = rewrite_refs st bind in
        let top = { top with desc = Define (recur, v, bind) } in
        { st with all_tops_rev = top :: st.all_tops_rev })
  in
  let final_value_tops = List.rev st.all_tops_rev in
  let typedef_loc = (List.hd tops |> of_option "empty program" |> ok_exn).loc in
  (* Every concrete TyRecord/TyVariant in the program gets a fresh, hint-derived name *)
  let all_tops_tc = assign_names ~typedef_loc final_value_tops in
  let tops = List.map all_tops_tc ~f:top_of_tc in
  promote_int_vecs (Program tops)
;;

let monomorphize t = try_with (fun () -> monomorphize_exn t)
