(** Typechecking is done with Hindley-Milner inference (Algorithm W), but extended
    with typeclasses and broadcasting specific for operator overloading in GLSL *)

open Core
open Sexplib.Sexp
open Type_system

include Compiler_error.Pass (struct
    let name = "typecheck"
  end)

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Lam of string * term
  | App of term * term
  | Let of Frontend.recur * string * constr list * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of term list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (Frontend.pat * term) list
  | Coerce of ty * term
  | Tuple of term list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let sexp_of_forall_ty constrs ty =
  if List.is_empty constrs
  then sexp_of_ty ty
  else List [ Atom "forall"; List (List.map constrs ~f:sexp_of_constr); sexp_of_ty ty ]
;;

let rec sexp_of_term_desc = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_term)
  | Lam (v, body) -> List [ Atom "lambda"; Atom v; sexp_of_term body ]
  | App (f, x) -> List [ Atom "app"; sexp_of_term f; sexp_of_term x ]
  | Let (Rec n, v, constrs, bind, body) ->
    let rec_tag = List [ Atom "rec"; Atom (Int.to_string n) ] in
    let bind_sexp =
      List [ sexp_of_term_desc bind.desc; Atom ":"; sexp_of_forall_ty constrs bind.ty ]
    in
    List [ Atom "let"; rec_tag; Atom v; bind_sexp; sexp_of_term body ]
  | Let (Nonrec, v, constrs, bind, body) ->
    let bind_sexp =
      List [ sexp_of_term_desc bind.desc; Atom ":"; sexp_of_forall_ty constrs bind.ty ]
    in
    List [ Atom "let"; Atom v; bind_sexp; sexp_of_term body ]
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
  | Coerce (target, inner) ->
    List [ Atom "coerce"; sexp_of_ty target; sexp_of_term inner ]
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
  ; scheme_constrs : constr list
  }

let sexp_of_top t =
  List [ sexp_of_top_desc t.desc; Atom ":"; sexp_of_forall_ty t.scheme_constrs t.ty ]
;;

type t = Program of top list [@@deriving sexp_of]

(** Represents polymorphic [forall 'vars. constrs => ty] *)
type type_scheme = string list * constr list * ty [@@deriving sexp_of]

(** Maps type variables to type schemes *)
type context = type_scheme String.Map.t

(** Threaded state for typechecker *)
type env =
  { aliases : (string list * ty) String.Map.t
  ; structs : (string list * (string * ty) list) String.Map.t
  ; variants : (string list * (string * ty list) list) String.Map.t
  ; ctx : context
  }

let subst_context (sub : substitution) (ctx : context) : type_scheme String.Map.t =
  Map.map ctx ~f:(fun (vars, constrs, ty) ->
    let sub =
      List.filter sub ~f:(fun (v, _) -> not (List.mem vars v ~equal:String.equal))
    in
    vars, subst_constraints sub constrs, subst_ty sub ty)
;;

(** Pre-order fold over every subterm of [t]. *)
let rec fold_term ~(f : 'a -> term -> 'a) (acc : 'a) (t : term) : 'a =
  let fold = fold_term ~f in
  let acc = f acc t in
  match t.desc with
  | Var _ | Float _ | Int _ | Bool _ -> acc
  | Vec (_, ts) | Builtin (_, ts) -> List.fold ts ~init:acc ~f:fold
  | Record ts -> List.fold ts ~init:acc ~f:fold
  | Lam (_, body) -> fold acc body
  | App (fn, x) -> fold (fold acc fn) x
  | Let (_, _, _, bind, body) -> fold (fold acc bind) body
  | If (c, t, e) -> fold (fold (fold acc c) t) e
  | Bop (_, l, r) -> fold (fold acc l) r
  | Index (t, _) | Field (t, _) -> fold acc t
  | Variant (_, args) -> List.fold args ~init:acc ~f:fold
  | Match (scrutinee, cases) ->
    let acc = fold acc scrutinee in
    List.fold cases ~init:acc ~f:(fun acc (_, body) -> fold acc body)
  | Coerce (_, inner) -> fold acc inner
  | Tuple ts -> List.fold ts ~init:acc ~f:fold
;;

(** Apply substitution to term *)
let rec subst_term (sub : substitution) (t : term) : term =
  let subst = subst_term sub in
  let (desc : term_desc) =
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:subst)
    | Lam (v, body) -> Lam (v, subst body)
    | App (f, x) -> App (subst f, subst x)
    | Let (recur, v, constrs, bind, body) ->
      Let (recur, v, subst_constraints sub constrs, subst bind, subst body)
    | If (c, t, f) -> If (subst c, subst t, subst f)
    | Bop (op, l, r) -> Bop (op, subst l, subst r)
    | Index (t, i) -> Index (subst t, i)
    | Builtin (b, args) -> Builtin (b, List.map args ~f:subst)
    | Record args -> Record (List.map args ~f:subst)
    | Field (t, f) -> Field (subst t, f)
    | Variant (ctor, args) -> Variant (ctor, List.map args ~f:subst)
    | Match (scrutinee, cases) ->
      Match (subst scrutinee, List.map cases ~f:(fun (pat, body) -> pat, subst body))
    | Coerce (target, inner) -> Coerce (subst_ty sub target, subst inner)
    | Tuple ts -> Tuple (List.map ts ~f:subst)
  in
  { t with desc; ty = subst_ty sub t.ty }
;;

let ftv_of_context (ctx : context) : String.Set.t =
  let ftv_of_scheme (vars, constrs, ty) =
    let bound = String.Set.of_list vars in
    let ftv_constrs = String.Set.union_list (List.map constrs ~f:ftv_of_constraint) in
    Set.diff (Set.union (ftv_of_ty ty) ftv_constrs) bound
  in
  Map.data ctx |> List.map ~f:ftv_of_scheme |> String.Set.union_list
;;

(** Generalize a type by quantifying variables not in context.
    Constraints whose free vars are all generalizable go into the scheme;
    remaining constraints propagate upward. *)
let generalize (ctx : context) (deferred : constr list) (ty : ty)
  : type_scheme * constr list
  =
  let ftv_ty = ftv_of_ty ty in
  let ftv_deferred_all = String.Set.union_list (List.map deferred ~f:ftv_of_constraint) in
  let ftv_ctx = ftv_of_context ctx in
  (* If a constraint links a non-generalizable var to other vars, those become
     non-generalizable too. This prevents over-generalizing let-bindings whose
     types are constrained by context variables (like FieldAccess / IndexAccess) *)
  let non_generalizable =
    let rec go non_gen =
      let non_gen' =
        deferred
        |> List.map ~f:ftv_of_constraint
        |> List.filter ~f:(Fn.non (Set.are_disjoint non_gen))
        |> String.Set.union_list
        |> Set.union non_gen
      in
      if Set.equal non_gen non_gen' then non_gen else go non_gen'
    in
    go ftv_ctx
  in
  let generalizable = Set.diff (Set.union ftv_ty ftv_deferred_all) non_generalizable in
  let scheme_constrs, remaining =
    List.partition_tf deferred ~f:(fun c ->
      Set.is_subset (ftv_of_constraint c) ~of_:generalizable)
  in
  (Set.to_list generalizable, scheme_constrs, ty), remaining
;;

(** Value restriction check for generalization. *)
let rec is_value (t : Desugar.term) : bool =
  match t.desc with
  | Float _ | Int _ | Bool _ | Var _ | Lam _ -> true
  | Vec (_, ts) -> List.for_all ts ~f:is_value
  | Tuple ts -> List.for_all ts ~f:is_value
  | Record fields -> List.for_all fields ~f:(fun (_, t) -> is_value t)
  | Variant (_, args) -> List.for_all args ~f:is_value
  | Field (t, _) | Index (t, _) -> is_value t
  | Let (_, _, _, _, _, body) -> is_value body
  | App _ | If _ | Bop _ | Builtin _ | Sample _ | Match _ -> false
;;

let rec resolve_stlc_ty ~(loc : Lexer.loc) (env : env) (t : Frontend.ty) : ty =
  let resolve = resolve_stlc_ty ~loc env in
  let resolve_variant_or_struct name args =
    match Map.find env.variants name, Map.find env.structs name with
    | Some (params, ctors), _ ->
      (match List.zip params args with
       | Unequal_lengths ->
         raise "wrong number of type args" ~loc ~d:[%message (name : string)]
       | Ok sub ->
         let ctors = List.map ctors ~f:(Tuple2.map_snd ~f:(List.map ~f:(subst_ty sub))) in
         TyVariant (name, ctors))
    | _, Some (params, fields) ->
      (match List.zip params args with
       | Unequal_lengths ->
         raise "wrong number of type args" ~loc ~d:[%message (name : string)]
       | Ok sub ->
         let fields = List.map fields ~f:(fun (n, t) -> n, subst_ty sub t) in
         TyRecord (name, fields))
    | None, None ->
      raise "type not a variant or record" ~loc ~d:[%message (t : Frontend.ty)]
  in
  match t with
  | TyName name ->
    (match Map.find env.aliases name with
     | Some ([], body) -> body
     | Some (params, _) ->
       raise
         "type alias requires type arguments"
         ~loc
         ~d:[%message (name : string) (params : string list)]
     | None -> resolve_variant_or_struct name [])
  | TyApp (name, args) ->
    (match Map.find env.aliases name with
     | Some (params, body) ->
       (match List.zip params (List.map args ~f:resolve) with
        | Unequal_lengths ->
          raise "wrong number of type args" ~loc ~d:[%message (name : string)]
        | Ok sub -> subst_ty sub body)
     | None -> args |> List.map ~f:resolve |> resolve_variant_or_struct name)
  | TyArrow (l, r) -> TyArrow (resolve l, resolve r)
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec (n, t) -> TyVec (n, resolve t)
  | TyVar v -> TyVar v
  | TyTuple ts -> TyTuple (List.map ts ~f:resolve)
  | TySampler -> TySampler
;;

let resolve_constr (env : env) (constr : Frontend.constr) : constr =
  let resolve = resolve_stlc_ty ~loc:constr.loc env in
  let desc : constr_desc =
    match constr.desc with
    | CNumeric t -> HasClass (Numeric, resolve t)
    | CBroadcast (a, b, x) -> Broadcast (resolve a, resolve b, resolve x)
    | CMulBroadcast (a, b, x) -> MulBroadcast (resolve a, resolve b, resolve x)
  in
  { desc; loc = constr.loc }
;;

let bind_var ctx v ty = Map.set ctx ~key:v ~data:([], [], ty)

(** Find variant for constuctor, or record for field *)
let lookup_unique map ~f ~loc =
  match Map.filter map ~f |> Map.to_alist with
  | [ (name, (params, labels)) ] ->
    let params = List.map params ~f:(fun p -> p, fresh_tyvar ()) in
    name, params, labels
  | [] -> raise "unknown constructor/field in pattern" ~loc
  | _ -> raise "ambigious constructor/field in pattern" ~loc
;;

(** Thread [check_pat] over a list of (pat, expected_ty) pairs *)
let rec check_pats env loc (pats_with_tys : (Frontend.pat * ty) list)
  : context * constr list
  =
  List.fold pats_with_tys ~init:(env.ctx, []) ~f:(fun (ctx, acc) (p, ty) ->
    let ctx', cs = check_pat { env with ctx } loc ty p in
    ctx', acc @ cs)

and check_pat (env : env) loc (expected_ty : ty) (pat : Frontend.pat)
  : context * constr list
  =
  let eq t t' : constr = { desc = Eq (t, t'); loc } in
  match pat with
  | PatWildcard -> env.ctx, []
  | PatVar v -> bind_var env.ctx v expected_ty, []
  | PatLitBool _ -> env.ctx, [ eq expected_ty TyBool ]
  | PatLitInt _ -> env.ctx, [ eq expected_ty TyInt ]
  | PatLitFloat _ -> env.ctx, [ eq expected_ty TyFloat ]
  | PatBracket pats ->
    let elem_ty = fresh_tyvar () in
    let head_constr = eq expected_ty (TyVec (List.length pats, elem_ty)) in
    let ctx, sub_constrs = check_pats env loc (List.map pats ~f:(fun p -> p, elem_ty)) in
    ctx, head_constr :: sub_constrs
  | PatTuple pats ->
    let elem_tys = List.map pats ~f:(fun _ -> fresh_tyvar ()) in
    let head_constr = eq expected_ty (TyTuple elem_tys) in
    let ctx, sub_constrs = check_pats env loc (List.zip_exn pats elem_tys) in
    ctx, head_constr :: sub_constrs
  | PatCtor (ctor, sub_pats) ->
    let name, sub, ctors =
      lookup_unique env.variants ~loc ~f:(fun (_, ctors) ->
        List.exists ctors ~f:(fun (c, _) -> String.equal c ctor))
    in
    let ctors = List.map ctors ~f:(fun (n, ts) -> n, List.map ts ~f:(subst_ty sub)) in
    let head_constr = eq expected_ty (TyVariant (name, ctors)) in
    let expected_arg_tys = List.Assoc.find_exn ctors ~equal:String.equal ctor in
    (match List.zip sub_pats expected_arg_tys with
     | Unequal_lengths ->
       raise
         "wrong number of bindings in match case"
         ~loc
         ~d:
           [%message
             (ctor : string)
               ~expected:(List.length expected_arg_tys : int)
               ~got:(List.length sub_pats : int)]
     | Ok zipped ->
       let ctx, sub_constrs = check_pats env loc zipped in
       ctx, head_constr :: sub_constrs)
  | PatRecord (fields, is_partial) ->
    let name, sub, struct_fields =
      lookup_unique env.structs ~loc ~f:(fun (_, decl) ->
        fields
        |> List.map ~f:fst
        |> List.for_all ~f:(fun k -> List.exists decl ~f:(fun (f, _) -> String.equal f k)))
    in
    let struct_fields = List.map struct_fields ~f:(fun (n, t) -> n, subst_ty sub t) in
    let seen_names =
      List.fold fields ~init:String.Set.empty ~f:(fun seen (fname, _) ->
        if Set.mem seen fname
        then raise "duplicate field" ~loc ~d:[%message (fname : string)]
        else Set.add seen fname)
    in
    let all_fields = List.map struct_fields ~f:fst |> String.Set.of_list in
    if (not is_partial) && not (Set.is_empty (Set.diff all_fields seen_names))
    then raise "non-exhaustive record pat" ~loc;
    let pats_with_tys =
      List.map fields ~f:(fun (fname, fpat) ->
        let field_ty =
          List.Assoc.find struct_fields ~equal:String.equal fname
          |> of_option "unknown field" ~loc ~d:[%message (fname : string)]
          |> ok_exn
        in
        fpat, field_ty)
    in
    let head_constr = eq expected_ty (TyRecord (name, struct_fields)) in
    let ctx, sub_constrs = check_pats env loc pats_with_tys in
    ctx, head_constr :: sub_constrs
;;

let coerce_term loc (target : ty) (t : term) : term =
  { desc = Coerce (target, t); ty = target; loc }
;;

let coerce_arg_to_ty loc (arg : term) (expected_ty : ty) : term * constr list =
  let c desc = { desc; loc } in
  match expected_ty with
  | TyFloat ->
    let coerce_ty = fresh_tyvar () in
    ( coerce_term loc coerce_ty arg
    , [ c (HasClass (Comparable, arg.ty)); c (Broadcast (arg.ty, TyFloat, coerce_ty)) ] )
  | _ -> coerce_term loc expected_ty arg, [ c (Coerce (arg.ty, expected_ty)) ]
;;

(** Typecheck a [let] binding, returns the enclosing scope needs to continue inference: *)
let rec infer_binding
          (env : env)
          (loc : Lexer.loc)
          (bind_stlc : Desugar.term)
          (recur : Frontend.recur)
          (v : string)
          (return_ty : Frontend.ty option)
          (constrs : Frontend.constr list)
  : term * ty * env * constr list * constr list * substitution
  =
  let return_ty =
    match return_ty with
    | None -> None
    | Some return_ty -> Some (resolve_stlc_ty ~loc env return_ty)
  in
  let constrs = List.map ~f:(resolve_constr env) constrs in
  let ty_v_opt =
    match recur with
    | Nonrec -> None
    | Rec _ -> Some (Option.value return_ty ~default:(fresh_tyvar ()))
  in
  let env_gen =
    match ty_v_opt with
    | None -> env
    | Some ty_v -> { env with ctx = Map.set env.ctx ~key:v ~data:([], [], ty_v) }
  in
  let bind, constrs_bind, term_sub = gen_term env_gen bind_stlc in
  let constr desc = { desc; loc } in
  let constrs =
    let rec_constrs =
      match ty_v_opt with
      | None -> constrs_bind
      | Some ty_v -> constr (Eq (ty_v, bind.ty)) :: constrs_bind
    in
    match recur, return_ty with
    | Nonrec, Some full_ty -> constrs @ (constr (Coerce (bind.ty, full_ty)) :: rec_constrs)
    | _ -> constrs @ rec_constrs
  in
  let bind =
    match recur, return_ty with
    | Nonrec, Some full_ty -> coerce_term bind.loc full_ty bind
    | _ -> bind
  in
  let sub_bind, deferred = Constraint_solver.solve constrs in
  let ty_bind = subst_ty sub_bind bind.ty in
  let bind = subst_term sub_bind bind in
  let ctx = subst_context sub_bind (subst_context term_sub env.ctx) in
  let deferred = subst_constraints sub_bind deferred in
  let returns_fn =
    match ty_bind with
    | TyArrow _ -> true
    | _ -> false
  in
  let scheme, remaining =
    if returns_fn || is_value bind_stlc
    then generalize ctx deferred ty_bind
    else ([], [], ty_bind), deferred
  in
  let _, scheme_constrs, _ = scheme in
  let ctx = Map.set ctx ~key:v ~data:scheme in
  let env = { env with ctx } in
  bind, ty_bind, env, scheme_constrs, remaining, sub_bind

(** Generate the typed term and constraint set for a term *)
and gen_term (env : env) (t : Desugar.term) : term * constr list * substitution =
  let loc = t.loc in
  let make desc ty constrs = ({ desc; ty; loc } : term), constrs, [] in
  let constr desc = { desc; loc } in
  match t.desc with
  | Float f -> make (Float f) TyFloat []
  | Int i -> make (Int i) TyInt []
  | Bool b -> make (Bool b) TyBool []
  | Var v ->
    let vs, scheme_constrs, ty_scheme =
      match Map.find env.ctx v with
      | Some s -> s
      | None -> raise "var not found in type map" ~loc ~d:[%message (v : string)]
    in
    let sub = List.map vs ~f:(fun v -> v, fresh_tyvar ()) in
    let ty = subst_ty sub ty_scheme in
    if equal_ty ty TySampler
    then raise "sampler may only be used in #texture" ~loc ~d:[%message (v : string)];
    make (Var v) ty (subst_constraints sub scheme_constrs)
  | Lam (v, ty_ann, body_stlc) ->
    let ty_v =
      match ty_ann with
      | Some t -> resolve_stlc_ty ~loc env t
      | None -> fresh_tyvar ()
    in
    let env = { env with ctx = Map.set env.ctx ~key:v ~data:([], [], ty_v) } in
    let body, constrs, body_sub = gen_term env body_stlc in
    let ty_v = subst_ty body_sub ty_v in
    { desc = Lam (v, body); ty = TyArrow (ty_v, body.ty); loc }, constrs, body_sub
  | App (f, x) ->
    let f, constrs_f, sub_f = gen_term env f in
    let x, constrs_x, sub_x = gen_term env x in
    let arg_ty = fresh_tyvar () in
    let ret_ty = fresh_tyvar () in
    (* NOTE: We put [Eq(arg_ty, x.ty)] after [constrs_f] so the function's arg type
       is resolved first, so [int-in-float] contexts label arg as [float] not [int] *)
    let constrs =
      (constr (Eq (f.ty, TyArrow (arg_ty, ret_ty))) :: constrs_f)
      @ constrs_x
      @ [ constr (Coerce (x.ty, arg_ty)) ]
    in
    let x = coerce_term x.loc arg_ty x in
    let composed = compose_sub sub_x sub_f in
    { desc = App (f, x); ty = ret_ty; loc }, constrs, composed
  | Let (recur, v, return_ty, constrs, bind_stlc, body_stlc) ->
    let bind, _, env, scheme_constrs, remaining, sub_bind =
      infer_binding env loc bind_stlc recur v return_ty constrs
    in
    let body, constrs_body, body_sub = gen_term env body_stlc in
    let bind = subst_term body_sub bind in
    let remaining = subst_constraints body_sub remaining in
    let scheme_constrs = subst_constraints body_sub scheme_constrs in
    let composed = compose_sub body_sub sub_bind in
    ( { desc = Let (recur, v, scheme_constrs, bind, body); ty = body.ty; loc }
    , remaining @ constrs_body
    , composed )
  | If (c, t, e) ->
    let c, constrs_c, sub_c = gen_term env c in
    let t, constrs_t, sub_t = gen_term env t in
    let e, constrs_e, sub_e = gen_term env e in
    let join_ty = fresh_tyvar () in
    let constrs =
      constr (Eq (c.ty, TyBool))
      :: constr (Coerce (t.ty, join_ty))
      :: constr (Coerce (e.ty, join_ty))
      :: (constrs_c @ constrs_t @ constrs_e)
    in
    let t = coerce_term t.loc join_ty t in
    let e = coerce_term e.loc join_ty e in
    let composed = compose_sub sub_e (compose_sub sub_t sub_c) in
    { desc = If (c, t, e); ty = join_ty; loc }, constrs, composed
  | Bop (op, l, r) ->
    let l, constrs_l, sub_l = gen_term env l in
    let r, constrs_r, sub_r = gen_term env r in
    let ret_ty = fresh_tyvar () in
    let op_constrs =
      match op with
      | Add | Sub -> [ constr (Broadcast (l.ty, r.ty, ret_ty)) ]
      | Mod ->
        let bt = fresh_tyvar () in
        [ constr (Broadcast (l.ty, r.ty, bt))
        ; constr (Broadcast (bt, TyFloat, ret_ty))
        ; constr (HasClass (GenType, ret_ty))
        ]
      | Mul | Div -> [ constr (MulBroadcast (l.ty, r.ty, ret_ty)) ]
      | Eq ->
        let eq_ty = fresh_tyvar () in
        [ constr (HasClass (Equatable, eq_ty))
        ; constr (Coerce (l.ty, eq_ty))
        ; constr (Coerce (r.ty, eq_ty))
        ; constr (Eq (ret_ty, TyBool))
        ]
      | Lt | Gt | Leq | Geq ->
        let fresh_ty = fresh_tyvar () in
        [ constr (Broadcast (l.ty, r.ty, fresh_ty))
        ; constr (HasClass (Comparable, fresh_ty))
        ; constr (Eq (ret_ty, TyBool))
        ]
      | And | Or ->
        [ constr (Eq (l.ty, TyBool))
        ; constr (Eq (r.ty, TyBool))
        ; constr (Eq (ret_ty, TyBool))
        ]
    in
    let composed = compose_sub sub_r sub_l in
    ( { desc = Bop (op, l, r); ty = ret_ty; loc }
    , op_constrs @ constrs_l @ constrs_r
    , composed )
  | Index (t, i) ->
    let t, constrs_t, sub_t = gen_term env t in
    let ret_ty = fresh_tyvar () in
    ( { desc = Index (t, i); ty = ret_ty; loc }
    , constr (IndexAccess (t.ty, i, ret_ty)) :: constrs_t
    , sub_t )
  | Sample (sampler, coord) ->
    let s_ty =
      match Map.find env.ctx sampler with
      | Some (_, _, ty) -> ty
      | None -> raise "var not found in type map" ~loc ~d:[%message (sampler : string)]
    in
    let coord, constrs_coord, sub = gen_term env coord in
    let coord_ty = TyVec (2, TyFloat) in
    let constrs =
      constr (Eq (s_ty, TySampler))
      :: constr (Coerce (coord.ty, coord_ty))
      :: constrs_coord
    in
    let coord = coerce_term coord.loc coord_ty coord in
    let s_term : term = { desc = Var sampler; ty = TySampler; loc } in
    ( { desc = Builtin (Glsl.Texture, [ s_term; coord ]); ty = TyVec (4, TyFloat); loc }
    , constrs
    , sub )
  | Builtin (b, args) -> gen_builtin env loc b args
  | Vec (n, args) ->
    let elem_ty = fresh_tyvar () in
    let sub_args, results =
      List.fold_map args ~init:[] ~f:(fun acc_sub arg ->
        let arg, constrs, sub = gen_term env arg in
        let coerced = coerce_term arg.loc elem_ty arg in
        let all_constrs = constr (Coerce (arg.ty, elem_ty)) :: constrs in
        compose_sub sub acc_sub, (coerced, all_constrs))
    in
    let args, constrs_list = List.unzip results in
    let constrs_args = List.concat constrs_list in
    if List.length args = n
    then { desc = Vec (n, args); ty = TyVec (n, elem_ty); loc }, constrs_args, sub_args
    else raise "vec size mismatch" ~loc ~d:[%message (n : int)]
  | Record fields -> gen_record env loc fields
  | Field (t, f) ->
    let t, constrs_t, sub_t = gen_term env t in
    let ret_ty = fresh_tyvar () in
    ( { desc = Field (t, f); ty = ret_ty; loc }
    , constr (FieldAccess (t.ty, f, ret_ty)) :: constrs_t
    , sub_t )
  | Variant (ctor, args) -> gen_variant env loc ctor args
  | Match (scrutinee, cases) -> gen_match env loc scrutinee cases
  | Tuple ts ->
    let sub_args, results =
      List.fold_map ts ~init:[] ~f:(fun acc t ->
        let t, constrs, sub = gen_term env t in
        compose_sub sub acc, (t, constrs))
    in
    let args, constrs_list = List.unzip results in
    let ty = TyTuple (List.map args ~f:(fun a -> a.ty)) in
    { desc = Tuple args; ty; loc }, List.concat constrs_list, sub_args

and gen_builtin
      (env : env)
      (loc : Lexer.loc)
      (b : Glsl.builtin)
      (args : Desugar.term list)
  : term * constr list * substitution
  =
  let constr desc = { desc; loc } in
  let sub_args, results =
    List.fold_map args ~init:[] ~f:(fun acc_sub arg ->
      let arg, constrs, sub = gen_term env arg in
      compose_sub sub acc_sub, (arg, constrs))
  in
  let args, constrs_list = List.unzip results in
  let constrs_args = List.concat constrs_list in
  let ty = fresh_tyvar () in
  let arg_tys = List.map args ~f:(fun a -> a.ty) in
  let builtin_constrs =
    match b, arg_tys with
    | Float, [ t ] -> [ constr (HasClass (Comparable, t)); constr (Eq (ty, TyFloat)) ]
    | ( ( Sin | Cos | Tan | Asin | Acos | Atan | Exp | Log |
          Exp2 | Log2 | Sqrt | Abs | Sign | Floor | Ceil )
          [@ocamlformat "disable"]
      , [ t ] ) ->
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t, TyFloat, bt))
      ; constr (HasClass (GenType, bt))
      ; constr (Eq (ty, bt))
      ]
    | (Min | Max | Pow), [ t; t' ] ->
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t, t', bt))
      ; constr (Broadcast (bt, TyFloat, ty))
      ; constr (HasClass (GenType, ty))
      ]
    | Clamp, [ t; t'; t'' ] ->
      let tmp = fresh_tyvar () in
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t', t'', tmp))
      ; constr (Broadcast (t, tmp, bt))
      ; constr (Broadcast (bt, TyFloat, ty))
      ; constr (HasClass (GenType, ty))
      ]
    | Mix, [ t; t'; t'' ] ->
      let tmp = fresh_tyvar () in
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t, t', tmp))
      ; constr (Broadcast (tmp, t'', bt))
      ; constr (Broadcast (bt, TyFloat, ty))
      ; constr (HasClass (GenType, ty))
      ]
    | Length, [ t ] ->
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t, TyFloat, bt))
      ; constr (HasClass (GenType, bt))
      ; constr (Eq (ty, TyFloat))
      ]
    | (Distance | Dot), [ t; t' ] ->
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t, t', bt))
      ; constr (HasClass (GenType, bt))
      ; constr (Eq (ty, TyFloat))
      ]
    | Cross, [ t; t' ] ->
      [ constr (Eq (t, TyVec (3, TyFloat)))
      ; constr (Eq (t', TyVec (3, TyFloat)))
      ; constr (Eq (ty, TyVec (3, TyFloat)))
      ]
    | Normalize, [ t ] | Fract, [ t ] ->
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t, TyFloat, bt))
      ; constr (HasClass (GenType, bt))
      ; constr (Eq (ty, bt))
      ]
    | Step, [ t; t' ] ->
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t, t', bt))
      ; constr (Broadcast (bt, TyFloat, ty))
      ; constr (HasClass (GenType, ty))
      ]
    | Reflect, [ t; t' ] ->
      [ constr (HasClass (GenType, t)); constr (Eq (t, t')); constr (Eq (ty, t)) ]
    | Smoothstep, [ t; t'; t'' ] ->
      let tmp = fresh_tyvar () in
      let bt = fresh_tyvar () in
      [ constr (Broadcast (t, t', tmp))
      ; constr (Broadcast (tmp, t'', bt))
      ; constr (Broadcast (bt, TyFloat, ty))
      ; constr (HasClass (GenType, ty))
      ]
    | _ -> raise "invalid builtin arguments" ~loc ~d:[%message (b : Glsl.builtin)]
  in
  { desc = Builtin (b, args); ty; loc }, builtin_constrs @ constrs_args, sub_args

and gen_record (env : env) (loc : Lexer.loc) (fields : (string * Desugar.term) list)
  : term * constr list * substitution
  =
  let provided_fields = String.Set.of_list (List.map fields ~f:fst) in
  let candidates =
    Map.filter env.structs ~f:(fun (_, struct_fields) ->
      struct_fields |> List.map ~f:fst |> String.Set.of_list |> Set.equal provided_fields)
  in
  match Map.to_alist candidates with
  | [] ->
    raise
      "record does not match any known struct"
      ~loc
      ~d:[%message (provided_fields : String.Set.t)]
  | _ :: _ :: _ ->
    raise
      "record is ambiguous, matches multiple structs"
      ~loc
      ~d:[%message (provided_fields : String.Set.t)]
  | [ (struct_name, (params, struct_fields)) ] ->
    let sub = List.map params ~f:(fun p -> p, fresh_tyvar ()) in
    let inst_fields = List.map struct_fields ~f:(fun (n, ty) -> n, subst_ty sub ty) in
    let sub_rec, results =
      List.fold_map inst_fields ~init:[] ~f:(fun acc_sub (name, ty) ->
        let arg_node = List.Assoc.find_exn fields ~equal:String.equal name in
        let arg, constrs, sub = gen_term env arg_node in
        let arg, field_constrs = coerce_arg_to_ty loc arg ty in
        compose_sub sub acc_sub, (arg, field_constrs @ constrs))
    in
    let args, constrs_list = List.unzip results in
    ( { desc = Record args; ty = TyRecord (struct_name, inst_fields); loc }
    , List.concat constrs_list
    , sub_rec )

and gen_variant (env : env) (loc : Lexer.loc) (ctor : string) (args : Desugar.term list)
  : term * constr list * substitution
  =
  let variant_name, params, all_ctors =
    let found =
      Map.fold env.variants ~init:[] ~f:(fun ~key ~data:(params, ctors) acc ->
        if List.exists ctors ~f:(fun (c, _) -> String.equal c ctor)
        then (key, params, ctors) :: acc
        else acc)
    in
    match found with
    | [ x ] -> x
    | [] -> raise "unknown constructor" ~loc ~d:[%message (ctor : string)]
    | _ -> raise "ambiguous constructor" ~loc ~d:[%message (ctor : string)]
  in
  let param_sub = List.map params ~f:(fun p -> p, fresh_tyvar ()) in
  let inst_ctors =
    List.map all_ctors ~f:(fun (n, ts) -> n, List.map ts ~f:(subst_ty param_sub))
  in
  let expected_arg_tys =
    List.Assoc.find inst_ctors ~equal:String.equal ctor
    |> of_option
         "(unreachable) ctor not in instantiated ctors"
         ~loc
         ~d:[%message (ctor : string)]
    |> ok_exn
  in
  let args, constrs_args, sub_var =
    match List.zip args expected_arg_tys with
    | Unequal_lengths ->
      raise "wrong number of args to constructor" ~loc ~d:[%message (ctor : string)]
    | Ok arg_pairs ->
      List.fold
        arg_pairs
        ~init:([], [], [])
        ~f:(fun (acc_args, acc_constrs, acc_sub) (arg, expected_ty) ->
          let arg, constrs, sub = gen_term env arg in
          let arg, arg_constrs = coerce_arg_to_ty loc arg expected_ty in
          arg :: acc_args, arg_constrs @ constrs @ acc_constrs, compose_sub sub acc_sub)
  in
  ( { desc = Variant (ctor, List.rev args)
    ; ty = TyVariant (variant_name, inst_ctors)
    ; loc
    }
  , constrs_args
  , sub_var )

and gen_match
      (env : env)
      (loc : Lexer.loc)
      (scrutinee_stlc : Desugar.term)
      (cases : (Frontend.pat * Desugar.term) list)
  : term * constr list * substitution
  =
  let constr desc = { desc; loc } in
  let scrutinee, constrs_s, sub_s = gen_term env scrutinee_stlc in
  let ret_ty = fresh_tyvar () in
  let cases, constrs_cases, sub_cases =
    List.fold
      cases
      ~init:([], [], [])
      ~f:(fun (acc_cases, acc_constrs, acc_sub) (pat, body) ->
        let ctx, pat_constrs = check_pat env loc scrutinee.ty pat in
        let body, constrs_body, body_sub = gen_term { env with ctx } body in
        let body_wrapped = coerce_term body.loc ret_ty body in
        ( (pat, body_wrapped) :: acc_cases
        , pat_constrs @ (constr (Coerce (body.ty, ret_ty)) :: constrs_body) @ acc_constrs
        , compose_sub body_sub acc_sub ))
  in
  let cases = List.rev cases in
  let pat_sub, _ = Constraint_solver.solve (constrs_s @ constrs_cases) in
  let () =
    (* NOTE: Maranget usefulness check time *)
    let scrutinee_ty = subst_ty pat_sub scrutinee.ty in
    let pats = List.map cases ~f:fst in
    Pattern_match.is_redundant ~scrutinee_ty pats
    |> Option.iter ~f:(fun id ->
      raise "redundant match arm" ~loc ~d:[%message (id : int)]);
    Pattern_match.is_exhaustive ~scrutinee_ty pats
    |> Option.iter ~f:(fun witness ->
      raise "non-exhaustive match" ~loc ~d:[%message "missing" (witness : Frontend.pat)])
  in
  ( { desc = Match (scrutinee, cases); ty = ret_ty; loc }
  , constrs_s @ constrs_cases
  , compose_sub sub_cases sub_s )
;;

let enforce_main_type _env bind ty loc v =
  if not (String.equal v "main")
  then bind, ty
  else (
    let expected = TyArrow (TyVec (2, TyFloat), TyVec (4, TyFloat)) in
    match
      try Some (Constraint_solver.solve [ { desc = Coerce (ty, expected); loc } ]) with
      | Compiler_error.Compile_error _ -> None
    with
    | Some (sub, []) ->
      let bind = subst_term sub bind in
      let bind = coerce_term bind.loc expected bind in
      bind, bind.ty
    | _ -> raise "main must have type vec2 -> vec4" ~loc ~d:[%message (ty : ty)])
;;

let typecheck_impl (Program terms : Desugar.t) : t =
  let _, tops =
    List.fold
      terms
      ~init:
        ( { aliases = String.Map.empty
          ; structs = String.Map.empty
          ; variants = String.Map.empty
          ; ctx = String.Map.empty
          }
        , [] )
      ~f:(fun (env, acc) top ->
        match top.desc with
        | Define (recur, v, return_ty, constrs, bind) ->
          let bind, ty, env, scheme_constrs, remaining, _ =
            infer_binding env top.loc bind recur v return_ty constrs
          in
          if not (List.is_empty remaining)
          then
            raise
              "unresolved top-level constraints"
              ~loc:top.loc
              ~d:[%message (remaining : constr list)]
          else (
            let bind, ty = enforce_main_type env bind ty top.loc v in
            let top =
              { desc = Define (recur, v, bind); ty; loc = top.loc; scheme_constrs }
            in
            env, top :: acc)
        | Extern (ty, v) ->
          let ty = resolve_stlc_ty ~loc:top.loc env ty in
          let env = { env with ctx = Map.set env.ctx ~key:v ~data:([], [], ty) } in
          let top = { desc = Extern v; ty; loc = top.loc; scheme_constrs = [] } in
          env, top :: acc
        | TypeDef (name, params, RecordDecl fields) ->
          let fields =
            List.map fields ~f:(fun (f, ty) -> f, resolve_stlc_ty ~loc:top.loc env ty)
          in
          let env =
            { env with structs = Map.set env.structs ~key:name ~data:(params, fields) }
          in
          let top =
            { desc = TypeDef (name, RecordDecl (params, fields))
            ; ty = TyRecord (name, fields)
            ; loc = top.loc
            ; scheme_constrs = []
            }
          in
          env, top :: acc
        | TypeDef (name, params, VariantDecl ctors) ->
          let ctors =
            List.map ctors ~f:(fun (c, tys) ->
              c, List.map tys ~f:(resolve_stlc_ty ~loc:top.loc env))
          in
          let env =
            { env with variants = Map.set env.variants ~key:name ~data:(params, ctors) }
          in
          let top =
            { desc = TypeDef (name, VariantDecl (params, ctors))
            ; ty = TyVariant (name, ctors)
            ; loc = top.loc
            ; scheme_constrs = []
            }
          in
          env, top :: acc
        | TypeDef (name, params, AliasDecl ty) ->
          let rec occurs_in (ty : Frontend.ty) =
            match ty with
            | TyFloat | TyInt | TyBool | TyVec _ | TyVar _ | TySampler -> false
            | TyName s -> String.equal s name
            | TyApp (s, args) -> String.equal s name || List.exists args ~f:occurs_in
            | TyArrow (l, r) -> occurs_in l || occurs_in r
            | TyTuple ts -> List.exists ts ~f:occurs_in
          in
          if occurs_in ty
          then
            raise "type alias cycle detected" ~loc:top.loc ~d:[%message (name : string)]
          else (
            let body = resolve_stlc_ty ~loc:top.loc env ty in
            let env =
              { env with aliases = Map.set env.aliases ~key:name ~data:(params, body) }
            in
            env, acc))
  in
  Program (List.rev tops)
;;

let typecheck t = try_with (fun () -> typecheck_impl t)
