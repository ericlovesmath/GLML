(** Typechecking is done with Hindley-Milner inference (Algorithm W), but extended
    with typeclasses and broadcasting specific for operator overloading in GLSL *)

open Core
open Sexplib.Sexp
open Stlc
open Or_error.Let_syntax

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int
  | TyMat of int * int
  | TyArrow of ty * ty
  | TyRecord of string
  | TyVariant of string
  | TyVar of string
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
  | TyVar v -> Atom ("'" ^ v)
;;

type type_decl =
  | RecordDecl of (string * ty) list
  | VariantDecl of (string * ty list) list
[@@deriving sexp_of]

(** Internal typeclasses grouping GLSL types by their supported operations
    Most of the typeclasses are borrowed directly from GLSL *)
type type_class =
  | GenType
  | GenBType
  | GenIType
  | MatType
  | Numeric
  | Comparable
  | Equatable
[@@deriving sexp_of]

(** Constraints emitted during type inference. *)
type constr_desc =
  | Eq of ty * ty (** Standard equality constraint *)
  | HasClass of type_class * ty (** Membership in a GLSL typeclass *)
  | Broadcast of ty * ty * ty (** Scalar-vector broadcasting (e.g. float + vec3) *)
  | MulBroadcast of ty * ty * ty (** Matrix multiplication rules *)
  | IndexAccess of ty * int * ty (** Vector/Matrix indexing *)
  | FieldAccess of ty * string * ty (** Field access on a record *)

type constr =
  { desc : constr_desc
  ; loc : Lexer.loc
  }

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty * term
  | App of term * term
  | Let of recur * string * constr list * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of string * term list
  | Field of term * string
  | Variant of string * string * term list
  | Match of term * (string * string list * term) list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let sexp_of_constr_desc = function
  | Eq (l, r) -> List [ sexp_of_ty l; Atom "~"; sexp_of_ty r ]
  | HasClass (cls, ty) -> List [ sexp_of_type_class cls; sexp_of_ty ty ]
  | Broadcast (l, r, ret) ->
    List [ Atom "Broadcast"; sexp_of_ty l; sexp_of_ty r; sexp_of_ty ret ]
  | MulBroadcast (l, r, ret) ->
    List [ Atom "MulBroadcast"; sexp_of_ty l; sexp_of_ty r; sexp_of_ty ret ]
  | IndexAccess (t, i, ret) ->
    List [ Atom "IndexAccess"; sexp_of_ty t; Atom (Int.to_string i); sexp_of_ty ret ]
  | FieldAccess (t, f, ret) ->
    List [ Atom "FieldAccess"; sexp_of_ty t; Atom f; sexp_of_ty ret ]
;;

let sexp_of_constr (c : constr) = sexp_of_constr_desc c.desc

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
  | Mat (x, y, ts) ->
    List
      (Atom ("mat" ^ Int.to_string x ^ "x" ^ Int.to_string y)
       :: List.map ts ~f:sexp_of_term)
  | Lam (v, ty, body) ->
    List [ Atom "lambda"; List [ Atom v; sexp_of_ty ty ]; sexp_of_term body ]
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
  | Record (s, ts) -> List (Atom s :: List.map ts ~f:sexp_of_term)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ty_name, ctor, args) ->
    List (Atom "Variant" :: Atom ty_name :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (ctor, vars, body) =
      List [ Atom ctor; List (List.map vars ~f:(fun v -> Atom v)); sexp_of_term body ]
    in
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
  ; scheme_constrs : constr list
  }

let sexp_of_top t =
  List [ sexp_of_top_desc t.desc; Atom ":"; sexp_of_forall_ty t.scheme_constrs t.ty ]
;;

type t = Program of top list [@@deriving sexp_of]

(** Map from type variable names to their resolved types *)
type substitution = (string * ty) list [@@deriving sexp_of]

(** Represents polymorphic [forall 'vars. constrs => ty] *)
type type_scheme = string list * constr list * ty [@@deriving sexp_of]

(** Maps type variables to type schemes *)
type context = type_scheme String.Map.t

let fresh_tyvar () = TyVar (Utils.fresh "v")

let rec subst_ty (sub : substitution) (ty : ty) : ty =
  match ty with
  | TyVar v -> List.Assoc.find ~equal:String.equal sub v |> Option.value ~default:ty
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ | TyVariant _ -> ty
  | TyArrow (f, x) -> TyArrow (subst_ty sub f, subst_ty sub x)
;;

let subst_constraints (sub : substitution) (con : constr list) : constr list =
  List.map con ~f:(fun c ->
    let desc =
      match c.desc with
      | Eq (l, r) -> Eq (subst_ty sub l, subst_ty sub r)
      | HasClass (cls, ty) -> HasClass (cls, subst_ty sub ty)
      | Broadcast (l, r, ret) ->
        Broadcast (subst_ty sub l, subst_ty sub r, subst_ty sub ret)
      | MulBroadcast (l, r, ret) ->
        MulBroadcast (subst_ty sub l, subst_ty sub r, subst_ty sub ret)
      | IndexAccess (t, i, ret) -> IndexAccess (subst_ty sub t, i, subst_ty sub ret)
      | FieldAccess (t, f, ret) -> FieldAccess (subst_ty sub t, f, subst_ty sub ret)
    in
    { c with desc })
;;

let subst_context (sub : substitution) (ctx : context) : type_scheme String.Map.t =
  Map.map ctx ~f:(fun (vars, constrs, ty) ->
    let sub =
      List.filter sub ~f:(fun (v, _) -> not (List.mem vars v ~equal:String.equal))
    in
    vars, subst_constraints sub constrs, subst_ty sub ty)
;;

(** Apply substitution to term *)
let rec subst_term (sub : substitution) (t : term) : term =
  let subst = subst_term sub in
  let (desc : term_desc) =
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:subst)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:subst)
    | Lam (v, ty, body) -> Lam (v, subst_ty sub ty, subst body)
    | App (f, x) -> App (subst f, subst x)
    | Let (recur, v, constrs, bind, body) ->
      Let (recur, v, subst_constraints sub constrs, subst bind, subst body)
    | If (c, t, f) -> If (subst c, subst t, subst f)
    | Bop (op, l, r) -> Bop (op, subst l, subst r)
    | Index (t, i) -> Index (subst t, i)
    | Builtin (b, args) -> Builtin (b, List.map args ~f:subst)
    | Record (name, args) -> Record (name, List.map args ~f:subst)
    | Field (t, f) -> Field (subst t, f)
    | Variant (ty_name, ctor, args) -> Variant (ty_name, ctor, List.map args ~f:subst)
    | Match (scrutinee, cases) ->
      Match
        ( subst scrutinee
        , List.map cases ~f:(fun (ctor, vars, body) -> ctor, vars, subst body) )
  in
  { t with desc; ty = subst_ty sub t.ty }
;;

let rec ftv_of_ty = function
  | TyVar v -> String.Set.singleton v
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ | TyVariant _ ->
    String.Set.empty
  | TyArrow (t1, t2) -> Set.union (ftv_of_ty t1) (ftv_of_ty t2)
;;

let ftv_of_constraint (c : constr) : String.Set.t =
  match c.desc with
  | Eq (l, r) -> Set.union (ftv_of_ty l) (ftv_of_ty r)
  | HasClass (_, ty) -> ftv_of_ty ty
  | Broadcast (l, r, ret) | MulBroadcast (l, r, ret) ->
    String.Set.union_list [ ftv_of_ty l; ftv_of_ty r; ftv_of_ty ret ]
  | IndexAccess (t, _, ret) | FieldAccess (t, _, ret) ->
    Set.union (ftv_of_ty t) (ftv_of_ty ret)
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
  let generalizable = Set.diff (Set.union ftv_ty ftv_deferred_all) ftv_ctx in
  let scheme_constrs, remaining =
    List.partition_tf deferred ~f:(fun c ->
      Set.is_subset (ftv_of_constraint c) ~of_:generalizable)
  in
  (Set.to_list generalizable, scheme_constrs, ty), remaining
;;

(** Unify two types into a substitution *)
let rec unify (con : (Lexer.loc * ty * ty) list) : substitution Or_error.t =
  match con with
  | [] -> return []
  | (loc, TyVar v, ty) :: con | (loc, ty, TyVar v) :: con ->
    let rec occurs_in = function
      | TyVar v' -> String.equal v v'
      | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ | TyVariant _ -> false
      | TyArrow (ty, ty') -> occurs_in ty || occurs_in ty'
    in
    if equal_ty (TyVar v) ty
    then unify con
    else if occurs_in ty
    then
      error_s
        [%message
          "typecheck: recursive unification" (loc : Lexer.loc) (v : string) (ty : ty)]
    else (
      let%bind sub =
        unify
          (List.map con ~f:(fun (l, t, t') ->
             l, subst_ty [ v, ty ] t, subst_ty [ v, ty ] t'))
      in
      return ((v, subst_ty sub ty) :: sub))
  | (loc, TyArrow (f, x), TyArrow (f', x')) :: con ->
    unify ((loc, f, f') :: (loc, x, x') :: con)
  | (loc, ty, ty') :: con ->
    if equal_ty ty ty'
    then unify con
    else
      error_s [%message "typecheck: type mismatch" (loc : Lexer.loc) (ty : ty) (ty' : ty)]
;;

(** Validate if a concrete type belongs to a GLSL typeclass. *)
let check_class (cls : type_class) (ty : ty) : bool =
  match cls, ty with
  | GenType, (TyFloat | TyVec _)
  | GenBType, TyBool
  | GenIType, TyInt
  | MatType, TyMat _
  | Numeric, (TyFloat | TyInt | TyVec _ | TyMat _)
  | Comparable, (TyFloat | TyInt)
  | Equatable, (TyFloat | TyInt | TyBool | TyVec _ | TyMat _) -> true
  | _, _ -> false
;;

(** Resolve GLSL overloading constraints using concrete types. *)
let resolve_constraints structs (constrs : constr list)
  : (constr list * (Lexer.loc * ty * ty) list) Or_error.t
  =
  let rec aux deferred eqs (constrs : constr list) =
    match constrs with
    | [] -> return (List.rev deferred, List.rev eqs)
    | { desc = Eq (l, r); loc } :: rest -> aux deferred ((loc, l, r) :: eqs) rest
    | ({ desc = HasClass (cls, ty); loc } as c) :: rest ->
      (match ty with
       | TyVar _ -> aux (c :: deferred) eqs rest
       | _ ->
         if check_class cls ty
         then aux deferred eqs rest
         else
           error_s
             [%message
               "typecheck: class constraint failed"
                 (loc : Lexer.loc)
                 (cls : type_class)
                 (ty : ty)])
    | ({ desc = Broadcast (l, r, ret); loc } as c) :: rest ->
      (match l, r with
       | TyVar a, TyVar b when String.equal a b ->
         aux (c :: deferred) ((loc, ret, l) :: eqs) rest
       | TyFloat, TyVar _ -> aux (c :: deferred) ((loc, ret, r) :: eqs) rest
       | TyVar _, TyFloat -> aux (c :: deferred) ((loc, ret, l) :: eqs) rest
       | TyVar _, _ | _, TyVar _ -> aux (c :: deferred) eqs rest
       | TyFloat, TyFloat -> aux deferred ((loc, ret, TyFloat) :: eqs) rest
       | TyInt, TyInt -> aux deferred ((loc, ret, TyInt) :: eqs) rest
       | TyInt, TyFloat | TyFloat, TyInt -> aux deferred ((loc, ret, TyFloat) :: eqs) rest
       | TyVec n, TyVec n' when n = n' -> aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyFloat, TyVec n | TyVec n, TyFloat ->
         aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyMat (x, y), TyMat (w, z) when x = w && y = z ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | _ ->
         error_s
           [%message "typecheck: invalid broadcast" (loc : Lexer.loc) (l : ty) (r : ty)])
    | ({ desc = MulBroadcast (l, r, ret); loc } as c) :: rest ->
      (match l, r with
       | TyVar a, TyVar b when String.equal a b ->
         aux (c :: deferred) ((loc, ret, l) :: eqs) rest
       | TyFloat, TyVar _ -> aux (c :: deferred) ((loc, ret, r) :: eqs) rest
       | TyVar _, TyFloat -> aux (c :: deferred) ((loc, ret, l) :: eqs) rest
       | TyVar _, _ | _, TyVar _ -> aux (c :: deferred) eqs rest
       | TyMat (x, y), TyMat (w, z) when x = w && y = z ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | TyMat (x, y), TyFloat | TyFloat, TyMat (x, y) ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | TyMat (x, y), TyVec n when y = n ->
         aux deferred ((loc, ret, TyVec x) :: eqs) rest
       | TyVec n, TyMat (x, y) when n = x ->
         aux deferred ((loc, ret, TyVec y) :: eqs) rest
       | TyFloat, TyFloat -> aux deferred ((loc, ret, TyFloat) :: eqs) rest
       | TyInt, TyInt -> aux deferred ((loc, ret, TyInt) :: eqs) rest
       | TyInt, TyFloat | TyFloat, TyInt -> aux deferred ((loc, ret, TyFloat) :: eqs) rest
       | TyVec n, TyVec n' when n = n' -> aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyFloat, TyVec n | TyVec n, TyFloat ->
         aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | _ ->
         error_s
           [%message
             "typecheck: invalid mul/div broadcast" (loc : Lexer.loc) (l : ty) (r : ty)])
    | ({ desc = IndexAccess (t, i, ret); loc } as c) :: rest ->
      (match t with
       | TyVec n ->
         if 0 <= i && i < n
         then aux deferred ((loc, ret, TyFloat) :: eqs) rest
         else
           error_s
             [%message "vec index out of bounds" (loc : Lexer.loc) (n : int) (i : int)]
       | TyMat (x, y) ->
         if 0 <= i && i < x
         then aux deferred ((loc, ret, TyVec y) :: eqs) rest
         else
           error_s
             [%message "mat index out of bounds" (loc : Lexer.loc) (x : int) (i : int)]
       | TyVar _ -> aux (c :: deferred) eqs rest
       | ty -> error_s [%message "expected vec or mat" (loc : Lexer.loc) (ty : ty)])
    | ({ desc = FieldAccess (ty, f, ret); loc } as c) :: rest ->
      (match ty with
       | TyVar _ -> aux (c :: deferred) eqs rest
       | TyRecord struct_name ->
         (match Map.find structs struct_name with
          | None -> error_s [%message "unknown struct" (loc : Lexer.loc) struct_name]
          | Some fields ->
            (match List.Assoc.find fields ~equal:String.equal f with
             | None ->
               error_s
                 [%message "field not found in struct" (loc : Lexer.loc) f struct_name]
             | Some field_ty -> aux deferred ((loc, ret, field_ty) :: eqs) rest))
       | ty ->
         error_s [%message "field access on non-record type" (loc : Lexer.loc) (ty : ty)])
  in
  aux [] [] constrs
;;

(** Solve a set of constraints to produce a substitution and deferred constraints. *)
let solve structs (constrs : constr list) : (substitution * constr list) Or_error.t =
  let rec go sub constrs =
    let%bind deferred, eqs = resolve_constraints structs constrs in
    if List.is_empty eqs
    then return (sub, deferred)
    else (
      let%bind new_sub = unify eqs in
      if List.is_empty new_sub
      then return (sub, deferred)
      else (
        let sub = List.map sub ~f:(fun (v, t) -> v, subst_ty new_sub t) @ new_sub in
        let deferred = subst_constraints new_sub deferred in
        go sub deferred))
  in
  go [] constrs
;;

(** Solve scheme constraints given an initial substitution from monomorphization.
    Applies the sub to constraints, solves, and combines substitutions. *)
let solve_scheme_constrs (constrs : constr list) (sub : substitution)
  : substitution Or_error.t
  =
  if List.is_empty constrs
  then return sub
  else (
    let constrs = subst_constraints sub constrs in
    let%bind sub', _ = solve String.Map.empty constrs in
    return (List.map sub ~f:(fun (v, t) -> v, subst_ty sub' t) @ sub'))
;;

(** Value restriction check for generalization. *)
let rec is_value (t : Stlc.term) : bool =
  match t.desc with
  | Float _ | Int _ | Bool _ | Var _ | Lam _ -> true
  | Vec (_, ts) -> List.for_all ts ~f:is_value
  | Mat (_, _, ts) -> List.for_all ts ~f:is_value
  | Record fields -> List.for_all fields ~f:(fun (_, t) -> is_value t)
  | Variant (_, args) -> List.for_all args ~f:is_value
  | Field (t, _) | Index (t, _) -> is_value t
  | App _ | Let _ | If _ | Bop _ | Builtin _ | Match _ -> false
;;

let rec resolve_stlc_ty (variants : 'a String.Map.t) (t : Stlc.ty) : ty =
  match t with
  | TyName s when Map.mem variants s -> TyVariant s
  | TyName s -> TyRecord s
  | TyArrow (l, r) -> TyArrow (resolve_stlc_ty variants l, resolve_stlc_ty variants r)
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVec n -> TyVec n
  | TyMat (n, m) -> TyMat (n, m)
  | TyVar v -> TyVar v
;;

(** Build a function type from lambda param annotations and a return type. *)
let rec build_function_type variants (term : Stlc.term) (ret_ty : ty) : ty =
  match term.desc with
  | Lam (_, Some param_ty, body) ->
    TyArrow (resolve_stlc_ty variants param_ty, build_function_type variants body ret_ty)
  | Lam (_, None, body) ->
    TyArrow (fresh_tyvar (), build_function_type variants body ret_ty)
  | _ -> ret_ty
;;

(* TODO; There has to be a way that doesn't involving passing 4 million params *)
(** Infer the type of a binding (used between top-level Define and inner Let).
    Returns [substituted term * resolved type * new context * scheme constraints * remaining constraints] *)
let rec infer_binding
          (structs : substitution String.Map.t)
          (variants : (string * ty list) list String.Map.t)
          (ctx : context)
          (loc : Lexer.loc)
          (bind_stlc : Stlc.term)
          (recur : recur)
          (v : string)
          (return_ty : Stlc.ty option)
  : (term * ty * context * constr list * constr list) Or_error.t
  =
  let return_ty = Option.map return_ty ~f:(resolve_stlc_ty variants) in
  let ty_v_opt =
    match recur with
    | Nonrec -> None
    | Rec _ ->
      Some
        (match return_ty with
         | None -> fresh_tyvar ()
         | Some ret_ty -> build_function_type variants bind_stlc ret_ty)
  in
  let ctx_gen =
    match ty_v_opt with
    | None -> ctx
    | Some ty_v -> Map.set ctx ~key:v ~data:([], [], ty_v)
  in
  let%bind bind, constrs_bind = gen_term structs variants ctx_gen bind_stlc in
  let constr desc = { desc; loc } in
  let constrs =
    let rec_constrs =
      match ty_v_opt with
      | None -> constrs_bind
      | Some ty_v -> constr (Eq (ty_v, bind.ty)) :: constrs_bind
    in
    match recur, return_ty with
    | Nonrec, Some ret_ty ->
      constr (Eq (build_function_type variants bind_stlc ret_ty, bind.ty)) :: rec_constrs
    | _ -> rec_constrs
  in
  let%bind sub_bind, deferred = solve structs constrs in
  let ty_bind = subst_ty sub_bind bind.ty in
  let bind = subst_term sub_bind bind in
  let ctx = subst_context sub_bind ctx in
  let deferred = subst_constraints sub_bind deferred in
  let scheme, remaining =
    if is_value bind_stlc
    then generalize ctx deferred ty_bind
    else ([], [], ty_bind), deferred
  in
  let _, scheme_constrs, _ = scheme in
  let ctx = Map.set ctx ~key:v ~data:scheme in
  Ok (bind, ty_bind, ctx, scheme_constrs, remaining)

(** Generate typed term and constraints from STLC term. *)
and gen_term structs variants ctx (t : Stlc.term) : (term * constr list) Or_error.t =
  let loc = t.loc in
  let make desc ty constrs = Ok (({ desc; ty; loc } : term), constrs) in
  let constr desc = { desc; loc } in
  match t.desc with
  | Float f -> make (Float f) TyFloat []
  | Int i -> make (Int i) TyInt []
  | Bool b -> make (Bool b) TyBool []
  | Var v ->
    let%bind vs, scheme_constrs, ty_scheme =
      match Map.find ctx v with
      | Some s -> Ok s
      | None ->
        error_s [%message "var not found in type map" (loc : Lexer.loc) (v : string)]
    in
    let sub = List.map vs ~f:(fun v -> v, fresh_tyvar ()) in
    make (Var v) (subst_ty sub ty_scheme) (subst_constraints sub scheme_constrs)
  | Lam (v, ty_ann, body) ->
    let ty_v =
      match ty_ann with
      | Some t -> resolve_stlc_ty variants t
      | None -> fresh_tyvar ()
    in
    let ctx = Map.set ctx ~key:v ~data:([], [], ty_v) in
    let%bind body, constrs = gen_term structs variants ctx body in
    make (Lam (v, ty_v, body)) (TyArrow (ty_v, body.ty)) constrs
  | App (f, x) ->
    let%bind f, constrs_f = gen_term structs variants ctx f in
    let%bind x, constrs_x = gen_term structs variants ctx x in
    let ret_ty = fresh_tyvar () in
    let constrs = constr (Eq (f.ty, TyArrow (x.ty, ret_ty))) :: (constrs_f @ constrs_x) in
    make (App (f, x)) ret_ty constrs
  | Let (Nonrec, v, return_ty, bind, body) ->
    let%bind bind, _, ctx, scheme_constrs, remaining =
      infer_binding structs variants ctx loc bind Nonrec v return_ty
    in
    let%bind body, constrs_body = gen_term structs variants ctx body in
    make (Let (Nonrec, v, scheme_constrs, bind, body)) body.ty (remaining @ constrs_body)
  | Let (Rec n, v, return_ty, bind, body) ->
    let%bind bind, _, ctx, scheme_constrs, remaining =
      infer_binding structs variants ctx loc bind (Rec n) v return_ty
    in
    let%bind body, constrs_body = gen_term structs variants ctx body in
    make (Let (Rec n, v, scheme_constrs, bind, body)) body.ty (remaining @ constrs_body)
  | If (c, t, e) ->
    let%bind c, constrs_c = gen_term structs variants ctx c in
    let%bind t, constrs_t = gen_term structs variants ctx t in
    let%bind e, constrs_e = gen_term structs variants ctx e in
    let constrs =
      constr (Eq (c.ty, TyBool))
      :: constr (Eq (t.ty, e.ty))
      :: (constrs_c @ constrs_t @ constrs_e)
    in
    make (If (c, t, e)) t.ty constrs
  | Bop (op, l, r) ->
    let%bind l, constrs_l = gen_term structs variants ctx l in
    let%bind r, constrs_r = gen_term structs variants ctx r in
    let ret_ty = fresh_tyvar () in
    let op_constrs =
      match op with
      | Add | Sub -> [ constr (Broadcast (l.ty, r.ty, ret_ty)) ]
      | Mod ->
        [ constr (HasClass (GenType, l.ty))
        ; constr (HasClass (GenType, r.ty))
        ; constr (Broadcast (l.ty, r.ty, ret_ty))
        ]
      | Mul | Div -> [ constr (MulBroadcast (l.ty, r.ty, ret_ty)) ]
      | Eq ->
        [ constr (HasClass (Equatable, l.ty))
        ; constr (Eq (l.ty, r.ty))
        ; constr (Eq (ret_ty, TyBool))
        ]
      | Lt | Gt | Leq | Geq ->
        [ constr (HasClass (Comparable, l.ty))
        ; constr (Eq (l.ty, r.ty))
        ; constr (Eq (ret_ty, TyBool))
        ]
      | And | Or ->
        [ constr (Eq (l.ty, TyBool))
        ; constr (Eq (r.ty, TyBool))
        ; constr (Eq (ret_ty, TyBool))
        ]
    in
    make (Bop (op, l, r)) ret_ty (op_constrs @ constrs_l @ constrs_r)
  | Index (t, i) ->
    let%bind t, constrs_t = gen_term structs variants ctx t in
    let ret_ty = fresh_tyvar () in
    make (Index (t, i)) ret_ty (constr (IndexAccess (t.ty, i, ret_ty)) :: constrs_t)
  | Builtin (b, args) ->
    let%bind args, constrs_args =
      List.fold_result args ~init:([], []) ~f:(fun (acc_args, acc_constrs) arg ->
        let%bind arg', constrs = gen_term structs variants ctx arg in
        return (arg' :: acc_args, constrs @ acc_constrs))
    in
    let args = List.rev args in
    let ty = fresh_tyvar () in
    let arg_tys = List.map args ~f:(fun a -> a.ty) in
    let%bind builtin_constrs =
      match b, arg_tys with
      | ( ( Sin
          | Cos
          | Tan
          | Asin
          | Acos
          | Atan
          | Exp
          | Log
          | Exp2
          | Log2
          | Sqrt
          | Abs
          | Sign
          | Floor
          | Ceil )
        , [ t ] ) -> Ok [ constr (HasClass (GenType, t)); constr (Eq (ty, t)) ]
      | (Min | Max | Pow), [ t; t' ] ->
        Ok [ constr (HasClass (GenType, ty)); constr (Broadcast (t, t', ty)) ]
      | Clamp, [ t; t'; t'' ] ->
        let tmp = fresh_tyvar () in
        Ok
          [ constr (HasClass (GenType, ty))
          ; constr (Broadcast (t', t'', tmp))
          ; constr (Broadcast (t, tmp, ty))
          ]
      | Mix, [ t; t'; t'' ] ->
        let tmp = fresh_tyvar () in
        Ok
          [ constr (HasClass (GenType, ty))
          ; constr (Broadcast (t, t', tmp))
          ; constr (Broadcast (tmp, t'', ty))
          ]
      | Length, [ t ] -> Ok [ constr (HasClass (GenType, t)); constr (Eq (ty, TyFloat)) ]
      | (Distance | Dot), [ t; t' ] ->
        Ok
          [ constr (HasClass (GenType, t))
          ; constr (Eq (t, t'))
          ; constr (Eq (ty, TyFloat))
          ]
      | Cross, [ t; t' ] ->
        Ok
          [ constr (Eq (t, TyVec 3))
          ; constr (Eq (t', TyVec 3))
          ; constr (Eq (ty, TyVec 3))
          ]
      | Normalize, [ t ] -> Ok [ constr (HasClass (GenType, t)); constr (Eq (ty, t)) ]
      | Fract, [ t ] -> Ok [ constr (HasClass (GenType, t)); constr (Eq (ty, t)) ]
      | Step, [ t; t' ] ->
        Ok [ constr (HasClass (GenType, ty)); constr (Broadcast (t, t', ty)) ]
      | Reflect, [ t; t' ] ->
        Ok [ constr (HasClass (GenType, t)); constr (Eq (t, t')); constr (Eq (ty, t)) ]
      | Smoothstep, [ t; t'; t'' ] ->
        let tmp = fresh_tyvar () in
        Ok
          [ constr (HasClass (GenType, ty))
          ; constr (Broadcast (t, t', tmp))
          ; constr (Broadcast (tmp, t'', ty))
          ]
      | _ ->
        error_s
          [%message "invalid builtin arguments" (loc : Lexer.loc) (b : Glsl.builtin)]
    in
    make (Builtin (b, args)) ty (builtin_constrs @ constrs_args)
  | Vec (n, args) ->
    let%bind args, constrs_args =
      List.fold_result args ~init:([], []) ~f:(fun (acc_args, acc_constrs) arg ->
        let%bind arg, constrs = gen_term structs variants ctx arg in
        return
          ( arg :: acc_args
          , (constr (HasClass (Comparable, arg.ty)) :: constrs) @ acc_constrs ))
    in
    let args = List.rev args in
    if List.length args = n
    then make (Vec (n, args)) (TyVec n) constrs_args
    else error_s [%message "vec size mismatch" (loc : Lexer.loc) (n : int)]
  | Mat (n, m, args) ->
    let%bind args, constrs_args =
      List.fold_result args ~init:([], []) ~f:(fun (acc_args, acc_constrs) arg ->
        let%bind arg, constrs = gen_term structs variants ctx arg in
        return
          ( arg :: acc_args
          , (constr (HasClass (Comparable, arg.ty)) :: constrs) @ acc_constrs ))
    in
    let args = List.rev args in
    if List.length args = n * m
    then make (Mat (n, m, args)) (TyMat (n, m)) constrs_args
    else error_s [%message "mat size mismatch" (loc : Lexer.loc) (n : int) (m : int)]
  | Record fields ->
    let provided_fields = String.Set.of_list (List.map fields ~f:fst) in
    let candidates =
      Map.filter structs ~f:(fun struct_fields ->
        struct_fields
        |> List.map ~f:fst
        |> String.Set.of_list
        |> Set.equal provided_fields)
    in
    let error_record msg =
      error_s [%message msg (loc : Lexer.loc) (provided_fields : String.Set.t)]
    in
    (match Map.to_alist candidates with
     | [] -> error_record "record does not match any known struct"
     | _ :: _ :: _ -> error_record "record is ambiguous, matches multiple structs"
     | [ (struct_name, struct_fields) ] ->
       let%bind args, constrs_args =
         List.fold_result
           struct_fields
           ~init:([], [])
           ~f:(fun (acc, acc_constrs) (name, ty) ->
             match List.Assoc.find fields ~equal:String.equal name with
             | Some arg ->
               let%bind arg, constrs = gen_term structs variants ctx arg in
               return (arg :: acc, (constr (Eq (arg.ty, ty)) :: constrs) @ acc_constrs)
             | None ->
               error_s [%message "(unreachable) missing field" (loc : Lexer.loc) name])
       in
       make (Record (struct_name, List.rev args)) (TyRecord struct_name) constrs_args)
  | Field (t, f) ->
    let%bind t, constrs_t = gen_term structs variants ctx t in
    let ret_ty = fresh_tyvar () in
    make (Field (t, f)) ret_ty (constr (FieldAccess (t.ty, f, ret_ty)) :: constrs_t)
  | Variant (ctor, args) ->
    let%bind variant_name, expected_arg_tys =
      let found =
        Map.fold variants ~init:[] ~f:(fun ~key:vname ~data:ctors acc ->
          match List.find ctors ~f:(fun (c, _) -> String.equal c ctor) with
          | Some (_, arg_tys) -> (vname, arg_tys) :: acc
          | None -> acc)
      in
      match found with
      | [ x ] -> Ok x
      | [] ->
        error_s
          [%message "typecheck: unknown constructor" (loc : Lexer.loc) (ctor : string)]
      | _ ->
        error_s
          [%message "typecheck: ambiguous constructor" (loc : Lexer.loc) (ctor : string)]
    in
    if List.length args <> List.length expected_arg_tys
    then
      error_s
        [%message
          "typecheck: wrong number of args to constructor"
            (loc : Lexer.loc)
            (ctor : string)]
    else (
      let%bind args, constrs_args =
        List.fold2_exn
          args
          expected_arg_tys
          ~init:(Ok ([], []))
          ~f:(fun acc arg expected_ty ->
            let%bind acc_args, acc_constrs = acc in
            let%bind arg, constrs = gen_term structs variants ctx arg in
            return
              ( arg :: acc_args
              , (constr (Eq (arg.ty, expected_ty)) :: constrs) @ acc_constrs ))
      in
      make
        (Variant (variant_name, ctor, List.rev args))
        (TyVariant variant_name)
        constrs_args)
  | Match (scrutinee, cases) ->
    let%bind scrutinee, constrs_s = gen_term structs variants ctx scrutinee in
    let ret_ty = fresh_tyvar () in
    let%bind variant_name, variant_ctors =
      let find_from_ty ty =
        match ty with
        | TyVariant name ->
          (match Map.find variants name with
           | Some ctors -> Some (name, ctors)
           | None -> None)
        | _ -> None
      in
      match find_from_ty scrutinee.ty with
      | Some x -> Ok x
      | None ->
        let case_ctors = List.map cases ~f:(fun (ctor, _, _) -> ctor) in
        let candidates =
          Map.filter variants ~f:(fun ctors ->
            let ctor_names = List.map ctors ~f:fst in
            List.for_all case_ctors ~f:(fun c ->
              List.mem ctor_names c ~equal:String.equal))
        in
        (match Map.to_alist candidates with
         | [ (name, ctors) ] -> Ok (name, ctors)
         | [] ->
           error_s
             [%message
               "typecheck: match cases don't match any variant type" (loc : Lexer.loc)]
         | _ ->
           error_s [%message "typecheck: ambiguous match variant type" (loc : Lexer.loc)])
    in
    (* [Exhaustive Checking] *)
    let case_ctors = List.map cases ~f:(fun (c, _, _) -> c) |> String.Set.of_list in
    let all_ctors = List.map variant_ctors ~f:fst |> String.Set.of_list in
    let missing = Set.diff all_ctors case_ctors in
    let%bind () =
      if Set.is_empty missing
      then Ok ()
      else
        error_s
          [%message
            "typecheck: non-exhaustive match" (loc : Lexer.loc) (missing : String.Set.t)]
    in
    (* Type-check each case *)
    let%bind cases, constrs_cases =
      List.fold_result
        cases
        ~init:([], [])
        ~f:(fun (acc_cases, acc_constrs) (ctor, vars, body) ->
          let%bind _, expected_arg_tys =
            match List.find variant_ctors ~f:(fun (c, _) -> String.equal c ctor) with
            | Some x -> Ok x
            | None ->
              error_s
                [%message
                  "typecheck: unknown constructor in match"
                    (loc : Lexer.loc)
                    (ctor : string)]
          in
          if List.length vars <> List.length expected_arg_tys
          then
            error_s
              [%message
                "typecheck: wrong number of bindings in match case"
                  (loc : Lexer.loc)
                  (ctor : string)
                  ~expected:(List.length expected_arg_tys : int)
                  ~got:(List.length vars : int)]
          else (
            let ctx =
              List.fold2_exn vars expected_arg_tys ~init:ctx ~f:(fun ctx v ty ->
                Map.set ctx ~key:v ~data:([], [], ty))
            in
            let%bind body, constrs_body = gen_term structs variants ctx body in
            return
              ( (ctor, vars, body) :: acc_cases
              , (constr (Eq (body.ty, ret_ty)) :: constrs_body) @ acc_constrs )))
    in
    let scrutinee_constr = constr (Eq (scrutinee.ty, TyVariant variant_name)) in
    make
      (Match (scrutinee, List.rev cases))
      ret_ty
      (scrutinee_constr :: (constrs_s @ constrs_cases))
;;

let typecheck (Program terms : Stlc.t) : t Or_error.t =
  let%map _, _, _, tops =
    List.fold_result
      terms
      ~init:(String.Map.empty, String.Map.empty, String.Map.empty, [])
      ~f:(fun (ctx, structs, variants, acc) top ->
        match top.desc with
        | Define (Rec n, v, return_ty, bind) ->
          let%bind bind, ty, ctx, scheme_constrs, remaining =
            infer_binding structs variants ctx top.loc bind (Rec n) v return_ty
          in
          if not (List.is_empty remaining)
          then
            error_s
              [%message
                "typecheck: unresolved top-level constraints" (remaining : constr list)]
          else (
            let top =
              { desc = Define (Rec n, v, bind); ty; loc = top.loc; scheme_constrs }
            in
            Ok (ctx, structs, variants, top :: acc))
        | Define (Nonrec, v, return_ty, bind) ->
          let%bind bind, ty, ctx, scheme_constrs, remaining =
            infer_binding structs variants ctx top.loc bind Nonrec v return_ty
          in
          if not (List.is_empty remaining)
          then
            error_s
              [%message
                "typecheck: unresolved top-level constraints" (remaining : constr list)]
          else (
            let top =
              { desc = Define (Nonrec, v, bind); ty; loc = top.loc; scheme_constrs }
            in
            Ok (ctx, structs, variants, top :: acc))
        | Extern (ty, v) ->
          let ty = resolve_stlc_ty variants ty in
          let ctx = Map.set ctx ~key:v ~data:([], [], ty) in
          let top = { desc = Extern v; ty; loc = top.loc; scheme_constrs = [] } in
          Ok (ctx, structs, variants, top :: acc)
        | TypeDef (name, Stlc.RecordDecl fields) ->
          let fields =
            List.map fields ~f:(fun (f, ty) -> f, resolve_stlc_ty variants ty)
          in
          let structs = Map.set structs ~key:name ~data:fields in
          let top =
            { desc = TypeDef (name, RecordDecl fields)
            ; ty = TyRecord name
            ; loc = top.loc
            ; scheme_constrs = []
            }
          in
          Ok (ctx, structs, variants, top :: acc)
        | TypeDef (name, Stlc.VariantDecl ctors) ->
          let ctors =
            List.map ctors ~f:(fun (c, tys) ->
              c, List.map tys ~f:(resolve_stlc_ty variants))
          in
          let variants = Map.set variants ~key:name ~data:ctors in
          let top =
            { desc = TypeDef (name, VariantDecl ctors)
            ; ty = TyVariant name
            ; loc = top.loc
            ; scheme_constrs = []
            }
          in
          Ok (ctx, structs, variants, top :: acc))
  in
  Program (List.rev tops)
;;
