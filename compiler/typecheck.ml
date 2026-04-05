(** Typechecking is done with Hindley-Milner inference (Algorithm W), but extended
    with typeclasses and broadcasting specific for operator overloading in GLSL *)

open Core
open Sexplib.Sexp
open Stlc
open Compiler_error.Let_syntax

module Err = Compiler_error.Pass (struct
    let name = "typecheck"
  end)

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int
  | TyMat of int * int
  | TyArrow of ty * ty
  | TyRecord of string * ty list
  | TyVariant of string * ty list
  | TyVar of string
[@@deriving equal, compare]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec i -> List [ Atom "vec"; Atom (Int.to_string i) ]
  | TyMat (x, y) -> List [ Atom "mat"; Atom (Int.to_string x); Atom (Int.to_string y) ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
  | TyRecord (s, []) -> Atom s
  | TyRecord (s, args) -> List (Atom s :: List.map args ~f:sexp_of_ty)
  | TyVariant (s, []) -> Atom s
  | TyVariant (s, args) -> List (Atom s :: List.map args ~f:sexp_of_ty)
  | TyVar v -> Atom ("'" ^ v)
;;

type type_decl =
  | RecordDecl of string list * (string * ty) list
  | VariantDecl of string list * (string * ty list) list
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
  | Lam of string * term
  | App of term * term
  | Let of recur * string * constr list * term * term
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
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ -> ty
  | TyVariant (s, args) -> TyVariant (s, List.map args ~f:(subst_ty sub))
  | TyRecord (s, args) -> TyRecord (s, List.map args ~f:(subst_ty sub))
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
    | Lam (v, body) -> Lam (v, subst body)
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
      Match (subst scrutinee, List.map cases ~f:(fun (pat, body) -> pat, subst body))
  in
  { t with desc; ty = subst_ty sub t.ty }
;;

let rec ftv_of_ty = function
  | TyVar v -> String.Set.singleton v
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ -> String.Set.empty
  | TyVariant (_, args) | TyRecord (_, args) ->
    String.Set.union_list (List.map args ~f:ftv_of_ty)
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

(** Unify two types into a substitution *)
let rec unify (con : (Lexer.loc * ty * ty) list) : substitution Compiler_error.t =
  match con with
  | [] -> return []
  | (loc, TyVar v, ty) :: con | (loc, ty, TyVar v) :: con ->
    let rec occurs_in = function
      | TyVar v' -> String.equal v v'
      | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ -> false
      | TyVariant (_, args) | TyRecord (_, args) -> List.exists args ~f:occurs_in
      | TyArrow (ty, ty') -> occurs_in ty || occurs_in ty'
    in
    if equal_ty (TyVar v) ty
    then unify con
    else if occurs_in ty
    then Err.fail "recursive unification" ~loc ~d:[%message (v : string) (ty : ty)]
    else (
      let%bind sub =
        unify
          (List.map con ~f:(fun (l, t, t') ->
             l, subst_ty [ v, ty ] t, subst_ty [ v, ty ] t'))
      in
      return ((v, subst_ty sub ty) :: sub))
  | (loc, TyArrow (f, x), TyArrow (f', x')) :: con ->
    unify ((loc, f, f') :: (loc, x, x') :: con)
  | (loc, TyRecord (s, args), TyRecord (s', args')) :: con
    when String.equal s s' && List.length args = List.length args' ->
    unify (List.map2_exn args args' ~f:(fun a a' -> loc, a, a') @ con)
  | (loc, TyVariant (s, args), TyVariant (s', args')) :: con
    when String.equal s s' && List.length args = List.length args' ->
    unify (List.map2_exn args args' ~f:(fun a a' -> loc, a, a') @ con)
  | (loc, ty, ty') :: con ->
    if equal_ty ty ty'
    then unify con
    else Err.fail "type mismatch" ~loc ~d:[%message (ty : ty) (ty' : ty)]
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
  : (constr list * (Lexer.loc * ty * ty) list) Compiler_error.t
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
           Err.fail
             "class constraint failed"
             ~loc
             ~d:[%message (cls : type_class) (ty : ty)])
    | ({ desc = Broadcast (l, r, ret); loc } as c) :: rest ->
      (match l, r with
       | TyVar a, TyVar b when String.equal a b ->
         aux (c :: deferred) ((loc, ret, l) :: eqs) rest
       | TyVar _, _ | _, TyVar _ -> aux (c :: deferred) eqs rest
       | TyFloat, TyFloat -> aux deferred ((loc, ret, TyFloat) :: eqs) rest
       | TyInt, TyInt -> aux deferred ((loc, ret, TyInt) :: eqs) rest
       | TyInt, TyFloat | TyFloat, TyInt -> aux deferred ((loc, ret, TyFloat) :: eqs) rest
       | TyVec n, TyVec n' when n = n' -> aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyFloat, TyVec n | TyVec n, TyFloat ->
         aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyInt, TyVec n | TyVec n, TyInt -> aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyInt, TyMat (x, y) | TyMat (x, y), TyInt ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | TyMat (x, y), TyMat (w, z) when x = w && y = z ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | _ -> Err.fail "invalid broadcast" ~loc ~d:[%message (l : ty) (r : ty)])
    | ({ desc = MulBroadcast (l, r, ret); loc } as c) :: rest ->
      (match l, r with
       | TyVar a, TyVar b when String.equal a b ->
         aux (c :: deferred) ((loc, ret, l) :: eqs) rest
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
       | TyInt, TyVec n | TyVec n, TyInt -> aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyInt, TyMat (x, y) | TyMat (x, y), TyInt ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | _ -> Err.fail "invalid mul/div broadcast" ~loc ~d:[%message (l : ty) (r : ty)])
    | ({ desc = IndexAccess (t, i, ret); loc } as c) :: rest ->
      (match t with
       | TyVec n ->
         if 0 <= i && i < n
         then aux deferred ((loc, ret, TyFloat) :: eqs) rest
         else Err.fail "vec index out of bounds" ~loc ~d:[%message (n : int) (i : int)]
       | TyMat (x, y) ->
         if 0 <= i && i < x
         then aux deferred ((loc, ret, TyVec y) :: eqs) rest
         else Err.fail "mat index out of bounds" ~loc ~d:[%message (x : int) (i : int)]
       | TyVar _ -> aux (c :: deferred) eqs rest
       | ty -> Err.fail "expected vec or mat" ~loc ~d:[%message (ty : ty)])
    | ({ desc = FieldAccess (ty, f, ret); loc } as c) :: rest ->
      (match ty with
       | TyVar _ -> aux (c :: deferred) eqs rest
       | TyRecord (struct_name, type_args) ->
         (match Map.find structs struct_name with
          | None -> Err.fail "unknown struct" ~loc ~d:[%message (struct_name : string)]
          | Some (params, fields) ->
            if List.length params <> List.length type_args
            then
              Err.fail
                "wrong number of type args for struct"
                ~loc
                ~d:[%message (struct_name : string)]
            else (
              let sub = List.zip_exn params type_args in
              match List.Assoc.find fields ~equal:String.equal f with
              | None ->
                Err.fail
                  "field not found in struct"
                  ~loc
                  ~d:[%message (f : string) (struct_name : string)]
              | Some field_ty ->
                let field_ty = subst_ty sub field_ty in
                aux deferred ((loc, ret, field_ty) :: eqs) rest))
       | ty -> Err.fail "field access on non-record type" ~loc ~d:[%message (ty : ty)])
  in
  aux [] [] constrs
;;

(** Solve a set of constraints to produce a substitution and deferred constraints. *)
let solve structs (constrs : constr list) : (substitution * constr list) Compiler_error.t =
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
    Applies the sub to constraints, solves, and combines substitutions, then applies *)
let instantiate_scheme ?(structs = String.Map.empty) constrs term sub =
  let%map sub =
    if List.is_empty constrs
    then return sub
    else (
      let constrs = subst_constraints sub constrs in
      let%map sub', _ = solve structs constrs in
      List.map sub ~f:(fun (v, t) -> v, subst_ty sub' t) @ sub')
  in
  subst_term sub term
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
  | TyName s when Map.mem variants s -> TyVariant (s, [])
  | TyName s -> TyRecord (s, [])
  | TyApp (name, args) when Map.mem variants name ->
    TyVariant (name, List.map args ~f:(resolve_stlc_ty variants))
  | TyApp (name, args) -> TyRecord (name, List.map args ~f:(resolve_stlc_ty variants))
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
          (structs : (string list * (string * ty) list) String.Map.t)
          (variants : (string list * (string * ty list) list) String.Map.t)
          (ctx : context)
          (loc : Lexer.loc)
          (bind_stlc : Stlc.term)
          (recur : recur)
          (v : string)
          (return_ty : Stlc.ty option)
  : (term * ty * context * constr list * constr list) Compiler_error.t
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
and gen_term structs variants ctx (t : Stlc.term) : (term * constr list) Compiler_error.t =
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
      | None -> Err.fail "var not found in type map" ~loc ~d:[%message (v : string)]
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
    make (Lam (v, body)) (TyArrow (ty_v, body.ty)) constrs
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
        let bt = fresh_tyvar () in
        [ constr (Broadcast (l.ty, r.ty, bt))
        ; constr (Broadcast (bt, TyFloat, ret_ty))
        ; constr (HasClass (GenType, ret_ty))
        ]
      | Mul | Div -> [ constr (MulBroadcast (l.ty, r.ty, ret_ty)) ]
      | Eq ->
        [ constr (HasClass (Equatable, l.ty))
        ; constr (Eq (l.ty, r.ty))
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
        , [ t ] ) ->
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t, TyFloat, bt))
          ; constr (HasClass (GenType, bt))
          ; constr (Eq (ty, bt))
          ]
      | (Min | Max | Pow), [ t; t' ] ->
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t, t', bt))
          ; constr (Broadcast (bt, TyFloat, ty))
          ; constr (HasClass (GenType, ty))
          ]
      | Clamp, [ t; t'; t'' ] ->
        let tmp = fresh_tyvar () in
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t', t'', tmp))
          ; constr (Broadcast (t, tmp, bt))
          ; constr (Broadcast (bt, TyFloat, ty))
          ; constr (HasClass (GenType, ty))
          ]
      | Mix, [ t; t'; t'' ] ->
        let tmp = fresh_tyvar () in
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t, t', tmp))
          ; constr (Broadcast (tmp, t'', bt))
          ; constr (Broadcast (bt, TyFloat, ty))
          ; constr (HasClass (GenType, ty))
          ]
      | Length, [ t ] ->
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t, TyFloat, bt))
          ; constr (HasClass (GenType, bt))
          ; constr (Eq (ty, TyFloat))
          ]
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
      | Normalize, [ t ] ->
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t, TyFloat, bt))
          ; constr (HasClass (GenType, bt))
          ; constr (Eq (ty, bt))
          ]
      | Fract, [ t ] ->
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t, TyFloat, bt))
          ; constr (HasClass (GenType, bt))
          ; constr (Eq (ty, bt))
          ]
      | Step, [ t; t' ] ->
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t, t', bt))
          ; constr (Broadcast (bt, TyFloat, ty))
          ; constr (HasClass (GenType, ty))
          ]
      | Reflect, [ t; t' ] ->
        Ok [ constr (HasClass (GenType, t)); constr (Eq (t, t')); constr (Eq (ty, t)) ]
      | Smoothstep, [ t; t'; t'' ] ->
        let tmp = fresh_tyvar () in
        let bt = fresh_tyvar () in
        Ok
          [ constr (Broadcast (t, t', tmp))
          ; constr (Broadcast (tmp, t'', bt))
          ; constr (Broadcast (bt, TyFloat, ty))
          ; constr (HasClass (GenType, ty))
          ]
      | _ -> Err.fail "invalid builtin arguments" ~loc ~d:[%message (b : Glsl.builtin)]
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
    else Err.fail "vec size mismatch" ~loc ~d:[%message (n : int)]
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
    else Err.fail "mat size mismatch" ~loc ~d:[%message (n : int) (m : int)]
  | Record fields ->
    let provided_fields = String.Set.of_list (List.map fields ~f:fst) in
    let candidates =
      Map.filter structs ~f:(fun (_, struct_fields) ->
        struct_fields
        |> List.map ~f:fst
        |> String.Set.of_list
        |> Set.equal provided_fields)
    in
    (match Map.to_alist candidates with
     | [] ->
       Err.fail
         "record does not match any known struct"
         ~loc
         ~d:[%message (provided_fields : String.Set.t)]
     | _ :: _ :: _ ->
       Err.fail
         "record is ambiguous, matches multiple structs"
         ~loc
         ~d:[%message (provided_fields : String.Set.t)]
     | [ (struct_name, (params, struct_fields)) ] ->
       let sub = List.map params ~f:(fun p -> p, fresh_tyvar ()) in
       let inst_fields = List.map struct_fields ~f:(fun (n, ty) -> n, subst_ty sub ty) in
       let type_args = List.map sub ~f:snd in
       let%bind args, constrs_args =
         List.fold_result
           inst_fields
           ~init:([], [])
           ~f:(fun (acc, acc_constrs) (name, ty) ->
             match List.Assoc.find fields ~equal:String.equal name with
             | Some arg ->
               let%bind arg, constrs = gen_term structs variants ctx arg in
               let field_constr =
                 match ty with
                 (* Concrete field allows coercion via promote_ints *)
                 | TyFloat -> constr (HasClass (Comparable, arg.ty))
                 | _ -> constr (Eq (arg.ty, ty))
               in
               return (arg :: acc, (field_constr :: constrs) @ acc_constrs)
             | None ->
               Err.fail "(unreachable) missing field" ~loc ~d:[%message (name : string)])
       in
       make
         (Record (struct_name, List.rev args))
         (TyRecord (struct_name, type_args))
         constrs_args)
  | Field (t, f) ->
    let%bind t, constrs_t = gen_term structs variants ctx t in
    let ret_ty = fresh_tyvar () in
    make (Field (t, f)) ret_ty (constr (FieldAccess (t.ty, f, ret_ty)) :: constrs_t)
  | Variant (ctor, args) ->
    let%bind variant_name, params, ctor_arg_tys =
      let found =
        Map.fold variants ~init:[] ~f:(fun ~key:vname ~data:(params, ctors) acc ->
          match List.find ctors ~f:(fun (c, _) -> String.equal c ctor) with
          | Some (_, arg_tys) -> (vname, params, arg_tys) :: acc
          | None -> acc)
      in
      match found with
      | [ x ] -> Ok x
      | [] -> Err.fail "unknown constructor" ~loc ~d:[%message (ctor : string)]
      | _ -> Err.fail "ambiguous constructor" ~loc ~d:[%message (ctor : string)]
    in
    let param_sub = List.map params ~f:(fun p -> p, fresh_tyvar ()) in
    let expected_arg_tys = List.map ctor_arg_tys ~f:(subst_ty param_sub) in
    let type_args = List.map param_sub ~f:snd in
    if List.length args <> List.length expected_arg_tys
    then Err.fail "wrong number of args to constructor" ~loc ~d:[%message (ctor : string)]
    else (
      let%bind args, constrs_args =
        List.fold2_exn
          args
          expected_arg_tys
          ~init:(Ok ([], []))
          ~f:(fun acc arg expected_ty ->
            let%bind acc_args, acc_constrs = acc in
            let%bind arg, constrs = gen_term structs variants ctx arg in
            let arg_constr =
              match expected_ty with
              | TyFloat ->
                (* Concrete float ctor arg allows coercion via promote_ints *)
                constr (HasClass (Comparable, arg.ty))
              | _ -> constr (Eq (arg.ty, expected_ty))
            in
            return (arg :: acc_args, (arg_constr :: constrs) @ acc_constrs))
      in
      make
        (Variant (variant_name, ctor, List.rev args))
        (TyVariant (variant_name, type_args))
        constrs_args)
  | Match (scrutinee, cases) ->
    (* TODO: The pattern matching typechecking code sucks so much my god, use Eq constrs? *)
    let%bind scrutinee, constrs_s = gen_term structs variants ctx scrutinee in
    let ret_ty = fresh_tyvar () in
    let has_catchall =
      List.exists cases ~f:(fun (pat, _) ->
        match pat with
        | PatVar _ -> true
        | _ -> false)
    in
    (* Classify each non-var pattern *)
    let kind_of_pat = function
      | PatCtor _ -> Some `MatchVariant
      | PatLitBool _ -> Some `MatchBool
      | PatLitInt _ -> Some `MatchInt
      | PatLitFloat _ -> Some `MatchFloat
      | PatVar _ -> None
    in
    let first_kind = List.find_map cases ~f:(fun (p, _) -> kind_of_pat p) in
    (* Validate no mixing of pattern kinds *)
    let%bind () =
      List.fold_result cases ~init:() ~f:(fun () (pat, _) ->
        match kind_of_pat pat with
        | None -> Ok ()
        | Some k ->
          if Option.exists first_kind ~f:(Poly.equal k)
          then Ok ()
          else Err.fail "mixed pattern kinds in match" ~loc)
    in
    let typecheck_cases ~scrutinee_ty ~ctx_for_pat =
      let%bind cases, constrs_cases =
        List.fold_result
          cases
          ~init:([], [])
          ~f:(fun (acc_cases, acc_constrs) (pat, body) ->
            let%bind ctx = ctx_for_pat pat in
            let%bind body, constrs_body = gen_term structs variants ctx body in
            return
              ( (pat, body) :: acc_cases
              , (constr (Eq (body.ty, ret_ty)) :: constrs_body) @ acc_constrs ))
      in
      let scrutinee_constr = constr (Eq (scrutinee.ty, scrutinee_ty)) in
      make
        (Match (scrutinee, List.rev cases))
        ret_ty
        (scrutinee_constr :: (constrs_s @ constrs_cases))
    in
    let prim_ctx_for_pat ty pat =
      match pat with
      | PatVar v -> Ok (Map.set ctx ~key:v ~data:([], [], ty))
      | _ -> Ok ctx
    in
    let require_catchall msg = if has_catchall then Ok () else Err.fail msg ~loc in
    let check_dup_pats ~extract ~equal ~err_msg ~sexp_of_dup =
      let pats = List.filter_map cases ~f:(fun (p, _) -> extract p) in
      List.fold_result pats ~init:[] ~f:(fun seen k ->
        if List.exists seen ~f:(equal k)
        then Err.fail err_msg ~loc ~d:(sexp_of_dup k)
        else Ok (k :: seen))
      |> Result.ignore_m
    in
    (match first_kind with
     | None ->
       (* All vars: scrutinee can be any type *)
       typecheck_cases
         ~scrutinee_ty:scrutinee.ty
         ~ctx_for_pat:(prim_ctx_for_pat scrutinee.ty)
     | Some `MatchBool ->
       (* Bool match *)
       let%bind () =
         if has_catchall
         then Ok ()
         else (
           let has_true =
             List.exists cases ~f:(fun (p, _) -> equal_pat p (PatLitBool true))
           in
           let has_false =
             List.exists cases ~f:(fun (p, _) -> equal_pat p (PatLitBool false))
           in
           if has_true && has_false
           then Ok ()
           else Err.fail "non-exhaustive bool match (missing true or false)" ~loc)
       in
       let%bind () =
         check_dup_pats
           ~extract:(function
             | PatLitBool b -> Some b
             | _ -> None)
           ~equal:Bool.equal
           ~err_msg:"duplicate bool pattern"
           ~sexp_of_dup:(fun b -> [%message (b : bool)])
       in
       typecheck_cases ~scrutinee_ty:TyBool ~ctx_for_pat:(prim_ctx_for_pat TyBool)
     | Some `MatchInt ->
       let%bind () = require_catchall "int match must have a catch-all" in
       let%bind () =
         check_dup_pats
           ~extract:(function
             | PatLitInt n -> Some n
             | _ -> None)
           ~equal:Int.equal
           ~err_msg:"duplicate int pattern"
           ~sexp_of_dup:(fun n -> [%message (n : int)])
       in
       typecheck_cases ~scrutinee_ty:TyInt ~ctx_for_pat:(prim_ctx_for_pat TyInt)
     | Some `MatchFloat ->
       let%bind () = require_catchall "float match must have a catch-all" in
       let%bind () =
         check_dup_pats
           ~extract:(function
             | PatLitFloat f -> Some f
             | _ -> None)
           ~equal:Float.equal
           ~err_msg:"duplicate float pattern"
           ~sexp_of_dup:(fun f -> [%message (f : float)])
       in
       typecheck_cases ~scrutinee_ty:TyFloat ~ctx_for_pat:(prim_ctx_for_pat TyFloat)
     | Some `MatchVariant ->
       (* Variant match *)
       let%bind variant_name, variant_params, variant_ctors =
         let find_from_ty ty =
           match ty with
           | TyVariant (name, _) ->
             (match Map.find variants name with
              | Some (params, ctors) -> Some (name, params, ctors)
              | None -> None)
           | _ -> None
         in
         match find_from_ty scrutinee.ty with
         | Some x -> Ok x
         | None ->
           let case_ctors =
             List.filter_map cases ~f:(fun (pat, _) ->
               match pat with
               | PatCtor (c, _) -> Some c
               | _ -> None)
           in
           let candidates =
             Map.filter variants ~f:(fun (_, ctors) ->
               let ctor_names = List.map ctors ~f:fst in
               List.for_all case_ctors ~f:(fun c ->
                 List.mem ctor_names c ~equal:String.equal))
           in
           (match Map.to_alist candidates with
            | [ (name, (params, ctors)) ] -> Ok (name, params, ctors)
            | [] -> Err.fail "match cases don't match any variant type" ~loc
            | _ -> Err.fail "ambiguous match variant type" ~loc)
       in
       let param_sub = List.map variant_params ~f:(fun p -> p, fresh_tyvar ()) in
       let type_args = List.map param_sub ~f:snd in
       let scrutinee_ty = TyVariant (variant_name, type_args) in
       (* Exhaustiveness Checking *)
       let%bind () =
         if has_catchall
         then Ok ()
         else (
           let case_ctors =
             List.filter_map cases ~f:(fun (pat, _) ->
               match pat with
               | PatCtor (c, _) -> Some c
               | _ -> None)
             |> String.Set.of_list
           in
           let all_ctors = List.map variant_ctors ~f:fst |> String.Set.of_list in
           let missing = Set.diff all_ctors case_ctors in
           if Set.is_empty missing
           then Ok ()
           else
             Err.fail "non-exhaustive match" ~loc ~d:[%message (missing : String.Set.t)])
       in
       let%bind () =
         check_dup_pats
           ~extract:(function
             | PatCtor (c, _) -> Some c
             | _ -> None)
           ~equal:String.equal
           ~err_msg:"duplicate match case"
           ~sexp_of_dup:(fun ctor -> [%message (ctor : string)])
       in
       typecheck_cases ~scrutinee_ty ~ctx_for_pat:(fun pat ->
         match pat with
         | PatCtor (ctor, vars) ->
           let%bind _, ctor_arg_tys =
             match List.find variant_ctors ~f:(fun (c, _) -> String.equal c ctor) with
             | Some x -> Ok x
             | None ->
               Err.fail "unknown constructor in match" ~loc ~d:[%message (ctor : string)]
           in
           let expected_arg_tys = List.map ctor_arg_tys ~f:(subst_ty param_sub) in
           if List.length vars <> List.length expected_arg_tys
           then (
             let expected = List.length expected_arg_tys in
             let got = List.length vars in
             Err.fail
               "wrong number of bindings in match case"
               ~loc
               ~d:[%message (ctor : string) (expected : int) (got : int)])
           else
             Ok
               (List.fold2_exn vars expected_arg_tys ~init:ctx ~f:(fun ctx v ty ->
                  Map.set ctx ~key:v ~data:([], [], ty)))
         | PatVar v -> Ok (Map.set ctx ~key:v ~data:([], [], scrutinee_ty))
         | PatLitBool _ | PatLitInt _ | PatLitFloat _ -> Ok ctx))
;;

let typecheck (Program terms : Stlc.t) : t Compiler_error.t =
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
            Err.fail
              "unresolved top-level constraints"
              ~loc:top.loc
              ~d:[%message (remaining : constr list)]
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
            Err.fail
              "unresolved top-level constraints"
              ~loc:top.loc
              ~d:[%message (remaining : constr list)]
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
        | TypeDef (name, Stlc.RecordDecl (params, fields)) ->
          let fields =
            List.map fields ~f:(fun (f, ty) -> f, resolve_stlc_ty variants ty)
          in
          let structs = Map.set structs ~key:name ~data:(params, fields) in
          let top =
            { desc = TypeDef (name, RecordDecl (params, fields))
            ; ty = TyRecord (name, [])
            ; loc = top.loc
            ; scheme_constrs = []
            }
          in
          Ok (ctx, structs, variants, top :: acc)
        | TypeDef (name, Stlc.VariantDecl (params, ctors)) ->
          let ctors =
            List.map ctors ~f:(fun (c, tys) ->
              c, List.map tys ~f:(resolve_stlc_ty variants))
          in
          let variants = Map.set variants ~key:name ~data:(params, ctors) in
          let param_tyvars = List.map params ~f:(fun p -> TyVar p) in
          let top =
            { desc = TypeDef (name, VariantDecl (params, ctors))
            ; ty = TyVariant (name, param_tyvars)
            ; loc = top.loc
            ; scheme_constrs = []
            }
          in
          Ok (ctx, structs, variants, top :: acc))
  in
  Program (List.rev tops)
;;
