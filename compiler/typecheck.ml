(** Typechecking is done with Hindley-Milner inference (Algorithm W), but extended
    with typeclasses and broadcasting specific for operator overloading in GLSL *)

(* TODO: Add more documentation on the relevant papers *)

open Core
open Sexplib.Sexp
open Stlc
open Or_error.Let_syntax

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of string * ty * term
  | App of term * term
  | Let of recur * string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of string * term list
  | Field of term * string

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

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
  | Let (Rec (n, ty_opt), v, bind, body) ->
    let ty = Option.sexp_of_t Stlc.sexp_of_ty ty_opt in
    let rec_tag = List [ Atom "rec"; Atom (Int.to_string n); ty ] in
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

and sexp_of_term t = List [ sexp_of_term_desc t.desc; Atom ":"; Stlc.sexp_of_ty t.ty ]

type top_desc =
  | Define of recur * string * term
  | Extern of string
  | RecordDef of string * (string * ty) list
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let sexp_of_top t = List [ sexp_of_top_desc t.desc; Atom ":"; Stlc.sexp_of_ty t.ty ]

type t = Program of top list [@@deriving sexp_of]

(** Map from type variable names to their resolved types *)
type substitution = (string * ty) list [@@deriving sexp_of]

(** Represents polymorphic [forall 'vars. ty] *)
type type_scheme = string list * ty [@@deriving sexp_of]

(** Maps type variables to type schemes *)
type context = type_scheme String.Map.t

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
[@@deriving sexp_of] [@@warning "-37"]

(* TODO: Factor out the [loc] into its own type again? *)

(** Constraints emitted during type inference. *)
type constr =
  | Eq of Lexer.loc * ty * ty (** Standard equality constraint *)
  | HasClass of Lexer.loc * type_class * ty (** Membership in a GLSL typeclass *)
  | Broadcast of Lexer.loc * ty * ty * ty
  (** Scalar-vector broadcasting (e.g. float + vec3) *)
  | MulBroadcast of Lexer.loc * ty * ty * ty (** Matrix multiplication rules *)
  | IndexAccess of Lexer.loc * ty * int * ty (** Vector/Matrix indexing *)
[@@deriving sexp_of]

let fresh_tyvar () = TyVar (Utils.fresh "v")

let rec subst_ty (sub : substitution) (ty : ty) : ty =
  match ty with
  | TyVar v -> List.Assoc.find ~equal:String.equal sub v |> Option.value ~default:ty
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ -> ty
  | TyArrow (f, x) -> TyArrow (subst_ty sub f, subst_ty sub x)
;;

let subst_context (sub : substitution) (ctx : context) : type_scheme String.Map.t =
  Map.map ctx ~f:(fun (vars, ty) ->
    let sub =
      List.filter sub ~f:(fun (v, _) -> not (List.mem vars v ~equal:String.equal))
    in
    vars, subst_ty sub ty)
;;

let subst_constraints (sub : substitution) (con : constr list) : constr list =
  List.map con ~f:(function
    | Eq (loc, l, r) -> Eq (loc, subst_ty sub l, subst_ty sub r)
    | HasClass (loc, cls, ty) -> HasClass (loc, cls, subst_ty sub ty)
    | Broadcast (loc, l, r, ret) ->
      Broadcast (loc, subst_ty sub l, subst_ty sub r, subst_ty sub ret)
    | MulBroadcast (loc, l, r, ret) ->
      MulBroadcast (loc, subst_ty sub l, subst_ty sub r, subst_ty sub ret)
    | IndexAccess (loc, t, i, ret) ->
      IndexAccess (loc, subst_ty sub t, i, subst_ty sub ret))
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
    | Let (recur, v, bind, body) -> Let (recur, v, subst bind, subst body)
    | If (c, t, f) -> If (subst c, subst t, subst f)
    | Bop (op, l, r) -> Bop (op, subst l, subst r)
    | Index (t, i) -> Index (subst t, i)
    | Builtin (b, args) -> Builtin (b, List.map args ~f:subst)
    | Record (name, args) -> Record (name, List.map args ~f:subst)
    | Field (t, f) -> Field (subst t, f)
  in
  { t with desc; ty = subst_ty sub t.ty }
;;

let rec ftv_of_ty = function
  | TyVar v -> String.Set.singleton v
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ -> String.Set.empty
  | TyArrow (t1, t2) -> Set.union (ftv_of_ty t1) (ftv_of_ty t2)
;;

let ftv_of_context (ctx : context) : String.Set.t =
  let ftv_of_scheme (vars, ty) = Set.diff (ftv_of_ty ty) (String.Set.of_list vars) in
  Map.data ctx |> List.map ~f:ftv_of_scheme |> String.Set.union_list
;;

let ftv_of_constraint (con : constr) : String.Set.t =
  match con with
  | Eq (_, l, r) -> Set.union (ftv_of_ty l) (ftv_of_ty r)
  | HasClass (_, _, ty) -> ftv_of_ty ty
  | Broadcast (_, l, r, ret) | MulBroadcast (_, l, r, ret) ->
    String.Set.union_list [ ftv_of_ty l; ftv_of_ty r; ftv_of_ty ret ]
  | IndexAccess (_, t, _, ret) -> Set.union (ftv_of_ty t) (ftv_of_ty ret)
;;

(** Generalize a type by quantifying variables not in context or deferred constraints. *)
let generalize (ctx : context) (deferred : constr list) (ty : ty) : type_scheme =
  let ftv_ty = ftv_of_ty ty in
  let ftv_ctx = ftv_of_context ctx in
  let ftv_deferred = List.map ~f:ftv_of_constraint deferred in
  let restricted = String.Set.union_list (ftv_ctx :: ftv_deferred) in
  Set.to_list (Set.diff ftv_ty restricted), ty
;;

(** Unify two types into a substitution *)
let rec unify (con : (Lexer.loc * ty * ty) list) : substitution Or_error.t =
  match con with
  | [] -> return []
  | (loc, TyVar v, ty) :: con | (loc, ty, TyVar v) :: con ->
    let rec occurs_in = function
      | TyVar v' -> String.equal v v'
      | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ -> false
      | TyArrow (ty, ty') -> occurs_in ty || occurs_in ty'
    in
    if occurs_in ty
    then
      error_s
        [%message
          "typecheck: recursive unification" (loc : Lexer.loc) (v : string) (ty : ty)]
    else if equal_ty (TyVar v) ty
    then unify con
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
let resolve_constraints (constrs : constr list)
  : (constr list * (Lexer.loc * ty * ty) list) Or_error.t
  =
  let rec aux deferred eqs = function
    | [] -> return (List.rev deferred, List.rev eqs)
    | Eq (loc, l, r) :: rest -> aux deferred ((loc, l, r) :: eqs) rest
    | HasClass (loc, cls, ty) :: rest ->
      (match ty with
       | TyVar _ -> aux (HasClass (loc, cls, ty) :: deferred) eqs rest
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
    | Broadcast (loc, l, r, ret) :: rest ->
      (match l, r with
       | TyVar _, _ | _, TyVar _ -> aux (Broadcast (loc, l, r, ret) :: deferred) eqs rest
       | TyFloat, TyFloat -> aux deferred ((loc, ret, TyFloat) :: eqs) rest
       | TyInt, TyInt -> aux deferred ((loc, ret, TyInt) :: eqs) rest
       | TyVec n, TyVec n' when n = n' -> aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyFloat, TyVec n | TyVec n, TyFloat | TyInt, TyVec n | TyVec n, TyInt ->
         aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyMat (x, y), TyMat (w, z) when x = w && y = z ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | _ ->
         error_s
           [%message "typecheck: invalid broadcast" (loc : Lexer.loc) (l : ty) (r : ty)])
    | MulBroadcast (loc, l, r, ret) :: rest ->
      (match l, r with
       | TyVar _, _ | _, TyVar _ ->
         aux (MulBroadcast (loc, l, r, ret) :: deferred) eqs rest
       | TyMat (x, y), TyMat (w, z) when x = w && y = z ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | TyMat (x, y), TyFloat | TyFloat, TyMat (x, y) ->
         aux deferred ((loc, ret, TyMat (x, y)) :: eqs) rest
       | TyMat (x, y), TyVec n when y = n ->
         aux deferred ((loc, ret, TyVec x) :: eqs) rest
       | TyFloat, TyFloat -> aux deferred ((loc, ret, TyFloat) :: eqs) rest
       | TyInt, TyInt -> aux deferred ((loc, ret, TyInt) :: eqs) rest
       | TyVec n, TyVec n' when n = n' -> aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | TyFloat, TyVec n | TyVec n, TyFloat | TyInt, TyVec n | TyVec n, TyInt ->
         aux deferred ((loc, ret, TyVec n) :: eqs) rest
       | _ ->
         error_s
           [%message
             "typecheck: invalid mul/div broadcast" (loc : Lexer.loc) (l : ty) (r : ty)])
    | IndexAccess (loc, t, i, ret) :: rest ->
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
       | TyVar _ -> aux (IndexAccess (loc, t, i, ret) :: deferred) eqs rest
       | ty -> error_s [%message "expected vec or mat" (loc : Lexer.loc) (ty : ty)])
  in
  aux [] [] constrs
;;

(** Solve a set of constraints to produce a substitution. *)
let solve (constrs : constr list) : substitution Or_error.t =
  let rec go sub constrs =
    let%bind deferred, eqs = resolve_constraints constrs in
    if List.is_empty eqs
    then
      if List.is_empty deferred
      then return sub
      else
        error_s
          [%message "typecheck: unresolved overloaded operators" (deferred : constr list)]
    else (
      let%bind new_sub = unify eqs in
      let sub = List.map sub ~f:(fun (v, t) -> v, subst_ty new_sub t) @ new_sub in
      let deferred = subst_constraints new_sub deferred in
      go sub deferred)
  in
  go [] constrs
;;

(** Value restriction check for generalization. *)
let rec is_value (t : Stlc.term) : bool =
  match t.desc with
  | Float _ | Int _ | Bool _ | Var _ | Lam _ -> true
  | Vec (_, ts) -> List.for_all ts ~f:is_value
  | Mat (_, _, ts) -> List.for_all ts ~f:is_value
  | Record fields -> List.for_all fields ~f:(fun (_, t) -> is_value t)
  | Field (t, _) | Index (t, _) -> is_value t
  | App _ | Let _ | If _ | Bop _ | Builtin _ -> false
;;

(** Generate typed term and constraints from STLC term. *)
let rec gen_term structs fields_env ctx (t : Stlc.term) : (term * constr list) Or_error.t =
  let loc = t.loc in
  let make desc ty constrs = Ok (({ desc; ty; loc } : term), constrs) in
  match t.desc with
  | Float f -> make (Float f) TyFloat []
  | Int i -> make (Int i) TyInt []
  | Bool b -> make (Bool b) TyBool []
  | Var v ->
    let%bind vs, ty_scheme =
      match Map.find ctx v with
      | Some s -> Ok s
      | None ->
        error_s [%message "var not found in type map" (loc : Lexer.loc) (v : string)]
    in
    let sub = List.map vs ~f:(fun v -> v, fresh_tyvar ()) in
    make (Var v) (subst_ty sub ty_scheme) []
  | Lam (v, ty_ann, body) ->
    let ty_v =
      match ty_ann with
      | Some t -> t
      | None -> fresh_tyvar ()
    in
    let ctx = Map.set ctx ~key:v ~data:([], ty_v) in
    let%bind body, constrs = gen_term structs fields_env ctx body in
    make (Lam (v, ty_v, body)) (TyArrow (ty_v, body.ty)) constrs
  | App (f, x) ->
    let%bind f, constrs_f = gen_term structs fields_env ctx f in
    let%bind x, constrs_x = gen_term structs fields_env ctx x in
    let ret_ty = fresh_tyvar () in
    let constrs = Eq (loc, f.ty, TyArrow (x.ty, ret_ty)) :: (constrs_f @ constrs_x) in
    make (App (f, x)) ret_ty constrs
  | Let (Nonrec, v, bind, body) ->
    let%bind bind', constrs_bind = gen_term structs fields_env ctx bind in
    let%bind sub_bind = solve constrs_bind in
    let ty_bind = subst_ty sub_bind bind'.ty in
    let ctx_subbed = subst_context sub_bind ctx in
    let scheme =
      if is_value bind then generalize ctx_subbed [] ty_bind else [], ty_bind
    in
    let ctx' = Map.set ctx_subbed ~key:v ~data:scheme in
    let%bind body, constrs_body = gen_term structs fields_env ctx' body in
    make (Let (Nonrec, v, bind', body)) body.ty (constrs_bind @ constrs_body)
  | Let (Rec (n, ty_ann), v, bind, body) ->
    let ty_v =
      match ty_ann with
      | Some t -> t
      | None -> fresh_tyvar ()
    in
    let ctx' = Map.set ctx ~key:v ~data:([], ty_v) in
    let%bind bind', constrs_bind = gen_term structs fields_env ctx' bind in
    let%bind sub_bind = solve (Eq (loc, ty_v, bind'.ty) :: constrs_bind) in
    let ty_bind = subst_ty sub_bind bind'.ty in
    let ctx_subbed = subst_context sub_bind ctx in
    let scheme =
      if is_value bind then generalize ctx_subbed [] ty_bind else [], ty_bind
    in
    let ctx = Map.set ctx_subbed ~key:v ~data:scheme in
    let%bind body', constrs_body = gen_term structs fields_env ctx body in
    let constrs = (Eq (loc, ty_v, bind'.ty) :: constrs_bind) @ constrs_body in
    make (Let (Rec (n, ty_ann), v, bind', body')) body'.ty constrs
  | If (c, t, e) ->
    let%bind c, constrs_c = gen_term structs fields_env ctx c in
    let%bind t, constrs_t = gen_term structs fields_env ctx t in
    let%bind e, constrs_e = gen_term structs fields_env ctx e in
    let constrs =
      Eq (loc, c.ty, TyBool) :: Eq (loc, t.ty, e.ty) :: (constrs_c @ constrs_t @ constrs_e)
    in
    make (If (c, t, e)) t.ty constrs
  | Bop (op, l, r) ->
    let%bind l, constrs_l = gen_term structs fields_env ctx l in
    let%bind r, constrs_r = gen_term structs fields_env ctx r in
    let ret_ty = fresh_tyvar () in
    let op_constrs =
      match op with
      | Add | Sub | Mod -> [ Broadcast (loc, l.ty, r.ty, ret_ty) ]
      | Mul | Div -> [ MulBroadcast (loc, l.ty, r.ty, ret_ty) ]
      | Eq ->
        [ HasClass (loc, Equatable, l.ty)
        ; Eq (loc, l.ty, r.ty)
        ; Eq (loc, ret_ty, TyBool)
        ]
      | Lt | Gt | Leq | Geq ->
        [ HasClass (loc, Comparable, l.ty)
        ; Eq (loc, l.ty, r.ty)
        ; Eq (loc, ret_ty, TyBool)
        ]
      | And | Or ->
        [ Eq (loc, l.ty, TyBool); Eq (loc, r.ty, TyBool); Eq (loc, ret_ty, TyBool) ]
    in
    make (Bop (op, l, r)) ret_ty (op_constrs @ constrs_l @ constrs_r)
  | Index (t, i) ->
    let%bind t, constrs_t = gen_term structs fields_env ctx t in
    let ret_ty = fresh_tyvar () in
    make (Index (t, i)) ret_ty (IndexAccess (loc, t.ty, i, ret_ty) :: constrs_t)
  | Builtin (b, args) ->
    let%bind args, constrs_args =
      List.fold_result args ~init:([], []) ~f:(fun (acc_args, acc_constrs) arg ->
        let%bind arg', constrs = gen_term structs fields_env ctx arg in
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
        , [ t ] ) -> Ok [ HasClass (loc, GenType, t); Eq (loc, ty, t) ]
      | (Min | Max | Pow), [ t; t' ] ->
        Ok [ HasClass (loc, GenType, ty); Broadcast (loc, t, t', ty) ]
      | Clamp, [ t; t'; t'' ] ->
        let tmp = fresh_tyvar () in
        Ok
          [ HasClass (loc, GenType, ty)
          ; Broadcast (loc, t', t'', tmp)
          ; Broadcast (loc, t, tmp, ty)
          ]
      | Mix, [ t; t'; t'' ] ->
        let tmp = fresh_tyvar () in
        Ok
          [ HasClass (loc, GenType, ty)
          ; Broadcast (loc, t, t', tmp)
          ; Broadcast (loc, tmp, t'', ty)
          ]
      | Length, [ t ] -> Ok [ HasClass (loc, GenType, t); Eq (loc, ty, TyFloat) ]
      | (Distance | Dot), [ t; t' ] ->
        Ok [ HasClass (loc, GenType, t); Eq (loc, t, t'); Eq (loc, ty, TyFloat) ]
      | Cross, [ t; t' ] ->
        Ok [ Eq (loc, t, TyVec 3); Eq (loc, t', TyVec 3); Eq (loc, ty, TyVec 3) ]
      | Normalize, [ t ] -> Ok [ HasClass (loc, GenType, t); Eq (loc, ty, t) ]
      | _ ->
        error_s
          [%message "invalid builtin arguments" (loc : Lexer.loc) (b : Glsl.builtin)]
    in
    make (Builtin (b, args)) ty (builtin_constrs @ constrs_args)
  | Vec (n, args) ->
    let%bind args, constrs_args =
      List.fold_result args ~init:([], []) ~f:(fun (acc_args, acc_constrs) arg ->
        let%bind arg, constrs = gen_term structs fields_env ctx arg in
        return (arg :: acc_args, (Eq (loc, arg.ty, TyFloat) :: constrs) @ acc_constrs))
    in
    let args = List.rev args in
    if List.length args = n
    then make (Vec (n, args)) (TyVec n) constrs_args
    else error_s [%message "vec size mismatch" (loc : Lexer.loc) (n : int)]
  | Mat (n, m, args) ->
    let%bind args, constrs_args =
      List.fold_result args ~init:([], []) ~f:(fun (acc_args, acc_constrs) arg ->
        let%bind arg, constrs = gen_term structs fields_env ctx arg in
        return (arg :: acc_args, (Eq (loc, arg.ty, TyFloat) :: constrs) @ acc_constrs))
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
               let%bind arg, constrs = gen_term structs fields_env ctx arg in
               return (arg :: acc, (Eq (loc, arg.ty, ty) :: constrs) @ acc_constrs)
             | None ->
               error_s [%message "(unreachable) missing field" (loc : Lexer.loc) name])
       in
       make (Record (struct_name, List.rev args)) (TyRecord struct_name) constrs_args)
  | Field (t, f) ->
    let%bind t, constrs_t = gen_term structs fields_env ctx t in
    let ret_ty = fresh_tyvar () in
    (match Map.find fields_env f with
     | Some struct_name ->
       let struct_fields = Map.find_exn structs struct_name in
       let field_ty = List.Assoc.find_exn struct_fields ~equal:String.equal f in
       let constrs =
         Eq (loc, t.ty, TyRecord struct_name) :: Eq (loc, ret_ty, field_ty) :: constrs_t
       in
       make (Field (t, f)) ret_ty constrs
     | None -> error_s [%message "field not found in any struct" (loc : Lexer.loc) f])
;;

let typecheck (Program terms : Stlc.t) : t Or_error.t =
  let%map _, _, _, tops =
    List.fold_result
      terms
      ~init:(String.Map.empty, String.Map.empty, String.Map.empty, [])
        (* TODO: There has to be a better way than to pass everything like this *)
      ~f:(fun (ctx, structs, fields_env, acc) top ->
        match top.desc with
        | Define (Rec (n, ty_ann), v, bind) ->
          let ty_v =
            match ty_ann with
            | Some t -> t
            | None -> fresh_tyvar ()
          in
          let ctx' = Map.set ctx ~key:v ~data:([], ty_v) in
          let%bind bind', constrs_bind = gen_term structs fields_env ctx' bind in
          let%bind sub_bind = solve (Eq (top.loc, ty_v, bind'.ty) :: constrs_bind) in
          let ty_bind = subst_ty sub_bind bind'.ty in
          let ctx_subbed = subst_context sub_bind ctx in
          let scheme =
            if is_value bind then generalize ctx_subbed [] ty_bind else [], ty_bind
          in
          let ctx = Map.set ctx_subbed ~key:v ~data:scheme in
          let top =
            { desc = Define (Rec (n, ty_ann), v, subst_term sub_bind bind')
            ; ty = ty_bind
            ; loc = top.loc
            }
          in
          Ok (ctx, structs, fields_env, top :: acc)
        | Define (Nonrec, v, bind) ->
          let%bind bind', constrs_bind = gen_term structs fields_env ctx bind in
          let%bind sub_bind = solve constrs_bind in
          let ty_bind = subst_ty sub_bind bind'.ty in
          let ctx_subbed = subst_context sub_bind ctx in
          let scheme =
            if is_value bind then generalize ctx_subbed [] ty_bind else [], ty_bind
          in
          let ctx = Map.set ctx_subbed ~key:v ~data:scheme in
          let top =
            { desc = Define (Nonrec, v, subst_term sub_bind bind')
            ; ty = ty_bind
            ; loc = top.loc
            }
          in
          Ok (ctx, structs, fields_env, top :: acc)
        | Extern (ty, v) ->
          let ctx = Map.set ctx ~key:v ~data:([], ty) in
          let top = { desc = Extern v; ty; loc = top.loc } in
          Ok (ctx, structs, fields_env, top :: acc)
        | RecordDef (name, fields) ->
          let structs = Map.set structs ~key:name ~data:fields in
          let fields_env =
            List.fold fields ~init:fields_env ~f:(fun env (f_name, _) ->
              Map.set env ~key:f_name ~data:name)
          in
          let top =
            { desc = RecordDef (name, fields); ty = TyRecord name; loc = top.loc }
          in
          Ok (ctx, structs, fields_env, top :: acc))
  in
  Program (List.rev tops)
;;
