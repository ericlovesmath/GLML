open Core
open Sexplib.Sexp

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyRecord of (string[@equal.ignore] [@compare.ignore]) * (string * ty) list
  | TyVariant of (string[@equal.ignore] [@compare.ignore]) * (string * ty list) list
  | TyVar of string
  | TyTuple of ty list
[@@deriving equal, compare]

let merge_hint a b = if String.is_empty a then b else a

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec (i, t) -> List [ Atom "vec"; Atom (Int.to_string i); sexp_of_ty t ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
  | TyRecord (hint, fields) ->
    List
      (Atom "record"
       :: Atom hint
       :: List.map fields ~f:(fun (n, t) -> List [ Atom n; sexp_of_ty t ]))
  | TyVariant (hint, ctors) ->
    List
      (Atom "variant"
       :: Atom hint
       :: List.map ctors ~f:(fun (n, ts) -> List (Atom n :: List.map ts ~f:sexp_of_ty)))
  | TyVar v -> Atom ("'" ^ v)
  | TyTuple ts -> List (Atom "tuple" :: List.map ts ~f:sexp_of_ty)
;;

type type_decl =
  | RecordDecl of string list * (string * ty) list
  | VariantDecl of string list * (string * ty list) list
[@@deriving sexp_of]

type type_class =
  | GenType
  | GenBType
  | GenIType
  | MatType
  | Numeric
  | Comparable
  | Equatable
[@@deriving sexp_of]

type constr_desc =
  | Eq of ty * ty
  | HasClass of type_class * ty
  | Broadcast of ty * ty * ty
  | MulBroadcast of ty * ty * ty
  | IndexAccess of ty * int * ty
  | FieldAccess of ty * string * ty
  | Coerce of ty * ty

type constr =
  { desc : constr_desc
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
  | Coerce (from_ty, to_ty) ->
    List [ Atom "Coerce"; sexp_of_ty from_ty; sexp_of_ty to_ty ]
;;

let sexp_of_constr (c : constr) = sexp_of_constr_desc c.desc

type substitution = (string * ty) list [@@deriving sexp_of]

let fresh_tyvar () = TyVar (Utils.fresh "v")

let rec subst_ty (sub : substitution) (ty : ty) : ty =
  match ty with
  | TyVar v -> List.Assoc.find ~equal:String.equal sub v |> Option.value ~default:ty
  | TyFloat | TyInt | TyBool -> ty
  | TyVec (n, t) -> TyVec (n, subst_ty sub t)
  | TyVariant (hint, ctors) ->
    TyVariant (hint, List.map ctors ~f:(fun (n, ts) -> n, List.map ts ~f:(subst_ty sub)))
  | TyRecord (hint, fields) ->
    TyRecord (hint, List.map fields ~f:(fun (n, t) -> n, subst_ty sub t))
  | TyArrow (f, x) -> TyArrow (subst_ty sub f, subst_ty sub x)
  | TyTuple ts -> TyTuple (List.map ts ~f:(subst_ty sub))
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
      | Coerce (from_ty, to_ty) -> Coerce (subst_ty sub from_ty, subst_ty sub to_ty)
    in
    { c with desc })
;;

let compose_sub (s : substitution) (s' : substitution) : substitution =
  List.map s' ~f:(fun (v, ty) -> v, subst_ty s ty)
  @ List.filter s ~f:(fun (v, _) ->
    not (List.exists s' ~f:(fun (v', _) -> String.equal v v')))
;;

let rec ftv_of_ty = function
  | TyVar v -> String.Set.singleton v
  | TyFloat | TyInt | TyBool -> String.Set.empty
  | TyVec (_, t) -> ftv_of_ty t
  | TyRecord (_, fields) ->
    String.Set.union_list (List.map fields ~f:(fun (_, t) -> ftv_of_ty t))
  | TyVariant (_, ctors) ->
    String.Set.union_list
      (List.concat_map ctors ~f:(fun (_, ts) -> List.map ts ~f:ftv_of_ty))
  | TyArrow (t1, t2) -> Set.union (ftv_of_ty t1) (ftv_of_ty t2)
  | TyTuple ts -> String.Set.union_list (List.map ts ~f:ftv_of_ty)
;;

let ftv_of_constraint (c : constr) : String.Set.t =
  match c.desc with
  | Eq (l, r) -> Set.union (ftv_of_ty l) (ftv_of_ty r)
  | HasClass (_, ty) -> ftv_of_ty ty
  | Broadcast (l, r, ret) | MulBroadcast (l, r, ret) ->
    String.Set.union_list [ ftv_of_ty l; ftv_of_ty r; ftv_of_ty ret ]
  | IndexAccess (t, _, ret) | FieldAccess (t, _, ret) ->
    Set.union (ftv_of_ty t) (ftv_of_ty ret)
  | Coerce (from_ty, to_ty) -> Set.union (ftv_of_ty from_ty) (ftv_of_ty to_ty)
;;
