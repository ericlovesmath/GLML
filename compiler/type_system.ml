open Core
open Sexplib.Sexp

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyRecord of string * (string * ty) list
  | TyVariant of string * (string * ty list) list
  | TyVar of string
  | TyTuple of ty list
  | TySampler
  | TyAbstract of string
[@@deriving equal, compare]

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
  | TySampler -> Atom "sampler"
  | TyAbstract g -> Atom ("#" ^ g)
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

(** Apply [f] to each sub[ty] *)
let map_ty_children (f : ty -> ty) : ty -> ty = function
  | (TyFloat | TyInt | TyBool | TySampler | TyVar _ | TyAbstract _) as t -> t
  | TyVec (n, t) -> TyVec (n, f t)
  | TyArrow (a, b) -> TyArrow (f a, f b)
  | TyRecord (hint, fields) -> TyRecord (hint, List.map fields ~f:(fun (n, t) -> n, f t))
  | TyVariant (hint, ctors) ->
    TyVariant (hint, List.map ctors ~f:(fun (n, ts) -> n, List.map ts ~f))
  | TyTuple ts -> TyTuple (List.map ts ~f)
;;

(** Fold [f] over each immediate sub[ty] *)
let fold_ty_children (f : 'a -> ty -> 'a) (acc : 'a) : ty -> 'a = function
  | TyFloat | TyInt | TyBool | TySampler | TyVar _ | TyAbstract _ -> acc
  | TyVec (_, t) -> f acc t
  | TyArrow (a, b) -> f (f acc a) b
  | TyRecord (_, fields) -> List.fold fields ~init:acc ~f:(fun acc (_, t) -> f acc t)
  | TyVariant (_, ctors) ->
    List.fold ctors ~init:acc ~f:(fun acc (_, ts) -> List.fold ts ~init:acc ~f)
  | TyTuple ts -> List.fold ts ~init:acc ~f
;;

let rec subst_ty (sub : substitution) (ty : ty) : ty =
  match ty with
  | TyVar v -> List.Assoc.find ~equal:String.equal sub v |> Option.value ~default:ty
  | ty -> map_ty_children (subst_ty sub) ty
;;

(** Map [f] over each [ty] in a constraint *)
let map_constr_tys ~(f : ty -> ty) (c : constr) : constr =
  let desc =
    match c.desc with
    | Eq (l, r) -> Eq (f l, f r)
    | HasClass (cls, ty) -> HasClass (cls, f ty)
    | Broadcast (l, r, ret) -> Broadcast (f l, f r, f ret)
    | MulBroadcast (l, r, ret) -> MulBroadcast (f l, f r, f ret)
    | IndexAccess (t, i, ret) -> IndexAccess (f t, i, f ret)
    | FieldAccess (t, fld, ret) -> FieldAccess (f t, fld, f ret)
    | Coerce (from_ty, to_ty) -> Coerce (f from_ty, f to_ty)
  in
  { c with desc }
;;

let subst_constraints (sub : substitution) (con : constr list) : constr list =
  List.map con ~f:(map_constr_tys ~f:(subst_ty sub))
;;

let compose_sub (s : substitution) (s' : substitution) : substitution =
  List.map s' ~f:(fun (v, ty) -> v, subst_ty s ty)
  @ List.filter s ~f:(fun (v, _) ->
    not (List.exists s' ~f:(fun (v', _) -> String.equal v v')))
;;

let rec ftv_of_ty = function
  | TyVar v -> String.Set.singleton v
  | ty -> fold_ty_children (fun acc t -> Set.union acc (ftv_of_ty t)) String.Set.empty ty
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
