open Core
open Typecheck
open Type_system

let rec has_tyvar = function
  | TyVar _ -> true
  | TyFloat | TyInt | TyBool -> false
  | TyVec (_, t) -> has_tyvar t
  | TyArrow (a, b) -> has_tyvar a || has_tyvar b
  | TyRecord (_, args) | TyVariant (_, args) -> List.exists args ~f:has_tyvar
;;

let rec coercible (from_ty : ty) (to_ty : ty) : bool =
  if equal_ty from_ty to_ty
  then true
  else (
    match from_ty, to_ty with
    | TyInt, TyFloat -> true
    | TyArrow (p, r), TyArrow (p', r') -> coercible p' p && coercible r r'
    | TyVec (n, t), TyVec (n', t') when n = n' -> coercible t t'
    | TyRecord (s, args), TyRecord (s', args')
    | TyVariant (s, args), TyVariant (s', args')
      when String.equal s s' && List.length args = List.length args' ->
      List.for_all2_exn args args' ~f:coercible
    | _ -> false)
;;

let is_scalar = function
  | TyFloat | TyInt -> true
  | _ -> false
;;

(** Lower a [Coerce] where children have already been rewritten *)
let lower_coerce ~loc (target : ty) (inner : term) : term =
  if equal_ty inner.ty target
  then inner
  else if has_tyvar target || has_tyvar inner.ty
  then { desc = Coerce (target, inner); ty = target; loc }
  else (
    match target, inner.ty, inner.desc with
    | TyFloat, TyInt, Int i -> { desc = Float (Float.of_int i); ty = TyFloat; loc }
    | TyFloat, TyInt, _ -> { desc = Builtin (Glsl.Float, [ inner ]); ty = TyFloat; loc }
    | _ when coercible inner.ty target -> { inner with ty = target }
    | _ -> inner)
;;

let rec rewrite (t : term) : term =
  match t.desc with
  | Coerce (target, inner) ->
    let inner = rewrite inner in
    lower_coerce ~loc:t.loc target inner
  | Bop (op, l, r) ->
    (* Bop has no explicit Coerce wraps in [gen_term], derive operand
       expectations here so int/float promotion gets inserted *)
    let target = t.ty in
    let unresolved = has_tyvar l.ty || has_tyvar r.ty in
    let l_exp, r_exp =
      if unresolved
      then None, None
      else (
        match op with
        | Add | Sub | Mul | Div | Mod ->
          if is_scalar l.ty && is_scalar r.ty
          then
            if equal_ty target TyFloat || equal_ty l.ty TyFloat || equal_ty r.ty TyFloat
            then Some TyFloat, Some TyFloat
            else None, None
          else Some TyFloat, Some TyFloat
        | Lt | Gt | Leq | Geq | Eq ->
          if
            is_scalar l.ty
            && is_scalar r.ty
            && (equal_ty l.ty TyFloat || equal_ty r.ty TyFloat)
          then Some TyFloat, Some TyFloat
          else None, None
        | And | Or -> Some TyBool, Some TyBool)
    in
    let recur expected x =
      match expected with
      | None -> rewrite x
      | Some target -> lower_coerce ~loc:x.loc target (rewrite x)
    in
    { t with desc = Bop (op, recur l_exp l, recur r_exp r) }
  | Builtin (Glsl.Float, ts) ->
    { t with desc = Builtin (Glsl.Float, List.map ts ~f:rewrite) }
  | Builtin (b, ts) ->
    let coerce_arg (a : term) = lower_coerce ~loc:a.loc TyFloat (rewrite a) in
    { t with desc = Builtin (b, List.map ts ~f:coerce_arg) }
  | Var _ | Float _ | Int _ | Bool _ -> t
  | Vec (n, ts) ->
    (* NOTE: GLSL has no ivecm so vec elements are always float at the boundary. *)
    let coerce_elem (a : term) = lower_coerce ~loc:a.loc TyFloat (rewrite a) in
    { t with desc = Vec (n, List.map ts ~f:coerce_elem) }
  | Lam (v, body) -> { t with desc = Lam (v, rewrite body) }
  | App (f, x) -> { t with desc = App (rewrite f, rewrite x) }
  | Let (recur, v, constrs, bind, body) ->
    { t with desc = Let (recur, v, constrs, rewrite bind, rewrite body) }
  | If (c, t1, e) -> { t with desc = If (rewrite c, rewrite t1, rewrite e) }
  | Index (inner, i) -> { t with desc = Index (rewrite inner, i) }
  | Record (s, ts) -> { t with desc = Record (s, List.map ts ~f:rewrite) }
  | Field (inner, f) -> { t with desc = Field (rewrite inner, f) }
  | Variant (tn, ctor, args) ->
    { t with desc = Variant (tn, ctor, List.map args ~f:rewrite) }
  | Match (scrut, cases) ->
    { t with desc = Match (rewrite scrut, List.map cases ~f:(Tuple2.map_snd ~f:rewrite)) }
;;

let materialize (Program tops : t) : t =
  let materialize_top (top : top) : top =
    match top.desc with
    | Define (recur, v, bind) -> { top with desc = Define (recur, v, rewrite bind) }
    | Extern _ | TypeDef _ -> top
  in
  Program (List.map tops ~f:materialize_top)
;;
