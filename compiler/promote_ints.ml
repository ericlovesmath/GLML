open Core
open Typecheck
open Type_system

let has_tyvar ty = not (Set.is_empty (ftv_of_ty ty))

let rec coercible (from_ty : ty) (to_ty : ty) : bool =
  if equal_ty from_ty to_ty
  then true
  else (
    match from_ty, to_ty with
    | TyInt, TyFloat -> true
    | TyArrow (p, r), TyArrow (p', r') -> coercible p' p && coercible r r'
    | TyVec (n, t), TyVec (n', t') when n = n' -> coercible t t'
    | TyRecord (n, fs), TyRecord (n', fs')
      when String.equal n n' && List.length fs = List.length fs' ->
      List.for_all2_exn fs fs' ~f:(fun (_, a) (_, b) -> coercible a b)
    | TyVariant (n, cs), TyVariant (n', cs')
      when String.equal n n' && List.length cs = List.length cs' ->
      List.for_all2_exn cs cs' ~f:(fun (_, ts) (_, ts') ->
        List.length ts = List.length ts' && List.for_all2_exn ts ts' ~f:coercible)
    | TyTuple ts, TyTuple ts' when List.length ts = List.length ts' ->
      List.for_all2_exn ts ts' ~f:coercible
    | _ -> false)
;;

let is_scalar = function
  | TyFloat | TyInt -> true
  | _ -> false
;;

(** Lower a [Coerce] where children have already been rewritten *)
let rec lower_coerce ~loc (target : ty) (inner : term) : term =
  if equal_ty inner.ty target
  then inner
  else if has_tyvar target || has_tyvar inner.ty
  then { desc = Coerce (target, inner); ty = target; loc }
  else (
    match target, inner.ty, inner.desc with
    | TyFloat, TyInt, Int i -> { desc = Float (Float.of_int i); ty = TyFloat; loc }
    | TyFloat, TyInt, _ -> { desc = Builtin (Glsl.Float, [ inner ]); ty = TyFloat; loc }
    | TyVariant (_, t_ctors), TyVariant _, Variant (ctor, args) ->
      let target_arg_tys = List.Assoc.find_exn t_ctors ~equal:String.equal ctor in
      let new_args =
        List.map2_exn args target_arg_tys ~f:(fun a t -> lower_coerce ~loc:a.loc t a)
      in
      { desc = Variant (ctor, new_args); ty = target; loc = inner.loc }
    | TyRecord (_, t_fields), TyRecord _, Record args ->
      let new_args =
        List.map2_exn args t_fields ~f:(fun a (_, t) -> lower_coerce ~loc:a.loc t a)
      in
      { desc = Record new_args; ty = target; loc = inner.loc }
    | TyVec (_, t), TyVec _, Vec (n, ts) ->
      let new_ts = List.map ts ~f:(fun a -> lower_coerce ~loc:a.loc t a) in
      { desc = Vec (n, new_ts); ty = target; loc = inner.loc }
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
  | Record ts -> { t with desc = Record (List.map ts ~f:rewrite) }
  | Field (inner, f) -> { t with desc = Field (rewrite inner, f) }
  | Variant (ctor, args) -> { t with desc = Variant (ctor, List.map args ~f:rewrite) }
  | Match (scrut, cases) ->
    { t with desc = Match (rewrite scrut, List.map cases ~f:(Tuple2.map_snd ~f:rewrite)) }
  | Tuple ts -> { t with desc = Tuple (List.map ts ~f:rewrite) }
;;

let materialize (Program tops : t) : t =
  let materialize_top (top : top) : top =
    match top.desc with
    | Define (recur, v, bind) -> { top with desc = Define (recur, v, rewrite bind) }
    | Extern _ | TypeDef _ -> top
  in
  Program (List.map tops ~f:materialize_top)
;;
