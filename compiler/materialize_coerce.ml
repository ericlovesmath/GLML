open Core
open Typecheck

(** True when [from_ty] is coercible to [to_ty] under the same rules used by
    [Typecheck.solve] for the [Coerce] constraint. *)
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

let rec has_tyvar = function
  | TyVar _ -> true
  | TyFloat | TyInt | TyBool -> false
  | TyVec (_, t) -> has_tyvar t
  | TyArrow (a, b) -> has_tyvar a || has_tyvar b
  | TyRecord (_, args) | TyVariant (_, args) -> List.exists args ~f:has_tyvar
;;

(** Rewrite [t.ty] to [target] for structural coercions; on a scalar
    [TyInt -> TyFloat] mismatch, wrap in [Promote]. *)
let coerce_top (target : ty) (t : term) : term =
  if equal_ty t.ty target || has_tyvar t.ty || has_tyvar target
  then t
  else if coercible t.ty target
  then (
    match t.ty, target, t.desc with
    | TyInt, TyFloat, Int i ->
      { desc = Float (Float.of_int i); ty = TyFloat; loc = t.loc }
    | TyInt, TyFloat, _ ->
      let inner = { t with ty = TyInt } in
      { desc = Promote inner; ty = TyFloat; loc = t.loc }
    | _ -> { t with ty = target })
  else t
;;

let rec mater ?(expected : ty option) (t : term) : term =
  let target = Option.value expected ~default:t.ty in
  let t = coerce_top target t in
  let target_ty = t.ty in
  let r = mater in
  let none = mater ?expected:None in
  let desc : term_desc =
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) ->
      let elem_exp =
        match target_ty with
        | TyVec (_, e) -> Some e
        | _ -> None
      in
      Vec (n, List.map ts ~f:(fun t -> r ?expected:elem_exp t))
    | Lam (v, body) ->
      let body_exp =
        match target_ty with
        | TyArrow (_, ret) -> Some ret
        | _ -> None
      in
      Lam (v, r ?expected:body_exp body)
    | App (f, x) ->
      let f' = none f in
      let arg_exp =
        match f'.ty with
        | TyArrow (a, _) -> Some a
        | _ -> None
      in
      let x' = r ?expected:arg_exp x in
      App (f', x')
    | Let (recur, v, constrs, bind, body) ->
      Let (recur, v, constrs, none bind, r ~expected:target_ty body)
    | If (c, t, e) ->
      If (r ~expected:TyBool c, r ~expected:target_ty t, r ~expected:target_ty e)
    | Bop (op, l, rt) -> Bop (op, none l, none rt)
    | Index (t, i) -> Index (none t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:none)
    | Record (s, ts) -> Record (s, List.map ts ~f:none)
    | Field (t, f) -> Field (none t, f)
    | Variant (tn, ctor, args) -> Variant (tn, ctor, List.map args ~f:none)
    | Match (scrut, cases) ->
      Match
        ( none scrut
        , List.map cases ~f:(fun (pat, body) -> pat, r ~expected:target_ty body) )
    | Promote inner -> Promote (none inner)
  in
  { t with desc }
;;

let materialize_top (top : top) : top =
  match top.desc with
  | Define (recur, v, bind) ->
    let bind = mater ~expected:top.ty bind in
    { top with desc = Define (recur, v, bind) }
  | Extern _ | TypeDef _ -> top
;;

let materialize (Program tops : t) : t = Program (List.map tops ~f:materialize_top)
