open Core
open Compiler_error.Let_syntax
open Type_system

module Err = Compiler_error.Pass (struct
    let name = "constraint solver"
  end)

(** Unify two types into a substitution *)
let rec unify (con : (Lexer.loc * ty * ty) list) : substitution Compiler_error.t =
  match con with
  | [] -> return []
  | (loc, TyVar v, ty) :: con | (loc, ty, TyVar v) :: con ->
    let rec occurs_in = function
      | TyVar v' -> String.equal v v'
      | TyFloat | TyInt | TyBool -> false
      | TyVec (_, t) -> occurs_in t
      | TyRecord (_, fields) -> List.exists fields ~f:(fun (_, t) -> occurs_in t)
      | TyVariant (_, ctors) ->
        List.exists ctors ~f:(fun (_, ts) -> List.exists ts ~f:occurs_in)
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
  | (loc, TyVec (n, t), TyVec (n', t')) :: con when n = n' -> unify ((loc, t, t') :: con)
  | (loc, TyRecord (_, fs), TyRecord (_, fs')) :: con
    when List.length fs = List.length fs' ->
    unify (List.map2_exn fs fs' ~f:(fun (_, t) (_, t') -> loc, t, t') @ con)
  | (loc, TyVariant (_, cs), TyVariant (_, cs')) :: con
    when List.length cs = List.length cs' ->
    let pairs =
      List.map2_exn cs cs' ~f:(fun (_, ts) (_, ts') ->
        List.map2_exn ts ts' ~f:(Tuple3.create loc))
      |> List.concat
    in
    unify (pairs @ con)
  | (loc, ty, ty') :: con ->
    if equal_ty ty ty'
    then unify con
    else Err.fail "type mismatch" ~loc ~d:[%message (ty : ty) (ty' : ty)]
;;

(** int <: float subtyping to make canonical type *)
let rec widen_numeric = function
  | TyInt -> TyFloat
  | TyVec (n, t) -> TyVec (n, widen_numeric t)
  | TyRecord (hint, fields) ->
    TyRecord (hint, List.map fields ~f:(fun (n, t) -> n, widen_numeric t))
  | TyVariant (hint, ctors) ->
    TyVariant (hint, List.map ctors ~f:(fun (n, ts) -> n, List.map ts ~f:widen_numeric))
  | TyArrow (a, b) -> TyArrow (widen_numeric a, widen_numeric b)
  | t -> t
;;

let is_scalar = function
  | TyFloat | TyInt -> true
  | _ -> false
;;

(** Validate if a concrete type belongs to a GLSL typeclass. Numeric classes
    accept ints anywhere a float is expected, so we widen first and the patterns
    only need to mention TyFloat.

    [GenIType]/[GenBType] are int and bool specific so we skip widening. *)
let check_class (cls : type_class) (ty : ty) : bool =
  let ty =
    match cls with
    | GenIType | GenBType -> ty
    | _ -> widen_numeric ty
  in
  match cls, ty with
  | GenType, TyFloat
  | GenType, TyVec (_, TyFloat)
  | GenBType, TyBool
  | GenIType, TyInt
  | MatType, TyVec (_, TyVec (_, TyFloat))
  | Numeric, TyFloat
  | Numeric, TyVec (_, TyFloat)
  | Numeric, TyVec (_, TyVec (_, TyFloat))
  | Comparable, TyFloat
  | Equatable, (TyFloat | TyBool)
  | Equatable, TyVec (_, TyFloat)
  | Equatable, TyVec (_, TyVec (_, TyFloat)) -> true
  | _, _ -> false
;;

(** Resolve GLSL overloading constraints using concrete types. *)
let resolve_constraints (constrs : constr list)
  : (constr list * (Lexer.loc * ty * ty) list) Compiler_error.t
  =
  let rec aux deferred eqs (constrs : constr list) =
    match constrs with
    | [] -> return (List.rev deferred, List.rev eqs)
    | { desc = Eq (l, r); loc } :: rest -> aux deferred ((loc, l, r) :: eqs) rest
    | ({ desc = HasClass (cls, ty); loc } as c) :: rest ->
      if not (Set.is_empty (ftv_of_ty ty))
      then aux (c :: deferred) eqs rest
      else if check_class cls ty
      then aux deferred eqs rest
      else
        Err.fail "class constraint failed" ~loc ~d:[%message (cls : type_class) (ty : ty)]
    | ({ desc = Broadcast (l, r, ret); loc } as c) :: rest ->
      let mk desc = { desc; loc } in
      let broadcast_vec_scalar n t s =
        let bt = fresh_tyvar () in
        aux deferred ((loc, ret, TyVec (n, bt)) :: eqs) (mk (Broadcast (s, t, bt)) :: rest)
      in
      let coerce_pair a b =
        aux deferred eqs (mk (Coerce (a, ret)) :: mk (Coerce (b, ret)) :: rest)
      in
      (match l, r with
       | TyVar a, TyVar b when String.equal a b ->
         aux (c :: deferred) ((loc, ret, l) :: eqs) rest
       | TyVec (n, t), TyVec (n', t') when n = n' ->
         let bt = fresh_tyvar () in
         aux
           deferred
           ((loc, ret, TyVec (n, bt)) :: eqs)
           (mk (Broadcast (t, t', bt)) :: rest)
       | TyVec (n, t), s when is_scalar s -> broadcast_vec_scalar n t s
       | s, TyVec (n, t) when is_scalar s -> broadcast_vec_scalar n t s
       | _, _ when is_scalar l && is_scalar r -> coerce_pair l r
       | (TyVar _ as v), s
         when is_scalar s && Set.is_empty (ftv_of_ty ret) && is_scalar ret ->
         coerce_pair v s
       | s, (TyVar _ as v)
         when is_scalar s && Set.is_empty (ftv_of_ty ret) && is_scalar ret ->
         coerce_pair s v
       | TyVar _, _ | _, TyVar _ -> aux (c :: deferred) eqs rest
       | _ -> Err.fail "invalid broadcast" ~loc ~d:[%message (l : ty) (r : ty)])
    | ({ desc = MulBroadcast (l, r, ret); loc } as c) :: rest ->
      let mk desc = { desc; loc } in
      let mul_mat_scalar n m t s =
        let bt = fresh_tyvar () in
        aux
          deferred
          ((loc, ret, TyVec (n, TyVec (m, bt))) :: eqs)
          (mk (MulBroadcast (s, t, bt)) :: rest)
      in
      let mul_vec_scalar n t s =
        let bt = fresh_tyvar () in
        aux
          deferred
          ((loc, ret, TyVec (n, bt)) :: eqs)
          (mk (MulBroadcast (s, t, bt)) :: rest)
      in
      let coerce_pair a b =
        aux deferred eqs (mk (Coerce (a, ret)) :: mk (Coerce (b, ret)) :: rest)
      in
      (match l, r with
       | TyVar a, TyVar b when String.equal a b ->
         aux (c :: deferred) ((loc, ret, l) :: eqs) rest
       | TyVec (n, TyVec (m, t)), TyVec (n', TyVec (m', t')) when n = n' && m = m' ->
         let bt = fresh_tyvar () in
         aux
           deferred
           ((loc, ret, TyVec (n, TyVec (m, bt))) :: eqs)
           (mk (MulBroadcast (t, t', bt)) :: rest)
       | TyVec (n, TyVec (m, t)), s when is_scalar s -> mul_mat_scalar n m t s
       | s, TyVec (n, TyVec (m, t)) when is_scalar s -> mul_mat_scalar n m t s
       | TyVec (cols, TyVec (rows, t)), TyVec (rows', t') when rows = rows' ->
         let bt = fresh_tyvar () in
         aux
           deferred
           ((loc, ret, TyVec (cols, bt)) :: eqs)
           (mk (MulBroadcast (t, t', bt)) :: rest)
       | TyVec (cols, t), TyVec (cols', TyVec (rows, t')) when cols = cols' ->
         let bt = fresh_tyvar () in
         aux
           deferred
           ((loc, ret, TyVec (rows, bt)) :: eqs)
           (mk (MulBroadcast (t, t', bt)) :: rest)
       | TyVec (n, t), TyVec (n', t') when n = n' ->
         let bt = fresh_tyvar () in
         aux
           deferred
           ((loc, ret, TyVec (n, bt)) :: eqs)
           (mk (MulBroadcast (t, t', bt)) :: rest)
       | TyVec (n, t), s when is_scalar s -> mul_vec_scalar n t s
       | s, TyVec (n, t) when is_scalar s -> mul_vec_scalar n t s
       | _, _ when is_scalar l && is_scalar r -> coerce_pair l r
       | (TyVar _ as v), s
         when is_scalar s && Set.is_empty (ftv_of_ty ret) && is_scalar ret ->
         coerce_pair v s
       | s, (TyVar _ as v)
         when is_scalar s && Set.is_empty (ftv_of_ty ret) && is_scalar ret ->
         coerce_pair s v
       | TyVar _, _ | _, TyVar _ -> aux (c :: deferred) eqs rest
       | _ -> Err.fail "invalid mul/div broadcast" ~loc ~d:[%message (l : ty) (r : ty)])
    | ({ desc = IndexAccess (t, i, ret); loc } as c) :: rest ->
      (match t with
       | TyVec (n, elem_ty) ->
         let scalar_ty = widen_numeric elem_ty in
         if 0 <= i && i < n
         then aux deferred ((loc, ret, scalar_ty) :: eqs) rest
         else Err.fail "vec index out of bounds" ~loc ~d:[%message (n : int) (i : int)]
       | TyVar _ -> aux (c :: deferred) eqs rest
       | ty -> Err.fail "expected vec" ~loc ~d:[%message (ty : ty)])
    | ({ desc = FieldAccess (ty, f, ret); loc } as c) :: rest ->
      (match ty with
       | TyVar _ -> aux (c :: deferred) eqs rest
       | TyRecord (_, fields) ->
         (match List.Assoc.find fields ~equal:String.equal f with
          | None -> Err.fail "field not found in record" ~loc ~d:[%message (f : string)]
          | Some field_ty -> aux deferred ((loc, ret, field_ty) :: eqs) rest)
       | ty -> Err.fail "field access on non-record type" ~loc ~d:[%message (ty : ty)])
    | ({ desc = Coerce (from_ty, to_ty); loc } as c) :: rest ->
      let mk desc = { desc; loc } in
      if equal_ty from_ty to_ty
      then aux deferred eqs rest
      else (
        match from_ty, to_ty with
        | TyInt, TyFloat -> aux deferred eqs rest
        | TyArrow (p, r), TyArrow (p', r') ->
          aux deferred eqs (mk (Coerce (p', p)) :: mk (Coerce (r, r')) :: rest)
        | TyVec (n, t), TyVec (n', t') when n = n' ->
          aux deferred eqs (mk (Coerce (t, t')) :: rest)
        | TyRecord (_, fs), TyRecord (_, fs') when List.length fs = List.length fs' ->
          aux
            deferred
            eqs
            (List.map2_exn fs fs' ~f:(fun (_, a) (_, b) -> mk (Coerce (a, b))) @ rest)
        | TyVariant (_, cs), TyVariant (_, cs') when List.length cs = List.length cs' ->
          let coerces =
            List.map2_exn cs cs' ~f:(fun (_, ts) (_, ts') ->
              List.map2_exn ts ts' ~f:(fun a b -> mk (Coerce (a, b))))
            |> List.concat
          in
          aux deferred eqs (coerces @ rest)
        | (TyRecord _ | TyVariant _), (TyRecord _ | TyVariant _) ->
          aux deferred ((loc, from_ty, to_ty) :: eqs) rest
        | TyVar _, _ | _, TyVar _ ->
          (* NOTE: When we have a [TyVar], defer for the LUB-resolution phase.
             We SHOULD NOT eagerly unify here, premature unification can lock
             a [var] to [int] when it later needs to be [float]. *)
          aux (c :: deferred) eqs rest
        | _ -> aux deferred ((loc, from_ty, to_ty) :: eqs) rest)
  in
  aux [] [] constrs
;;

(** Join concrete types under int <: float lattice *)
let rec join_ty (a : ty) (b : ty) : ty option =
  if equal_ty a b
  then Some a
  else (
    match a, b with
    | TyInt, TyFloat | TyFloat, TyInt -> Some TyFloat
    | TyVec (n, t), TyVec (n', t') when n = n' ->
      Option.map (join_ty t t') ~f:(fun t -> TyVec (n, t))
    | TyRecord (h1, fs), TyRecord (h2, fs') when List.length fs = List.length fs' ->
      List.map2_exn fs fs' ~f:(fun (n, t) (_, t') ->
        Option.map (join_ty t t') ~f:(fun jt -> n, jt))
      |> Option.all
      |> Option.map ~f:(fun fields -> TyRecord (merge_hint h1 h2, fields))
    | TyVariant (h1, cs), TyVariant (h2, cs') when List.length cs = List.length cs' ->
      List.map2_exn cs cs' ~f:(fun (n, ts) (_, ts') ->
        if List.length ts <> List.length ts'
        then None
        else
          List.map2_exn ts ts' ~f:join_ty |> Option.all |> Option.map ~f:(fun ts -> n, ts))
      |> Option.all
      |> Option.map ~f:(fun ctors -> TyVariant (merge_hint h1 h2, ctors))
    | _ -> None)
;;

let lub_of (tys : ty list) : ty option =
  match tys with
  | [] -> None
  | t :: rest ->
    List.fold_right rest ~init:(Some t) ~f:(fun t -> Option.bind ~f:(join_ty t))
;;

(** Look at the deferred [Coerce] constraints and try to resolve any tyvar
    whose lower bounds are all concrete: bind it to the LUB of its lowers.
    A tyvar with only concrete upper bounds is bound to its single upper. *)
let resolve_subtype_bounds (deferred : constr list) : substitution =
  let add_to map v t = Map.update map v ~f:(fun e -> t :: Option.value e ~default:[]) in
  let add_edge map ~from_v ~to_v =
    Map.update map to_v ~f:(fun e ->
      Set.add (Option.value e ~default:String.Set.empty) from_v)
  in
  (* edges_into[b] = set of a such that a <: b. Concrete lowers of a flow to b. *)
  let lowers, uppers, edges_into =
    List.fold
      deferred
      ~init:(String.Map.empty, String.Map.empty, String.Map.empty)
      ~f:(fun ((l, u, e) as acc) c ->
        match c.desc with
        | Coerce (TyVar a, TyVar b) -> l, u, add_edge e ~from_v:a ~to_v:b
        | Coerce (t, TyVar a) when Set.is_empty (ftv_of_ty t) -> add_to l a t, u, e
        | Coerce (TyVar a, t) when Set.is_empty (ftv_of_ty t) -> l, add_to u a t, e
        | _ -> acc)
  in
  let step lowers =
    Map.fold edges_into ~init:lowers ~f:(fun ~key:to_v ~data:froms acc ->
      Set.fold froms ~init:acc ~f:(fun acc from_v ->
        match Map.find acc from_v with
        | None -> acc
        | Some lows ->
          let existing = Map.find acc to_v |> Option.value ~default:[] in
          let new_lows =
            List.filter lows ~f:(fun l -> not (List.exists existing ~f:(equal_ty l)))
          in
          if List.is_empty new_lows
          then acc
          else Map.set acc ~key:to_v ~data:(new_lows @ existing)))
  in
  let rec fix lowers =
    let lowers' = step lowers in
    let equal_lowers = String.Map.equal (List.equal equal_ty) in
    if equal_lowers lowers lowers' then lowers else fix lowers'
  in
  let lowers = fix lowers in
  Set.union (Map.key_set lowers) (Map.key_set uppers)
  |> Set.to_list
  |> List.filter_map ~f:(fun v ->
    match Map.find lowers v, Map.find uppers v with
    | Some lows, _ ->
      let promote_vec_lb = function
        | TyVec (n, t) -> TyVec (n, widen_numeric t)
        | t -> t
      in
      let lows = List.map lows ~f:promote_vec_lb in
      (match lub_of lows with
       | Some t -> Some (v, t)
       | None -> List.hd lows |> Option.map ~f:(fun t -> v, t))
    | None, Some us ->
      us
      |> List.dedup_and_sort ~compare:compare_ty
      |> List.hd
      |> Option.map ~f:(Tuple2.create v)
    | None, _ -> None)
;;

(** Solve a set of constraints to produce a substitution and deferred constraints. *)
let solve (constrs : constr list) : (substitution * constr list) Compiler_error.t =
  let apply_sub sub new_sub deferred =
    let sub = List.map sub ~f:(fun (v, t) -> v, subst_ty new_sub t) @ new_sub in
    let deferred = subst_constraints new_sub deferred in
    sub, deferred
  in
  let rec go sub constrs =
    let%bind deferred, eqs = resolve_constraints constrs in
    let%bind new_sub = if List.is_empty eqs then return [] else unify eqs in
    if List.is_empty new_sub
    then (
      let lub_sub = resolve_subtype_bounds deferred in
      if List.is_empty lub_sub
      then return (sub, deferred)
      else (
        let sub, deferred = apply_sub sub lub_sub deferred in
        go sub deferred))
    else (
      let sub, deferred = apply_sub sub new_sub deferred in
      go sub deferred)
  in
  go [] constrs
;;

let solve_scheme constrs sub =
  if List.is_empty constrs
  then return sub
  else (
    let constrs = subst_constraints sub constrs in
    let%map sub', _ = solve constrs in
    List.map sub ~f:(fun (v, t) -> v, subst_ty sub' t) @ sub')
;;
