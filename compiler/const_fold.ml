(* TODO: Builtin functions *)

open Core
open Anf

module Err = Compiler_error.Pass (struct
    let name = "const_fold"
  end)

type literal =
  | Int of int
  | Float of float
  | Bool of bool

type value =
  | Top
  | Lit of literal
  | Alias of string
  | Vec of value list
  | Fields of value String.Map.t

type ctx =
  { env : value String.Map.t
  ; records : (string * Lower_variants.ty) list String.Map.t
  }

let bind ctx v value = { ctx with env = Map.set ctx.env ~key:v ~data:value }

let equal_atom (a : atom) (b : atom) : bool =
  match a.desc, b.desc with
  | Var x, Var y -> String.equal x y
  | Float x, Float y -> Float.equal x y
  | Int x, Int y -> Int.equal x y
  | Bool x, Bool y -> Bool.equal x y
  | _ -> false
;;

let equal_lit_float (target : float) (l : literal) : bool =
  match l with
  | Float f -> Float.equal f target
  | Int i -> Float.equal (Float.of_int i) target
  | Bool _ -> false
;;

let lit_of_desc : atom_desc -> literal option = function
  | Int i -> Some (Int i)
  | Float f -> Some (Float f)
  | Bool b -> Some (Bool b)
  | Var _ -> None
;;

let desc_of_lit : literal -> atom_desc = function
  | Int i -> Int i
  | Float f -> Float f
  | Bool b -> Bool b
;;

(** Return the dereferenced atom along with the value bound *)
let resolve (ctx : ctx) (a : atom) : atom * value option =
  match a.desc with
  | Var v ->
    let rec go v =
      match Map.find ctx.env v with
      | Some (Alias v') when not (String.equal v v') -> go v'
      | other -> v, other
    in
    let v, abs = go v in
    { a with desc = Var v }, abs
  | _ -> a, None
;;

let lookup ctx a = snd (resolve ctx a)

(** Walks alias chains and substitutes literals *)
let rewrite_atom ctx a =
  match resolve ctx a with
  | a, Some (Lit l) -> { a with desc = desc_of_lit l }
  | a, _ -> a
;;

let fold_bop (op : Glsl.binary_op) (a : literal) (b : literal) : literal option =
  match op, a, b with
  | Add, Int x, Int y -> Some (Int (x + y))
  | Sub, Int x, Int y -> Some (Int (x - y))
  | Mul, Int x, Int y -> Some (Int (x * y))
  | Div, Int x, Int y when not (Int.equal y 0) -> Some (Int (x / y))
  | Mod, Int x, Int y when not (Int.equal y 0) -> Some (Int (x mod y))
  | Eq, Int x, Int y -> Some (Bool (Int.equal x y))
  | Lt, Int x, Int y -> Some (Bool Int.(x < y))
  | Gt, Int x, Int y -> Some (Bool Int.(x > y))
  | Leq, Int x, Int y -> Some (Bool Int.(x <= y))
  | Geq, Int x, Int y -> Some (Bool Int.(x >= y))
  | Add, Float x, Float y -> Some (Float (x +. y))
  | Sub, Float x, Float y -> Some (Float (x -. y))
  | Mul, Float x, Float y -> Some (Float (x *. y))
  | Div, Float x, Float y when not Float.(equal y 0.0) -> Some (Float (x /. y))
  | Eq, Float x, Float y -> Some (Bool (Float.equal x y))
  | Lt, Float x, Float y -> Some (Bool Float.(x < y))
  | Gt, Float x, Float y -> Some (Bool Float.(x > y))
  | Leq, Float x, Float y -> Some (Bool Float.(x <= y))
  | Geq, Float x, Float y -> Some (Bool Float.(x >= y))
  | And, Bool x, Bool y -> Some (Bool (x && y))
  | Or, Bool x, Bool y -> Some (Bool (x || y))
  | Eq, Bool x, Bool y -> Some (Bool (Bool.equal x y))
  | _ -> None
;;

let lit_of_value = function
  | Lit l -> Some l
  | _ -> None
;;

(** Constant scalar components of [a] if all components are statically known *)
let const_components (ctx : ctx) (a : atom) : literal list option =
  let v =
    match lit_of_desc a.desc with
    | Some l -> Some (Lit l)
    | None -> lookup ctx a
  in
  match v with
  | Some (Lit l) -> Some [ l ]
  | Some (Vec cs) -> Option.all (List.map cs ~f:lit_of_value)
  | _ -> None
;;

let broadcast xs ys =
  match xs, ys with
  | [ x ], ys -> Some (List.map ys ~f:(fun y -> x, y))
  | xs, [ y ] -> Some (List.map xs ~f:(fun x -> x, y))
  | xs, ys ->
    (match List.zip xs ys with
     | Ok zip -> Some zip
     | Unequal_lengths -> None)
;;

let unexpected_branch (t : term) =
  Err.raise ~loc:t.loc "unexpected branching term" ~d:[%message (t : term)]
;;

let atom_of_lit (l : literal) ~ty ~loc : atom = { desc = desc_of_lit l; ty; loc }

let inner_ty_of (t : term) =
  match t.ty with
  | TyVec (_, inner) -> inner
  | other -> other
;;

let const_all_eq_float ctx (a : atom) (target : float) : bool =
  Option.value_map
    (const_components ctx a)
    ~default:false
    ~f:(List.for_all ~f:(equal_lit_float target))
;;

(** Splat constant [k] across the scalar/vector shape of [t] *)
let splat (t : term) (k : float) : term_desc option =
  let scalar : Lower_variants.ty -> atom_desc option = function
    | TyFloat -> Some (Float k)
    | TyInt -> Some (Int (Float.to_int k))
    | _ -> None
  in
  let loc = t.loc in
  match t.ty with
  | (TyFloat | TyInt) as ty ->
    Option.map (scalar ty) ~f:(fun d -> Atom { desc = d; ty; loc })
  | TyVec (n, inner) ->
    Option.map (scalar inner) ~f:(fun d ->
      let a : atom = { desc = d; ty = inner; loc } in
      (Vec (n, List.init n ~f:(Fn.const a)) : term_desc))
  | _ -> None
;;

(** Literal-fold a fully-constant Bop, broadcasting as needed. *)
let try_fold_bop ctx (t : term) : term_desc option =
  let open Option.Let_syntax in
  match t.desc with
  | Bop (op, a, b) ->
    let%bind xs = const_components ctx a in
    let%bind ys = const_components ctx b in
    let%bind pairs = broadcast xs ys in
    let%map ls = List.map pairs ~f:(fun (x, y) -> fold_bop op x y) |> Option.all in
    (match ls with
     | [ l ] -> Atom (atom_of_lit l ~ty:t.ty ~loc:t.loc)
     | ls ->
       let atoms = List.map ls ~f:(atom_of_lit ~ty:(inner_ty_of t) ~loc:t.loc) in
       Vec (List.length ls, atoms))
  | _ -> None
;;

(** Algebraic identities on Bop *)
let try_identity_bop ctx (t : term) : term_desc option =
  match t.desc with
  | Bop (op, a, b) ->
    let is k x = const_all_eq_float ctx x k in
    let bool_atom b : atom = { desc = Bool b; ty = t.ty; loc = t.loc } in
    (match op, a.desc, b.desc with
     | Add, _, _ when is 0. b -> Some (Atom a)
     | Add, _, _ when is 0. a -> Some (Atom b)
     | Sub, _, _ when is 0. b -> Some (Atom a)
     | Sub, _, _ when equal_atom a b -> splat t 0.
     | Mul, _, _ when is 1. b -> Some (Atom a)
     | Mul, _, _ when is 1. a -> Some (Atom b)
     | Mul, _, _ when is 0. a || is 0. b -> splat t 0.
     | Div, _, _ when is 1. b -> Some (Atom a)
     | Div, _, _ when equal_atom a b -> splat t 1.
     | Eq, _, _ when equal_atom a b -> Some (Atom (bool_atom true))
     | And, Bool true, _ -> Some (Atom b)
     | And, _, Bool true -> Some (Atom a)
     | And, Bool false, _ | And, _, Bool false -> Some (Atom (bool_atom false))
     | Or, Bool false, _ -> Some (Atom b)
     | Or, _, Bool false -> Some (Atom a)
     | Or, Bool true, _ | Or, _, Bool true -> Some (Atom (bool_atom true))
     | _ -> None)
  | _ -> None
;;

let try_fold_builtin ctx (t : term) : term_desc option =
  match t.desc with
  | Builtin (b, args) ->
    let is k x = const_all_eq_float ctx x k in
    (match b, args with
     | Float, [ a ] ->
       (match const_components ctx a with
        | Some [ Int n ] ->
          Some (Atom (atom_of_lit (Float (Float.of_int n)) ~ty:t.ty ~loc:t.loc))
        | _ -> None)
     | Pow, [ _; e ] when is 0. e -> splat t 1.
     | Pow, [ x; e ] when is 1. e -> Some (Atom x)
     | Pow, [ x; e ] when is 2. e -> Some (Bop (Mul, x, x))
     | Pow, [ x; e ] when is 0.5 e -> Some (Builtin (Sqrt, [ x ]))
     | Sin, [ a ] when is 0. a -> splat t 0.
     | Cos, [ a ] when is 0. a -> splat t 1.
     | (Min | Max), [ a; b ] when equal_atom a b -> Some (Atom a)
     | _ -> None)
  | _ -> None
;;

let try_project ctx (t : term) : term_desc option =
  let open Option.Let_syntax in
  let project (a : atom) ~(select : value -> value option) =
    let%bind v = lookup ctx a in
    let%bind sub = select v in
    let atom desc : atom = { desc; ty = t.ty; loc = t.loc } in
    match sub with
    | Lit l -> Some (Atom (atom (desc_of_lit l)))
    | Alias v -> Some (Atom (atom (Var v)))
    | Vec cs ->
      let%map ls = List.map cs ~f:lit_of_value |> Option.all in
      let atoms = List.map ls ~f:(atom_of_lit ~ty:(inner_ty_of t) ~loc:t.loc) in
      (Vec (List.length atoms, atoms) : term_desc)
    | Top | Fields _ -> None
  in
  match t.desc with
  | Index (a, i) ->
    project a ~select:(function
      | Vec cs -> List.nth cs i
      | _ -> None)
  | Field (a, name) ->
    project a ~select:(function
      | Fields fs -> Map.find fs name
      | _ -> None)
  | _ -> None
;;

(** Rewrite atoms in place using [ctx]; pure mechanical substitution. *)
let normalize_atoms ctx (t : term) : term =
  let rw = rewrite_atom ctx in
  let desc =
    match t.desc with
    | Atom a -> Atom (rw a)
    | Vec (n, atoms) -> Vec (n, List.map atoms ~f:rw)
    | Builtin (b, atoms) -> Builtin (b, List.map atoms ~f:rw)
    | App (n, atoms) -> App (n, List.map atoms ~f:rw)
    | Record atoms -> Record (List.map atoms ~f:rw)
    | Bop (op, a, b) -> Bop (op, rw a, rw b)
    | Index (a, i) -> Index (rw a, i)
    | Field (a, n) -> Field (rw a, n)
    | If _ | Switch _ -> unexpected_branch t
  in
  { t with desc }
;;

let simplify_primitive_term ctx (t : term) : term =
  let t = normalize_atoms ctx t in
  let simplified =
    match t.desc with
    | Bop _ -> Option.first_some (try_fold_bop ctx t) (try_identity_bop ctx t)
    | Builtin _ -> try_fold_builtin ctx t
    | Index _ | Field _ -> try_project ctx t
    | Atom _ | Vec _ | App _ | Record _ -> None
    | If _ | Switch _ -> unexpected_branch t
  in
  Option.value_map simplified ~default:t ~f:(fun desc -> { t with desc })
;;

let abs_of_atom (a : atom) : value =
  match a.desc with
  | Var v -> Alias v
  | Int i -> Lit (Int i)
  | Float f -> Lit (Float f)
  | Bool b -> Lit (Bool b)
;;

let abs_of_term ctx (t : term) : value =
  match t.desc with
  | Atom a -> abs_of_atom a
  | Vec (_, atoms) -> Vec (List.map atoms ~f:abs_of_atom)
  | Record atoms ->
    (match t.ty with
     | TyRecord s ->
       (match Map.find ctx.records s with
        | None -> Top
        | Some fields ->
          (match List.zip (List.map ~f:fst fields) (List.map ~f:abs_of_atom atoms) with
           | Unequal_lengths -> Err.raise "record literal arity mismatch" ~loc:t.loc
           | Ok pairs -> String.Map.of_alist_or_error pairs)
          |> Err.of_or_error ~loc:t.loc
          |> Err.ok_exn
          |> fun m -> Fields m)
     | _ -> Err.raise "Record does not have type record" ~loc:t.loc)
  | Field (a, name) ->
    lookup ctx a
    |> Option.bind ~f:(function
      | Fields fs -> Map.find fs name
      | _ -> None)
    |> Option.value ~default:Top
  | Index (a, i) ->
    lookup ctx a
    |> Option.bind ~f:(function
      | Vec cs -> List.nth cs i
      | _ -> None)
    |> Option.value ~default:Top
  | _ -> Top
;;

let rec tail_abs ctx (a : anf) : value =
  match a.desc with
  | Return t -> abs_of_term ctx t
  | Let (_, _, k) -> tail_abs ctx k
;;

(** Resolve every [Alias _] through [env]; returns a value with no free names *)
let rec ground (ctx : ctx) (v : value) : value =
  match v with
  | Top -> Top
  | Lit _ as l -> l
  | Alias v ->
    (match Map.find ctx.env v with
     | None -> Top
     | Some v -> ground ctx v)
  | Vec cs -> Vec (List.map cs ~f:(ground ctx))
  | Fields fs -> Fields (Map.map fs ~f:(ground ctx))
;;

(** Rewrite each [Return t] term. *)
let rec splice (a : anf) ~(k : term -> anf) : anf =
  match a.desc with
  | Return t -> k t
  | Let (v, t, tl) -> { a with desc = Let (v, t, splice tl ~k) }
;;

(** [Picked]: scrutinee was a known constant, this arm is taken.
    [Rebuild]: arms were rewritten, emit the branching term as-is. *)
type branch_resolution =
  | Picked of anf
  | Rebuild of term

let rec rewrite_anf (ctx : ctx) (a : anf) : anf =
  match a.desc with
  | Return t -> rewrite_return ctx a t
  | Let (v, t, k) -> rewrite_let ctx a v t k

and rewrite_return ctx a t =
  match t.desc with
  | If _ | Switch _ ->
    (match resolve_branch ctx t with
     | Picked branch -> rewrite_anf ctx branch
     | Rebuild t -> { a with desc = Return t })
  | _ -> { a with desc = Return (simplify_primitive_term ctx t) }

and rewrite_let ctx a v t k =
  match t.desc with
  | If _ | Switch _ ->
    (match resolve_branch ctx t with
     | Picked branch ->
       let branch = rewrite_anf ctx branch in
       let value = tail_abs ctx branch in
       let k = rewrite_anf (bind ctx v value) k in
       splice branch ~k:(fun final_t -> { a with desc = Let (v, final_t, k) })
     | Rebuild t -> { a with desc = Let (v, t, rewrite_anf (bind ctx v Top) k) })
  | _ ->
    let t = simplify_primitive_term ctx t in
    let value = abs_of_term ctx t in
    { a with desc = Let (v, t, rewrite_anf (bind ctx v value) k) }

and resolve_branch ctx (t : term) : branch_resolution =
  match t.desc with
  | If (c, tr, e) ->
    let c = rewrite_atom ctx c in
    (match c.desc with
     | Bool true -> Picked tr
     | Bool false -> Picked e
     | _ -> Rebuild { t with desc = If (c, rewrite_anf ctx tr, rewrite_anf ctx e) })
  | Switch (s, cases) ->
    let s = rewrite_atom ctx s in
    let picked =
      match s.desc with
      | Int n ->
        List.find_map cases ~f:(function
          | Case n', a when Int.equal n' n -> Some a
          | Default, a -> Some a
          | _ -> None)
      | _ -> None
    in
    (match picked with
     | Some branch -> Picked branch
     | None ->
       let cases = List.map cases ~f:(fun (lbl, c) -> lbl, rewrite_anf ctx c) in
       Rebuild { t with desc = Switch (s, cases) })
  | _ -> unexpected_branch t
;;

let rewrite_top
      (records : (string * Lower_variants.ty) list String.Map.t)
      (env : value String.Map.t)
      (top : top)
  : top * value String.Map.t
  =
  let rewrite anf = rewrite_anf { env; records } anf in
  match top.desc with
  | Const (name, anf) ->
    let anf = rewrite anf in
    let env =
      let rec tail_in local_env (a : anf) : value =
        let ctx = { env = local_env; records } in
        match a.desc with
        | Return t -> ground ctx (abs_of_term ctx t)
        | Let (v, t, k) ->
          let value = abs_of_term ctx t in
          tail_in (Map.set local_env ~key:v ~data:value) k
      in
      match tail_in env anf with
      | Top -> env
      | grounded -> Map.set env ~key:name ~data:grounded
    in
    { top with desc = Const (name, anf) }, env
  | Define { name; recur; args; body; ret_ty } ->
    { top with desc = Define { name; recur; args; body = rewrite body; ret_ty } }, env
  | Extern _ | TypeDef _ -> top, env
;;

let collect_records (tops : top list) : (string * Lower_variants.ty) list String.Map.t =
  List.filter_map tops ~f:(fun top ->
    match top.desc with
    | TypeDef (name, RecordDecl fields) -> Some (name, fields)
    | _ -> None)
  |> String.Map.of_alist_or_error
  |> Err.of_or_error
  |> Err.ok_exn
;;

let rewrite (Program tops : t) : t =
  let records = collect_records tops in
  let _, tops_rev =
    List.fold tops ~init:(String.Map.empty, []) ~f:(fun (env, acc) top ->
      let top, env = rewrite_top records env top in
      env, top :: acc)
  in
  Program (List.rev tops_rev)
;;
