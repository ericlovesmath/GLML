(* TODO: Algebraic identities for bops and prims *)
(* TODO: Vec Broadcasting *)
(* TODO: Index and Field *)

open Core
open Remove_placeholder

module Err = Compiler_error.Pass (struct
    let name = "const_fold"
  end)

type value =
  | Top
  | Const of atom_desc
  | Alias of string

type ctx =
  { env : value String.Map.t
  ; mut : String.Set.t (* names that appear as [Set] targets *)
  }

let bind ctx v value = { ctx with env = Map.set ctx.env ~key:v ~data:value }

(** Walks alias chains and substitutes literals. *)
let rec rewrite_atom (ctx : ctx) (a : atom) : atom =
  match a.desc with
  | Int _ | Float _ | Bool _ -> a
  | Var v ->
    (match Map.find ctx.env v with
     | None | Some Top -> a
     | Some (Const d) -> { a with desc = d }
     | Some (Alias v') when not (String.equal v v') ->
       rewrite_atom ctx { a with desc = Var v' }
     | Some (Alias _) -> a)
;;

let fold_bop (op : Glsl.binary_op) (a : atom_desc) (b : atom_desc) : atom_desc option =
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

let rec collect_set_anf (a : anf) (acc : String.Set.t) : String.Set.t =
  match a.desc with
  | Return t -> collect_set_term t acc
  | Let (_, b, t) -> acc |> collect_set_term b |> collect_set_anf t
  | Placeholder (_, t) -> collect_set_anf t acc
  | While (c, b, tl) ->
    acc |> collect_set_term c |> collect_set_anf b |> collect_set_anf tl
  | Set (v, _, t) -> collect_set_anf t (Set.add acc v)
  | Continue -> acc

and collect_set_term (t : term) (acc : String.Set.t) : String.Set.t =
  match t.desc with
  | If (_, t, e) -> acc |> collect_set_anf t |> collect_set_anf e
  | Switch (_, cases) ->
    List.fold_right cases ~init:acc ~f:(fun (_, a) -> collect_set_anf a)
  | Atom _ | Bop _ | Vec _ | Index _ | Builtin _ | App _ | Record _ | Field _ -> acc
;;

let unexpected_branch (t : term) =
  Err.raise ~loc:t.loc "unexpected branching term" ~d:[%message (t : term)]
;;

let simplify_primitive_term (ctx : ctx) (t : term) : term =
  let r = rewrite_atom ctx in
  let desc =
    match t.desc with
    | Atom a -> Atom (r a)
    | Bop (op, a, b) ->
      let a = r a
      and b = r b in
      (match fold_bop op a.desc b.desc with
       | Some d -> Atom { desc = d; ty = t.ty; loc = t.loc }
       | None -> Bop (op, a, b))
    | Vec (n, atoms) -> Vec (n, List.map atoms ~f:r)
    | Index (a, i) -> Index (r a, i)
    | Builtin (b, atoms) -> Builtin (b, List.map atoms ~f:r)
    | App (n, atoms) -> App (n, List.map atoms ~f:r)
    | Record atoms -> Record (List.map atoms ~f:r)
    | Field (a, name) -> Field (r a, name)
    | If _ | Switch _ -> unexpected_branch t
  in
  { t with desc }
;;

let value_of_term ctx (t : term) : value =
  match t.desc with
  | Atom { desc = (Int _ | Float _ | Bool _) as d; _ } -> Const d
  | Atom { desc = Var v; _ } -> if Set.mem ctx.mut v then Top else Alias v
  | _ -> Top
;;

let rec tail_value ctx (a : anf) : value =
  match a.desc with
  | Return t -> value_of_term ctx t
  | Let (_, _, k) | Placeholder (_, k) | While (_, _, k) | Set (_, _, k) ->
    tail_value ctx k
  | Continue -> Top
;;

(** Rewrite each [Return t] term. *)
let rec splice (a : anf) ~(k : term -> anf) : anf =
  match a.desc with
  | Return t -> k t
  | Let (v, t, tl) -> { a with desc = Let (v, t, splice tl ~k) }
  | Placeholder (v, tl) -> { a with desc = Placeholder (v, splice tl ~k) }
  | While (c, b, tl) -> { a with desc = While (c, b, splice tl ~k) }
  | Set (v, x, tl) -> { a with desc = Set (v, x, splice tl ~k) }
  | Continue -> a
;;

(** [Picked]: scrutinee was a known constant, this arm is taken.
    [Rebuild]: arms were rewritten, emit the branching term as-is. *)
type branch_resolution =
  | Picked of anf
  | Rebuild of term

let rec rewrite_anf (ctx : ctx) (a : anf) : anf =
  match a.desc with
  | Continue -> a
  | Return t -> rewrite_return ctx a t
  | Let (v, t, k) -> rewrite_let ctx a v t k
  | Placeholder (v, k) ->
    { a with desc = Placeholder (v, rewrite_anf (bind ctx v Top) k) }
  | While (cond, body, tl) ->
    let cond = simplify_primitive_term ctx cond in
    { a with desc = While (cond, rewrite_anf ctx body, rewrite_anf ctx tl) }
  | Set (v, x, k) -> { a with desc = Set (v, rewrite_atom ctx x, rewrite_anf ctx k) }

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
       let value = if Set.mem ctx.mut v then Top else tail_value ctx branch in
       let k = rewrite_anf (bind ctx v value) k in
       splice branch ~k:(fun final_t -> { a with desc = Let (v, final_t, k) })
     | Rebuild t -> { a with desc = Let (v, t, rewrite_anf (bind ctx v Top) k) })
  | _ ->
    let t = simplify_primitive_term ctx t in
    let value = if Set.mem ctx.mut v then Top else value_of_term ctx t in
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

let rewrite_top (env : value String.Map.t) (top : top) : top * value String.Map.t =
  let rewrite anf = rewrite_anf { env; mut = collect_set_anf anf String.Set.empty } anf in
  match top.desc with
  | Const (name, anf) ->
    let anf = rewrite anf in
    let env =
      match anf.desc with
      | Return { desc = Atom { desc = (Int _ | Float _ | Bool _) as d; _ }; _ } ->
        Map.set env ~key:name ~data:(Const d)
      | _ -> env
    in
    { top with desc = Const (name, anf) }, env
  | Define { name; args; body; ret_ty } ->
    { top with desc = Define { name; args; body = rewrite body; ret_ty } }, env
  | Extern _ | TypeDef _ -> top, env
;;

let rewrite (Program tops : t) : t =
  let _, tops_rev =
    List.fold tops ~init:(String.Map.empty, []) ~f:(fun (env, acc) top ->
      let top, env = rewrite_top env top in
      env, top :: acc)
  in
  Program (List.rev tops_rev)
;;
