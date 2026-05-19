(* TODO: Algebraic identities for bops and prims *)
(* TODO: Builtin functions *)

open Core
open Remove_placeholder

module Err = Compiler_error.Pass (struct
    let name = "const_fold"
  end)

type value =
  | Top
  | Const of atom_desc
  | Alias of string
  | VecLit of atom list
  | RecordLit of atom String.Map.t

type ctx =
  { env : value String.Map.t
  ; mut : String.Set.t (* names that appear as [Set] targets *)
  ; records : (string * Monomorphize.ty) list String.Map.t
  }

let bind ctx v value = { ctx with env = Map.set ctx.env ~key:v ~data:value }

(** [a] rewritten to point at the terminal name and the value bound there, if any *)
let resolve (ctx : ctx) (a : atom) : atom * value option =
  match a.desc with
  | Var v ->
    let rec go v =
      match Map.find ctx.env v with
      | Some (Alias v') when not (String.equal v v') -> go v'
      | other -> v, other
    in
    let v, value = go v in
    { a with desc = Var v }, value
  | _ -> a, None
;;

let lookup_value ctx a = snd (resolve ctx a)

(** Walks alias chains and substitutes literals. *)
let rewrite_atom ctx a =
  match resolve ctx a with
  | a, Some (Const d) -> { a with desc = d }
  | a, _ -> a
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

let lit_of_desc = function
  | (Int _ | Float _ | Bool _) as d -> Some d
  | Var _ -> None
;;

(** Constant component descs of [a] if all components are statically known *)
let const_components (ctx : ctx) (a : atom) : atom_desc list option =
  match lit_of_desc a.desc with
  | Some d -> Some [ d ]
  | None ->
    (match lookup_value ctx a with
     | Some (Const d) -> Some [ d ]
     | Some (VecLit atoms) -> Option.all (List.map atoms ~f:(fun a -> lit_of_desc a.desc))
     | _ -> None)
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

let try_fold_bop ctx op (a : atom) (b : atom) (t : term) : term_desc option =
  let open Option.Let_syntax in
  let%bind xs = const_components ctx a in
  let%bind ys = const_components ctx b in
  let%bind pairs = broadcast xs ys in
  let%bind ds = List.map pairs ~f:(fun (x, y) -> fold_bop op x y) |> Option.all in
  match ds with
  | [ desc ] -> Some (Atom { desc; ty = t.ty; loc = t.loc })
  | ds ->
    let ty =
      match t.ty with
      | TyVec (_, inner) -> inner
      | other -> other
    in
    let atoms = List.map ds ~f:(fun desc -> ({ desc; ty; loc = t.loc } : atom)) in
    Some (Vec (List.length ds, atoms))
;;

let try_fold_builtin ctx (b : Glsl.builtin) (args : atom list) (t : term)
  : term_desc option
  =
  let open Option.Let_syntax in
  match b, args with
  | Float, [ a ] ->
    (match%bind const_components ctx a with
     | [ Int n ] -> Some (Atom { desc = Float (Float.of_int n); ty = t.ty; loc = t.loc })
     | _ -> None)
  | _ -> None
;;

(** Looks up [a]'s structured value and projects out a sub-atom *)
let try_project ctx (a : atom) (t : term) ~f : term_desc option =
  let open Option.Let_syntax in
  let%bind v = lookup_value ctx a in
  let%bind (sub : atom) = f v in
  Some (Atom { desc = sub.desc; loc = t.loc; ty = t.ty })
;;

let simplify_primitive_term (ctx : ctx) (t : term) : term =
  let rewrite = rewrite_atom ctx in
  let ( <|> ) opt default = Option.value opt ~default in
  let desc =
    match t.desc with
    | Atom a -> Atom (rewrite a)
    | Vec (n, atoms) -> Vec (n, List.map atoms ~f:rewrite)
    | Builtin (b, atoms) ->
      let atoms = List.map atoms ~f:rewrite in
      try_fold_builtin ctx b atoms t <|> Builtin (b, atoms)
    | App (n, atoms) -> App (n, List.map atoms ~f:rewrite)
    | Record atoms -> Record (List.map atoms ~f:rewrite)
    | Bop (op, a, b) ->
      let a, b = rewrite a, rewrite b in
      try_fold_bop ctx op a b t <|> Bop (op, a, b)
    | Index (a, i) ->
      let a = rewrite a in
      try_project ctx a t ~f:(function
        | VecLit atoms -> List.nth atoms i
        | _ -> None)
      <|> Index (a, i)
    | Field (a, name) ->
      let a = rewrite a in
      try_project ctx a t ~f:(function
        | RecordLit fields -> Map.find fields name
        | _ -> None)
      <|> Field (a, name)
    | If _ | Switch _ -> unexpected_branch t
  in
  { t with desc }
;;

let value_of_term ctx (t : term) : value =
  match t.desc with
  | Atom { desc = (Int _ | Float _ | Bool _) as d; _ } -> Const d
  | Atom { desc = Var v; _ } -> if Set.mem ctx.mut v then Top else Alias v
  | Vec (_, atoms) -> VecLit atoms
  | Record atoms ->
    (match t.ty with
     | TyRecord s ->
       (match Map.find ctx.records s with
        | None -> Top
        | Some fields ->
          (match List.map2 fields atoms ~f:(fun (name, _) a -> name, a) with
           | Unequal_lengths -> Err.raise "record literal arity mismatch" ~loc:t.loc
           | Ok pairs -> String.Map.of_alist_or_error pairs)
          |> Err.of_or_error ~loc:t.loc
          |> Err.ok_exn
          |> fun m -> RecordLit m)
     | _ -> Err.raise "Record does not have type record" ~loc:t.loc)
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

let rewrite_top
      (records : (string * Monomorphize.ty) list String.Map.t)
      (env : value String.Map.t)
      (top : top)
  : top * value String.Map.t
  =
  let rewrite anf =
    rewrite_anf { env; mut = collect_set_anf anf String.Set.empty; records } anf
  in
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

let collect_records (tops : top list) : (string * Monomorphize.ty) list String.Map.t =
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
