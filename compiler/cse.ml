open Core
open Anf

(** Hashable representation of [atom] and [t] ignoring [loc] and [ty],
    so that [same hash = common subexpression] *)
module Key = struct
  module T = struct
    type atom =
      | Var of string
      | Float of float
      | Int of int
      | Bool of bool
    [@@deriving compare, sexp_of]

    type t =
      | Bop of Glsl.binary_op * atom * atom
      | Vec of int * atom list
      | Index of atom * int
      | Builtin of Glsl.builtin * atom list
      | App of string * atom list
      | Record of atom list
      | Field of atom * string
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

let key_atom (a : atom) : Key.atom =
  match a.desc with
  | Var v -> Key.Var v
  | Float f -> Key.Float f
  | Int i -> Key.Int i
  | Bool b -> Key.Bool b
;;

(** Build the hash key for a term, [None] for terms we never deduplicate *)
let key_of_term (t : term) : Key.t option =
  let f = key_atom in
  match t.desc with
  | Bop (((Sub | Div | Mod | Lt | Gt | Leq | Geq) as op), l, r) ->
    Some (Key.Bop (op, f l, f r))
  | Bop (((Add | Mul | Eq | And | Or) as op), l, r) ->
    (* Canonize order for commutative binary operations *)
    let l, r = f l, f r in
    let l, r = if Key.compare_atom l r > 0 then r, l else l, r in
    Some (Key.Bop (op, l, r))
  | Vec (n, atoms) -> Some (Key.Vec (n, List.map atoms ~f))
  | Index (a, i) -> Some (Key.Index (f a, i))
  | Builtin (b, atoms) -> Some (Key.Builtin (b, List.map atoms ~f))
  | App (name, atoms) -> Some (Key.App (name, List.map atoms ~f))
  | Record atoms -> Some (Key.Record (List.map atoms ~f))
  | Field (a, fld) -> Some (Key.Field (f a, fld))
  | Atom _ | If _ | Switch _ -> None
;;

(** [canon]: Eliminated to canonical variable renaming (technically we don't need
    to do this since constant folding handles it for us, but doing this here is
    easy and if we didn't do it, [cse] could only unroll one layer at a time)

    [seen]: Value numbering, "have we computed this before" *)
type ctx =
  { canon : string String.Map.t
  ; seen : string Key.Map.t
  }

let canonize_atom ~subst (a : atom) : atom =
  match a.desc with
  | Var v ->
    (match Map.find subst v with
     | Some v -> { a with desc = Var v }
     | None -> a)
  | Int _ | Float _ | Bool _ -> a
;;

(** Rewrites atoms so only canonical variables are used *)
let canonize_term ~subst (t : term) : term =
  let f = canonize_atom ~subst in
  let desc =
    match t.desc with
    | Atom a -> Atom (f a)
    | Bop (op, l, r) -> Bop (op, f l, f r)
    | Vec (n, atoms) -> Vec (n, List.map atoms ~f)
    | Index (a, i) -> Index (f a, i)
    | Builtin (b, atoms) -> Builtin (b, List.map atoms ~f)
    | App (name, atoms) -> App (name, List.map atoms ~f)
    | Record atoms -> Record (List.map atoms ~f)
    | Field (a, field) -> Field (f a, field)
    | If _ | Switch _ -> t.desc
  in
  { t with desc }
;;

let rec rewrite_anf ctx (a : anf) : anf =
  match a.desc with
  | Return t -> { a with desc = Return (rewrite_term ctx t) }
  | Let (v, b, body) ->
    let b = rewrite_term ctx b in
    (match key_of_term b with
     | None ->
       (* Not dedupable, keep binding *)
       { a with desc = Let (v, b, rewrite_anf ctx body) }
     | Some key ->
       (match Map.find ctx.seen key with
        | Some v' ->
          (* Drop [let v = b] and redirect [v] to [v']*)
          let ctx = { ctx with canon = Map.set ctx.canon ~key:v ~data:v' } in
          rewrite_anf ctx body
        | None ->
          (* Keep the binding and make [v] canon for [body] *)
          let ctx = { ctx with seen = Map.set ctx.seen ~key ~data:v } in
          { a with desc = Let (v, b, rewrite_anf ctx body) }))

and rewrite_term ctx (term : term) : term =
  let subst = ctx.canon in
  match term.desc with
  | If (c, t, e) ->
    let c = canonize_atom ~subst c in
    { term with desc = If (c, rewrite_anf ctx t, rewrite_anf ctx e) }
  | Switch (s, cases) ->
    let s = canonize_atom ~subst s in
    let cases = List.map cases ~f:(fun (lbl, a) -> lbl, rewrite_anf ctx a) in
    { term with desc = Switch (s, cases) }
  | _ -> canonize_term ~subst term
;;

let rewrite_top (top : top) : top =
  let init = { canon = String.Map.empty; seen = Key.Map.empty } in
  match top.desc with
  | Define { name; recur; args; body; ret_ty } ->
    { top with desc = Define { name; recur; args; body = rewrite_anf init body; ret_ty } }
  | Const (name, anf) -> { top with desc = Const (name, rewrite_anf init anf) }
  | Extern _ | TypeDef _ -> top
;;

let rewrite (Program tops : t) : t = Program (List.map tops ~f:rewrite_top)
