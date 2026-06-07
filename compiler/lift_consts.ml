open Core
open Anf

(* [externs] is the set of names treated as non-constable *)
let is_constable_atom (externs : String.Set.t) (a : atom) =
  match a.desc with
  | Int _ | Bool _ | Float _ -> true
  | Var v -> not (Set.mem externs v)
;;

let is_constable_term (externs : String.Set.t) (t : term) =
  match t.desc with
  | Bop (_, a, a') -> is_constable_atom externs a && is_constable_atom externs a'
  | Vec (_, atoms) | Builtin (_, atoms) | Record atoms ->
    List.for_all ~f:(is_constable_atom externs) atoms
  | Index (a, _) | Field (a, _) -> is_constable_atom externs a
  | App _ | If _ | Switch _ -> false
  | Atom a -> is_constable_atom externs a
;;

let rec is_constable (externs : String.Set.t) (a : anf) =
  match a.desc with
  | Return t -> is_constable_term externs t
  | Let (_, bind, tl) -> is_constable_term externs bind && is_constable externs tl
;;

(* Collect the names of [Const] nodes that must be promoted to zero-arg functions *)
let find_promoted (real_externs : String.Set.t) (tops : top list) : String.Set.t =
  List.fold tops ~init:real_externs ~f:(fun externs top ->
    match top.desc with
    | Const (name, anf) when not (is_constable externs anf) -> Set.add externs name
    | _ -> externs)
  |> Fn.flip Set.diff real_externs
;;

let atoms_of_term_desc = function
  | Atom a -> [ a ]
  | Bop (_, l, r) -> [ l; r ]
  | Vec (_, ts) | Builtin (_, ts) | App (_, ts) | Record ts -> ts
  | Index (a, _) | Field (a, _) -> [ a ]
  | If _ | Switch _ -> []
;;

type binds = (string * term) list

let make_binds (binds : binds) (anf : anf) : anf =
  List.fold_right binds ~init:anf ~f:(fun (name, bind) body ->
    ({ body with desc = Let (name, bind, body) } : anf))
;;

let lift_atom (promoted : String.Set.t) (a : atom) : binds * atom =
  match a.desc with
  | Var v when Set.mem promoted v ->
    let fresh_name = Utils.fresh "_lc" in
    let bind_term : term = { desc = App (v, []); ty = a.ty; loc = a.loc } in
    [ fresh_name, bind_term ], { a with desc = Var fresh_name }
  | _ -> [], a
;;

let lift_atoms (promoted : String.Set.t) (t : term) : binds * term =
  let subst =
    atoms_of_term_desc t.desc
    |> List.filter_map ~f:(fun a ->
      match a.desc with
      | Var v when Set.mem promoted v -> Some (v, a)
      | _ -> None)
    |> List.stable_dedup ~compare:(fun (v, _) (v', _) -> String.compare v v')
    |> List.map ~f:(fun (name, a) -> name, Utils.fresh "_lc", a)
  in
  let rewrite_atom (a : atom) : atom =
    match a.desc with
    | Var v ->
      (match List.find subst ~f:(fun (n, _, _) -> String.equal n v) with
       | Some (_, fresh, _) -> { a with desc = Var fresh }
       | None -> a)
    | _ -> a
  in
  let desc =
    match t.desc with
    | Atom a -> Atom (rewrite_atom a)
    | Bop (op, l, r) -> Bop (op, rewrite_atom l, rewrite_atom r)
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rewrite_atom)
    | Index (a, i) -> Index (rewrite_atom a, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rewrite_atom)
    | App (f, ts) -> App (f, List.map ts ~f:rewrite_atom)
    | Record ts -> Record (List.map ts ~f:rewrite_atom)
    | Field (a, f) -> Field (rewrite_atom a, f)
    | If _ | Switch _ ->
      Compiler_error.raise
        ~pass:"lift_consts"
        ~d:[%message (t : term)]
        "If/Switch should be handled by rewrite_branching"
  in
  let binds =
    List.map subst ~f:(fun (orig, fresh, a) ->
      fresh, ({ desc = App (orig, []); ty = a.ty; loc = a.loc } : term))
  in
  binds, { t with desc }
;;

let rec rewrite_branching (promoted : String.Set.t) (t : term) =
  match t.desc with
  | If (cond, then_, else_) ->
    let binds, cond = lift_atom promoted cond in
    let then_ = rewrite_anf promoted then_ in
    let else_ = rewrite_anf promoted else_ in
    binds, { t with desc = If (cond, then_, else_) }
  | Switch (tag, cases) ->
    let binds, tag = lift_atom promoted tag in
    let cases = List.map cases ~f:(fun (lbl, a) -> lbl, rewrite_anf promoted a) in
    binds, { t with desc = Switch (tag, cases) }
  | _ -> lift_atoms promoted t

and rewrite_anf (promoted : String.Set.t) (anf : anf) : anf =
  match anf.desc with
  | Return t ->
    let binds, t = rewrite_branching promoted t in
    make_binds binds { anf with desc = Return t }
  | Let (v, bind, tl) ->
    let binds, bind = rewrite_branching promoted bind in
    let tl = rewrite_anf promoted tl in
    make_binds binds { anf with desc = Let (v, bind, tl) }
;;

let rewrite_top (promoted : String.Set.t) (top : top) : top =
  match top.desc with
  | Const (name, anf) when Set.mem promoted name ->
    let body = rewrite_anf promoted anf in
    { top with desc = Define { name; recur = Nonrec; args = []; body; ret_ty = top.ty } }
  | Const (name, anf) -> { top with desc = Const (name, rewrite_anf promoted anf) }
  | Define { name; recur; args; body; ret_ty } ->
    let body = rewrite_anf promoted body in
    { top with desc = Define { name; recur; args; body; ret_ty } }
  | Extern _ | TypeDef _ -> top
;;

let lift (Program tops : t) : t =
  let externs =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | Extern v -> Some v
      | _ -> None)
    |> String.Set.of_list
  in
  let promoted = find_promoted externs tops in
  let tops = List.map ~f:(rewrite_top promoted) tops in
  Program tops
;;
