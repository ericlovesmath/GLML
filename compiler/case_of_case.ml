open Core
open Anf

(** Do all branches of a term return a struct? *)
let all_branches_return_struct (t : term) : bool =
  let rec returns_struct (a : anf) : bool =
    match a.desc with
    | Return t ->
      (match t.desc with
       | Init_struct _ | Record _ -> true
       | _ -> false)
    | Let (_, _, k) -> returns_struct k
  in
  match t.desc with
  | If (_, t, e) -> returns_struct t && returns_struct e
  | Switch (_, cases) -> List.for_all cases ~f:(fun (_, a) -> returns_struct a)
  | Atom _
  | Bop _
  | Vec _
  | Index _
  | Builtin _
  | App _
  | Record _
  | Init_struct _
  | Field _ -> false
;;

(** Does [name] appear anywhere in the code? *)
let rec mentions name (a : anf) : bool =
  match a.desc with
  | Return t -> mentions_t name t
  | Let (_, t, k) -> mentions_t name t || mentions name k

and mentions_t name (t : term) : bool =
  let found (a : atom) =
    match a.desc with
    | Var v -> String.equal v name
    | Float _ | Int _ | Bool _ -> false
  in
  match t.desc with
  | Atom a | Index (a, _) | Field (a, _) -> found a
  | Bop (_, l, r) -> found l || found r
  | Vec (_, xs) | Builtin (_, xs) | Record xs | App (_, xs) -> List.exists xs ~f:found
  | Init_struct fields -> List.exists fields ~f:(fun (_, a) -> found a)
  | If (c, t, e) -> found c || mentions name t || mentions name e
  | Switch (s, cases) -> found s || List.exists cases ~f:(fun (_, a) -> mentions name a)
;;

(** Rewrite every arm of the branch with [f] *)
let map_arms (term : term) ~f : term =
  match term.desc with
  | If (c, t, e) -> { term with desc = If (c, f t, f e) }
  | Switch (s, cases) ->
    { term with desc = Switch (s, List.map cases ~f:(fun (l, a) -> l, f a)) }
  | Atom _
  | Bop _
  | Vec _
  | Index _
  | Builtin _
  | App _
  | Record _
  | Init_struct _
  | Field _ -> term
;;

(** Leading [Let]s of [rest] that don't read [result_var], safe to hoist! *)
let rec peel_independent (v : string) (rest : anf) : (string * term) list * anf =
  match rest.desc with
  | Let (x, t, k) when not (mentions_t v t) ->
    let lets, core = peel_independent v k in
    (x, t) :: lets, core
  | _ -> [], rest
;;

let rec rewrite_term (term : term) : term =
  match term.desc with
  | If (c, t, e) -> { term with desc = If (c, rewrite_anf t, rewrite_anf e) }
  | Switch (s, cases) ->
    { term with desc = Switch (s, List.map cases ~f:(fun (l, a) -> l, rewrite_anf a)) }
  | Atom _
  | Bop _
  | Vec _
  | Index _
  | Builtin _
  | App _
  | Record _
  | Init_struct _
  | Field _ -> term

and rewrite_anf (a : anf) : anf =
  match a.desc with
  | Return t -> { a with desc = Return (rewrite_term t) }
  | Let (v, branch, rest) ->
    let branch = rewrite_term branch in
    let rest = rewrite_anf rest in
    let default = { a with desc = Let (v, branch, rest) } in
    (match branch.desc with
     | (If _ | Switch _) when all_branches_return_struct branch ->
       (* Case-of-case: Hoist the result independent prefix above the branch,
          put rest into each arm *)
       let prefix, core = peel_independent v rest in
       (* If the result is unused after peeling then DCE should handle it... *)
       if not (mentions v core)
       then default
       else (
         let rec bind_at_tail (a : anf) : anf =
           match a.desc with
           | Return ctor -> { desc = Let (v, ctor, core); ty = core.ty; loc = a.loc }
           | Let (x, t, tl) ->
             let tl = bind_at_tail tl in
             { a with desc = Let (x, t, tl); ty = tl.ty }
         in
         let arms = map_arms branch ~f:bind_at_tail in
         let pushed : anf =
           { desc = Return { arms with ty = core.ty }; ty = core.ty; loc = a.loc }
         in
         List.fold_right prefix ~init:pushed ~f:(fun (x, t) (acc : anf) ->
           { desc = Let (x, t, acc); ty = acc.ty; loc = t.loc }))
     | _ -> default)
;;

let rewrite_top (top : top) : top =
  match top.desc with
  | Define { name; recur; args; body; ret_ty } ->
    { top with desc = Define { name; recur; args; body = rewrite_anf body; ret_ty } }
  | Const (name, body) -> { top with desc = Const (name, rewrite_anf body) }
  | Extern _ | TypeDef _ -> top
;;

let rewrite (Program tops : t) : t = Program (List.map tops ~f:rewrite_top)
