open Core
open Remove_placeholder

(* ========== Inlining Logic ========== *)

type inlinable = (string * Monomorphize.ty) list * anf

let lookup r v = Map.find r v |> Option.value ~default:v
let bind r v = Map.set r ~key:v ~data:(Utils.fresh v)

let on_atom ~subst ~env (a : atom) : atom =
  match a.desc with
  | Var v ->
    (match Map.find env v with
     | Some renamed -> { a with desc = (Var renamed : atom_desc) }
     | None ->
       (match Map.find subst v with
        | Some replacement -> { replacement with loc = a.loc }
        | None -> a))
  | Int _ | Float _ | Bool _ -> a
;;

(** Produce a fresh copy of [body], formals replaced by actual atoms *)
let instantiate ~formals ~actuals ~body =
  let init_subst =
    List.zip_exn formals actuals
    |> List.map ~f:(fun ((n, _), a) -> n, a)
    |> String.Map.of_alist_or_error
    |> Compiler_error.of_or_error ~pass:"inline"
    |> Compiler_error.ok_exn
  in
  let rec on_anf subst (env : string String.Map.t) (a : anf) =
    let desc : anf_desc =
      match a.desc with
      | Return t -> Return (on_term subst env t)
      | Let (v, t, k) ->
        let t = on_term subst env t in
        let env = bind env v in
        let subst = Map.remove subst v in
        Let (lookup env v, t, on_anf subst env k)
      | Placeholder (v, k) ->
        let env = bind env v in
        let subst = Map.remove subst v in
        Placeholder (lookup env v, on_anf subst env k)
      | While (c, b, tl) ->
        While (on_term subst env c, on_anf subst env b, on_anf subst env tl)
      | Set (v, x, tl) -> Set (lookup env v, on_atom ~subst ~env x, on_anf subst env tl)
      | Continue -> Continue
    in
    { a with desc }
  and on_term subst (env : string String.Map.t) (t : term) =
    let atom = on_atom ~subst ~env in
    let desc : term_desc =
      match t.desc with
      | Atom a -> Atom (atom a)
      | Bop (op, a, b) -> Bop (op, atom a, atom b)
      | Vec (n, xs) -> Vec (n, List.map xs ~f:atom)
      | Index (a, i) -> Index (atom a, i)
      | Builtin (b, xs) -> Builtin (b, List.map xs ~f:atom)
      | App (n, xs) -> App (n, List.map xs ~f:atom)
      | If (c, t, e) -> If (atom c, on_anf subst env t, on_anf subst env e)
      | Record xs -> Record (List.map xs ~f:atom)
      | Field (a, n) -> Field (atom a, n)
      | Switch (a, cases) ->
        Switch (atom a, List.map cases ~f:(fun (l, b) -> l, on_anf subst env b))
    in
    { t with desc }
  in
  on_anf init_subst String.Map.empty body
;;

(* ========== Size Heuristic ========== *)

let rec size_anf (a : anf) =
  match a.desc with
  | Return t -> size_term t
  | Continue -> 0
  | Let (_, b, tl) -> 1 + size_term b + size_anf tl
  | Placeholder (_, tl) -> size_anf tl
  | While (_, b, tl) -> 1 + size_anf b + size_anf tl
  | Set (_, _, tl) -> 1 + size_anf tl

and size_term (t : term) =
  match t.desc with
  | If (_, th, e) -> 1 + size_anf th + size_anf e
  | Switch (_, cs) -> 1 + List.sum (module Int) cs ~f:(fun (_, a) -> size_anf a)
  | Atom _ -> 0
  | Bop _ | Vec _ | Index _ | Builtin _ | App _ | Record _ | Field _ -> 1
;;

(** Collects functions that we should inline, heuristics should live here *)
let collect_inlinable (tops : top list) : inlinable String.Map.t =
  List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
    match top.desc with
    (* Inline functions of size <= 3 *)
    | Define { name; args; body; _ } when size_anf body <= 3 ->
      Map.set acc ~key:name ~data:(args, body)
    | Define _ | Const _ | Extern _ | TypeDef _ -> acc)
;;

(* ========== Inlining ========== *)

(** Splice [body] into the caller, each [Return final] tail along the
    linear spine becomes [Let v = final in tl]. *)
let rec splice ~v ~tl (t : anf) =
  let desc : anf_desc =
    match t.desc with
    | Return t -> Let (v, t, tl)
    | Let (v', b, t) -> Let (v', b, splice ~v ~tl t)
    | Placeholder (v', t) -> Placeholder (v', splice ~v ~tl t)
    | While (c, b, t) -> While (c, b, splice ~v ~tl t)
    | Set (v', x, t) -> Set (v', x, splice ~v ~tl t)
    | Continue -> Continue
  in
  { t with desc }
;;

let inline_top (inlinable : inlinable String.Map.t) (top : top) : top =
  let try_inline f actuals =
    Map.find inlinable f
    |> Option.map ~f:(fun (formals, body) -> instantiate ~formals ~actuals ~body)
  in
  let rec try_expand_app (t : term) : anf option =
    match t.desc with
    | App (f, xs) -> try_inline f xs |> Option.map ~f:on_anf
    | _ -> None
  and on_anf (a : anf) : anf =
    match a.desc with
    | Continue -> a
    | Return t ->
      (match try_expand_app t with
       | Some body -> body
       | None -> { a with desc = Return (on_term t) })
    | Let (v, t, tl) ->
      let tl = on_anf tl in
      (match try_expand_app t with
       | Some body -> splice ~v ~tl body
       | None -> { a with desc = Let (v, on_term t, tl) })
    | Placeholder (v, k) -> { a with desc = Placeholder (v, on_anf k) }
    | While (c, b, tl) -> { a with desc = While (on_term c, on_anf b, on_anf tl) }
    | Set (v, x, k) -> { a with desc = Set (v, x, on_anf k) }
  and on_term (t : term) : term =
    let desc =
      match t.desc with
      | If (c, t, e) -> If (c, on_anf t, on_anf e)
      | Switch (s, cases) -> Switch (s, List.map cases ~f:(fun (l, b) -> l, on_anf b))
      | d -> d
    in
    { t with desc }
  in
  match top.desc with
  | Define { name; args; body; ret_ty } ->
    { top with desc = Define { name; args; body = on_anf body; ret_ty } }
  | Const (n, b) -> { top with desc = Const (n, on_anf b) }
  | Extern _ | TypeDef _ -> top
;;

let inline (Program tops : t) : t =
  let inlinable = collect_inlinable tops in
  Program (List.map tops ~f:(inline_top inlinable))
;;
