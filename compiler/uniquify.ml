open Core
open Stlc

let rec aux (ctx : string String.Map.t) (t : term) : term =
  let desc =
    match t.desc with
    | Float _ | Int _ | Bool _ -> t.desc
    | Var v -> Var (Option.value (Map.find ctx v) ~default:v)
    | Lam (v, ty, body) ->
      let v' = Utils.fresh v in
      let ctx = Map.set ctx ~key:v ~data:v' in
      Lam (v', ty, aux ctx body)
    | App (f, x) -> App (aux ctx f, aux ctx x)
    | Let (recur, v, bind, body) ->
      let bind = aux ctx bind in
      let v' = Utils.fresh v in
      let ctx = Map.set ctx ~key:v ~data:v' in
      let body = aux ctx body in
      Let (recur, v', bind, body)
    | If (c, t_true, f) -> If (aux ctx c, aux ctx t_true, aux ctx f)
    | Vec (n, ts) -> Vec (n, List.map ts ~f:(aux ctx))
    | Mat (x, y, ts) -> Mat (x, y, List.map ts ~f:(aux ctx))
    | Bop (op, t1, t2) -> Bop (op, aux ctx t1, aux ctx t2)
    | Index (t_sub, i) -> Index (aux ctx t_sub, i)
    | Builtin (f, args) -> Builtin (f, List.map args ~f:(aux ctx))
  in
  { desc; loc = t.loc }
;;

let uniquify_top (ctx : string String.Map.t) (t : top) : string String.Map.t * top =
  match t.desc with
  | Define (recur, v, bind) ->
    let v' = Utils.fresh v in
    let ctx' = Map.set ctx ~key:v ~data:v' in
    let bind =
      match recur with
      | Nonrec -> aux ctx bind
      | Rec _ -> aux ctx' bind
    in
    ctx', { desc = Define (recur, v', bind); loc = t.loc }
  | Extern _ -> ctx, t
;;

let uniquify (Program terms) =
  let _, terms = List.fold_map terms ~init:String.Map.empty ~f:uniquify_top in
  Program terms
;;
