open Core
open Stlc

let rec aux (ctx : string String.Map.t) (t : term) : term =
  match t with
  | Float _ | Int _ | Bool _ | Unit -> t
  | Var v -> Var (Option.value (Map.find ctx v) ~default:v)
  | Lam (v, ty, body) ->
    let v' = Utils.fresh v in
    let ctx = Map.set ctx ~key:v ~data:v' in
    Lam (v', ty, aux ctx body)
  | App (f, x) -> App (aux ctx f, aux ctx x)
  | Let (v, bind, body) ->
    let bind = aux ctx bind in
    let v' = Utils.fresh v in
    let ctx = Map.set ctx ~key:v ~data:v' in
    let body = aux ctx body in
    Let (v', bind, body)
  | If (c, t, f) -> If (aux ctx c, aux ctx t, aux ctx f)
  | Vec (n, ts) -> Vec (n, List.map ts ~f:(aux ctx))
  | Mat (x, y, ts) -> Mat (x, y, List.map ts ~f:(aux ctx))
  | Bop (op, t, t') -> Bop (op, aux ctx t, aux ctx t')
  | Index (t, i) -> Index (aux ctx t, i)
;;

let uniquify_top (ctx : string String.Map.t) (t : top) : string String.Map.t * top =
  match t with
  | Define (v, bind) ->
    let bind = aux ctx bind in
    let v' = Utils.fresh v in
    let ctx = Map.set ctx ~key:v ~data:v' in
    ctx, Define (v', bind)
  | Extern _ -> ctx, t
;;

let uniquify (Program terms) =
  let _, terms = List.fold_map terms ~init:String.Map.empty ~f:uniquify_top in
  Program terms
;;
