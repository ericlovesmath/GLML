open Core
open Stlc

let rec aux (ctx : string String.Map.t) (t : term) : term =
  match t with
  | Float _ | Int _ | Bool _ -> t
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
  | Vec3 (t, t', t'') -> Vec3 (aux ctx t, aux ctx t', aux ctx t'')
  | Bop (op, t, t') -> Bop (op, aux ctx t, aux ctx t')
;;

let uniquify (Program terms) = Program (List.map ~f:(aux String.Map.empty) terms)
