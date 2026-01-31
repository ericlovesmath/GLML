open Core

let counter = ref 0

let fresh name =
  let id = !counter in
  counter := id + 1;
  Printf.sprintf "%s.%d" name id

let rec aux (ctx : string String.Map.t) (e : Stlc.t) : Stlc.t =
  match e with
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
  | Var v -> Var (Option.value (Map.find ctx v) ~default:v)
  | Lam (v, ty, body) ->
      let v' = fresh v in
      let ctx = Map.set ctx ~key:v ~data:v' in
      Lam (v', ty, aux ctx body)
  | App (f, x) -> App (aux ctx f, aux ctx x)
  | Let (v, bind, body) ->
      let bind = aux ctx bind in
      let v' = fresh v in
      let ctx = Map.set ctx ~key:v ~data:v' in
      let body = aux ctx body in
      Let (v', bind, body)
  | If (c, t, f) -> If (aux ctx c, aux ctx t, aux ctx f)
  | Bop (op, t, t') -> Bop (op, aux ctx t, aux ctx t')

let uniquify e =
  counter := 0;
  aux String.Map.empty e
