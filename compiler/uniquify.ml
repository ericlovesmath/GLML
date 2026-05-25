open Core
open Frontend
open Desugar

include Compiler_error.Pass (struct
    let name = "uniquify"
  end)

type env = string String.Map.t

let fresh v ctx =
  let v' = Utils.fresh v in
  let ctx = Map.set ctx ~key:v ~data:v' in
  v', ctx
;;

let rec uniquify_term (ctx : env) (t : term) : term =
  let pure desc : term = { desc; loc = t.loc } in
  let aux = uniquify_term ctx in
  let aux_list ts = List.map ~f:aux ts in
  match t.desc with
  | Float _ | Int _ | Bool _ -> pure t.desc
  | Var v ->
    let v =
      Map.find ctx v
      |> of_option "unbound variable" ~loc:t.loc ~d:[%message (v : string)]
      |> ok_exn
    in
    pure (Var v)
  | Lam (v, ty, body) ->
    let v, ctx = fresh v ctx in
    pure (Lam (v, ty, uniquify_term ctx body))
  | App (f, x) -> pure (App (aux f, aux x))
  | Let (recur, v, return_ty, constrs, bind, body) ->
    let v, ctx' = fresh v ctx in
    let bind =
      match recur with
      | Nonrec -> uniquify_term ctx bind
      | Rec _ -> uniquify_term ctx' bind
    in
    pure (Let (recur, v, return_ty, constrs, bind, uniquify_term ctx' body))
  | If (c, t, f) -> pure (If (aux c, aux t, aux f))
  | Vec (n, ts) -> pure (Vec (n, aux_list ts))
  | Bop (op, t, t') -> pure (Bop (op, aux t, aux t'))
  | Index (t, i) -> pure (Index (aux t, i))
  | Builtin (f, args) -> pure (Builtin (f, aux_list args))
  | Record fields -> pure (Record (List.map fields ~f:(fun (f, t) -> f, aux t)))
  | Field (t, f) -> pure (Field (aux t, f))
  | Variant (ctor, args) -> pure (Variant (ctor, aux_list args))
  | Tuple ts -> pure (Tuple (aux_list ts))
  | Match (scrutinee, cases) ->
    let scrutinee = aux scrutinee in
    let cases =
      List.map cases ~f:(fun (pat, body) ->
        let bound = Frontend.pat_bound_vars pat in
        let ctx =
          List.fold bound ~init:ctx ~f:(fun ctx v ->
            Map.set ctx ~key:v ~data:(Utils.fresh v))
        in
        let pat =
          Frontend.pat_map_vars pat ~f:(fun v ->
            Map.find ctx v |> Option.value ~default:v)
        in
        pat, uniquify_term ctx body)
    in
    pure (Match (scrutinee, cases))
;;

let uniquify_top (ctx : env) (t : top) : env * top =
  match t.desc with
  | Define (recur, v, return_ty, constrs, bind) ->
    let v' = Utils.fresh v in
    let ctx' = Map.set ctx ~key:v ~data:v' in
    let bind =
      match recur with
      | Nonrec -> uniquify_term ctx bind
      | Rec _ -> uniquify_term ctx' bind
    in
    ctx', { desc = Define (recur, v', return_ty, constrs, bind); loc = t.loc }
  | Extern (_, v) -> Map.set ctx ~key:v ~data:v, t
  | TypeDef _ -> ctx, t
;;

let uniquify (Program tops) =
  try_with (fun () ->
    let _, tops = List.fold_map tops ~init:String.Map.empty ~f:uniquify_top in
    Program tops)
;;
