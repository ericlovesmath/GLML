open Core
open Stlc
open Compiler_error.Let_syntax

module Err = Compiler_error.Pass (struct
    let name = "uniquify"
  end)

type env = string String.Map.t

let rec uniquify_term (ctx : env) (t : term) : term Compiler_error.t =
  let pure desc : term Compiler_error.t = Ok { desc; loc = t.loc } in
  let aux = uniquify_term ctx in
  let aux_list ts = Compiler_error.all (List.map ~f:aux ts) in
  match t.desc with
  | Float _ | Int _ | Bool _ -> pure t.desc
  | Var v ->
    let%bind v =
      Map.find ctx v
      |> Err.of_option "unbound variable" ~loc:t.loc ~d:[%message (v : string)]
    in
    pure (Var v)
  | Lam (v, ty, body) ->
    let v' = Utils.fresh v in
    let ctx = Map.set ctx ~key:v ~data:v' in
    let%bind body = uniquify_term ctx body in
    pure (Lam (v', ty, body))
  | App (f, x) ->
    let%bind f = aux f in
    let%bind x = aux x in
    pure (App (f, x))
  | Let (recur, v, return_ty, bind, body) ->
    let v' = Utils.fresh v in
    let ctx' = Map.set ctx ~key:v ~data:v' in
    let%bind bind =
      match recur with
      | Nonrec -> uniquify_term ctx bind
      | Rec _ -> uniquify_term ctx' bind
    in
    let%bind body = uniquify_term ctx' body in
    pure (Let (recur, v', return_ty, bind, body))
  | If (c, t, f) ->
    let%bind c = aux c in
    let%bind t = aux t in
    let%bind f = aux f in
    pure (If (c, t, f))
  | Vec (n, ts) ->
    let%bind ts = aux_list ts in
    pure (Vec (n, ts))
  | Mat (x, y, ts) ->
    let%bind ts = aux_list ts in
    pure (Mat (x, y, ts))
  | Bop (op, t, t') ->
    let%bind t = aux t in
    let%bind t' = aux t' in
    pure (Bop (op, t, t'))
  | Index (t, i) ->
    let%bind t = aux t in
    pure (Index (t, i))
  | Builtin (f, args) ->
    let%bind args = aux_list args in
    pure (Builtin (f, args))
  | Record fields ->
    let%bind fields =
      fields
      |> List.map ~f:(fun (f, t) ->
        let%map t = aux t in
        f, t)
      |> Compiler_error.all
    in
    pure (Record fields)
  | Field (t, f) ->
    let%bind t = aux t in
    pure (Field (t, f))
  | Variant (ctor, args) ->
    let%bind args = aux_list args in
    pure (Variant (ctor, args))
  | Match (scrutinee, cases) ->
    let%bind scrutinee = aux scrutinee in
    let%bind cases =
      cases
      |> List.map ~f:(fun (ctor, vs, body) ->
        let vs' = List.map vs ~f:Utils.fresh in
        let ctx =
          List.fold2 vs vs' ~init:ctx ~f:(fun ctx v v' -> Map.set ctx ~key:v ~data:v')
        in
        let%bind ctx =
          match ctx with
          | Ok ctx -> Ok ctx
          | Unequal_lengths -> Err.fail "unequal lengths, unreachable"
        in
        let%map body = uniquify_term ctx body in
        ctor, vs', body)
      |> Compiler_error.all
    in
    pure (Match (scrutinee, cases))
;;

let uniquify_top (ctx : env) (t : top) : (env * top) Compiler_error.t =
  match t.desc with
  | Define (recur, v, return_ty, bind) ->
    let v' = Utils.fresh v in
    let ctx' = Map.set ctx ~key:v ~data:v' in
    let%map bind =
      match recur with
      | Nonrec -> uniquify_term ctx bind
      | Rec _ -> uniquify_term ctx' bind
    in
    ctx', { desc = Define (recur, v', return_ty, bind); loc = t.loc }
  | Extern (_, v) -> Ok (Map.set ctx ~key:v ~data:v, t)
  | TypeDef _ -> Ok (ctx, t)
;;

let uniquify (Program tops) =
  let folder (ctx, acc) top =
    let%map ctx, top = uniquify_top ctx top in
    ctx, top :: acc
  in
  let%map _, tops = List.fold_result tops ~init:(String.Map.empty, []) ~f:folder in
  Program (List.rev tops)
;;
