open Core
open Anf

(* ========== Instantiating a body ========== *)

let lookup env v = Map.find env v |> Option.value ~default:v
let bind env v = Map.set env ~key:v ~data:(Utils.fresh v)

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

(** Produce a fresh copy of [body], formals replaced by actual atoms. *)
let instantiate ~formals ~actuals ~body =
  let init_subst =
    List.zip_exn formals actuals
    |> List.map ~f:(fun ((n, _), a) -> n, a)
    |> String.Map.of_alist_or_error
    |> Compiler_error.of_or_error ~pass:"inline"
    |> Compiler_error.ok_exn
  in
  let rec on_anf subst env (a : anf) =
    let desc : anf_desc =
      match a.desc with
      | Return t -> Return (on_term subst env t)
      | Let (v, t, k) ->
        let t = on_term subst env t in
        let env = bind env v in
        let subst = Map.remove subst v in
        Let (lookup env v, t, on_anf subst env k)
    in
    { a with desc }
  and on_term subst env (t : term) =
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
      | Init_struct fields -> Init_struct (List.map fields ~f:(fun (n, a) -> n, atom a))
      | Field (a, n) -> Field (atom a, n)
      | Switch (a, cases) ->
        Switch (atom a, List.map cases ~f:(fun (l, b) -> l, on_anf subst env b))
    in
    { t with desc }
  in
  on_anf init_subst String.Map.empty body
;;

(* ========== Size / shape heuristics ========== *)

let rec size_anf (a : anf) =
  match a.desc with
  | Return t -> size_term t
  | Let (_, b, tl) -> 1 + size_term b + size_anf tl

and size_term (t : term) =
  match t.desc with
  | If (_, th, e) -> 1 + size_anf th + size_anf e
  | Switch (_, cs) -> 1 + List.sum (module Int) cs ~f:(fun (_, a) -> size_anf a)
  | Atom _ -> 0
  | Bop _ | Vec _ | Index _ | Builtin _ | App _ | Record _ | Init_struct _ | Field _ -> 1
;;

let count_call_sites (tops : top list) : int String.Map.t =
  let rec on_anf (a : anf) acc =
    match a.desc with
    | Return t -> on_term t acc
    | Let (_, t, tl) -> acc |> on_term t |> on_anf tl
  and on_term (t : term) acc =
    match t.desc with
    | App (f, _) ->
      Map.update acc f ~f:(function
        | None -> 1
        | Some n -> n + 1)
    | If (_, t, e) -> on_anf t acc |> on_anf e
    | Switch (_, cases) -> List.fold_right cases ~init:acc ~f:(fun (_, b) -> on_anf b)
    | Atom _ | Bop _ | Vec _ | Index _ | Builtin _ | Record _ | Init_struct _ | Field _ ->
      acc
  in
  List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
    match top.desc with
    | Define { body; _ } | Const (_, body) -> on_anf body acc
    | Extern _ | TypeDef _ -> acc)
;;

(** Does [body] read any formal via [Field _] or [Switch(Field _, _)]?
    This is used for [case-of-known-constructor] *)
let projects_formal (formals : (string * Lower_variants.ty) list) (body : anf) : bool =
  let formals = String.Set.of_list (List.map formals ~f:fst) in
  let is_formal (a : atom) =
    match a.desc with
    | Var v -> Set.mem formals v
    | _ -> false
  in
  let rec on_anf (a : anf) =
    match a.desc with
    | Return t -> on_term t
    | Let (_, t, k) -> on_term t || on_anf k
  and on_term (t : term) =
    match t.desc with
    | Field (a, _) -> is_formal a
    | Switch (s, cases) -> is_formal s || List.exists cases ~f:(fun (_, b) -> on_anf b)
    | If (_, t, e) -> on_anf t || on_anf e
    | Atom _ | Bop _ | Vec _ | Index _ | Builtin _ | App _ | Record _ | Init_struct _ ->
      false
  in
  on_anf body
;;

(* ========== Inlining entries ========== *)

type guard =
  | Unconditional (* Always inline *)
  | On_record_actual (* Only inline when actual argument is a literal record *)

type entry =
  { formals : (string * Lower_variants.ty) list
  ; body : anf
  ; guard : guard
  }

let collect_entries (tops : top list) : entry String.Map.t =
  let counts = count_call_sites tops in
  let call_count n = Map.find counts n |> Option.value ~default:0 in
  List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
    match top.desc with
    | Define { name; recur = Nonrec; args; body; _ } ->
      let guard =
        if size_anf body <= 3 || call_count name = 1
        then Some Unconditional
        else if projects_formal args body
        then Some On_record_actual
        else None
      in
      (match guard with
       | Some guard -> Map.set acc ~key:name ~data:{ formals = args; body; guard }
       | None -> acc)
    | Define _ | Const _ | Extern _ | TypeDef _ -> acc)
;;

(** Names of top-level [Const]s whose body tail is a [Record] literal *)
let collect_record_const_names (tops : top list) : String.Set.t =
  let rec returns_record (a : anf) : bool =
    match a.desc with
    | Return t ->
      (match t.desc with
       | Record _ | Init_struct _ -> true
       | _ -> false)
    | Let (_, _, tl) -> returns_record tl
  in
  List.fold tops ~init:String.Set.empty ~f:(fun acc top ->
    match top.desc with
    | Const (name, body) when returns_record body -> Set.add acc name
    | Define _ | Const _ | Extern _ | TypeDef _ -> acc)
;;

(* ========== Splicing and inlining ========== *)

let rec splice ~v ~tl (t : anf) =
  match t.desc with
  | Return rt -> { t with desc = Let (v, rt, tl); ty = tl.ty }
  | Let (v', b, t') -> { t with desc = Let (v', b, splice ~v ~tl t'); ty = tl.ty }
;;

let is_known_record records (a : atom) =
  match a.desc with
  | Var v -> Set.mem records v
  | _ -> false
;;

let track_record_binding records v (t : term) : String.Set.t =
  match t.desc with
  | Record _ | Init_struct _ -> Set.add records v
  | Atom a when is_known_record records a -> Set.add records v
  | Field (a, _) when is_known_record records a ->
    (match t.ty with
     | TyRecord _ -> Set.add records v
     | _ -> records)
  | _ -> records
;;

let try_inline entries records (t : term) : anf option =
  let open Option.Let_syntax in
  match t.desc with
  | App (f, actuals) ->
    let%bind { formals; body; guard } = Map.find entries f in
    (match guard with
     | Unconditional -> Some (instantiate ~formals ~actuals ~body)
     | On_record_actual when List.exists actuals ~f:(is_known_record records) ->
       Some (instantiate ~formals ~actuals ~body)
     | On_record_actual -> None)
  | _ -> None
;;

let rec rewrite_anf entries records (a : anf) : anf =
  let go = rewrite_anf entries records in
  match a.desc with
  | Return t ->
    (match try_inline entries records t with
     | Some body -> go body
     | None -> { a with desc = Return (rewrite_term entries records t) })
  | Let (v, t, tl) ->
    let tl = rewrite_anf entries (track_record_binding records v t) tl in
    (match try_inline entries records t with
     | Some body -> go (splice ~v ~tl body)
     | None -> { a with desc = Let (v, rewrite_term entries records t, tl) })

and rewrite_term entries records (t : term) : term =
  let go = rewrite_anf entries records in
  let desc : term_desc =
    match t.desc with
    | If (c, t, e) -> If (c, go t, go e)
    | Switch (s, cases) -> Switch (s, List.map cases ~f:(fun (l, b) -> l, go b))
    | ( Atom _
      | Bop _
      | Vec _
      | Index _
      | Builtin _
      | App _
      | Record _
      | Init_struct _
      | Field _ ) as d -> d
  in
  { t with desc }
;;

let inline (tops : top list) : top list =
  let entries = collect_entries tops in
  let records = collect_record_const_names tops in
  let body_of = rewrite_anf entries records in
  List.map tops ~f:(fun top ->
    match top.desc with
    | Define d -> { top with desc = Define { d with body = body_of d.body } }
    | Const (n, b) -> { top with desc = Const (n, body_of b) }
    | Extern _ | TypeDef _ -> top)
;;

let rewrite (Program tops : t) : t = Program (inline tops)
