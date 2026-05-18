open Core
open Remove_placeholder

type inlinable = (string * Monomorphize.ty) list * anf

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
      | Field (a, n) -> Field (atom a, n)
      | Switch (a, cases) ->
        Switch (atom a, List.map cases ~f:(fun (l, b) -> l, on_anf subst env b))
    in
    { t with desc }
  in
  on_anf init_subst String.Map.empty body
;;

(* ========== Inlining Heuristics ========== *)

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

(* TODO: Fix [While] inlining, currently just not inlining them *)
let rec has_while_anf (a : anf) =
  match a.desc with
  | While _ -> true
  | Return t -> has_while_term t
  | Let (_, t, k) -> has_while_term t || has_while_anf k
  | Placeholder (_, k) -> has_while_anf k
  | Set (_, _, k) -> has_while_anf k
  | Continue -> false

and has_while_term (t : term) =
  match t.desc with
  | If (_, th, el) -> has_while_anf th || has_while_anf el
  | Switch (_, cs) -> List.exists cs ~f:(fun (_, b) -> has_while_anf b)
  | Atom _ | Bop _ | Vec _ | Index _ | Builtin _ | App _ | Record _ | Field _ -> false
;;

(* Collect the name of every function called anywhere in [tops]. *)
let all_call_sites (tops : top list) : string list =
  let rec anf (a : anf) =
    match a.desc with
    | Return t -> term t
    | Continue -> []
    | Let (_, t, tl) -> term t @ anf tl
    | Placeholder (_, tl) -> anf tl
    | While (c, b, tl) -> term c @ anf b @ anf tl
    | Set (_, _, tl) -> anf tl
  and term (t : term) =
    match t.desc with
    | App (f, _) -> [ f ]
    | If (_, t, e) -> anf t @ anf e
    | Switch (_, cases) -> List.concat_map cases ~f:(fun (_, b) -> anf b)
    | Atom _ | Bop _ | Vec _ | Index _ | Builtin _ | Record _ | Field _ -> []
  in
  List.concat_map tops ~f:(fun top ->
    match top.desc with
    | Define { body; _ } | Const (_, body) -> anf body
    | Extern _ | TypeDef _ -> [])
;;

let count_call_sites tops : int String.Map.t =
  all_call_sites tops
  |> List.fold ~init:String.Map.empty ~f:(fun m f ->
    Map.update m f ~f:(function
      | None -> 1
      | Some n -> n + 1))
;;

(** Collects functions that we should inline, heuristics live here.

    Body size <= 3 or if there is exactly one call site *)
let collect_inlinable (tops : top list) : inlinable String.Map.t =
  let counts = count_call_sites tops in
  let call_count n = Map.find counts n |> Option.value ~default:0 in
  List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
    match top.desc with
    | Define { name; args; body; _ }
      when (not (has_while_anf body)) && (size_anf body <= 3 || call_count name = 1) ->
      Map.set acc ~key:name ~data:(args, body)
    | Define _ | Const _ | Extern _ | TypeDef _ -> acc)
;;

(* ========== Splicing and inlining ========== *)

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

let inline_top (init : inlinable String.Map.t) (top : top) : top =
  let rec on_anf inlinable (a : anf) : anf =
    match a.desc with
    | Continue -> a
    | Return t ->
      (match try_inline_term inlinable t with
       | Some body -> body
       | None -> { a with desc = Return (on_term inlinable t) })
    | Let (v, t, tl) ->
      let tl = on_anf inlinable tl in
      (match try_inline_term inlinable t with
       | Some body -> splice ~v ~tl body
       | None -> { a with desc = Let (v, on_term inlinable t, tl) })
    | Placeholder (v, k) -> { a with desc = Placeholder (v, on_anf inlinable k) }
    | While (c, b, tl) ->
      { a with
        desc = While (on_term inlinable c, on_anf inlinable b, on_anf inlinable tl)
      }
    | Set (v, x, k) -> { a with desc = Set (v, x, on_anf inlinable k) }
  and on_term inlinable (t : term) : term =
    let desc : term_desc =
      match t.desc with
      | If (c, th, el) -> If (c, on_anf inlinable th, on_anf inlinable el)
      | Switch (s, cases) ->
        Switch (s, List.map cases ~f:(fun (l, b) -> l, on_anf inlinable b))
      | (Atom _ | Bop _ | Vec _ | Index _ | Builtin _ | App _ | Record _ | Field _) as d
        -> d
    in
    { t with desc }
  and try_inline_term inlinable (t : term) : anf option =
    match t.desc with
    | App (f, actuals) ->
      Map.find inlinable f
      |> Option.map ~f:(fun (formals, body) ->
        on_anf (Map.remove inlinable f) (instantiate ~formals ~actuals ~body))
    | _ -> None
  in
  match top.desc with
  | Define { name; args; body; ret_ty } ->
    let inlinable = Map.remove init name in
    { top with desc = Define { name; args; body = on_anf inlinable body; ret_ty } }
  | Const (n, b) -> { top with desc = Const (n, on_anf init b) }
  | Extern _ | TypeDef _ -> top
;;

let inline (Program tops : t) : t =
  let inlinable = collect_inlinable tops in
  Program (List.map tops ~f:(inline_top inlinable))
;;
