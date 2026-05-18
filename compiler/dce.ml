open Core
open Remove_placeholder

module Err = Compiler_error.Pass (struct
    let name = "dce"
  end)

let ftv_atom (a : atom) : String.Set.t =
  match a.desc with
  | Var v -> String.Set.singleton v
  | Int _ | Float _ | Bool _ -> String.Set.empty
;;

let ftv_atoms (atoms : atom list) : String.Set.t =
  String.Set.union_list (List.map ~f:ftv_atom atoms)
;;

(** Does [a] contain a [While]/[Set] *)
let rec effectful_anf (a : anf) : bool =
  match a.desc with
  | Set _ | While _ -> true
  | Return t -> effectful_term t
  | Let (_, b, t) -> effectful_term b || effectful_anf t
  | Placeholder (_, t) -> effectful_anf t
  | Continue -> false

and effectful_term (t : term) : bool =
  match t.desc with
  | Atom _ | Bop _ | Vec _ | Index _ | Builtin _ | App _ | Record _ | Field _ -> false
  | If (_, t, e) -> effectful_anf t || effectful_anf e
  | Switch (_, cases) -> List.exists cases ~f:(fun (_, a) -> effectful_anf a)
;;

(** Backward liveness analysis with simultaneous rewrite *)
let rec liveness_anf ~(live : String.Set.t) (a : anf) : anf * String.Set.t =
  match a.desc with
  | Continue -> a, live
  | Return t ->
    let t, live = liveness_term ~live t in
    { a with desc = Return t }, live
  | Let (v, b, t) ->
    let t, live_t = liveness_anf ~live t in
    if not (Set.mem live_t v || effectful_term b)
    then t, Set.remove live_t v
    else (
      let b, live_b = liveness_term ~live b in
      let live = Set.union (Set.remove live_t v) live_b in
      { a with desc = Let (v, b, t) }, live)
  | Placeholder (v, t) ->
    let t, live_t = liveness_anf ~live t in
    { a with desc = Placeholder (v, t) }, Set.remove live_t v
  | Set (v, x, t) ->
    let t, live_t = liveness_anf ~live t in
    if Set.mem live_t v
    then (
      let live = Set.union (Set.remove live_t v) (ftv_atom x) in
      { a with desc = Set (v, x, t) }, live)
    else t, live_t
  | While (cond, body, tl) ->
    let tl, live_tl = liveness_anf ~live tl in
    let rec fix prev =
      let _, body_live = liveness_anf ~live:prev body in
      let _, cond_live = liveness_term ~live:prev cond in
      let next = Set.union cond_live (Set.union body_live live_tl) in
      if Set.equal prev next then prev else fix next
    in
    let live = fix live_tl in
    let body, _ = liveness_anf ~live body in
    let cond, _ = liveness_term ~live cond in
    { a with desc = While (cond, body, tl) }, live

and liveness_term ~(live : String.Set.t) (t : term) : term * String.Set.t =
  match t.desc with
  | Atom a -> t, ftv_atom a
  | Bop (_, l, r) -> t, Set.union (ftv_atom l) (ftv_atom r)
  | Vec (_, atoms) | Builtin (_, atoms) | Record atoms -> t, ftv_atoms atoms
  | App (_, atoms) -> t, ftv_atoms atoms
  | Index (a, _) | Field (a, _) -> t, ftv_atom a
  | If (c, th, e) ->
    let th, live_th = liveness_anf ~live th in
    let e, live_e = liveness_anf ~live e in
    let live = Set.union (ftv_atom c) (Set.union live_th live_e) in
    { t with desc = If (c, th, e) }, live
  | Switch (s, cases) ->
    let live, cases =
      List.fold_map cases ~init:(ftv_atom s) ~f:(fun acc (lbl, a) ->
        let a, live_a = liveness_anf ~live a in
        Set.union acc live_a, (lbl, a))
    in
    { t with desc = Switch (s, cases) }, live
;;

let liveness_top (top : top) : top =
  match top.desc with
  | Define { name; args; body; ret_ty } ->
    let body, _ = liveness_anf ~live:String.Set.empty body in
    { top with desc = Define { name; args; body; ret_ty } }
  | Const (name, anf) ->
    let anf, _ = liveness_anf ~live:String.Set.empty anf in
    { top with desc = Const (name, anf) }
  | Extern _ | TypeDef _ -> top
;;

(** Names referenced anywhere (for reachability) *)
let reachable_atom (acc : String.Set.t) (a : atom) : String.Set.t =
  match a.desc with
  | Var v -> Set.add acc v
  | Int _ | Float _ | Bool _ -> acc
;;

let rec reachable_term (acc : String.Set.t) (t : term) : String.Set.t =
  match t.desc with
  | Atom a -> reachable_atom acc a
  | Bop (_, l, r) -> reachable_atom (reachable_atom acc l) r
  | Vec (_, atoms) | Builtin (_, atoms) | Record atoms ->
    List.fold atoms ~init:acc ~f:reachable_atom
  | App (name, atoms) -> List.fold atoms ~init:(Set.add acc name) ~f:reachable_atom
  | Index (a, _) | Field (a, _) -> reachable_atom acc a
  | If (c, t, e) -> reachable_anf (reachable_anf (reachable_atom acc c) t) e
  | Switch (s, cases) ->
    List.fold cases ~init:(reachable_atom acc s) ~f:(fun acc (_, a) ->
      reachable_anf acc a)

and reachable_anf (acc : String.Set.t) (a : anf) : String.Set.t =
  match a.desc with
  | Return t -> reachable_term acc t
  | Let (_, b, t) -> reachable_anf (reachable_term acc b) t
  | Placeholder (_, t) -> reachable_anf acc t
  | While (c, b, tl) -> reachable_anf (reachable_anf (reachable_term acc c) b) tl
  | Set (_, x, t) -> reachable_anf (reachable_atom acc x) t
  | Continue -> acc
;;

(** Reachable top-level names starting from [main] *)
let reachable_tops (tops : top list) : String.Set.t =
  let body_of =
    List.filter_map tops ~f:(fun top ->
      match top.desc with
      | Define { name; body; _ } -> Some (name, body)
      | Const (name, body) -> Some (name, body)
      | Extern _ | TypeDef _ -> None)
    |> String.Map.of_alist_or_error
    |> Err.of_or_error
    |> Err.ok_exn
  in
  let rec dfs curr acc =
    match curr with
    | [] -> acc
    | name :: rest when Set.mem acc name -> dfs rest acc
    | name :: rest ->
      let acc = Set.add acc name in
      (match Map.find body_of name with
       | None -> dfs rest acc
       | Some body ->
         let refs = reachable_anf String.Set.empty body in
         dfs (Set.to_list refs @ rest) acc)
  in
  dfs [ "main" ] String.Set.empty
;;

let rewrite (Program tops : t) : t =
  let tops = List.map tops ~f:liveness_top in
  let live = reachable_tops tops in
  let tops =
    List.filter tops ~f:(fun top ->
      match top.desc with
      | Define { name; _ } | Const (name, _) -> Set.mem live name
      | Extern _ | TypeDef _ -> true)
  in
  Program tops
;;
