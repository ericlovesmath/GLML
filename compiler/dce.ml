open Core
open Remove_placeholder

module Err = Compiler_error.Pass (struct
    let name = "dce"
  end)

let var_of_atom (a : atom) =
  match a.desc with
  | Var v -> Some v
  | Int _ | Float _ | Bool _ -> None
;;

(** Atoms appearing directly in [t] (excluding sub-anfs of [If]/[Switch]). *)
let atoms_of_term (t : term) : atom list =
  match t.desc with
  | Atom a | Index (a, _) | Field (a, _) -> [ a ]
  | Bop (_, l, r) -> [ l; r ]
  | Vec (_, atoms) | Builtin (_, atoms) | Record atoms | App (_, atoms) -> atoms
  | If _ | Switch _ -> []
;;

let vars_of_atoms atoms = List.filter_map atoms ~f:var_of_atom |> String.Set.of_list

(** Backward liveness analysis with simultaneous rewrite *)
let rec liveness_anf ~(live : String.Set.t) (a : anf) : anf * String.Set.t =
  match a.desc with
  | Continue args -> { a with desc = Continue args }, vars_of_atoms args
  | Return t ->
    let t, live = liveness_term ~live t in
    { a with desc = Return t }, live
  | Let (v, b, t) ->
    let t, live_t = liveness_anf ~live t in
    if not (Set.mem live_t v)
    then t, Set.remove live_t v
    else (
      let b, live_b = liveness_term ~live b in
      let live = Set.union (Set.remove live_t v) live_b in
      { a with desc = Let (v, b, t) }, live)
  | Placeholder (v, t) ->
    let t, live_t = liveness_anf ~live t in
    if Set.mem live_t v
    then { a with desc = Placeholder (v, t) }, Set.remove live_t v
    else t, live_t
  | Loop (params, body) ->
    (* Nothing can come after a [Loop], so we compute body liveness to fixed
       point, subtract param names, then add the variables referenced by inits *)
    let param_names = String.Set.of_list (List.map params ~f:fst) in
    let rec fix prev =
      let _, live = liveness_anf ~live:prev body in
      let next = Set.union prev live in
      if Set.equal prev next then prev else fix next
    in
    let live_body = fix String.Set.empty in
    let body, _ = liveness_anf ~live:live_body body in
    let live_inits = vars_of_atoms (List.map params ~f:snd) in
    let live = Set.union (Set.diff live_body param_names) live_inits in
    { a with desc = Loop (params, body) }, live

and liveness_term ~(live : String.Set.t) (t : term) : term * String.Set.t =
  match t.desc with
  | If (c, th, e) ->
    let th, live_th = liveness_anf ~live th in
    let e, live_e = liveness_anf ~live e in
    ( { t with desc = If (c, th, e) }
    , Set.union (vars_of_atoms [ c ]) (Set.union live_th live_e) )
  | Switch (s, cases) ->
    let live, cases =
      List.fold_map cases ~init:(vars_of_atoms [ s ]) ~f:(fun acc (lbl, a) ->
        let a, live_a = liveness_anf ~live a in
        Set.union acc live_a, (lbl, a))
    in
    { t with desc = Switch (s, cases) }, live
  | _ -> t, vars_of_atoms (atoms_of_term t)
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

let rec ty_refs (acc : String.Set.t) (ty : Lower_variants.ty) : String.Set.t =
  match ty with
  | TyFloat | TyInt | TyBool -> acc
  | TyVec (_, t) -> ty_refs acc t
  | TyArrow (a, b) -> ty_refs (ty_refs acc a) b
  | TyRecord s -> Set.add acc s
;;

let atom_refs acc (a : atom) =
  Option.fold (var_of_atom a) ~init:(ty_refs acc a.ty) ~f:Set.add
;;

let rec term_refs acc (t : term) =
  let acc = ty_refs acc t.ty in
  match t.desc with
  | If (c, th, e) -> List.fold [ th; e ] ~init:(atom_refs acc c) ~f:anf_refs
  | Switch (s, cases) ->
    List.fold (List.map cases ~f:snd) ~init:(atom_refs acc s) ~f:anf_refs
  | App (name, atoms) -> List.fold atoms ~init:(Set.add acc name) ~f:atom_refs
  | _ -> List.fold (atoms_of_term t) ~init:acc ~f:atom_refs

and anf_refs acc (a : anf) =
  let acc = ty_refs acc a.ty in
  match a.desc with
  | Return t -> term_refs acc t
  | Let (_, b, t) -> anf_refs (term_refs acc b) t
  | Placeholder (_, t) -> anf_refs acc t
  | Loop (params, body) ->
    let acc = params |> List.map ~f:snd |> List.fold ~init:acc ~f:atom_refs in
    anf_refs acc body
  | Continue args -> List.fold args ~init:acc ~f:atom_refs
;;

let top_refs (top : top) : String.Set.t =
  let empty = String.Set.empty in
  match top.desc with
  | Define { name = _; args; body; ret_ty } ->
    List.fold (ret_ty :: List.map args ~f:snd) ~init:(anf_refs empty body) ~f:ty_refs
  | Const (_, body) -> anf_refs empty body
  | TypeDef (_, RecordDecl fields) ->
    List.fold (List.map fields ~f:snd) ~init:empty ~f:ty_refs
  | TypeDef (_, VariantDecl ctors) ->
    List.fold (List.concat_map ctors ~f:snd) ~init:empty ~f:ty_refs
  | Extern _ -> empty
;;

let name_of_top (top : top) =
  match top.desc with
  | Define { name; _ } | Const (name, _) | Extern name | TypeDef (name, _) -> name
;;

(** Reachable top-level names starting from [main]. Covers defines, consts,
    externs, and type names. *)
let reachable_tops (tops : top list) : String.Set.t =
  let top_by_name =
    List.map tops ~f:(fun top -> name_of_top top, top)
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
      (match Map.find top_by_name name with
       | None -> dfs rest acc
       | Some top -> dfs (Set.to_list (top_refs top) @ rest) acc)
  in
  dfs [ "main" ] String.Set.empty
;;

let rewrite (Program tops : t) : t =
  let tops = List.map tops ~f:liveness_top in
  let live = reachable_tops tops in
  Program (List.filter tops ~f:(fun top -> Set.mem live (name_of_top top)))
;;
