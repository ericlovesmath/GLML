open Core
open Anf
open Monomorphize
open Lower_variants

type bindings = (string * term) list

(** Prepend a list of (var, term) let-bindings before an anf node. *)
let make_lets (bindings : bindings) (loc : Lexer.loc) (body : anf) : anf =
  List.fold_right bindings ~init:body ~f:(fun (v, t) acc ->
    ({ desc = Let (v, t, acc); ty = body.ty; loc } : anf))
;;

let resolve_atom_ty (env : ty String.Map.t) (a : atom) : ty =
  match a.desc with
  | Var v ->
    (match Map.find env v with
     | Some ty -> ty
     | None -> a.ty)
  | _ -> a.ty
;;

(** Coerce an atom to float if it is int-typed. *)
let coerce_atom (env : ty String.Map.t) (loc : Lexer.loc) (a : atom) : atom * bindings =
  match a.desc with
  | Int i -> { a with desc = Float (Float.of_int i) }, []
  | Var v when equal_ty (Map.find_exn env v) TyInt ->
    let v = Utils.fresh "pf" in
    { a with desc = Var v }, [ v, { desc = Builtin (Float, [ a ]); ty = TyFloat; loc } ]
  | _ -> a, []
;;

let coerce_atoms env loc atoms =
  let atoms, binds = List.unzip (List.map atoms ~f:(coerce_atom env loc)) in
  atoms, List.concat binds
;;

(** Promote TyVec(n, TyInt) to TyVec(n, TyFloat) since GLSL has no ivec. *)
let rec promote_vec_ty = function
  | TyVec (n, TyInt) -> TyVec (n, TyFloat)
  | TyVec (n, inner) -> TyVec (n, promote_vec_ty inner)
  | TyArrow (a, b) -> TyArrow (promote_vec_ty a, promote_vec_ty b)
  | ty -> ty
;;

let map_ty_atom (a : atom) : atom = { a with ty = promote_vec_ty a.ty }

let rec map_ty_term (term : term) : term =
  let desc =
    match term.desc with
    | Atom a -> Atom (map_ty_atom a)
    | Bop (op, l, r) -> Bop (op, map_ty_atom l, map_ty_atom r)
    | Vec (n, atoms) -> Vec (n, List.map atoms ~f:map_ty_atom)
    | Index (a, i) -> Index (map_ty_atom a, i)
    | Builtin (f, atoms) -> Builtin (f, List.map atoms ~f:map_ty_atom)
    | App (f, atoms) -> App (f, List.map atoms ~f:map_ty_atom)
    | If (c, t, e) -> If (map_ty_atom c, map_ty_anf t, map_ty_anf e)
    | Record (s, atoms) -> Record (s, List.map atoms ~f:map_ty_atom)
    | Field (a, f) -> Field (map_ty_atom a, f)
    | Switch (tag, cases) ->
      Switch (map_ty_atom tag, List.map cases ~f:(fun (l, b) -> l, map_ty_anf b))
  in
  { term with desc; ty = promote_vec_ty term.ty }

and map_ty_anf (anf : anf) : anf =
  let anf = { anf with ty = promote_vec_ty anf.ty } in
  match anf.desc with
  | Let (v, bind, tl) -> { anf with desc = Let (v, map_ty_term bind, map_ty_anf tl) }
  | Return t -> { anf with desc = Return (map_ty_term t) }
  | While (cond, body, tl) ->
    { anf with desc = While (map_ty_term cond, map_ty_anf body, map_ty_anf tl) }
  | Set (v, a, tl) -> { anf with desc = Set (v, map_ty_atom a, map_ty_anf tl) }
  | Continue -> anf
;;

let map_ty_top (top : top) : top =
  let top = { top with ty = promote_vec_ty top.ty } in
  match top.desc with
  | Define ({ args; body; ret_ty; _ } as d) ->
    let args = List.map args ~f:(fun (v, ty) -> v, promote_vec_ty ty) in
    let ret_ty = promote_vec_ty ret_ty in
    { top with desc = Define { d with args; body = map_ty_anf body; ret_ty } }
  | Const (name, body) -> { top with desc = Const (name, map_ty_anf body) }
  | Extern _ -> top
  | TypeDef (name, RecordDecl fields) ->
    let fields = List.map fields ~f:(fun (f, ty) -> f, promote_vec_ty ty) in
    { top with desc = TypeDef (name, RecordDecl fields) }
  | TypeDef (name, VariantDecl ctors) ->
    let ctors = List.map ctors ~f:(fun (c, tys) -> c, List.map tys ~f:promote_vec_ty) in
    { top with desc = TypeDef (name, VariantDecl ctors) }
;;

(** Split an arrow type into its parameter types and the return type after [n] applications. *)
let arrow_parts (fn_ty : ty) (n_args : int) : ty list * ty =
  let rec collect = function
    | TyArrow (p, rest) -> p :: collect rest
    | _ -> []
  in
  let rec skip n = function
    | TyArrow (_, rest) when n > 0 -> skip (n - 1) rest
    | t -> t
  in
  collect fn_ty, skip n_args fn_ty
;;

let rec promote_anf (env : ty String.Map.t) (anf : anf) : anf =
  match anf.desc with
  | Let (v, bind, tl) ->
    let bind, binds = promote_term env bind in
    let env = Map.set env ~key:v ~data:bind.ty in
    let tl = promote_anf env tl in
    make_lets binds anf.loc { anf with desc = Let (v, bind, tl) }
  | Return term ->
    let term, binds = promote_term env term in
    make_lets binds anf.loc { anf with desc = Return term }
  | While (cond, body, tl) ->
    let cond, binds = promote_term env cond in
    let body = promote_anf env body in
    let after = promote_anf env tl in
    make_lets binds anf.loc { anf with desc = While (cond, body, after) }
  | Set (v, a, tl) ->
    let tl = promote_anf env tl in
    let a, binds =
      match Map.find env v with
      | Some TyFloat -> coerce_atom env anf.loc a
      | _ -> a, []
    in
    make_lets binds anf.loc { anf with desc = Set (v, a, tl) }
  | Continue -> anf

and promote_term (env : ty String.Map.t) (term : term) : term * bindings =
  let loc = term.loc in
  match term.desc, term.ty with
  | Atom a, TyFloat ->
    let a, bind = coerce_atom env loc a in
    { term with desc = Atom a }, bind
  | Atom ({ desc = Var v; _ } as a), TyInt ->
    (match Map.find env v with
     | Some TyFloat -> { term with desc = Atom a; ty = TyFloat }, []
     | _ -> term, [])
  | Atom _, _ -> term, []
  | Bop (op, l, r), (TyFloat | TyVec _) ->
    let l, bl = coerce_atom env loc l in
    let r, br = coerce_atom env loc r in
    { term with desc = Bop (op, l, r) }, bl @ br
  | Bop (((Lt | Gt | Leq | Geq) as op), l, r), TyBool
  (* TODO: Insane behavior that I should not be tolerating these stupid edge cases for *)
    when (equal_ty l.ty TyInt && equal_ty r.ty TyFloat)
         || (equal_ty l.ty TyFloat && equal_ty r.ty TyInt) ->
    let l, bl = coerce_atom env loc l in
    let r, br = coerce_atom env loc r in
    { term with desc = Bop (op, l, r) }, bl @ br
  | Bop (op, l, r), TyInt ->
    let lty = resolve_atom_ty env l in
    let rty = resolve_atom_ty env r in
    if equal_ty lty TyFloat || equal_ty rty TyFloat
    then (
      let l, bl = coerce_atom env loc l in
      let r, br = coerce_atom env loc r in
      { term with desc = Bop (op, l, r); ty = TyFloat }, bl @ br)
    else term, []
  | Bop _, _ -> term, []
  | Vec (n, atoms), _ ->
    let atoms, binds = coerce_atoms env loc atoms in
    { term with desc = Vec (n, atoms) }, binds
  | Builtin (f, atoms), (TyFloat | TyVec _) ->
    let atoms, binds =
      match f with
      | Float -> atoms, []
      | _ -> coerce_atoms env loc atoms
    in
    { term with desc = Builtin (f, atoms) }, binds
  | Builtin _, _ -> term, []
  | Index (a, i), TyInt ->
    let is_elem_float =
      match resolve_atom_ty env a with
      | TyVec (_, TyFloat) -> true
      | _ -> false
    in
    (if is_elem_float then { term with desc = Index (a, i); ty = TyFloat } else term), []
  | Index _, _ -> term, []
  | App (f, atoms), _ ->
    let param_tys, ret_ty =
      match Map.find env f with
      | Some fn_ty -> arrow_parts fn_ty (List.length atoms)
      | None -> [], term.ty
    in
    let atoms, binds =
      atoms
      |> List.mapi ~f:(fun i a ->
        match List.nth param_tys i with
        | Some TyFloat -> coerce_atom env loc a
        | _ -> a, [])
      |> List.unzip
      |> Tuple2.map_snd ~f:List.concat
    in
    { term with desc = App (f, atoms); ty = ret_ty }, binds
  | Record (s, atoms), _ ->
    let atoms, binds =
      atoms
      |> List.map ~f:(fun a ->
        if equal_ty a.ty TyFloat then coerce_atom env loc a else a, [])
      |> List.unzip
      |> Tuple2.map_snd ~f:List.concat
    in
    { term with desc = Record (s, atoms) }, binds
  | If (c, t, e), _ ->
    { term with desc = If (c, promote_anf env t, promote_anf env e) }, []
  | Switch (tag, cases), _ ->
    let desc = Switch (tag, List.map cases ~f:(fun (l, b) -> l, promote_anf env b)) in
    { term with desc }, []
  | Field _, _ -> term, []
;;

let promote_top (env : ty String.Map.t) (top : top) : top =
  match top.desc with
  | Define ({ args; body; _ } as d) ->
    let env =
      List.fold args ~init:env ~f:(fun acc (v, ty) -> Map.set acc ~key:v ~data:ty)
    in
    { top with desc = Define { d with body = promote_anf env body } }
  | Const (name, body) -> { top with desc = Const (name, promote_anf env body) }
  | Extern _ | TypeDef _ -> top
;;

let promote (Program tops : t) : t =
  let tops = List.map tops ~f:map_ty_top in
  let global_env =
    List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
      match top.desc with
      | Extern name -> Map.set acc ~key:name ~data:top.ty
      | Const (name, body) -> Map.set acc ~key:name ~data:body.ty
      | Define { name; _ } -> Map.set acc ~key:name ~data:top.ty
      | TypeDef _ -> acc)
  in
  Program (List.map tops ~f:(promote_top global_env))
;;
