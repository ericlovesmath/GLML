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

(** Coerce an atom to float if it is int-typed. *)
let coerce_atom (env : ty String.Map.t) (loc : Lexer.loc) (a : atom) : atom * bindings =
  match a.desc with
  | Int i -> { a with desc = Float (Float.of_int i) }, []
  | Var v when equal_ty (Map.find_exn env v) TyInt ->
    let v = Utils.fresh "pf" in
    { a with desc = Var v }, [ v, { desc = App ("float", [ a ]); ty = TyFloat; loc } ]
  | _ -> a, []
;;

let coerce_atoms env loc atoms =
  let atoms, binds = List.unzip (List.map atoms ~f:(coerce_atom env loc)) in
  atoms, List.concat binds
;;

(* TODO: I feel like the logic of this file can all be replaced with just
   "check if we have an int and type is defined as a float" or something? *)
let rec promote_anf
          (struct_env : ty list String.Map.t)
          (env : ty String.Map.t)
          (anf : anf)
  : anf
  =
  match anf.desc with
  | Let (v, bind, tl) ->
    let bind, binds = promote_term struct_env env bind in
    let env = Map.set env ~key:v ~data:bind.ty in
    let tl = promote_anf struct_env env tl in
    make_lets binds anf.loc { anf with desc = Let (v, bind, tl) }
  | Return term ->
    let term, binds = promote_term struct_env env term in
    make_lets binds anf.loc { anf with desc = Return term }
  | While (cond, body, tl) ->
    let cond, binds = promote_term struct_env env cond in
    let body = promote_anf struct_env env body in
    let after = promote_anf struct_env env tl in
    make_lets binds anf.loc { anf with desc = While (cond, body, after) }
  | Set (v, a, tl) ->
    let tl = promote_anf struct_env env tl in
    let a, binds =
      match Map.find env v with
      | Some TyFloat -> coerce_atom env anf.loc a
      | _ -> a, []
    in
    make_lets binds anf.loc { anf with desc = Set (v, a, tl) }
  | Continue -> anf

and promote_term (struct_env : ty list String.Map.t) (env : ty String.Map.t) (term : term)
  : term * bindings
  =
  let loc = term.loc in
  match term.desc, term.ty with
  | Atom a, TyFloat ->
    let a, bind = coerce_atom env loc a in
    { term with desc = Atom a }, bind
  | Bop (op, l, r), (TyFloat | TyVec _ | TyMat _) ->
    let l, bl = coerce_atom env loc l in
    let r, br = coerce_atom env loc r in
    { term with desc = Bop (op, l, r) }, bl @ br
  | Bop (((Lt | Gt | Leq | Geq) as op), l, r), TyBool
    when (equal_ty l.ty TyInt && equal_ty r.ty TyFloat)
         || (equal_ty l.ty TyFloat && equal_ty r.ty TyInt) ->
    let l, bl = coerce_atom env loc l in
    let r, br = coerce_atom env loc r in
    { term with desc = Bop (op, l, r) }, bl @ br
  | Vec (n, atoms), _ ->
    let atoms, binds = coerce_atoms env loc atoms in
    { term with desc = Vec (n, atoms) }, binds
  | Mat (n, m, atoms), _ ->
    let atoms, binds = coerce_atoms env loc atoms in
    { term with desc = Mat (n, m, atoms) }, binds
  | Builtin (f, atoms), (TyFloat | TyVec _ | TyMat _) ->
    let atoms, binds = coerce_atoms env loc atoms in
    { term with desc = Builtin (f, atoms) }, binds
  | Record (s, atoms), _ ->
    (match Map.find struct_env s with
     | None -> term, []
     | Some field_tys ->
       let atoms, binds =
         List.map2_exn atoms field_tys ~f:(fun atom field_ty ->
           match field_ty with
           | TyFloat -> coerce_atom env loc atom
           | _ -> atom, [])
         |> List.unzip
       in
       { term with desc = Record (s, atoms) }, List.concat binds)
  | If (c, t, e), _ ->
    let promote = promote_anf struct_env env in
    { term with desc = If (c, promote t, promote e) }, []
  | Switch (tag, cases), _ ->
    let desc =
      Switch (tag, List.map cases ~f:(fun (l, b) -> l, promote_anf struct_env env b))
    in
    { term with desc }, []
  | _, _ -> term, []
;;

let promote_top (struct_env : ty list String.Map.t) (env : ty String.Map.t) (top : top)
  : top
  =
  match top.desc with
  | Define ({ args; body; _ } as d) ->
    let env =
      List.fold args ~init:env ~f:(fun acc (v, ty) -> Map.set acc ~key:v ~data:ty)
    in
    let body = promote_anf struct_env env body in
    { top with desc = Define { d with body } }
  | Const (name, body) ->
    { top with desc = Const (name, promote_anf struct_env env body) }
  | Extern _ | TypeDef _ -> top
;;

let promote (Program tops : t) : t =
  let global_env =
    List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
      match top.desc with
      | Extern name -> Map.set acc ~key:name ~data:top.ty
      | Const (name, body) -> Map.set acc ~key:name ~data:body.ty
      | Define { name; _ } -> Map.set acc ~key:name ~data:top.ty
      | TypeDef _ -> acc)
  in
  let struct_env =
    List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
      match top.desc with
      | TypeDef (name, RecordDecl fields) ->
        Map.set acc ~key:name ~data:(List.map fields ~f:snd)
      | _ -> acc)
  in
  Program (List.map tops ~f:(promote_top struct_env global_env))
;;
