open Core
open Anf
open Monomorphize
open Lower_variants

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

let promote (Program tops : t) : t = Program (List.map tops ~f:map_ty_top)
