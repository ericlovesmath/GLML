open Core
open Stlc

type atom =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
[@@deriving sexp_of]

type term =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec3 of atom * atom * atom
  | App of atom * atom
  | If of atom * anf * anf
  | Lam of string * Stlc.ty * anf
[@@deriving sexp_of]

and anf =
  | Let of string * term * anf
  | Return of term
[@@deriving sexp_of]

type t = Program of Stlc.ty String.Map.t * anf list [@@deriving sexp_of]

let type_of_atom ctx = function
  | Var v -> Map.find_exn ctx v
  | Float _ -> TyFloat
  | Int _ -> TyInt
  | Bool _ -> TyBool
;;

let rec type_of_term ctx = function
  | Atom a -> type_of_atom ctx a
  | Bop (op, l, r) ->
    let ty_l = type_of_atom ctx l in
    let ty_r = type_of_atom ctx r in
    (match op, ty_l, ty_r with
     | (Add | Sub | Mul | Div | Mod), TyFloat, TyFloat -> TyFloat
     | (Add | Sub | Mul | Div | Mod), TyInt, TyInt -> TyInt
     | (Add | Sub | Mul | Div | Mod), _, _ -> failwith "get_term_ty: invalid bop"
     | (Lt | Gt | Leq | Geq | Eq | And | Or), _, _ -> TyBool)
  | Vec3 _ -> TyVec3
  | App (f, _) ->
    (match type_of_atom ctx f with
     | TyArrow (_, r) -> r
     | _ -> failwith "get_term_ty: invalid app")
  | If (_, t, _) -> type_of ctx t
  | Lam (v, ty_v, t) ->
    let map = Map.set ctx ~key:v ~data:ty_v in
    TyArrow (ty_v, type_of map t)

(* TODO: Typechecking stage should maybe occur after ANF? This feels redundant *)
and type_of ctx = function
  | Let (v, term, body) ->
    let ty_v = type_of_term ctx term in
    type_of (Map.set ctx ~key:v ~data:ty_v) body
  | Return term -> type_of_term ctx term
;;

(* TODO: Refactor to use some kind of State monad + let* for implicit CPS *)
let rec normalize (map : ty String.Map.t) (expr : Stlc.term) : ty String.Map.t * anf =
  match expr with
  | Var v -> map, Return (Atom (Var v))
  | Float f -> map, Return (Atom (Float f))
  | Int i -> map, Return (Atom (Int i))
  | Bool b -> map, Return (Atom (Bool b))
  | Lam (v, ty_v, t) ->
    let map = Map.set map ~key:v ~data:ty_v in
    let _, t = normalize map t in
    map, Return (Lam (v, ty_v, t))
  | Let (v, bind, body) ->
    let map, bind_anf = normalize map bind in
    let ty_v = type_of map bind_anf in
    let map = Map.set map ~key:v ~data:ty_v in
    let map, body_anf = normalize map body in
    let rec make_let = function
      | Let (v, bind, body) -> Let (v, bind, make_let body)
      | Return t -> Let (v, t, body_anf)
    in
    map, make_let bind_anf
  | App (f, x) ->
    atomize map f (fun map f -> atomize map x (fun map x -> map, Return (App (f, x))))
  | Bop (op, l, r) ->
    atomize map l (fun map l -> atomize map r (fun map r -> map, Return (Bop (op, l, r))))
  | Vec3 (x, y, z) ->
    atomize map x (fun map x ->
      atomize map y (fun map y ->
        atomize map z (fun map z -> map, Return (Vec3 (x, y, z)))))
  | If (c, t, e) ->
    atomize map c (fun map c ->
      let map, t_anf = normalize map t in
      let map, e_anf = normalize map e in
      map, Return (If (c, t_anf, e_anf)))

and atomize (map : ty String.Map.t) (expr : Stlc.term) k : ty String.Map.t * anf =
  match expr with
  | Var v -> k map (Var v)
  | Float f -> k map (Float f)
  | Int i -> k map (Int i)
  | Bool b -> k map (Bool b)
  | _ ->
    let map, anf_block = normalize map expr in
    let ty = type_of map anf_block in
    let v = Utils.fresh "anf" in
    let map = Map.set map ~key:v ~data:ty in
    let map, rem = k map (Var v) in
    let rec make_let = function
      | Let (v, bind, body) -> Let (v, bind, make_let body)
      | Return t -> Let (v, t, rem)
    in
    map, make_let anf_block
;;

let to_anf (Program (map, terms) : Typecheck.t) : t =
  let map, anfs = List.fold_map terms ~init:map ~f:normalize in
  Program (map, anfs)
;;
