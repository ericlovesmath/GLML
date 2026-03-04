open Core
open Stlc
open Sexplib.Sexp

type atom =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool

let sexp_of_atom = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
;;

type term_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Mat of int * int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | If of atom * anf * anf
  | Lam of (string * Stlc.ty) list * anf

and term =
  { desc : term_desc
  ; loc : Lexer.loc
  }

and anf_desc =
  | Let of string * term * anf
  | Return of term

and anf =
  { desc : anf_desc
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc : term_desc -> Sexp.t = function
  | Atom a -> sexp_of_atom a
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_atom l; sexp_of_atom r ]
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_atom)
  | Mat (x, y, ts) ->
    List
      (Atom ("mat" ^ Int.to_string x ^ "x" ^ Int.to_string y)
       :: List.map ts ~f:sexp_of_atom)
  | Index (t, i) -> List [ Atom "index"; sexp_of_atom t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_atom)
  | App (f, args) -> List (Atom f :: List.map args ~f:sexp_of_atom)
  | If (c, t, e) -> List [ Atom "if"; sexp_of_atom c; sexp_of_anf t; sexp_of_anf e ]
  | Lam (args, body) ->
    let args = List.map args ~f:(fun (v, ty) -> List [ Atom v; Stlc.sexp_of_ty ty ]) in
    List [ Atom "lambda"; List args; sexp_of_anf body ]

and sexp_of_term (t : term) = sexp_of_term_desc t.desc

and sexp_of_anf_desc = function
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_anf body ]
  | Return t -> List [ Atom "return"; sexp_of_term t ]

and sexp_of_anf (a : anf) = sexp_of_anf_desc a.desc

type top_desc =
  | Define of string * anf
  | Extern of ty * string
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; loc : Lexer.loc
  }

let sexp_of_top t = sexp_of_top_desc t.desc

type t = Program of Stlc.ty String.Map.t * top list [@@deriving sexp_of]

let type_of_atom ctx = function
  | Var v -> Map.find_exn ctx v
  | Float _ -> TyFloat
  | Int _ -> TyInt
  | Bool _ -> TyBool
;;

let rec type_of_term ctx (t : term) =
  match t.desc with
  | Atom a -> type_of_atom ctx a
  | Bop (op, l, r) ->
    let ty_l = type_of_atom ctx l in
    let ty_r = type_of_atom ctx r in
    (match op, ty_l, ty_r with
     | (Add | Sub | Mul | Div | Mod), TyFloat, TyFloat -> TyFloat
     | (Add | Sub | Mul | Div | Mod), TyInt, TyInt -> TyInt
     | (Add | Sub), TyVec n, TyVec _ -> TyVec n
     | (Mul | Div), TyVec n, TyFloat | (Mul | Div), TyFloat, TyVec n -> TyVec n
     | Mul, TyVec n, TyVec _ -> TyVec n
     | (Add | Sub | Mul | Div | Mod), TyMat (x, y), TyMat (x', y') when x = x' && y = y'
       -> TyMat (x, y)
     | (Mul | Div), TyMat (x, y), TyFloat | (Mul | Div), TyFloat, TyMat (x, y) ->
       TyMat (x, y)
     | (Mul | Div), TyMat (x, y), TyVec n when y = n -> TyVec x
     | (Add | Sub | Mul | Div | Mod), _, _ -> failwith "get_term_ty: invalid bop"
     | (Lt | Gt | Leq | Geq | Eq | And | Or), _, _ -> TyBool)
  | Vec (n, _) -> TyVec n
  | Mat (x, y, _) -> TyMat (x, y)
  | Index (t, _) ->
    (match type_of_atom ctx t with
     | TyVec _ -> TyFloat
     | TyMat (_, y) -> TyVec y
     | _ -> failwith "get_term_ty: invalid index")
  | Builtin (name, args) ->
    let tys = List.map args ~f:(type_of_atom ctx) in
    let check_unary_math () =
      match tys with
      | [ TyFloat ] -> TyFloat
      | [ TyVec n ] -> TyVec n
      | _ -> failwith "get_term_ty: builtin unary math error"
    in
    let check_binary_math () =
      match tys with
      | [ TyFloat; TyFloat ] -> TyFloat
      | [ TyVec n; TyVec n' ] when n = n' -> TyVec n
      | [ TyVec n; TyFloat ] | [ TyFloat; TyVec n ] -> TyVec n
      | _ -> failwith "get_term_ty: builtin binary math error"
    in
    let check_geometric () =
      match name, tys with
      | (Length | Distance | Dot), _ -> TyFloat
      | Cross, [ TyVec 3; TyVec 3 ] -> TyVec 3
      | Normalize, [ TyVec n ] -> TyVec n
      | Normalize, [ TyFloat ] -> TyFloat
      | _ -> failwith "get_term_ty: builtin geometric error"
    in
    let check_common () =
      match name, tys with
      | (Abs | Sign | Floor | Ceil), _ -> check_unary_math ()
      | (Min | Max), _ -> check_binary_math ()
      | Clamp, [ TyFloat; TyFloat; TyFloat ] -> TyFloat
      | Clamp, [ TyVec n; TyFloat; TyFloat ] -> TyVec n
      | Clamp, [ TyVec n; TyVec _; TyVec _ ] -> TyVec n
      | Mix, [ TyFloat; TyFloat; TyFloat ] -> TyFloat
      | Mix, [ TyVec n; TyVec _; TyFloat ] -> TyVec n
      | Mix, [ TyVec n; TyVec _; TyVec _ ] -> TyVec n
      | _ -> failwith "get_term_ty: builtin common error"
    in
    (match name with
     | Sin | Cos | Tan | Asin | Acos | Atan | Exp | Log | Exp2 | Log2 | Sqrt ->
       check_unary_math ()
     | Pow -> check_binary_math ()
     | Length | Distance | Dot | Cross | Normalize -> check_geometric ()
     | Abs | Sign | Floor | Ceil | Min | Max | Clamp | Mix -> check_common ())
  | App (f, args) ->
    let rec unpeel_type ty n =
      if n = 0
      then ty
      else (
        match ty with
        | Stlc.TyArrow (_, r) -> unpeel_type r (n - 1)
        | _ -> failwith "get_term_ty: applying too many arguments")
    in
    unpeel_type (Map.find_exn ctx f) (List.length args)
  | If (_, t, _) -> type_of ctx t
  | Lam (args, anf) ->
    let ctx =
      List.fold args ~init:ctx ~f:(fun map (v, ty) -> Map.set map ~key:v ~data:ty)
    in
    let ret_ty = type_of ctx anf in
    List.fold_right args ~init:ret_ty ~f:(fun (_, ty) acc -> Stlc.TyArrow (ty, acc))

and type_of ctx (anf : anf) =
  match anf.desc with
  | Let (v, term, body) ->
    let ty_v = type_of_term ctx term in
    type_of (Map.set ctx ~key:v ~data:ty_v) body
  | Return term -> type_of_term ctx term
;;

let rec normalize (map : Stlc.ty String.Map.t) (expr : Uncurry.term)
  : Stlc.ty String.Map.t * anf
  =
  let pure (desc : term_desc) : anf =
    { desc = Return { desc; loc = expr.loc }; loc = expr.loc }
  in
  match expr.desc with
  | Var v -> map, pure (Atom (Var v))
  | Float f -> map, pure (Atom (Float f))
  | Int i -> map, pure (Atom (Int i))
  | Bool b -> map, pure (Atom (Bool b))
  | Lam (args, t) ->
    let map =
      List.fold args ~init:map ~f:(fun map (v, ty) -> Map.set map ~key:v ~data:ty)
    in
    let map, t_anf = normalize map t in
    map, pure (Lam (args, t_anf))
  | Let (v, bind, body) ->
    let map, bind_anf = normalize map bind in
    let ty_v = type_of map bind_anf in
    let map = Map.set map ~key:v ~data:ty_v in
    let map, body_anf = normalize map body in
    let pure (desc : anf_desc) : anf = { desc; loc = expr.loc } in
    let rec make_let (a : anf) =
      match a.desc with
      | Let (v', bind', body') -> pure (Let (v', bind', make_let body'))
      | Return t -> pure (Let (v, t, body_anf))
    in
    map, make_let bind_anf
  | App (f, args) ->
    atomize map f (fun map f_atom ->
      match f_atom with
      | Var v ->
        atomize_list map args (fun map args_atoms -> map, pure (App (v, args_atoms)))
      | _ -> failwith "normalize: app function must be a variable for now")
  | Bop (op, l, r) ->
    atomize map l (fun map l_atom ->
      atomize map r (fun map r_atom -> map, pure (Bop (op, l_atom, r_atom))))
  | Vec (n, ts) -> atomize_list map ts (fun map ts_atoms -> map, pure (Vec (n, ts_atoms)))
  | Mat (x, y, ts) ->
    atomize_list map ts (fun map ts_atoms -> map, pure (Mat (x, y, ts_atoms)))
  | Index (t, i) -> atomize map t (fun map t_atom -> map, pure (Index (t_atom, i)))
  | Builtin (b, args) ->
    atomize_list map args (fun map args_atoms -> map, pure (Builtin (b, args_atoms)))
  | If (c, t, e) ->
    atomize map c (fun map c_atom ->
      let map, t_anf = normalize map t in
      let map, e_anf = normalize map e in
      map, pure (If (c_atom, t_anf, e_anf)))

and atomize (map : Stlc.ty String.Map.t) (expr : Uncurry.term) k
  : Stlc.ty String.Map.t * anf
  =
  match expr.desc with
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
    let pure (desc : anf_desc) : anf = { desc; loc = expr.loc } in
    let rec make_let (a : anf) =
      match a.desc with
      | Let (v', bind', body') -> pure (Let (v', bind', make_let body'))
      | Return t -> pure (Let (v, t, rem))
    in
    map, make_let anf_block

and atomize_list map ts k =
  match ts with
  | [] -> k map []
  | t :: ts ->
    atomize map t (fun map t -> atomize_list map ts (fun map ts -> k map (t :: ts)))
;;

let normalize_top (map : Stlc.ty String.Map.t) (t : Uncurry.top)
  : Stlc.ty String.Map.t * top
  =
  let pure (desc : top_desc) : top = { desc; loc = t.loc } in
  match t.desc with
  | Define (v, bind) ->
    let map, bind_anf = normalize map bind in
    let ty_v = type_of map bind_anf in
    let map = Map.set map ~key:v ~data:ty_v in
    map, pure (Define (v, bind_anf))
  | Extern (ty, v) -> map, pure (Extern (ty, v))
;;

let to_anf (Program (map, terms) : Uncurry.t) : t =
  let map, anfs = List.fold_map terms ~init:map ~f:normalize_top in
  Program (map, anfs)
;;
