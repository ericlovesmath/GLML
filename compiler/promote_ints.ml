open Core
open Anf
open Monomorphize
open Lower_variants

(* This pass inserts [float()] calls whenever an int is used in a float context.

   The pass works as a bidirectional checker for coercion and expected types.

   Once the typechecker has stamped every node with its intended type, we can
   insert [float()] casts purely by walking the tree with an [expected] type
   threaded down from each parent. At every parent to child boundary we derive
   the child's expected type. At each [Atom] whose type is [TyInt] in a context
   expecting [TyFloat], we emit the cast. *)

type bindings = (string * term) list

(** Prepend a list of (var, term) let-bindings before an anf node. *)
let make_lets (bindings : bindings) (loc : Lexer.loc) (body : anf) : anf =
  List.fold_right bindings ~init:body ~f:(fun (v, t) acc ->
    ({ desc = Let (v, t, acc); ty = body.ty; loc } : anf))
;;

let resolve_atom_ty (env : ty String.Map.t) (a : atom) : ty =
  match a.desc with
  | Var v -> Map.find env v |> Option.value ~default:a.ty
  | Float _ -> TyFloat
  | Int _ -> TyInt
  | Bool _ -> TyBool
  | Temp -> a.ty
;;

let coerce_atom_to_float (env : ty String.Map.t) (loc : Lexer.loc) (a : atom)
  : atom * bindings
  =
  let actual = resolve_atom_ty env a in
  match a.desc, actual with
  | Int i, _ -> { a with desc = Float (Float.of_int i); ty = TyFloat }, []
  | Var _, TyInt ->
    let v = Utils.fresh "pf" in
    let term : term =
      { desc = Builtin (Float, [ { a with ty = TyInt } ]); ty = TyFloat; loc }
    in
    { a with desc = Var v; ty = TyFloat }, [ v, term ]
  | _ -> a, []
;;

(** Coerce an atom toward an expected type. Currently we only insert casts at
    the int -> float boundary, so everything else is identity. *)
let coerce_atom_to ~(expected : ty) env loc (a : atom) : atom * bindings =
  match expected with
  | TyFloat -> coerce_atom_to_float env loc a
  | _ -> a, []
;;

let coerce_atoms_to ~expected env loc atoms =
  let atoms, binds = List.unzip (List.map atoms ~f:(coerce_atom_to ~expected env loc)) in
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

(** Split an arrow type into its params and return ty after [n] applications *)
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

type struct_env = (string * ty) list String.Map.t

let rec promote_term
          ~(expected : ty)
          (env : ty String.Map.t)
          (structs : struct_env)
          (term : term)
  : term * bindings
  =
  let loc = term.loc in
  match term.desc with
  | Atom a ->
    let a, binds = coerce_atom_to ~expected env loc a in
    (* NOTE: Only override if we actually promoted to float. *)
    let new_ty = if equal_ty expected TyFloat then a.ty else term.ty in
    { term with desc = Atom a; ty = new_ty }, binds
  | Bop (op, l, r) ->
    let ty_l = resolve_atom_ty env l in
    let ty_r = resolve_atom_ty env r in
    let is_scalar = function
      | TyFloat | TyInt -> true
      | _ -> false
    in
    let l, bl, r, br, result_ty =
      match op with
      | Add | Sub | Mul | Div | Mod ->
        if is_scalar ty_l && is_scalar ty_r
        then (
          let want_float =
            equal_ty expected TyFloat || equal_ty ty_l TyFloat || equal_ty ty_r TyFloat
          in
          if want_float
          then (
            let l, bl = coerce_atom_to_float env loc l in
            let r, br = coerce_atom_to_float env loc r in
            l, bl, r, br, TyFloat)
          else l, [], r, [], term.ty)
        else (
          (* A vec is involved. Vec elements are float post lower_vec_int_to_float,
             so any scalar operand needs to coerce to float; vec operands stay. *)
          let l, bl = coerce_atom_to_float env loc l in
          let r, br = coerce_atom_to_float env loc r in
          l, bl, r, br, term.ty)
      | Lt | Gt | Leq | Geq | Eq ->
        if is_scalar ty_l && is_scalar ty_r && (equal_ty ty_l TyFloat || equal_ty ty_r TyFloat)
        then (
          let l, bl = coerce_atom_to_float env loc l in
          let r, br = coerce_atom_to_float env loc r in
          l, bl, r, br, term.ty)
        else l, [], r, [], term.ty
      | And | Or -> l, [], r, [], term.ty
    in
    { term with desc = Bop (op, l, r); ty = result_ty }, bl @ br
  | Vec (n, atoms) ->
    let atoms, binds = coerce_atoms_to ~expected:TyFloat env loc atoms in
    { term with desc = Vec (n, atoms) }, binds
  | Index (a, i) ->
    let elem_ty =
      match resolve_atom_ty env a with
      | TyVec (_, t) -> t
      | _ -> term.ty
    in
    { term with desc = Index (a, i); ty = elem_ty }, []
  | Builtin (Float, atoms) ->
    (* The cast itself; don't re-coerce its argument. *)
    { term with desc = Builtin (Float, atoms) }, []
  | Builtin (f, atoms) ->
    let atoms, binds = coerce_atoms_to ~expected:TyFloat env loc atoms in
    { term with desc = Builtin (f, atoms) }, binds
  | App (fname, atoms) ->
    let param_tys, ret_ty =
      match Map.find env fname with
      | Some fn_ty -> arrow_parts fn_ty (List.length atoms)
      | None -> [], term.ty
    in
    let atoms, binds =
      atoms
      |> List.mapi ~f:(fun i a ->
        match List.nth param_tys i with
        | Some pt -> coerce_atom_to ~expected:pt env loc a
        | None -> a, [])
      |> List.unzip
      |> Tuple2.map_snd ~f:List.concat
    in
    { term with desc = App (fname, atoms); ty = ret_ty }, binds
  | If (c, t, e) ->
    let t' = promote_anf ~expected env structs t in
    let e' = promote_anf ~expected env structs e in
    { term with desc = If (c, t', e'); ty = expected }, []
  | Record (sname, atoms) ->
    let field_tys =
      match Map.find structs sname with
      | Some fields -> List.map fields ~f:snd
      | None -> List.map atoms ~f:(fun a -> a.ty)
    in
    let pairs =
      match List.zip atoms field_tys with
      | Ok ps -> ps
      | Unequal_lengths -> List.map atoms ~f:(fun a -> a, a.ty)
    in
    let atoms, binds =
      pairs
      |> List.map ~f:(fun (a, ft) -> coerce_atom_to ~expected:ft env loc a)
      |> List.unzip
      |> Tuple2.map_snd ~f:List.concat
    in
    { term with desc = Record (sname, atoms) }, binds
  | Field (a, f) ->
    let field_ty =
      match resolve_atom_ty env a with
      | TyRecord sname ->
        Map.find structs sname
        |> Option.bind ~f:(fun fields -> List.Assoc.find fields f ~equal:String.equal)
        |> Option.value ~default:term.ty
      | _ -> term.ty
    in
    { term with desc = Field (a, f); ty = field_ty }, []
  | Switch (tag, cases) ->
    let cases =
      List.map cases ~f:(fun (l, b) -> l, promote_anf ~expected env structs b)
    in
    { term with desc = Switch (tag, cases) }, []

and promote_anf
      ~(expected : ty)
      (env : ty String.Map.t)
      (structs : struct_env)
      (anf : anf)
  : anf
  =
  match anf.desc with
  | Let (v, bind, tl) ->
    let bind, binds = promote_term ~expected:bind.ty env structs bind in
    let env = Map.set env ~key:v ~data:bind.ty in
    let tl = promote_anf ~expected env structs tl in
    make_lets binds anf.loc { anf with desc = Let (v, bind, tl); ty = tl.ty }
  | Return term ->
    let term, binds = promote_term ~expected env structs term in
    make_lets binds anf.loc { anf with desc = Return term; ty = term.ty }
  | While (cond, body, tl) ->
    let cond, binds = promote_term ~expected:TyBool env structs cond in
    let body = promote_anf ~expected:body.ty env structs body in
    let tl = promote_anf ~expected env structs tl in
    make_lets binds anf.loc { anf with desc = While (cond, body, tl); ty = tl.ty }
  | Set (v, a, tl) ->
    let v_ty = Map.find env v |> Option.value ~default:a.ty in
    let a, binds = coerce_atom_to ~expected:v_ty env anf.loc a in
    let tl = promote_anf ~expected env structs tl in
    make_lets binds anf.loc { anf with desc = Set (v, a, tl); ty = tl.ty }
  | Continue -> anf
;;

let promote_top env structs (top : top) : top =
  match top.desc with
  | Define ({ args; body; ret_ty; _ } as d) ->
    let env =
      List.fold args ~init:env ~f:(fun acc (v, ty) -> Map.set acc ~key:v ~data:ty)
    in
    let body = promote_anf ~expected:ret_ty env structs body in
    { top with desc = Define { d with body } }
  | Const (name, body) ->
    let body = promote_anf ~expected:body.ty env structs body in
    { top with desc = Const (name, body) }
  | Extern _ | TypeDef _ -> top
;;

let build_global_env tops =
  List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
    match top.desc with
    | Extern name -> Map.set acc ~key:name ~data:top.ty
    | Const (name, body) -> Map.set acc ~key:name ~data:body.ty
    | Define { name; _ } -> Map.set acc ~key:name ~data:top.ty
    | TypeDef _ -> acc)
;;

let build_struct_env tops : struct_env =
  List.fold tops ~init:String.Map.empty ~f:(fun acc top ->
    match top.desc with
    | TypeDef (name, RecordDecl fields) -> Map.set acc ~key:name ~data:fields
    | _ -> acc)
;;

let promote (Program tops : t) : t =
  let tops = List.map tops ~f:map_ty_top in
  let global_env = build_global_env tops in
  let structs = build_struct_env tops in
  Program (List.map tops ~f:(promote_top global_env structs))
;;
