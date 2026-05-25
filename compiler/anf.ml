open Core
open Sexplib.Sexp

type atom_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Temp

let sexp_of_atom_desc = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Temp -> Atom "<temp>"
;;

type atom =
  { desc : atom_desc
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }

let sexp_of_atom t = sexp_of_atom_desc t.desc

type term_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | If of atom * anf * anf
  | Record of atom list
  | Field of atom * string
  | Variant of string * atom list
  | Match of atom * (Frontend.pat * anf) list

and term =
  { desc : term_desc
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }

and anf_desc =
  | Let of string * term * anf
  | Return of term

and anf =
  { desc : anf_desc
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc : term_desc -> Sexp.t = function
  | Atom a -> sexp_of_atom a
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_atom l; sexp_of_atom r ]
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_atom)
  | Index (t, i) -> List [ Atom "index"; sexp_of_atom t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_atom)
  | App (f, args) -> List (Atom f :: List.map args ~f:sexp_of_atom)
  | If (c, t, e) -> List [ Atom "if"; sexp_of_atom c; sexp_of_anf t; sexp_of_anf e ]
  | Record ts -> List (Atom "record" :: List.map ts ~f:sexp_of_atom)
  | Field (t, f) -> List [ Atom "."; sexp_of_atom t; Atom f ]
  | Variant (ctor, args) ->
    List (Atom "Variant" :: Atom ctor :: List.map args ~f:sexp_of_atom)
  | Match (scrutinee, cases) ->
    let sexp_of_case (pat, body) = List [ Frontend.sexp_of_pat pat; sexp_of_anf body ] in
    List (Atom "match" :: sexp_of_atom scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

and sexp_of_anf_desc = function
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_anf body ]
  | Return t -> List [ Atom "return"; sexp_of_term t ]

and sexp_of_anf t = sexp_of_anf_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; recur : Frontend.recur
      ; args : (string * Lower_tuples.ty) list
      ; body : anf
      ; ret_ty : Lower_tuples.ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * Lower_tuples.type_decl

let sexp_of_top_desc = function
  | Define { name; recur; args; body; ret_ty = _ } ->
    let args_sexp =
      List.map args ~f:(fun (v, ty) -> List [ Atom v; Lower_tuples.sexp_of_ty ty ])
    in
    List
      [ Atom "Define"
      ; Frontend.sexp_of_recur recur
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "args"; List args_sexp ]
      ; List [ Atom "body"; sexp_of_anf body ]
      ]
  | Const (name, term) -> List [ Atom "Const"; Atom name; sexp_of_anf term ]
  | Extern name -> List [ Atom "Extern"; Atom name ]
  | TypeDef (name, decl) ->
    List [ Atom "TypeDef"; Atom name; Lower_tuples.sexp_of_type_decl decl ]
;;

type top =
  { desc : top_desc
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }

let sexp_of_top t =
  List [ sexp_of_top_desc t.desc; Atom ":"; Lower_tuples.sexp_of_ty t.ty ]
;;

type t = Program of top list

let sexp_of_t (Program tops) = List (Atom "Program" :: List.map tops ~f:sexp_of_top)

let rec normalize (expr : Lambda_lift.term) : anf =
  let atom (desc : atom_desc) : term_desc = Atom { desc; ty = expr.ty; loc = expr.loc } in
  let pure (desc : term_desc) : anf =
    { desc = Return { desc; ty = expr.ty; loc = expr.loc }; ty = expr.ty; loc = expr.loc }
  in
  match expr.desc with
  | Var v -> pure (atom (Var v))
  | Float f -> pure (atom (Float f))
  | Int i -> pure (atom (Int i))
  | Bool b -> pure (atom (Bool b))
  | Let (v, bind, body) ->
    let bind = normalize bind in
    let body = normalize body in
    let pure (desc : anf_desc) : anf = { desc; ty = body.ty; loc = expr.loc } in
    let rec make_let (a : anf) =
      match a.desc with
      | Let (v, bind, body) -> pure (Let (v, bind, make_let body))
      | Return t -> pure (Let (v, t, body))
    in
    make_let bind
  | App (f, args) ->
    atomize f (fun (f : atom) ->
      match f.desc with
      | Var v -> atomize_list args (fun args_atoms -> pure (App (v, args_atoms)))
      | _ -> Compiler_error.raise ~pass:"anf" "app function must be a variable")
  | Bop (op, l, r) ->
    atomize l (fun l_atom -> atomize r (fun r_atom -> pure (Bop (op, l_atom, r_atom))))
  | Vec (n, ts) -> atomize_list ts (fun ts_atoms -> pure (Vec (n, ts_atoms)))
  | Index (t, i) -> atomize t (fun t_atom -> pure (Index (t_atom, i)))
  | Builtin (b, args) ->
    atomize_list args (fun args_atoms -> pure (Builtin (b, args_atoms)))
  | If (c, t, e) ->
    atomize c (fun c_atom ->
      let t_anf = normalize t in
      let e_anf = normalize e in
      pure (If (c_atom, t_anf, e_anf)))
  | Record ts -> atomize_list ts (fun args -> pure (Record args))
  | Field (t, f) -> atomize t (fun a -> pure (Field (a, f)))
  | Variant (ctor, args) -> atomize_list args (fun args -> pure (Variant (ctor, args)))
  | Match (scrutinee, cases) ->
    atomize scrutinee (fun s ->
      let cases = List.map cases ~f:(Tuple2.map_snd ~f:normalize) in
      pure (Match (s, cases)))

and atomize (expr : Lambda_lift.term) (k : atom -> anf) : anf =
  let pure (desc : atom_desc) : atom = { desc; ty = expr.ty; loc = expr.loc } in
  match expr.desc with
  | Var v -> k (pure (Var v))
  | Float f -> k (pure (Float f))
  | Int i -> k (pure (Int i))
  | Bool b -> k (pure (Bool b))
  | _ ->
    let anf_block = normalize expr in
    let v = Utils.fresh "anf" in
    let rem = k (pure (Var v)) in
    let pure (desc : anf_desc) : anf = { desc; ty = rem.ty; loc = expr.loc } in
    let rec make_let (a : anf) =
      match a.desc with
      | Let (v, bind, body) -> pure (Let (v, bind, make_let body))
      | Return t -> pure (Let (v, t, rem))
    in
    make_let anf_block

and atomize_list ts (k : atom list -> anf) =
  match ts with
  | [] -> k []
  | t :: ts -> atomize t (fun t -> atomize_list ts (fun ts -> k (t :: ts)))
;;

let normalize_top (t : Lambda_lift.top) : top =
  let pure desc = { desc; ty = t.ty; loc = t.loc } in
  match t.desc with
  | Define { name; recur; args; body; ret_ty } ->
    pure (Define { name; recur; args; body = normalize body; ret_ty })
  | Const (name, body) -> pure (Const (name, normalize body))
  | Extern v -> pure (Extern v)
  | TypeDef (name, decl) -> pure (TypeDef (name, decl))
;;

let to_anf (Program terms : Lambda_lift.t) : t Compiler_error.t =
  Compiler_error.try_with (fun () -> Program (List.map terms ~f:normalize_top))
;;
