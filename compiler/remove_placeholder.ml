open Core
open Lower_variants
open Sexplib.Sexp

type atom_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool

let sexp_of_atom_desc = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
;;

type atom =
  { desc : atom_desc
  ; ty : ty
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
  | Switch of atom * (Glsl.switch_case * anf) list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

and anf_desc =
  | Let of string * term * anf
  | Placeholder of string * anf
  | Return of term
  | Loop of (string * atom) list * anf
  | Continue of atom list

and anf =
  { desc : anf_desc
  ; ty : ty
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
  | Switch (tag, cases) ->
    let sexp_of_case (label, body) =
      let lbl =
        match label with
        | Glsl.Case i -> Int.to_string i
        | Glsl.Default -> "default"
      in
      List [ Atom lbl; sexp_of_anf body ]
    in
    List (Atom "switch" :: sexp_of_atom tag :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

and sexp_of_anf_desc = function
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_anf body ]
  | Placeholder (s, body) -> List [ Atom "placeholder"; Atom s; sexp_of_anf body ]
  | Return t -> List [ Atom "return"; sexp_of_term t ]
  | Loop (params, body) ->
    let sexp_of_param (n, init) = List [ Atom n; sexp_of_atom init ] in
    List [ Atom "loop"; List (List.map params ~f:sexp_of_param); sexp_of_anf body ]
  | Continue args -> List (Atom "continue" :: List.map args ~f:sexp_of_atom)

and sexp_of_anf t = sexp_of_anf_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; args : (string * ty) list
      ; body : anf
      ; ret_ty : ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * type_decl

let sexp_of_top_desc = function
  | Define { name; args; body; ret_ty = _ } ->
    let args_sexp = List.map args ~f:(fun (v, ty) -> List [ Atom v; sexp_of_ty ty ]) in
    List
      [ Atom "Define"
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "args"; List args_sexp ]
      ; List [ Atom "body"; sexp_of_anf body ]
      ]
  | Const (name, term) -> List [ Atom "Const"; Atom name; sexp_of_anf term ]
  | Extern name -> List [ Atom "Extern"; Atom name ]
  | TypeDef (name, decl) -> List [ Atom "TypeDef"; Atom name; sexp_of_type_decl decl ]
;;

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let sexp_of_top t = List [ sexp_of_top_desc t.desc; Atom ":"; sexp_of_ty t.ty ]

type t = Program of top list

let sexp_of_t (Program tops) = List (Atom "Program" :: List.map tops ~f:sexp_of_top)

type bindings = (string * ty) list

(** Prepend a list of (var, ty) placeholder bindings before an anf node. *)
let make_lets (loc : Lexer.loc) (placeholders : bindings) (body : anf) : anf =
  List.fold_right placeholders ~init:body ~f:(fun (v, ty) acc ->
    ({ desc = Placeholder (v, acc); ty; loc } : anf))
;;

let remove_atom (atom : Lower_variants.atom) : atom * bindings =
  let pure desc : atom = { desc; ty = atom.ty; loc = atom.loc } in
  match atom.desc with
  | Temp ->
    (match atom.ty with
     | TyFloat -> pure (Float 0.0), []
     | TyInt -> pure (Int 0), []
     | TyBool -> pure (Bool false), []
     | TyVec _ | TyRecord _ ->
       let name = Utils.fresh "_tmp" in
       pure (Var name), [ name, atom.ty ]
     | TyArrow _ -> failwith "no placeholders for arrow types")
  | Var v -> pure (Var v), []
  | Float f -> pure (Float f), []
  | Int i -> pure (Int i), []
  | Bool b -> pure (Bool b), []
;;

let remove_atoms (atoms : Lower_variants.atom list) : atom list * bindings =
  let atoms, binds = List.unzip (List.map ~f:remove_atom atoms) in
  atoms, List.concat binds
;;

let rec remove_anf (anf : Lower_variants.anf) : anf =
  let make bindings desc =
    make_lets anf.loc bindings { desc; ty = anf.ty; loc = anf.loc }
  in
  match anf.desc with
  | Let (v, bind, tl) ->
    let bind, binds = remove_term bind in
    let tl = remove_anf tl in
    make binds (Let (v, bind, tl))
  | Return term ->
    let term, binds = remove_term term in
    make binds (Return term)
  | Loop (params, body) ->
    let names, binds =
      params
      |> List.map ~f:(fun (n, a) ->
        let a, b = remove_atom a in
        (n, a), b)
      |> List.unzip
    in
    let binds = List.concat binds in
    let body = remove_anf body in
    make binds (Loop (names, body))
  | Continue args ->
    let args, binds = remove_atoms args in
    make binds (Continue args)

and remove_term (term : Lower_variants.term) : term * bindings =
  let pure desc bindings = ({ desc; ty = term.ty; loc = term.loc } : term), bindings in
  match term.desc with
  | Atom a ->
    let a, binds = remove_atom a in
    pure (Atom a) binds
  | Bop (op, a, a') ->
    let a, binds = remove_atom a in
    let a', binds' = remove_atom a' in
    pure (Bop (op, a, a')) (binds @ binds')
  | Vec (i, atoms) ->
    let atoms, binds = remove_atoms atoms in
    pure (Vec (i, atoms)) binds
  | Index (a, i) ->
    let a, binds = remove_atom a in
    pure (Index (a, i)) binds
  | Builtin (op, atoms) ->
    let atoms, binds = remove_atoms atoms in
    pure (Builtin (op, atoms)) binds
  | App (f, atoms) ->
    let atoms, binds = remove_atoms atoms in
    pure (App (f, atoms)) binds
  | If (c, t, f) ->
    let c, binds = remove_atom c in
    let t = remove_anf t in
    let f = remove_anf f in
    pure (If (c, t, f)) binds
  | Record atoms ->
    let atoms, binds = remove_atoms atoms in
    pure (Record atoms) binds
  | Field (a, s) ->
    let a, binds = remove_atom a in
    pure (Field (a, s)) binds
  | Switch (scrutinee, cases) ->
    let scrutinee, binds = remove_atom scrutinee in
    let cases = List.map ~f:(Tuple2.map_snd ~f:remove_anf) cases in
    pure (Switch (scrutinee, cases)) binds
;;

let remove_top (top : Lower_variants.top) : top =
  let pure desc = { desc; ty = top.ty; loc = top.loc } in
  match top.desc with
  | Define { name; args; body; ret_ty } ->
    let body = remove_anf body in
    pure (Define { name; args; body; ret_ty })
  | Const (name, body) -> pure (Const (name, remove_anf body))
  | Extern v -> pure (Extern v)
  | TypeDef (s, ty) -> pure (TypeDef (s, ty))
;;

let remove (Program tops : Lower_variants.t) : t = Program (List.map tops ~f:remove_top)
