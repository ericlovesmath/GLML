open Core
open Lower_variants
open Sexplib.Sexp

include Compiler_error.Pass (struct
    let name = "anf"
  end)

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
  | Init_struct of (string * atom) list
  | Field of atom * string
  | Switch of atom * (Glsl.switch_case * anf) list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

and anf_desc =
  | Let of string * term * anf
  | Return of term

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
  | Init_struct fields ->
    let sexp_of_field (f, a) = List [ Atom f; sexp_of_atom a ] in
    List (Atom "init_struct" :: List.map fields ~f:sexp_of_field)
  | Field (t, f) -> List [ Atom "."; sexp_of_atom t; Atom f ]
  | Switch (tag, cases) ->
    let sexp_of_case (label, body) =
      let label =
        match label with
        | Glsl.Case i -> Int.to_string i
        | Glsl.Default -> "default"
      in
      List [ Atom label; sexp_of_anf body ]
    in
    List (Atom "switch" :: sexp_of_atom tag :: List.map cases ~f:sexp_of_case)

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
      ; args : (string * ty) list
      ; body : anf
      ; ret_ty : ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * type_decl

let sexp_of_top_desc = function
  | Define { name; recur; args; body; ret_ty = _ } ->
    let args = List.map args ~f:(fun (v, ty) -> List [ Atom v; sexp_of_ty ty ]) in
    List
      [ Atom "Define"
      ; Frontend.sexp_of_recur recur
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "args"; List args ]
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

(* =============== Normalization ================ *)

let rec normalize (expr : Lower_variants.term) : anf =
  let atom (desc : atom_desc) : term_desc = Atom { desc; ty = expr.ty; loc = expr.loc } in
  let pure desc : anf =
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
    let mk_anf desc : anf = { desc; ty = body.ty; loc = expr.loc } in
    let rec splice (a : anf) : anf =
      match a.desc with
      | Let (v, b, body) -> mk_anf (Let (v, b, splice body))
      | Return t -> mk_anf (Let (v, t, body))
    in
    splice bind
  | App (f, args) -> atomize_list args (fun atoms -> pure (App (f, atoms)))
  | Bop (op, l, r) -> atomize l (fun l -> atomize r (fun r -> pure (Bop (op, l, r))))
  | Vec (n, ts) -> atomize_list ts (fun atoms -> pure (Vec (n, atoms)))
  | Index (t, i) -> atomize t (fun a -> pure (Index (a, i)))
  | Builtin (b, args) -> atomize_list args (fun atoms -> pure (Builtin (b, atoms)))
  | If (c, t, e) ->
    atomize c (fun c ->
      let t = normalize t in
      let e = normalize e in
      pure (If (c, t, e)))
  | Record args -> atomize_list args (fun atoms -> pure (Record atoms))
  | Init_struct fields ->
    let names, vals = List.unzip fields in
    atomize_list vals (fun atoms -> pure (Init_struct (List.zip_exn names atoms)))
  | Field (t, f) -> atomize t (fun a -> pure (Field (a, f)))
  | Switch (s, cases) ->
    atomize s (fun s ->
      let cases = List.map cases ~f:(fun (l, b) -> l, normalize b) in
      pure (Switch (s, cases)))

and atomize (expr : Lower_variants.term) (k : atom -> anf) : anf =
  let pure (desc : atom_desc) : atom = { desc; ty = expr.ty; loc = expr.loc } in
  match expr.desc with
  | Var v -> k (pure (Var v))
  | Float f -> k (pure (Float f))
  | Int i -> k (pure (Int i))
  | Bool b -> k (pure (Bool b))
  | _ ->
    let block = normalize expr in
    let rec splice (a : anf) : anf =
      match a.desc with
      | Let (v, b, t) ->
        let t = splice t in
        ({ desc = Let (v, b, t); ty = t.ty; loc = a.loc } : anf)
      | Return { desc = Atom existing; _ } -> k existing
      | Return t ->
        let v = Utils.fresh "anf" in
        let tl = k (pure (Var v)) in
        ({ desc = Let (v, t, tl); ty = tl.ty; loc = a.loc } : anf)
    in
    splice block

and atomize_list ts (k : atom list -> anf) =
  match ts with
  | [] -> k []
  | t :: ts -> atomize t (fun t -> atomize_list ts (fun ts -> k (t :: ts)))
;;

let normalize_top (t : Lower_variants.top) : top =
  let pure desc = { desc; ty = t.ty; loc = t.loc } in
  match t.desc with
  | Define { name; recur; args; body; ret_ty } ->
    pure (Define { name; recur; args; body = normalize body; ret_ty })
  | Const (name, body) -> pure (Const (name, normalize body))
  | Extern v -> pure (Extern v)
  | TypeDef (name, decl) -> pure (TypeDef (name, decl))
;;

let to_anf (Program terms : Lower_variants.t) : t Compiler_error.t =
  try_with (fun () -> Program (List.map terms ~f:normalize_top))
;;
