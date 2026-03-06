open Core
open Anf
open Sexplib.Sexp

type term_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Mat of int * int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | If of atom * anf * anf

and term =
  { desc : term_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }

and anf_desc =
  | Let of string * term * anf
  | Return of term
  | While of term * anf * term
  | Set of string * atom * anf
  | Continue

and anf =
  { desc : anf_desc
  ; ty : Stlc.ty
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

and sexp_of_term t = sexp_of_term_desc t.desc

and sexp_of_anf_desc = function
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_anf body ]
  | Return t -> List [ Atom "return"; sexp_of_term t ]
  | While (cond, body, after) ->
    List [ Atom "while"; sexp_of_term cond; sexp_of_anf body; sexp_of_term after ]
  | Set (v, bind, body) ->
    List [ Atom "set"; Atom v; sexp_of_atom bind; sexp_of_anf body ]
  | Continue -> Atom "continue"

and sexp_of_anf t = sexp_of_anf_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; args : (string * Stlc.ty) list
      ; body : anf
      ; ret_ty : Stlc.ty
      }
  | Const of string * anf
  | Extern of string

let sexp_of_top_desc = function
  | Define { name; args; body; ret_ty = _ } ->
    let args_sexp =
      List.map args ~f:(fun (v, ty) -> List [ Atom v; Stlc.sexp_of_ty ty ])
    in
    List
      [ Atom "Define"
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "args"; List args_sexp ]
      ; List [ Atom "body"; sexp_of_anf body ]
      ]
  | Const (name, term) -> List [ Atom "Const"; Atom name; sexp_of_anf term ]
  | Extern name -> List [ Atom "Extern"; Atom name ]
;;

type top =
  { desc : top_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }

let sexp_of_top t = List [ sexp_of_top_desc t.desc; Atom ":"; Stlc.sexp_of_ty t.ty ]

type t = Program of top list

let sexp_of_t (Program tops) = List (Atom "Program" :: List.map tops ~f:sexp_of_top)

let rec of_term (t : Anf.term) : term =
  let pure desc : term = { desc; ty = t.ty; loc = t.loc } in
  match t.desc with
  | Atom a -> pure (Atom a)
  | Bop (bop, a, a') -> pure (Bop (bop, a, a'))
  | Vec (n, ts) -> pure (Vec (n, ts))
  | Mat (n, m, ts) -> pure (Mat (n, m, ts))
  | Index (a, n) -> pure (Index (a, n))
  | Builtin (b, ts) -> pure (Builtin (b, ts))
  | App (f, xs) -> pure (App (f, xs))
  | If (c, t, f) -> pure (If (c, of_anf t, of_anf f))

and of_anf (anf : Anf.anf) : anf =
  let pure desc : anf = { desc; ty = anf.ty; loc = anf.loc } in
  match anf.desc with
  | Let (v, bind, tail) -> pure (Let (v, of_term bind, of_anf tail))
  | Return tail -> pure (Return (of_term tail))
;;

let placeholder_value_for_ty (ty : Stlc.ty) (loc : Lexer.loc) : term =
  let pure desc : term = { desc; ty; loc } in
  match ty with
  | TyInt -> pure (Atom (Int 0))
  | TyFloat -> pure (Atom (Float 0.0))
  | TyBool -> pure (Atom (Bool false))
  | TyVec n -> pure (Vec (n, List.init n ~f:(Fn.const (Float 0.0))))
  | TyMat (n, m) -> pure (Mat (n, m, List.init (n * m) ~f:(Fn.const (Float 0.0))))
  | TyArrow _ -> failwith "tail_call: arrow can't exist in tail pos"
;;

let patch_tail_anf (anf : Anf.anf) (name : string) (iter : string) (args : string list)
  : anf
  =
  let rec patch (anf : Anf.anf) : anf =
    let pure desc : anf = { desc; ty = anf.ty; loc = anf.loc } in
    match anf.desc with
    | Let (v, bind, tail) ->
      (* TODO: Maybe check that [bind] doesn't recursively call *)
      pure (Let (v, of_term bind, patch tail))
    | Return { desc = App (f, xs); ty = _; loc } when String.equal f name ->
      let tmp = Utils.fresh "_iter_inc" in
      let inc_iter_continue =
        let iter_inc : term = { desc = Bop (Add, Var iter, Int 1); ty = TyInt; loc } in
        pure (Let (tmp, iter_inc, pure (Set (iter, Var tmp, pure Continue))))
      in
      (* TODO: No exns, use [Or_error.t] *)
      List.fold_right2_exn args xs ~init:inc_iter_continue ~f:(fun name arg tail ->
        pure (Set (name, arg, tail)))
    | Return tail -> pure (Return (of_term tail))
  in
  patch anf
;;

let remove_rec_top (top : Anf.top) : top =
  let pure desc = { desc; ty = top.ty; loc = top.loc } in
  match top.desc with
  | Const (v, anf) -> pure (Const (v, of_anf anf))
  | Extern v -> pure (Extern v)
  | Define { name; recur = Nonrec; args; body; ret_ty } ->
    pure (Define { name; args; body = of_anf body; ret_ty })
  | Define { name; recur = Rec (limit, _); args; body; ret_ty } ->
    let loc = body.loc in
    let iter = Utils.fresh "_iter" in
    let while_cond : term = { desc = Bop (Lt, Var iter, Int limit); ty = top.ty; loc } in
    let while_body : anf = patch_tail_anf body name iter (List.map ~f:fst args) in
    let while_after = placeholder_value_for_ty ret_ty body.loc in
    let while_anf : anf =
      { desc = While (while_cond, while_body, while_after); ty = top.ty; loc }
    in
    let body : anf =
      { desc = Let (iter, { desc = Atom (Int 0); ty = TyInt; loc }, while_anf)
      ; ty = top.ty
      ; loc
      }
    in
    pure (Define { name; args; body; ret_ty })
;;

let remove_rec (Program t : Anf.t) : t = Program (List.map ~f:remove_rec_top t)
