open Core
open Anf
open Sexplib.Sexp

include Compiler_error.Pass (struct
    let name = "tail_call"
  end)

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
  | While of term * anf * anf
  | Set of string * atom * anf
  | Continue

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
  | While (cond, body, after) ->
    List [ Atom "while"; sexp_of_term cond; sexp_of_anf body; sexp_of_anf after ]
  | Set (v, bind, body) ->
    List [ Atom "set"; Atom v; sexp_of_atom bind; sexp_of_anf body ]
  | Continue -> Atom "continue"

and sexp_of_anf t = sexp_of_anf_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; args : (string * Lower_tuples.ty) list
      ; body : anf
      ; ret_ty : Lower_tuples.ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * Lower_tuples.type_decl

let sexp_of_top_desc = function
  | Define { name; args; body; ret_ty = _ } ->
    let args_sexp =
      List.map args ~f:(fun (v, ty) -> List [ Atom v; Lower_tuples.sexp_of_ty ty ])
    in
    List
      [ Atom "Define"
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

let rec of_term (t : Anf.term) : term =
  let pure desc : term = { desc; ty = t.ty; loc = t.loc } in
  match t.desc with
  | Atom a -> pure (Atom a)
  | Bop (bop, a, a') -> pure (Bop (bop, a, a'))
  | Vec (n, ts) -> pure (Vec (n, ts))
  | Index (a, n) -> pure (Index (a, n))
  | Builtin (b, ts) -> pure (Builtin (b, ts))
  | Record ts -> pure (Record ts)
  | Field (a, f) -> pure (Field (a, f))
  | App (f, xs) -> pure (App (f, xs))
  | If (c, t, f) -> pure (If (c, of_anf t, of_anf f))
  | Variant (ctor, args) -> pure (Variant (ctor, args))
  | Match (scrutinee, cases) ->
    pure (Match (scrutinee, List.map cases ~f:(Tuple2.map_snd ~f:of_anf)))

and of_anf (anf : Anf.anf) : anf =
  let pure desc : anf = { desc; ty = anf.ty; loc = anf.loc } in
  match anf.desc with
  | Let (v, bind, tail) -> pure (Let (v, of_term bind, of_anf tail))
  | Return tail -> pure (Return (of_term tail))
;;

let contains_call (t : Anf.term) (v : string) : bool =
  let rec contains_call_term (t : Anf.term) : bool =
    match t.desc with
    | App (f, _) -> String.equal f v
    | If (_, t, f) -> contains_call_anf t || contains_call_anf f
    | Match (_, cases) -> List.exists cases ~f:(fun (_, body) -> contains_call_anf body)
    | _ -> false
  and contains_call_anf (a : Anf.anf) : bool =
    match a.desc with
    | Let (_, b, t) -> contains_call_term b || contains_call_anf t
    | Return t -> contains_call_term t
  in
  contains_call_term t
;;

let patch_tail_anf (anf : Anf.anf) (name : string) (iter : string) (args : string list)
  : anf
  =
  let rec patch (anf : Anf.anf) : anf =
    let atom (desc : atom_desc) = ({ desc; ty = anf.ty; loc = anf.loc } : atom) in
    let pure desc : anf = { desc; ty = anf.ty; loc = anf.loc } in
    match anf.desc with
    | Let (v, bind, tail) ->
      if contains_call bind name
      then raise "non-tail rec call detected" ~loc:anf.loc ~d:[%message (name : string)]
      else pure (Let (v, of_term bind, patch tail))
    | Return { desc = If (c, t, f); ty; loc } ->
      pure (Return { desc = If (c, patch t, patch f); ty; loc })
    | Return { desc = Match (scrutinee, cases); ty; loc } ->
      let cases = List.map cases ~f:(fun (pat, body) -> pat, patch body) in
      pure (Return { desc = Match (scrutinee, cases); ty; loc })
    | Return { desc = App (f, xs); ty = _; loc } when String.equal f name ->
      let tmp = Utils.fresh "_iter_inc" in
      let inc_iter_continue =
        let iter_inc : term =
          let int_atom desc : atom = { desc; ty = TyInt; loc } in
          { desc = Bop (Add, int_atom (Var iter), int_atom (Int 1)); ty = TyInt; loc }
        in
        let continue = pure Continue in
        let set_iter_to_tmp = pure (Set (iter, atom (Var tmp), continue)) in
        pure (Let (tmp, iter_inc, set_iter_to_tmp))
      in
      let tail =
        List.fold_right2 args xs ~init:inc_iter_continue ~f:(fun name arg tail ->
          pure (Set (name, arg, tail)))
      in
      (match tail with
       | Ok res -> res
       | Unequal_lengths ->
         raise "args and xs don't match" ~loc ~d:[%message (args : string list)])
    | Return tail -> pure (Return (of_term tail))
  in
  patch anf
;;

let remove_rec_top (top : Anf.top) : top =
  let atom desc : atom = { desc; ty = top.ty; loc = top.loc } in
  let pure desc = { desc; ty = top.ty; loc = top.loc } in
  match top.desc with
  | Const (v, anf) -> pure (Const (v, of_anf anf))
  | Extern v -> pure (Extern v)
  | TypeDef (name, decl) -> pure (TypeDef (name, decl))
  | Define { name; recur = Nonrec; args; body; ret_ty } ->
    pure (Define { name; args; body = of_anf body; ret_ty })
  | Define { name; recur = Rec limit; args; body; ret_ty } ->
    let loc = body.loc in
    let iter = Utils.fresh "_iter" in
    let while_cond : term =
      let int_atom desc : atom = { desc; ty = TyInt; loc } in
      { desc = Bop (Lt, int_atom (Var iter), int_atom (Int limit)); ty = TyBool; loc }
    in
    let while_body = patch_tail_anf body name iter (List.map ~f:fst args) in
    let while_after =
      let ty = ret_ty in
      let loc = body.loc in
      let atom desc = ({ desc; ty; loc } : atom) in
      let make desc = ({ desc; ty; loc } : term) in
      let term = make (Atom (atom Temp)) in
      ({ desc = Return term; ty; loc } : anf)
    in
    let while_anf : anf =
      { desc = While (while_cond, while_body, while_after); ty = top.ty; loc }
    in
    let body : anf =
      let zero : term = { desc = Atom (atom (Int 0)); ty = TyInt; loc } in
      { desc = Let (iter, zero, while_anf); ty = top.ty; loc }
    in
    pure (Define { name; args; body; ret_ty })
;;

let remove_rec (Program tops : Anf.t) : t Compiler_error.t =
  try_with (fun () -> Program (List.map ~f:remove_rec_top tops))
;;
