open Core
open Anf
open Sexplib.Sexp
open Or_error.Let_syntax

type term_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Mat of int * int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | If of atom * anf * anf
  | Record of string * atom list
  | Field of atom * string
  | Variant of string * string * atom list
  | Match of atom * (string * string list * anf) list
  | Switch of atom * (int * anf) list

and term =
  { desc : term_desc
  ; ty : Monomorphize.ty
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
  ; ty : Monomorphize.ty
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
  | Record (s, ts) -> List (Atom s :: List.map ts ~f:sexp_of_atom)
  | Field (t, f) -> List [ Atom "."; sexp_of_atom t; Atom f ]
  | Variant (ty_name, ctor, args) ->
    List (Atom "Variant" :: Atom ty_name :: Atom ctor :: List.map args ~f:sexp_of_atom)
  | Match (scrutinee, cases) ->
    let sexp_of_case (ctor, vars, body) =
      List
        [ Atom ctor
        ; List (List.map vars ~f:(fun v -> Sexplib.Sexp.Atom v))
        ; sexp_of_anf body
        ]
    in
    List (Atom "match" :: sexp_of_atom scrutinee :: List.map cases ~f:sexp_of_case)
  | Switch (tag, cases) ->
    let sexp_of_case (i, body) = List [ Atom (Int.to_string i); sexp_of_anf body ] in
    List (Atom "switch" :: sexp_of_atom tag :: List.map cases ~f:sexp_of_case)

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
      ; args : (string * Monomorphize.ty) list
      ; body : anf
      ; ret_ty : Monomorphize.ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * Monomorphize.type_decl

let sexp_of_top_desc = function
  | Define { name; args; body; ret_ty = _ } ->
    let args_sexp =
      List.map args ~f:(fun (v, ty) -> List [ Atom v; Monomorphize.sexp_of_ty ty ])
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
    List [ Atom "TypeDef"; Atom name; Monomorphize.sexp_of_type_decl decl ]
;;

type top =
  { desc : top_desc
  ; ty : Monomorphize.ty
  ; loc : Lexer.loc
  }

let sexp_of_top t =
  List [ sexp_of_top_desc t.desc; Atom ":"; Monomorphize.sexp_of_ty t.ty ]
;;

type t = Program of top list

let sexp_of_t (Program tops) = List (Atom "Program" :: List.map tops ~f:sexp_of_top)

type type_env = Monomorphize.type_decl String.Map.t

let rec of_term (t : Anf.term) : term =
  let pure desc : term = { desc; ty = t.ty; loc = t.loc } in
  match t.desc with
  | Atom a -> pure (Atom a)
  | Bop (bop, a, a') -> pure (Bop (bop, a, a'))
  | Vec (n, ts) -> pure (Vec (n, ts))
  | Mat (n, m, ts) -> pure (Mat (n, m, ts))
  | Index (a, n) -> pure (Index (a, n))
  | Builtin (b, ts) -> pure (Builtin (b, ts))
  | Record (s, ts) -> pure (Record (s, ts))
  | Field (a, f) -> pure (Field (a, f))
  | App (f, xs) -> pure (App (f, xs))
  | If (c, t, f) -> pure (If (c, of_anf t, of_anf f))
  | Variant (ty_name, ctor, args) -> pure (Variant (ty_name, ctor, args))
  | Match (scrutinee, cases) ->
    pure
      (Match
         (scrutinee, List.map cases ~f:(fun (ctor, vars, body) -> ctor, vars, of_anf body)))

and of_anf (anf : Anf.anf) : anf =
  let pure desc : anf = { desc; ty = anf.ty; loc = anf.loc } in
  match anf.desc with
  | Let (v, bind, tail) -> pure (Let (v, of_term bind, of_anf tail))
  | Return tail -> pure (Return (of_term tail))
;;

let placeholder_anf_for_ty (tenv : type_env) (ty : Monomorphize.ty) (loc : Lexer.loc)
  : anf Or_error.t
  =
  let open Or_error.Let_syntax in
  let make ?(env = []) desc = Ok (({ desc; ty; loc } : term), env) in
  let rec build (ty : Monomorphize.ty) : (term * (string * term) list) Or_error.t =
    match ty with
    | TyInt -> make (Atom (Int 0))
    | TyFloat -> make (Atom (Float 0.0))
    | TyBool -> make (Atom (Bool false))
    | TyVec n -> make (Vec (n, List.init n ~f:(Fn.const (Float 0.0))))
    | TyMat (n, m) -> make (Mat (n, m, List.init (n * m) ~f:(Fn.const (Float 0.0))))
    | TyRecord s ->
      (match Map.find tenv s with
       | Some (RecordDecl fields) ->
         let%bind fields =
           Or_error.all (List.map fields ~f:(fun (_, f_ty) -> build f_ty))
         in
         let args, nested_bindings =
           List.fold_right
             fields
             ~init:([], [])
             ~f:(fun (t, bindings) (args, all_bindings) ->
               match t.desc with
               | Atom a -> a :: args, bindings @ all_bindings
               | _ ->
                 let name = Utils.fresh "_tco_struct" in
                 Var name :: args, bindings @ [ name, t ] @ all_bindings)
         in
         make ~env:nested_bindings (Record (s, args))
       | Some (VariantDecl _) | None ->
         error_s [%message "tail_call: unknown struct type" s])
    | TyVariant s ->
      (match Map.find tenv s with
       | Some (VariantDecl ((first_ctor, first_arg_tys) :: _)) ->
         let%bind arg_terms =
           Or_error.all (List.map first_arg_tys ~f:(fun arg_ty -> build arg_ty))
         in
         let args, nested_bindings =
           List.fold_right
             arg_terms
             ~init:([], [])
             ~f:(fun (t, bindings) (args, all_bindings) ->
               match t.desc with
               | Atom a -> a :: args, bindings @ all_bindings
               | _ ->
                 let name = Utils.fresh "_tco_variant" in
                 Var name :: args, bindings @ [ name, t ] @ all_bindings)
         in
         make ~env:nested_bindings (Variant (s, first_ctor, args))
       | Some (VariantDecl []) | Some (RecordDecl _) | None ->
         error_s [%message "tail_call: unknown variant type" s])
    | TyArrow _ ->
      error_s [%message "tail_call: unexpected arrow in tail" (ty : Monomorphize.ty)]
  in
  let%map term, bindings = build ty in
  List.fold_right
    bindings
    ~init:({ desc = Return term; ty; loc } : anf)
    ~f:(fun (v, t) acc -> ({ desc = Let (v, t, acc); ty; loc = acc.loc } : anf))
;;

let contains_call (t : Anf.term) (v : string) : bool =
  let rec contains_call_term (t : Anf.term) : bool =
    match t.desc with
    | App (f, _) -> String.equal f v
    | If (_, t, f) -> contains_call_anf t || contains_call_anf f
    | Match (_, cases) ->
      List.exists cases ~f:(fun (_, _, body) -> contains_call_anf body)
    | _ -> false
  and contains_call_anf (a : Anf.anf) : bool =
    match a.desc with
    | Let (_, b, t) -> contains_call_term b || contains_call_anf t
    | Return t -> contains_call_term t
  in
  contains_call_term t
;;

let patch_tail_anf (anf : Anf.anf) (name : string) (iter : string) (args : string list)
  : anf Or_error.t
  =
  let rec patch (anf : Anf.anf) : anf Or_error.t =
    let pure desc : anf Or_error.t = Ok { desc; ty = anf.ty; loc = anf.loc } in
    match anf.desc with
    | Let (v, bind, tail) ->
      if contains_call bind name
      then
        error_s
          [%message
            "tail_call: non-tail rec call detected" (name : string) (anf.loc : Lexer.loc)]
      else (
        let%bind tail = patch tail in
        pure (Let (v, of_term bind, tail)))
    | Return { desc = If (c, t, f); ty; loc } ->
      let%bind t = patch t in
      let%bind f = patch f in
      pure (Return { desc = If (c, t, f); ty; loc })
    | Return { desc = Match (scrutinee, cases); ty; loc } ->
      let%bind cases =
        cases
        |> List.map ~f:(fun (ctor, vars, body) ->
          let%map body = patch body in
          ctor, vars, body)
        |> Or_error.all
      in
      pure (Return { desc = Match (scrutinee, cases); ty; loc })
    | Return { desc = App (f, xs); ty = _; loc } when String.equal f name ->
      let tmp = Utils.fresh "_iter_inc" in
      let inc_iter_continue =
        let iter_inc : term = { desc = Bop (Add, Var iter, Int 1); ty = TyInt; loc } in
        let%bind continue = pure Continue in
        let%bind set_iter_to_tmp = pure (Set (iter, Var tmp, continue)) in
        pure (Let (tmp, iter_inc, set_iter_to_tmp))
      in
      let tail =
        List.fold_right2 args xs ~init:inc_iter_continue ~f:(fun name arg tail ->
          let%bind tail = tail in
          pure (Set (name, arg, tail)))
      in
      (match tail with
       | Ok res -> res
       | Unequal_lengths ->
         error_s [%message "tail_call: args and xs don't match" (loc : Lexer.loc)])
    | Return tail -> pure (Return (of_term tail))
  in
  patch anf
;;

let remove_rec_top (tenv : type_env) (top : Anf.top) : top Or_error.t =
  let pure desc = Ok { desc; ty = top.ty; loc = top.loc } in
  match top.desc with
  | Const (v, anf) -> pure (Const (v, of_anf anf))
  | Extern v -> pure (Extern v)
  | TypeDef (name, decl) -> pure (TypeDef (name, decl))
  | Define { name; recur = Nonrec; args; body; ret_ty } ->
    pure (Define { name; args; body = of_anf body; ret_ty })
  | Define { name; recur = Rec limit; args; body; ret_ty } ->
    let loc = body.loc in
    let iter = Utils.fresh "_iter" in
    let while_cond : term = { desc = Bop (Lt, Var iter, Int limit); ty = top.ty; loc } in
    let%bind while_body = patch_tail_anf body name iter (List.map ~f:fst args) in
    let%bind while_after = placeholder_anf_for_ty tenv ret_ty body.loc in
    let while_anf : anf =
      { desc = While (while_cond, while_body, while_after); ty = top.ty; loc }
    in
    let body : anf =
      { desc = Let (iter, { desc = Atom (Int 0); ty = Monomorphize.TyInt; loc }, while_anf)
      ; ty = top.ty
      ; loc
      }
    in
    pure (Define { name; args; body; ret_ty })
;;

let remove_rec (Program t : Anf.t) : t Or_error.t =
  let%bind tenv =
    t
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | TypeDef (s, decl) -> Some (s, decl)
      | Define _ | Extern _ | Const _ -> None)
    |> String.Map.of_alist_or_error
  in
  let%map tops = Or_error.all (List.map ~f:(remove_rec_top tenv) t) in
  Program tops
;;
