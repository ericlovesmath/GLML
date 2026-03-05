open Core
open Sexplib.Sexp

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | App of term * term list
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list

and term =
  { desc : term_desc
  ; ty : Stlc.ty
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_term)
  | Mat (x, y, ts) ->
    List
      (Atom ("mat" ^ Int.to_string x ^ "x" ^ Int.to_string y)
       :: List.map ts ~f:sexp_of_term)
  | App (f, args) -> List (Atom "app" :: sexp_of_term f :: List.map args ~f:sexp_of_term)
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_term body ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_term)

and sexp_of_term t = sexp_of_term_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; args : (string * Stlc.ty) list
      ; body : term
      ; ret_ty : Stlc.ty
      }
  | Const of string * term
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
      ; List [ Atom "body"; sexp_of_term body ]
      ]
  | Const (name, term) -> List [ Atom "Const"; Atom name; sexp_of_term term ]
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

let free_vars (lift_env : (string * string list) String.Map.t) (t : Uncurry.term)
  : String.Set.t
  =
  let rec fv (t : Uncurry.term) =
    match t.desc with
    | Var v ->
      (match Map.find lift_env v with
       | Some (_, captured) -> String.Set.of_list captured
       | None -> String.Set.singleton v)
    | Float _ | Int _ | Bool _ -> String.Set.empty
    | Vec (_, ts) | Builtin (_, ts) -> String.Set.union_list (List.map ts ~f:fv)
    | Mat (_, _, ts) -> String.Set.union_list (List.map ts ~f:fv)
    | Lam (args, body) ->
      let args_set = String.Set.of_list (List.map args ~f:fst) in
      Set.diff (fv body) args_set
    | App (fn, args) -> String.Set.union_list (fv fn :: List.map args ~f:fv)
    | Let (v, bind, body) -> Set.union (fv bind) (Set.remove (fv body) v)
    | If (c, t_true, e) -> String.Set.union_list [ fv c; fv t_true; fv e ]
    | Bop (_, l, r) -> Set.union (fv l) (fv r)
    | Index (t_sub, _) -> fv t_sub
  in
  fv t
;;

let lift (Uncurry.Program tops) =
  let open Or_error.Let_syntax in
  let globals =
    List.fold tops ~init:String.Set.empty ~f:(fun acc (top : Uncurry.top) ->
      match top.desc with
      | Define (n, _) -> Set.add acc n
      | Extern n -> Set.add acc n)
  in
  let lifted_tops = ref [] in
  let type_env = ref String.Map.empty in
  let lift_env = ref String.Map.empty in
  let rec lift_term (t : Uncurry.term) : term Or_error.t =
    let loc = t.loc in
    let ty = t.ty in
    match t.desc with
    | Var v ->
      (match Map.find !lift_env v with
       | Some _ ->
         Or_error.error_s
           [%message
             "First-class functions are not supported by the GLSL backend"
               (v : string)
               (loc : Lexer.loc)]
       | None -> return ({ desc = Var v; ty; loc } : term))
    | Float f -> return ({ desc = Float f; ty; loc } : term)
    | Int i -> return ({ desc = Int i; ty; loc } : term)
    | Bool b -> return ({ desc = Bool b; ty; loc } : term)
    | Vec (n, ts) ->
      let%map ts = ts |> List.map ~f:lift_term |> Or_error.all in
      ({ desc = Vec (n, ts); ty; loc } : term)
    | Mat (x, y, ts) ->
      let%map ts = ts |> List.map ~f:lift_term |> Or_error.all in
      ({ desc = Mat (x, y, ts); ty; loc } : term)
    | App ({ desc = Var v; _ }, args) ->
      let%bind args = args |> List.map ~f:lift_term |> Or_error.all in
      (match Map.find !lift_env v with
       | Some (lifted_name, free_vars) ->
         let fv_args =
           List.map free_vars ~f:(fun fv ->
             let fv_ty = Map.find_exn !type_env fv in
             ({ desc = Var fv; ty = fv_ty; loc } : term))
         in
         let fn_ty = Stlc.TyArrow (Stlc.TyInt, ty) in
         return
           ({ desc = App ({ desc = Var lifted_name; ty = fn_ty; loc }, fv_args @ args)
            ; ty
            ; loc
            }
            : term)
       | None ->
         let%map fn =
           lift_term
             ({ desc = Var v; ty = Stlc.TyArrow (Stlc.TyInt, ty); loc } : Uncurry.term)
         in
         ({ desc = App (fn, args); ty; loc } : term))
    | App (fn, args) ->
      let%bind fn = lift_term fn in
      let%map args = args |> List.map ~f:lift_term |> Or_error.all in
      ({ desc = App (fn, args); ty; loc } : term)
    | Let (v, { desc = Lam (args, body); ty = lam_ty; loc = lam_loc }, in_term) ->
      let fvs = free_vars !lift_env body in
      let fvs = Set.diff fvs (String.Set.of_list (List.map args ~f:fst)) in
      let fvs = Set.diff fvs globals in
      let fvs_list = Set.to_list fvs in
      let fvs_args = List.map fvs_list ~f:(fun fv -> fv, Map.find_exn !type_env fv) in
      let new_args = fvs_args @ args in
      let lifted_name = Utils.fresh v in
      lift_env := Map.set !lift_env ~key:v ~data:(lifted_name, fvs_list);
      let%bind lifted_body =
        let old_type_env = !type_env in
        type_env
        := List.fold args ~init:!type_env ~f:(fun acc (arg_name, arg_ty) ->
             Map.set acc ~key:arg_name ~data:arg_ty);
        let%bind res = lift_term body in
        type_env := old_type_env;
        return res
      in
      let ret_ty =
        let rec unroll = function
          | Stlc.TyArrow (_, r) -> unroll r
          | ty -> ty
        in
        unroll lam_ty
      in
      let new_top : top =
        { desc =
            Define { name = lifted_name; args = new_args; body = lifted_body; ret_ty }
        ; ty = lam_ty
        ; loc = lam_loc
        }
      in
      lifted_tops := new_top :: !lifted_tops;
      lift_term in_term
    | Let (v, bind, body) ->
      let%bind bind = lift_term bind in
      type_env := Map.set !type_env ~key:v ~data:bind.ty;
      let%map body = lift_term body in
      ({ desc = Let (v, bind, body); ty; loc } : term)
    | If (c, t_true, e) ->
      let%bind c = lift_term c in
      let%bind t_true = lift_term t_true in
      let%map e = lift_term e in
      ({ desc = If (c, t_true, e); ty; loc } : term)
    | Bop (op, l, r) ->
      let%bind l = lift_term l in
      let%map r = lift_term r in
      ({ desc = Bop (op, l, r); ty; loc } : term)
    | Index (t_sub, i) ->
      let%map t_sub = lift_term t_sub in
      ({ desc = Index (t_sub, i); ty; loc } : term)
    | Builtin (b, ts) ->
      let%map ts = ts |> List.map ~f:lift_term |> Or_error.all in
      ({ desc = Builtin (b, ts); ty; loc } : term)
    | Lam (args, body) ->
      let fvs = free_vars !lift_env body in
      let fvs = Set.diff fvs (String.Set.of_list (List.map args ~f:fst)) in
      let fvs = Set.diff fvs globals in
      let fvs_list = Set.to_list fvs in
      let fvs_args = List.map fvs_list ~f:(fun fv -> fv, Map.find_exn !type_env fv) in
      let new_args = fvs_args @ args in
      let lifted_name = Utils.fresh "anon_lam" in
      let%bind lifted_body =
        let old_type_env = !type_env in
        type_env
        := List.fold args ~init:!type_env ~f:(fun acc (arg_name, arg_ty) ->
             Map.set acc ~key:arg_name ~data:arg_ty);
        let%bind res = lift_term body in
        type_env := old_type_env;
        return res
      in
      let ret_ty =
        let rec unroll = function
          | Stlc.TyArrow (_, r) -> unroll r
          | ty -> ty
        in
        unroll ty
      in
      let new_top : top =
        { desc =
            Define { name = lifted_name; args = new_args; body = lifted_body; ret_ty }
        ; ty
        ; loc
        }
      in
      lifted_tops := new_top :: !lifted_tops;
      Or_error.error_s
        [%message
          "First-class anonymous functions are not supported by the GLSL backend"
            (loc : Lexer.loc)]
  in
  let%bind processed_tops =
    tops
    |> List.map ~f:(fun (top : Uncurry.top) ->
      match top.desc with
      | Define (name, { desc = Lam (args, body); ty; loc = _ }) ->
        type_env := String.Map.of_alist_exn args;
        lift_env := String.Map.empty;
        let%map body = lift_term body in
        let ret_ty =
          let rec unroll = function
            | Stlc.TyArrow (_, r) -> unroll r
            | ty -> ty
          in
          unroll ty
        in
        ({ desc = Define { name; args; body; ret_ty }; ty = top.ty; loc = top.loc } : top)
      | Define (name, term) ->
        type_env := String.Map.empty;
        lift_env := String.Map.empty;
        let%map term = lift_term term in
        ({ desc = Const (name, term); ty = top.ty; loc = top.loc } : top)
      | Extern v -> return ({ desc = Extern v; ty = top.ty; loc = top.loc } : top))
    |> Or_error.all
  in
  return (Program (!lifted_tops @ processed_tops))
;;
