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
    let open String.Set in
    match t.desc with
    | Var v ->
      (match Map.find lift_env v with
       | Some (_, captured) -> of_list captured
       | None -> singleton v)
    | Float _ | Int _ | Bool _ -> empty
    | Vec (_, ts) | Builtin (_, ts) -> union_list (List.map ts ~f:fv)
    | Mat (_, _, ts) -> union_list (List.map ts ~f:fv)
    | Lam (args, body) ->
      let args_set = of_list (List.map args ~f:fst) in
      Set.diff (fv body) args_set
    | App (fn, args) -> union_list (fv fn :: List.map args ~f:fv)
    | Let (v, bind, body) -> Set.union (fv bind) (Set.remove (fv body) v)
    | If (c, t_true, e) -> union_list [ fv c; fv t_true; fv e ]
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
  let rec lift_term type_env lift_env (t : Uncurry.term) : (term * top list) Or_error.t =
    let make term fvs = return (({ desc = term; ty = t.ty; loc = t.loc } : term), fvs) in
    match t.desc with
    | Var v ->
      (match Map.find lift_env v with
       | None -> make (Var v) []
       | Some _ ->
         Or_error.error_s
           [%message
             "First-class functions are not supported by the GLSL backend"
               (v : string)
               (t.loc : Lexer.loc)])
    | Float f -> make (Float f) []
    | Int i -> make (Int i) []
    | Bool b -> make (Bool b) []
    | Vec (n, ts) ->
      let%bind ts_res = ts |> List.map ~f:(lift_term type_env lift_env) |> Or_error.all in
      let ts, tops_list = List.unzip ts_res in
      make (Vec (n, ts)) (List.concat tops_list)
    | Mat (x, y, ts) ->
      let%bind ts_res = ts |> List.map ~f:(lift_term type_env lift_env) |> Or_error.all in
      let ts, tops_list = List.unzip ts_res in
      make (Mat (x, y, ts)) (List.concat tops_list)
    | App ({ desc = Var v; _ }, args) ->
      let%bind args_res =
        args |> List.map ~f:(lift_term type_env lift_env) |> Or_error.all
      in
      let args, args_tops = List.unzip args_res in
      let args_tops = List.concat args_tops in
      (match Map.find lift_env v with
       | Some (lifted_name, free_vars) ->
         let fv_args =
           List.map free_vars ~f:(fun fv ->
             let fv_ty = Map.find_exn type_env fv in
             ({ desc = Var fv; ty = fv_ty; loc = t.loc } : term))
         in
         let fn_ty = Stlc.TyArrow (Stlc.TyInt, t.ty) in
         make
           (App ({ desc = Var lifted_name; ty = fn_ty; loc = t.loc }, fv_args @ args))
           args_tops
       | None ->
         let%bind fn, fn_tops =
           lift_term
             type_env
             lift_env
             { desc = Var v; ty = Stlc.TyArrow (Stlc.TyInt, t.ty); loc = t.loc }
         in
         make (App (fn, args)) (fn_tops @ args_tops))
    | App (fn, args) ->
      let%bind fn, fn_tops = lift_term type_env lift_env fn in
      let%bind args_res =
        args |> List.map ~f:(lift_term type_env lift_env) |> Or_error.all
      in
      let args, args_tops = List.unzip args_res in
      make (App (fn, args)) (fn_tops @ List.concat args_tops)
    | Let (v, { desc = Lam (args, body); ty = lam_ty; loc = lam_loc }, in_term) ->
      let fvs = free_vars lift_env body in
      let fvs = Set.diff fvs (String.Set.of_list (List.map args ~f:fst)) in
      let fvs = Set.diff fvs globals in
      let fvs_list = Set.to_list fvs in
      let fvs_args = List.map fvs_list ~f:(fun fv -> fv, Map.find_exn type_env fv) in
      let new_args = fvs_args @ args in
      let lifted_name = Utils.fresh v in
      let lift_env_body = Map.set lift_env ~key:v ~data:(lifted_name, fvs_list) in
      let type_env_body =
        List.fold args ~init:type_env ~f:(fun acc (arg_name, arg_ty) ->
          Map.set acc ~key:arg_name ~data:arg_ty)
      in
      let%bind lifted_body, body_tops = lift_term type_env_body lift_env_body body in
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
      let%map in_term, in_tops = lift_term type_env lift_env_body in_term in
      in_term, body_tops @ [ new_top ] @ in_tops
    | Let (v, bind, body) ->
      let%bind bind, bind_tops = lift_term type_env lift_env bind in
      let type_env_body = Map.set type_env ~key:v ~data:bind.ty in
      let%bind body, body_tops = lift_term type_env_body lift_env body in
      make (Let (v, bind, body)) (bind_tops @ body_tops)
    | If (c, t_true, e) ->
      let%bind c, c_tops = lift_term type_env lift_env c in
      let%bind t_true, t_tops = lift_term type_env lift_env t_true in
      let%bind e, e_tops = lift_term type_env lift_env e in
      make (If (c, t_true, e)) (c_tops @ t_tops @ e_tops)
    | Bop (op, l, r) ->
      let%bind l, l_tops = lift_term type_env lift_env l in
      let%bind r, r_tops = lift_term type_env lift_env r in
      make (Bop (op, l, r)) (l_tops @ r_tops)
    | Index (t_sub, i) ->
      let%bind t_sub, t_tops = lift_term type_env lift_env t_sub in
      make (Index (t_sub, i)) t_tops
    | Builtin (b, ts) ->
      let%bind ts_res = ts |> List.map ~f:(lift_term type_env lift_env) |> Or_error.all in
      let ts, tops_list = List.unzip ts_res in
      make (Builtin (b, ts)) (List.concat tops_list)
    | Lam (_, _) ->
      Or_error.error_s
        [%message
          "First-class anonymous functions are not supported by the GLSL backend"
            (t.loc : Lexer.loc)]
  in
  let%bind processed_tops_lists =
    tops
    |> List.map ~f:(fun (top : Uncurry.top) ->
      let make desc = { desc; ty = top.ty; loc = top.loc } in
      match top.desc with
      | Define (name, { desc = Lam (args, body); ty; loc = _ }) ->
        let type_env = String.Map.of_alist_exn args in
        let lift_env = String.Map.empty in
        let%map body, body_tops = lift_term type_env lift_env body in
        let ret_ty =
          let rec unroll = function
            | Stlc.TyArrow (_, r) -> unroll r
            | ty -> ty
          in
          unroll ty
        in
        body_tops @ [ make (Define { name; args; body; ret_ty }) ]
      | Define (name, term) ->
        let type_env = String.Map.empty in
        let lift_env = String.Map.empty in
        let%map term, term_tops = lift_term type_env lift_env term in
        term_tops @ [ make (Const (name, term)) ]
      | Extern v -> return [ make (Extern v) ])
    |> Or_error.all
  in
  return (Program (List.concat processed_tops_lists))
;;
