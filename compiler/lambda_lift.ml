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

(** Map of lifted function names to function arguments *)
type env = (string * (string * Stlc.ty) list) String.Map.t

let free_vars (env : env) (t : Uncurry.term) : Stlc.ty String.Map.t =
  let rec fv (t : Uncurry.term) =
    let union m1 m2 =
      Map.merge m1 m2 ~f:(fun ~key:_ -> function
        | `Left v | `Right v | `Both (v, _) -> Some v)
    in
    let union_list ms = List.fold ms ~init:String.Map.empty ~f:union in
    match t.desc with
    | Var v ->
      (match Map.find env v with
       | Some (_, captured) -> String.Map.of_alist_exn captured
       | None -> String.Map.singleton v t.ty)
    | Float _ | Int _ | Bool _ -> String.Map.empty
    | Vec (_, ts) | Builtin (_, ts) -> union_list (List.map ts ~f:fv)
    | Mat (_, _, ts) -> union_list (List.map ts ~f:fv)
    | Lam (args, body) ->
      List.fold args ~init:(fv body) ~f:(fun acc (arg, _) -> Map.remove acc arg)
    | App (fn, args) -> union_list (fv fn :: List.map args ~f:fv)
    | Let (v, bind, body) -> union (fv bind) (Map.remove (fv body) v)
    | If (c, t_true, e) -> union_list [ fv c; fv t_true; fv e ]
    | Bop (_, l, r) -> union (fv l) (fv r)
    | Index (t_sub, _) -> fv t_sub
  in
  fv t
;;

let lift (Program tops : Uncurry.t) : t Or_error.t =
  let open Or_error.Let_syntax in
  let globals =
    tops
    |> List.map ~f:(fun top ->
      match top.desc with
      | Define (v, _) -> v
      | Extern v -> v)
    |> String.Set.of_list
  in
  let unroll_arrow ty =
    let rec go = function
      | Stlc.TyArrow (_, r) -> go r
      | ty -> ty
    in
    go ty
  in
  let rec lift_term (env : env) (t : Uncurry.term) : (term * top list) Or_error.t =
    let make term fvs = return (({ desc = term; ty = t.ty; loc = t.loc } : term), fvs) in
    match t.desc with
    | Var v ->
      (match Map.find env v with
       | None -> make (Var v) []
       | Some _ ->
         error_s [%message "first-class functions are not supported" (t.loc : Lexer.loc)])
    | Float f -> make (Float f) []
    | Int i -> make (Int i) []
    | Bool b -> make (Bool b) []
    | Vec (n, ts) ->
      let%bind ts_res = Or_error.all (List.map ~f:(lift_term env) ts) in
      let ts, tops_list = List.unzip ts_res in
      make (Vec (n, ts)) (List.concat tops_list)
    | Mat (n, m, ts) ->
      let%bind ts_res = Or_error.all (List.map ~f:(lift_term env) ts) in
      let ts, tops_list = List.unzip ts_res in
      make (Mat (n, m, ts)) (List.concat tops_list)
    | App (f, args) ->
      let%bind args_res = Or_error.all (List.map ~f:(lift_term env) args) in
      let args, args_tops = List.unzip args_res in
      let args_tops = List.concat args_tops in
      (match f.desc with
       | Var v ->
         (match Map.find env v with
          | Some (lifted_name, captured) ->
            let captured_args =
              List.map captured ~f:(fun (name, ty) ->
                ({ desc = Var name; ty; loc = t.loc } : term))
            in
            let lifted_fn : term = { desc = Var lifted_name; ty = f.ty; loc = f.loc } in
            make (App (lifted_fn, captured_args @ args)) args_tops
          | None ->
            let fn : term = { desc = Var v; ty = f.ty; loc = f.loc } in
            make (App (fn, args)) args_tops)
       | _ ->
         let%bind f, f_tops = lift_term env f in
         make (App (f, args)) (f_tops @ args_tops))
    | Let (v, { desc = Lam (args, body); ty = lam_ty; loc = lam_loc }, in_term) ->
      let captured =
        let fvs = free_vars env body in
        let fvs = List.fold args ~init:fvs ~f:(fun acc (arg, _) -> Map.remove acc arg) in
        let fvs = Map.remove fvs v in
        let fvs = Set.fold globals ~init:fvs ~f:Map.remove in
        Map.to_alist fvs
      in
      let lifted_name = Utils.fresh v in
      let env = Map.set env ~key:v ~data:(lifted_name, captured) in
      let%bind lifted_body, body_tops = lift_term env body in
      let lifted_fn : top =
        { desc =
            Define
              { name = lifted_name
              ; args = captured @ args
              ; body = lifted_body
              ; ret_ty = unroll_arrow lam_ty
              }
        ; ty = lam_ty
        ; loc = lam_loc
        }
      in
      let%bind in_term, in_tops = lift_term env in_term in
      return (in_term, body_tops @ [ lifted_fn ] @ in_tops)
    | Let (v, bind, body) ->
      let%bind bind, bind_tops = lift_term env bind in
      let%bind body, body_tops = lift_term env body in
      make (Let (v, bind, body)) (bind_tops @ body_tops)
    | If (c, t, e) ->
      let%bind c, c_tops = lift_term env c in
      let%bind t, t_tops = lift_term env t in
      let%bind e, e_tops = lift_term env e in
      make (If (c, t, e)) (c_tops @ t_tops @ e_tops)
    | Bop (op, l, r) ->
      let%bind l, l_tops = lift_term env l in
      let%bind r, r_tops = lift_term env r in
      make (Bop (op, l, r)) (l_tops @ r_tops)
    | Index (t, i) ->
      let%bind t, t_tops = lift_term env t in
      make (Index (t, i)) t_tops
    | Builtin (b, ts) ->
      let%bind ts_res = Or_error.all (List.map ~f:(lift_term env) ts) in
      let ts, top_blocks = List.unzip ts_res in
      make (Builtin (b, ts)) (List.concat top_blocks)
    | Lam _ ->
      error_s [%message "first-class anon functions are unsupported" (t.loc : Lexer.loc)]
  in
  let%bind top_blocks =
    tops
    |> List.map ~f:(fun (top : Uncurry.top) ->
      let make desc = { desc; ty = top.ty; loc = top.loc } in
      match top.desc with
      | Define (name, { desc = Lam (args, body); ty; loc = _ }) ->
        let%map body, body_tops = lift_term String.Map.empty body in
        let ret_ty = unroll_arrow ty in
        body_tops @ [ make (Define { name; args; body; ret_ty }) ]
      | Define (name, term) ->
        let%map term, term_tops = lift_term String.Map.empty term in
        term_tops @ [ make (Const (name, term)) ]
      | Extern v -> return [ make (Extern v) ])
    |> Or_error.all
  in
  return (Program (List.concat top_blocks))
;;
