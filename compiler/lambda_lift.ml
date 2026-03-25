open Core
open Sexplib.Sexp
open Compiler_error.Let_syntax

module Err = Compiler_error.Pass (struct
    let name = "lambda_lift"
  end)

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
  | Record of string * term list
  | Field of term * string
  | Variant of string * string * term list
  | Match of term * (string * string list * term) list

and term =
  { desc : term_desc
  ; ty : Monomorphize.ty
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
  | Record (s, ts) -> List (Atom s :: List.map ts ~f:sexp_of_term)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ty_name, ctor, args) ->
    List (Atom "Variant" :: Atom ty_name :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (ctor, vars, body) =
      List [ Atom ctor; List (List.map vars ~f:(fun v -> Atom v)); sexp_of_term body ]
    in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; recur : Stlc.recur
      ; args : (string * Monomorphize.ty) list
      ; body : term
      ; ret_ty : Monomorphize.ty
      }
  | Const of string * term
  | Extern of string
  | TypeDef of string * Monomorphize.type_decl

let sexp_of_top_desc = function
  | Define { name; recur; args; body; ret_ty = _ } ->
    let args_sexp =
      List.map args ~f:(fun (v, ty) -> List [ Atom v; Monomorphize.sexp_of_ty ty ])
    in
    List
      [ Atom "Define"
      ; Stlc.sexp_of_recur recur
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "args"; List args_sexp ]
      ; List [ Atom "body"; sexp_of_term body ]
      ]
  | Const (name, term) -> List [ Atom "Const"; Atom name; sexp_of_term term ]
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

(** Map of lifted function names to function arguments *)
type env = (string * (string * Monomorphize.ty) list) String.Map.t

let free_vars (env : env) (t : Uncurry.term) : Monomorphize.ty String.Map.t =
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
    | Let (_, v, bind, body) -> union (fv bind) (Map.remove (fv body) v)
    | If (c, t_true, e) -> union_list [ fv c; fv t_true; fv e ]
    | Bop (_, l, r) -> union (fv l) (fv r)
    | Index (t, _) -> fv t
    | Record (_, ts) -> union_list (List.map ts ~f:fv)
    | Field (t, _) -> fv t
    | Variant (_, _, args) -> union_list (List.map args ~f:fv)
    | Match (scrutinee, cases) ->
      let fv_cases =
        List.map cases ~f:(fun (_, vars, body) ->
          List.fold vars ~init:(fv body) ~f:Map.remove)
      in
      union_list (fv scrutinee :: fv_cases)
  in
  fv t
;;

(** Gets last type in arrow type (return type of function) *)
let unroll_arrow ty =
  let rec go = function
    | Monomorphize.TyArrow (_, r) -> go r
    | ty -> ty
  in
  go ty
;;

let rec lift_term (globals : String.Set.t) (env : env) (t : Uncurry.term)
  : (term * top list) Compiler_error.t
  =
  let lift = lift_term globals env in
  let lift_list ts =
    let%map pairs = Compiler_error.all (List.map ts ~f:lift) in
    let terms, tops = List.unzip pairs in
    terms, List.concat tops
  in
  let make term fvs = return (({ desc = term; ty = t.ty; loc = t.loc } : term), fvs) in
  match t.desc with
  | Var v ->
    (match Map.find env v with
     | None -> make (Var v) []
     | Some _ -> Err.fail "first-class functions are not supported" ~loc:t.loc)
  | Float f -> make (Float f) []
  | Int i -> make (Int i) []
  | Bool b -> make (Bool b) []
  | Vec (n, ts) ->
    let%bind ts, tops = lift_list ts in
    make (Vec (n, ts)) tops
  | Mat (n, m, ts) ->
    let%bind ts, tops = lift_list ts in
    make (Mat (n, m, ts)) tops
  | App (f, args) ->
    let%bind args, args_tops = lift_list args in
    (match f.desc with
     | Var v ->
       let var_name, extra =
         match Map.find env v with
         | Some (lifted_name, captured) ->
           ( lifted_name
           , List.map captured ~f:(fun (name, ty) ->
               ({ desc = Var name; ty; loc = f.loc } : term)) )
         | None -> v, []
       in
       let fn : term = { desc = Var var_name; ty = f.ty; loc = f.loc } in
       make (App (fn, extra @ args)) args_tops
     | _ ->
       let%bind f, f_tops = lift f in
       make (App (f, args)) (f_tops @ args_tops))
  | Let (recur, v, { desc = Lam (args, body); ty; loc }, bind) ->
    (* NOTE: This is where the lambda lifting happens *)
    let captured =
      let excluded = Set.union globals (String.Set.of_list (v :: List.map args ~f:fst)) in
      free_vars env body
      |> Map.filter_keys ~f:(fun k -> not (Set.mem excluded k))
      |> Map.to_alist
    in
    let name = Utils.fresh v in
    let env = Map.set env ~key:v ~data:(name, captured) in
    let%bind body, body_tops = lift_term globals env body in
    let desc =
      Define { name; recur; args = captured @ args; body; ret_ty = unroll_arrow ty }
    in
    let lifted_fn : top = { desc; ty; loc } in
    let%bind bind, bind_tops = lift_term globals env bind in
    return (bind, body_tops @ [ lifted_fn ] @ bind_tops)
  | Let (Rec _, _, _, _) -> Err.fail "rec tag on non-lambda" ~loc:t.loc
  | Let (Nonrec, v, bind, body) ->
    let%bind bind, bind_tops = lift bind in
    let%bind body, body_tops = lift body in
    make (Let (v, bind, body)) (bind_tops @ body_tops)
  | If (c, t, e) ->
    let%bind c, c_tops = lift c in
    let%bind t, t_tops = lift t in
    let%bind e, e_tops = lift e in
    make (If (c, t, e)) (c_tops @ t_tops @ e_tops)
  | Bop (op, l, r) ->
    let%bind l, l_tops = lift l in
    let%bind r, r_tops = lift r in
    make (Bop (op, l, r)) (l_tops @ r_tops)
  | Index (t, i) ->
    let%bind t, t_tops = lift t in
    make (Index (t, i)) t_tops
  | Builtin (b, ts) ->
    let%bind ts, tops = lift_list ts in
    make (Builtin (b, ts)) tops
  | Record (s, ts) ->
    let%bind ts, tops = lift_list ts in
    make (Record (s, ts)) tops
  | Field (t, f) ->
    let%bind t, tops = lift t in
    make (Field (t, f)) tops
  | Variant (ty_name, ctor, args) ->
    let%bind args, tops = lift_list args in
    make (Variant (ty_name, ctor, args)) tops
  | Match (scrutinee, cases) ->
    let%bind scrutinee, s_tops = lift scrutinee in
    let%bind cases, c_tops =
      List.fold_result
        cases
        ~init:([], [])
        ~f:(fun (acc_cases, acc_tops) (ctor, vars, body) ->
          let%map body, body_tops = lift body in
          (ctor, vars, body) :: acc_cases, acc_tops @ body_tops)
    in
    make (Match (scrutinee, List.rev cases)) (s_tops @ c_tops)
  | Lam _ -> Err.fail "first-class anon functions are unsupported" ~loc:t.loc
;;

let lift_top (globals : String.Set.t) (top : Uncurry.top) : top list Compiler_error.t =
  let make desc = { desc; ty = top.ty; loc = top.loc } in
  let lift = lift_term globals String.Map.empty in
  match top.desc with
  | Define (recur, name, { desc = Lam (args, body); ty; loc = _ }) ->
    let%map body, body_tops = lift body in
    body_tops @ [ make (Define { name; recur; args; body; ret_ty = unroll_arrow ty }) ]
  | Define (_, name, term) ->
    let%map term, term_tops = lift term in
    term_tops @ [ make (Const (name, term)) ]
  | Extern v -> return [ make (Extern v) ]
  | TypeDef (name, decl) -> return [ make (TypeDef (name, decl)) ]
;;

let lift (Program tops : Uncurry.t) : t Compiler_error.t =
  let globals =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | Define (_, v, _) -> Some v
      | Extern v -> Some v
      | TypeDef _ -> None)
    |> String.Set.of_list
  in
  let%bind top_blocks = Compiler_error.all (List.map tops ~f:(lift_top globals)) in
  return (Program (List.concat top_blocks))
;;
