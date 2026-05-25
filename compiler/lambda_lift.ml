open Core
open Sexplib.Sexp

include Compiler_error.Pass (struct
    let name = "lambda_lift"
  end)

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | App of term * term list
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of term list
  | Field of term * string
  | Variant of string * term list
  | Match of term * (Frontend.pat * term) list

and term =
  { desc : term_desc
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_term)
  | App (f, args) -> List (Atom "app" :: sexp_of_term f :: List.map args ~f:sexp_of_term)
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_term body ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_term)
  | Record ts -> List (Atom "record" :: List.map ts ~f:sexp_of_term)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Variant (ctor, args) ->
    List (Atom "Variant" :: Atom ctor :: List.map args ~f:sexp_of_term)
  | Match (scrutinee, cases) ->
    let sexp_of_case (pat, body) = List [ Frontend.sexp_of_pat pat; sexp_of_term body ] in
    List (Atom "match" :: sexp_of_term scrutinee :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; recur : Frontend.recur
      ; args : (string * Lower_tuples.ty) list
      ; body : term
      ; ret_ty : Lower_tuples.ty
      }
  | Const of string * term
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
      ; List [ Atom "body"; sexp_of_term body ]
      ]
  | Const (name, term) -> List [ Atom "Const"; Atom name; sexp_of_term term ]
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

(** Map of lifted function names to function arguments *)
type env = (string * (string * Lower_tuples.ty) list) String.Map.t

let free_vars (env : env) (t : Uncurry.term) : Lower_tuples.ty String.Map.t =
  let rec fv (t : Uncurry.term) =
    let union m1 m2 =
      Map.merge m1 m2 ~f:(fun ~key:_ -> function
        | `Left v | `Right v | `Both (v, _) -> Some v)
    in
    let union_list ms = List.fold ms ~init:String.Map.empty ~f:union in
    match t.desc with
    | Var v ->
      (match Map.find env v with
       | Some (_, captured) ->
         String.Map.of_alist_or_error captured |> of_or_error |> ok_exn
       | None -> String.Map.singleton v t.ty)
    | Float _ | Int _ | Bool _ -> String.Map.empty
    | Vec (_, ts) | Builtin (_, ts) -> union_list (List.map ts ~f:fv)
    | Lam (args, body) ->
      List.fold args ~init:(fv body) ~f:(fun acc (arg, _) -> Map.remove acc arg)
    | App (fn, args) -> union_list (fv fn :: List.map args ~f:fv)
    | Let (_, v, bind, body) -> union (fv bind) (Map.remove (fv body) v)
    | If (c, t_true, e) -> union_list [ fv c; fv t_true; fv e ]
    | Bop (_, l, r) -> union (fv l) (fv r)
    | Index (t, _) -> fv t
    | Record ts -> union_list (List.map ts ~f:fv)
    | Field (t, _) -> fv t
    | Variant (_, args) -> union_list (List.map args ~f:fv)
    | Match (scrutinee, cases) ->
      let fv_cases =
        List.map cases ~f:(fun (pat, body) ->
          let vars = Frontend.pat_bound_vars pat in
          List.fold vars ~init:(fv body) ~f:Map.remove)
      in
      union_list (fv scrutinee :: fv_cases)
  in
  fv t
;;

(** Gets last type in arrow type (return type of function) *)
let unroll_arrow ty =
  let rec go = function
    | Lower_tuples.TyArrow (_, r) -> go r
    | ty -> ty
  in
  go ty
;;

let rec lift_term (globals : String.Set.t) (env : env) (t : Uncurry.term)
  : term * top list
  =
  let lift = lift_term globals env in
  let lift_list ts =
    let pairs = List.map ts ~f:lift in
    let terms, tops = List.unzip pairs in
    terms, List.concat tops
  in
  let make term fvs = ({ desc = term; ty = t.ty; loc = t.loc } : term), fvs in
  match t.desc with
  | Var v ->
    (match Map.find env v with
     | None -> make (Var v) []
     | Some (lifted_name, []) -> make (Var lifted_name) []
     | Some (lifted_name, captured) ->
       let extra =
         List.map captured ~f:(fun (name, ty) ->
           ({ desc = Var name; ty; loc = t.loc } : term))
       in
       let term_desc = App ({ desc = Var lifted_name; ty = t.ty; loc = t.loc }, extra) in
       make term_desc [])
  | Float f -> make (Float f) []
  | Int i -> make (Int i) []
  | Bool b -> make (Bool b) []
  | Vec (n, ts) ->
    let ts, tops = lift_list ts in
    make (Vec (n, ts)) tops
  | App (f, args) ->
    let args, args_tops = lift_list args in
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
       let f, f_tops = lift f in
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
    let body, body_tops = lift_term globals env body in
    let desc =
      Define { name; recur; args = captured @ args; body; ret_ty = unroll_arrow ty }
    in
    let lifted_fn : top = { desc; ty; loc } in
    let bind, bind_tops = lift_term globals env bind in
    bind, body_tops @ [ lifted_fn ] @ bind_tops
  | Let (Rec _, _, _, _) -> raise "rec tag on non-lambda" ~loc:t.loc
  | Let (Nonrec, v, bind, body) ->
    let bind, bind_tops = lift bind in
    let body, body_tops = lift body in
    make (Let (v, bind, body)) (bind_tops @ body_tops)
  | If (c, t, e) ->
    let c, c_tops = lift c in
    let t, t_tops = lift t in
    let e, e_tops = lift e in
    make (If (c, t, e)) (c_tops @ t_tops @ e_tops)
  | Bop (op, l, r) ->
    let l, l_tops = lift l in
    let r, r_tops = lift r in
    make (Bop (op, l, r)) (l_tops @ r_tops)
  | Index (t, i) ->
    let t, t_tops = lift t in
    make (Index (t, i)) t_tops
  | Builtin (b, ts) ->
    let ts, tops = lift_list ts in
    make (Builtin (b, ts)) tops
  | Record ts ->
    let ts, tops = lift_list ts in
    make (Record ts) tops
  | Field (t, f) ->
    let t, tops = lift t in
    make (Field (t, f)) tops
  | Variant (ctor, args) ->
    let args, tops = lift_list args in
    make (Variant (ctor, args)) tops
  | Match (scrutinee, cases) ->
    let scrutinee, s_tops = lift scrutinee in
    let c_tops, cases =
      List.fold_map cases ~init:[] ~f:(fun acc (pat, body) ->
        let body, tops = lift body in
        List.rev_append tops acc, (pat, body))
    in
    make (Match (scrutinee, cases)) (s_tops @ List.rev c_tops)
  | Lam (args, body) ->
    (* Inline lambda in value position: lift it to a global, return Var/App referencing it *)
    let captured =
      let excluded = Set.union globals (String.Set.of_list (List.map args ~f:fst)) in
      free_vars env body |> Map.filter_keys ~f:(Fn.non (Set.mem excluded)) |> Map.to_alist
    in
    let name = Utils.fresh "lam" in
    let env = Map.set env ~key:name ~data:(name, captured) in
    let body, body_tops = lift_term globals env body in
    let lifted_fn =
      let args = captured @ args in
      let ret_ty = unroll_arrow t.ty in
      let desc = Define { name; recur = Nonrec; args; body; ret_ty } in
      { desc; ty = t.ty; loc = t.loc }
    in
    let name = ({ desc = Var name; ty = t.ty; loc = t.loc } : term) in
    let body = body_tops @ [ lifted_fn ] in
    if List.is_empty captured
    then name, body
    else (
      let extra =
        List.map captured ~f:(fun (n, ty) -> ({ desc = Var n; ty; loc = t.loc } : term))
      in
      ({ desc = App (name, extra); ty = t.ty; loc = t.loc } : term), body)
;;

let lift_top (globals : String.Set.t) (top : Uncurry.top) : top list =
  let make desc = { desc; ty = top.ty; loc = top.loc } in
  let lift = lift_term globals String.Map.empty in
  match top.desc with
  | Define (recur, name, { desc = Lam (args, body); ty; loc = _ }) ->
    let body, body_tops = lift body in
    body_tops @ [ make (Define { name; recur; args; body; ret_ty = unroll_arrow ty }) ]
  | Define (_, name, term) ->
    let term, term_tops = lift term in
    term_tops @ [ make (Const (name, term)) ]
  | Extern v -> [ make (Extern v) ]
  | TypeDef (name, decl) -> [ make (TypeDef (name, decl)) ]
;;

let lift_exn (t : Uncurry.t) : t =
  let (Program tops) = t in
  let globals =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | Define (_, v, _) -> Some v
      | Extern v -> Some v
      | TypeDef _ -> None)
    |> String.Set.of_list
  in
  let top_blocks = List.map tops ~f:(lift_top globals) in
  Program (List.concat top_blocks)
;;

let lift t = try_with (fun () -> lift_exn t)
