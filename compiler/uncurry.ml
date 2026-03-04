open Core

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | Mat of int * int * term list
  | Lam of (string * Stlc.ty) list * term
  | App of term * term list
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
[@@deriving sexp_of]

and term =
  { desc : term_desc
  ; loc : Lexer.loc [@sexp_drop_if Fn.const true]
  }
[@@deriving sexp_of]

type top_desc =
  | Define of string * term
  | Extern of Stlc.ty * string
[@@deriving sexp_of]

type top =
  { desc : top_desc
  ; loc : Lexer.loc
  }

let sexp_of_top t = sexp_of_top_desc t.desc

type t = Program of Stlc.ty String.Map.t * top list [@@deriving sexp_of]

let rec collect_lams (t : Stlc.term) : (string * Stlc.ty) list * term =
  match t.desc with
  | Lam (v, ty, body) ->
    let args, body = collect_lams body in
    (v, ty) :: args, body
  | _ -> [], uncurry_term t

and collect_apps (t : Stlc.term) : term * term list =
  match t.desc with
  | App (f, x) ->
    let f', args = collect_apps f in
    f', args @ [ uncurry_term x ]
  | _ -> uncurry_term t, []

and uncurry_term_desc (t : Stlc.term_desc) : term_desc =
  match t with
  | Var v -> Var v
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
  | Vec (n, ts) -> Vec (n, List.map ts ~f:uncurry_term)
  | Mat (x, y, ts) -> Mat (x, y, List.map ts ~f:uncurry_term)
  | Lam (v, ty, body) ->
    let args, body = collect_lams body in
    Lam ((v, ty) :: args, body)
  | App (f, x) ->
    let f', args = collect_apps f in
    App (f', args @ [ uncurry_term x ])
  | Let (v, bind, body) -> Let (v, uncurry_term bind, uncurry_term body)
  | If (c, t_true, e) -> If (uncurry_term c, uncurry_term t_true, uncurry_term e)
  | Bop (op, l, r) -> Bop (op, uncurry_term l, uncurry_term r)
  | Index (t_sub, i) -> Index (uncurry_term t_sub, i)
  | Builtin (b, ts) -> Builtin (b, List.map ts ~f:uncurry_term)

and uncurry_term (t : Stlc.term) : term = { desc = uncurry_term_desc t.desc; loc = t.loc }

let uncurry_top (t : Stlc.top) : top =
  let desc =
    match t.desc with
    | Define (v, term) -> Define (v, uncurry_term term)
    | Extern (ty, v) -> Extern (ty, v)
  in
  { desc; loc = t.loc }
;;

let uncurry (Typecheck.Program (map, tops)) : t =
  Program (map, List.map tops ~f:uncurry_top)
;;
