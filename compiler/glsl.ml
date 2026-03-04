open Core
open Sexplib.Sexp

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVoid
  | TyVec of int
  | TyMat of int * int
[@@deriving sexp_of]

let string_of_ty = function
  | TyFloat -> "float"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVoid -> "void"
  | TyVec i -> "vec" ^ Int.to_string i
  | TyMat (x, y) when x = y -> "mat" ^ Int.to_string x
  | TyMat (x, y) -> "mat" ^ Int.to_string x ^ "x" ^ Int.to_string y
;;

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Lt
  | Gt
  | Leq
  | Geq
  | And
  | Or
[@@deriving sexp_of]

type builtin =
  | Sin
  | Cos
  | Tan
  | Asin
  | Acos
  | Atan
  | Pow
  | Exp
  | Log
  | Exp2
  | Log2
  | Sqrt
  | Abs
  | Sign
  | Floor
  | Ceil
  | Min
  | Max
  | Clamp
  | Mix
  | Length
  | Distance
  | Dot
  | Cross
  | Normalize
[@@deriving sexp_of, string ~capitalize:"lower sentence case"]

let builtin_of_string_opt s = Option.try_with (fun () -> builtin_of_string s)

let string_of_binary_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
;;

type term =
  | Float of float
  | Int of int
  | Bool of bool
  | Var of string
  | Bop of binary_op * term * term
  | If of term * term * term
  | App of string * term list
  | Builtin of builtin * term list
  | Swizzle of term * string
  | Index of term * int

let rec sexp_of_term = function
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Var v -> Atom v
  | Bop (op, l, r) ->
    List [ Atom (string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | App (f, args) -> List (Atom f :: List.map args ~f:sexp_of_term)
  | Builtin (b, args) -> List (Atom (string_of_builtin b) :: List.map args ~f:sexp_of_term)
  | Swizzle (t, s) -> List [ Atom "."; sexp_of_term t; Atom s ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
;;

let rec string_of_term = function
  | Float f -> Float.to_string f
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b
  | Var v -> v
  | Bop (bop, t, t') ->
    let bop = string_of_binary_op bop in
    let t = string_of_term t in
    let t' = string_of_term t' in
    [%string "(%{t} %{bop} %{t'})"]
  | If (c, t, e) ->
    let c = string_of_term c in
    let t = string_of_term t in
    let e = string_of_term e in
    [%string "(%{c} ? %{t} : %{e})"]
  | App (f, args) ->
    let args = args |> List.map ~f:string_of_term |> String.concat ~sep:", " in
    [%string "%{f}(%{args})"]
  | Builtin (b, args) ->
    let b = string_of_builtin b in
    let args = args |> List.map ~f:string_of_term |> String.concat ~sep:", " in
    [%string "%{b}(%{args})"]
  | Swizzle (t, s) ->
    let t = string_of_term t in
    [%string "%{t}.%{s}"]
  | Index (t, i) ->
    let t = string_of_term t in
    [%string "%{t}[%{i#Int}]"]
;;

type qualifier =
  | Uniform
  | Out
  | Const
[@@deriving sexp_of, string ~capitalize:"lower sentence case"]

type stmt =
  | Decl of qualifier option * ty * string * term
  | Set of term * term
  | Return of term option
  | Expr of term
  | IfStmt of term * stmt * stmt option
  | For of stmt * term * stmt * stmt
  | Block of stmt list
  | Break

let rec sexp_of_stmt (s : stmt) : Sexp.t =
  match s with
  | Decl (qual, ty, name, t) ->
    let qual = Option.sexp_of_t (fun q -> Atom (string_of_qualifier q)) qual in
    List (Atom "set" :: qual :: [ Atom (string_of_ty ty); Atom name; sexp_of_term t ])
  | Set (lhs, rhs) -> List [ Atom "set"; sexp_of_term lhs; sexp_of_term rhs ]
  | Return (Some t) -> List [ Atom "return"; sexp_of_term t ]
  | Return None -> List [ Atom "return" ]
  | Expr t -> sexp_of_term t
  | IfStmt (cond, then_stmt, else_stmt) ->
    let else_stmt =
      match else_stmt with
      | Some s -> [ sexp_of_stmt s ]
      | None -> []
    in
    List ([ Atom "if"; sexp_of_term cond; sexp_of_stmt then_stmt ] @ else_stmt)
  | For (init, cond, iter, body) ->
    List
      [ Atom "for"
      ; sexp_of_stmt init
      ; sexp_of_term cond
      ; sexp_of_stmt iter
      ; sexp_of_stmt body
      ]
  | Block stmts -> List (Atom "Block" :: List.map stmts ~f:sexp_of_stmt)
  | Break -> Atom "break"
;;

let indent s =
  s |> String.split_lines |> List.map ~f:(String.append "    ") |> String.concat ~sep:"\n"
;;

let rec string_of_stmt = function
  | Decl (qual, ty, name, t) ->
    let t = string_of_term t in
    let ty = string_of_ty ty in
    (match qual with
     | None -> [%string "%{ty} %{name} = %{t};"]
     | Some q ->
       let q = string_of_qualifier q in
       [%string "%{q} %{ty} %{name} = %{t};"])
  | Set (lhs, rhs) ->
    let lhs = string_of_term lhs in
    let rhs = string_of_term rhs in
    [%string "%{lhs} = %{rhs};"]
  | Return t ->
    (match t with
     | None -> "return;"
     | Some t -> "return " ^ string_of_term t ^ ";")
  | Expr t -> string_of_term t ^ ";"
  | IfStmt (cond, t, e) ->
    let cond = string_of_term cond in
    let t = string_of_stmt t in
    (match e with
     | None -> [%string "if (%{cond}) %{t}"]
     | Some e ->
       let e = string_of_stmt e in
       [%string "if (%{cond}) %{t} else %{e}"])
  | For (init, cond, iter, body) ->
    let init = string_of_stmt init in
    let cond = string_of_term cond in
    let iter = string_of_stmt iter in
    (* Strip semicolon from iter if present *)
    let iter =
      if String.is_suffix iter ~suffix:";"
      then String.sub iter ~pos:0 ~len:(String.length iter - 1)
      else iter
    in
    let body = string_of_stmt body in
    [%string "for (%{init} %{cond}; %{iter}) %{body}"]
  | Block stmts ->
    stmts
    |> List.map ~f:string_of_stmt
    |> List.map ~f:indent
    |> String.concat ~sep:"\n"
    |> fun body -> [%string "{\n%{body}\n}"]
  | Break -> "break;"
;;

type decl =
  | Global of qualifier * ty * string
  | Function of
      { name : string
      ; desc : string option
      ; params : (ty * string) list
      ; ret_type : ty
      ; body : stmt list
      }
[@@deriving sexp_of]

type t = Program of decl list [@@deriving sexp_of]

let to_string (Program decls : t) : string =
  let string_of_decl = function
    | Global (qualifier, ty, name) ->
      let qualifier = string_of_qualifier qualifier in
      let ty = string_of_ty ty in
      [%string "%{qualifier} %{ty} %{name};"]
    | Function { name; desc = _; params; ret_type; body } ->
      let ret_type = string_of_ty ret_type in
      let params =
        params
        |> List.map ~f:(fun (ty, param) ->
          let ty = string_of_ty ty in
          [%string "%{ty} %{param}"])
        |> String.concat ~sep:", "
      in
      let body =
        body
        |> List.map ~f:string_of_stmt
        |> List.map ~f:indent
        |> String.concat ~sep:"\n"
      in
      [%string "%{ret_type} %{name}(%{params}) {\n%{body}\n}"]
  in
  List.map ~f:string_of_decl decls
  |> List.cons "precision highp float;"
  |> List.cons "#version 300 es"
  |> String.concat ~sep:"\n"
;;
