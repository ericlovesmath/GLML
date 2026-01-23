open Core
open Sexplib.Sexp

type ty_lit =
  | TyFloat
  | TyInt
  | TyBool
  | TyVoid
  | TyVec of int
  | TyMat of int * int
[@@deriving sexp]

let string_of_ty_lit = function
  | TyFloat -> "float"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVoid -> "void"
  | TyVec i -> "vec" ^ Int.to_string i
  | TyMat (x, y) when x = y -> "mat" ^ Int.to_string x
  | TyMat (x, y) -> "mat" ^ Int.to_string x ^ "x" ^ Int.to_string y
;;

let ty_lit_of_string = function
  | "float" -> TyFloat
  | "int" -> TyInt
  | "bool" -> TyBool
  | "void" -> TyVoid
  | "vec2" -> TyVec 2
  | "vec3" -> TyVec 3
  | "vec4" -> TyVec 4
  | "mat2" -> TyMat (2, 2)
  | "mat3" -> TyMat (3, 3)
  | "mat4" -> TyMat (4, 4)
  | s -> failwith ("Unknown type: " ^ s)
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
[@@deriving sexp]

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

type t =
  | Float of float
  | Int of int
  | Bool of bool
  | Var of string
  | Bop of binary_op * t * t
  | If of t * t * t
  | App of string * t list
  | Swizzle of t * string
[@@deriving sexp]

let rec string_of_t = function
  | Float f -> Float.to_string f
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b
  | Var v -> v
  | Bop (bop, t, t') ->
    let bop = string_of_binary_op bop in
    let t = string_of_t t in
    let t' = string_of_t t' in
    [%string "(%{t} %{bop} %{t'})"]
  | If (c, t, e) ->
    let c = string_of_t c in
    let t = string_of_t t in
    let e = string_of_t e in
    [%string "(%{c} ? %{t} : %{e})"]
  | App (f, args) ->
    let args = args |> List.map ~f:string_of_t |> String.concat ~sep:", " in
    [%string "%{f}(%{args})"]
  | Swizzle (t, s) ->
    let t = string_of_t t in
    [%string "%{t}.%{s}"]
;;

let rec t_of_sexp (s : Sexp.t) : t =
  match s with
  | Atom "true" -> Bool true
  | Atom "false" -> Bool false
  | Atom s ->
    (match Int.of_string_opt s with
     | Some i -> Int i
     | None ->
       (match Float.of_string_opt s with
        | Some f -> Float f
        | None -> Var s))
  | List (Atom op :: args) ->
    (match op, args with
     | "+", [ a; b ] -> Bop (Add, t_of_sexp a, t_of_sexp b)
     | "-", [ a; b ] -> Bop (Sub, t_of_sexp a, t_of_sexp b)
     | "*", [ a; b ] -> Bop (Mul, t_of_sexp a, t_of_sexp b)
     | "/", [ a; b ] -> Bop (Div, t_of_sexp a, t_of_sexp b)
     | "%", [ a; b ] -> Bop (Mod, t_of_sexp a, t_of_sexp b)
     | "==", [ a; b ] -> Bop (Eq, t_of_sexp a, t_of_sexp b)
     | "<", [ a; b ] -> Bop (Lt, t_of_sexp a, t_of_sexp b)
     | ">", [ a; b ] -> Bop (Gt, t_of_sexp a, t_of_sexp b)
     | "<=", [ a; b ] -> Bop (Leq, t_of_sexp a, t_of_sexp b)
     | ">=", [ a; b ] -> Bop (Geq, t_of_sexp a, t_of_sexp b)
     | "&&", [ a; b ] -> Bop (And, t_of_sexp a, t_of_sexp b)
     | "||", [ a; b ] -> Bop (Or, t_of_sexp a, t_of_sexp b)
     | "if", [ c; t; e ] -> If (t_of_sexp c, t_of_sexp t, t_of_sexp e)
     | ".", [ t; Atom s ] -> Swizzle (t_of_sexp t, s)
     | f, args -> App (f, List.map args ~f:t_of_sexp))
  | List _ -> failwith "Invalid expression structure"
;;

type qualifier =
  | Uniform
  | Out
  | Const
[@@deriving sexp, string ~capitalize:"lowercase"]

type stmt =
  | Decl of qualifier option * ty_lit * string * t
  | Assign of t * t
  | Return of t option
  | Expr of t
  | IfStmt of t * stmt * stmt option
  | For of stmt * t * stmt * stmt
  | Block of stmt list
  | Break
[@@deriving sexp]

let rec string_of_stmt = function
  | Decl (qual, ty, name, t) ->
    let t = string_of_t t in
    let ty = string_of_ty_lit ty in
    (match qual with
     | None -> [%string "%{ty} %{name} = %{t};"]
     | Some q ->
       let q = string_of_qualifier q in
       [%string "%{q} %{ty} %{name} = %{t};"])
  | Assign (lhs, rhs) ->
    let lhs = string_of_t lhs in
    let rhs = string_of_t rhs in
    [%string "%{lhs} = %{rhs};"]
  | Return t ->
    (match t with
     | None -> "return;"
     | Some t -> "return " ^ string_of_t t ^ ";")
  | Expr t -> string_of_t t ^ ";"
  | IfStmt (cond, t, e) ->
    let cond = string_of_t cond in
    let t = string_of_stmt t in
    (match e with
     | None -> [%string "if (%{cond}) %{t}"]
     | Some e ->
       let e = string_of_stmt e in
       [%string "if (%{cond}) %{t} else %{e}"])
  | For (init, cond, iter, body) ->
    let init = string_of_stmt init in
    let cond = string_of_t cond in
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
    let body =
      stmts
      |> List.map ~f:string_of_stmt
      |> List.map ~f:(fun s -> "    " ^ s)
      |> String.concat ~sep:"\n"
    in
    [%string "{\n%{body}\n}"]
  | Break -> "break;"
;;

let rec stmt_of_sexp (s : Sexp.t) : stmt =
  match s with
  | List [ Atom "assign"; Atom qual; Atom ty; Atom name; val_expr ] ->
    Decl (Some (qualifier_of_string qual), ty_lit_of_string ty, name, t_of_sexp val_expr)
  | List [ Atom "assign"; Atom ty_or_var; Atom name_or_expr; val_expr ] ->
    Decl (None, ty_lit_of_string ty_or_var, name_or_expr, t_of_sexp val_expr)
  | List [ Atom "assign"; lhs; rhs ] -> Assign (t_of_sexp lhs, t_of_sexp rhs)
  | List [ Atom "return"; val_expr ] -> Return (Some (t_of_sexp val_expr))
  | List [ Atom "return" ] -> Return None
  | List [ Atom "if"; cond; then_stmt ] ->
    IfStmt (t_of_sexp cond, stmt_of_sexp then_stmt, None)
  | List [ Atom "if"; cond; then_stmt; else_stmt ] ->
    IfStmt (t_of_sexp cond, stmt_of_sexp then_stmt, Some (stmt_of_sexp else_stmt))
  | List [ Atom "for"; init; cond; iter; body ] ->
    For (stmt_of_sexp init, t_of_sexp cond, stmt_of_sexp iter, stmt_of_sexp body)
  | List (Atom "block" :: stmts) -> Block (List.map stmts ~f:stmt_of_sexp)
  | List [ Atom "break" ] -> Break
  | _ -> Expr (t_of_sexp s)
;;

type decl =
  | Global of qualifier * ty_lit * string
  | Function of
      { name : string
      ; desc : string option
      ; params : (ty_lit * string) list
      ; ret_type : ty_lit
      ; body : stmt list
      }
[@@deriving sexp]

let sexp_of_decl (s : Sexp.t) : decl =
  match s with
  | List [ Atom "global"; Atom qual; Atom ty; Atom name ] ->
    Global (qualifier_of_string qual, ty_lit_of_string ty, name)
  | List (Atom "fun" :: Atom name :: List params :: Atom ret_type :: body) ->
    let params =
      List.map params ~f:(function
        | List [ Atom ty; Atom name ] -> ty_lit_of_string ty, name
        | _ -> failwith "Invalid param decl")
    in
    Function
      { name
      ; desc = None
      ; params
      ; ret_type = ty_lit_of_string ret_type
      ; body = List.map body ~f:stmt_of_sexp
      }
  | _ -> failwith "Invalid declaration"
;;

let string_of_decl_list (decls : decl list) : string =
  let string_of_decl = function
    | Global (qualifier, ty, name) ->
      let qualifier = string_of_qualifier qualifier in
      let ty = string_of_ty_lit ty in
      [%string "%{qualifier} %{ty} %{name};"]
    | Function { name; desc = _; params; ret_type; body } ->
      let ret_type = string_of_ty_lit ret_type in
      let params =
        params
        |> List.map ~f:(fun (ty, param) ->
          let ty = string_of_ty_lit ty in
          [%string "%{ty} %{param}"])
        |> String.concat ~sep:", "
      in
      let body =
        body
        |> List.map ~f:string_of_stmt
        |> List.map ~f:(String.append "    ")
        |> String.concat ~sep:"\n"
      in
      [%string "%{ret_type} %{name}(%{params}) {\n%{body}\n}"]
  in
  List.map ~f:string_of_decl decls
  |> List.cons "precision highp float;"
  |> List.cons "#version 300 es"
  |> String.concat ~sep:"\n"
;;

let compile_source src =
  match Sexp.of_string src with
  | List decls -> string_of_decl_list (List.map decls ~f:sexp_of_decl)
  | Atom _ -> failwith "Expected list of declarations"
;;
