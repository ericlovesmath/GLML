[@@@warning "-32-37-27"]
(* TODO: Remove Warning *)

open Base

type ty_lit =
  | TyFloat
  | TyBool
  | TyVec of int
  | TyMat of int * int

let string_of_ty_lit = function
  | TyFloat -> "float"
  | TyBool -> "bool"
  | TyVec i -> "vec" ^ Int.to_string i
  | TyMat (x, y) when x = y -> "mat" ^ Int.to_string x
  | TyMat (x, y) -> "mat" ^ Int.to_string x ^ "x" ^ Int.to_string y
;;

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | And
  | Or

let string_of_binary_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Lt -> "<"
  | And -> "&&"
  | Or -> "||"
;;

type t =
  | Float of float
  | Bool of bool
  | Var of string
  | Bop of binary_op * t * t
  | If of t * t
  | App of string * t list
  | Swizzle of t * string

let rec string_of_t = function
  | Float f -> Float.to_string f
  | Bool b -> Bool.to_string b
  | Var v -> v
  | Bop (bop, t, t') ->
    let bop = string_of_binary_op bop in
    let t = string_of_t t in
    let t' = string_of_t t' in
    [%string "(%{t} %{bop} %{t'})"]
  | If _ -> failwith "TODO string_of_t If"
  | App (f, args) ->
    let args = args |> List.map ~f:string_of_t |> String.concat ~sep:", " in
    [%string "%{f}(%{args})"]
  | Swizzle _ -> failwith "TODO string_of_t Swizzle"
;;

type qualifier =
  | Uniform
  | Out
  | Const

let string_of_qualifier = function
  | Uniform -> "uniform"
  | Out -> "out"
  | Const -> "const"
;;

type stmt =
  | Assign of ty_lit option * string * t
  | Return of t
  | Expr of t

let string_of_stmt = function
  | Assign (ty, variable, t) ->
    let t = string_of_t t in
    (match ty with
     | None -> [%string "%{variable} = %{t}"]
     | Some ty ->
       let ty = string_of_ty_lit ty in
       [%string "%{ty} %{variable} = %{t}"])
  | Return t -> "return " ^ string_of_t t ^ ";"
  | Expr t -> string_of_t t ^ ";"
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

(* TODO: Remove ppx_blob and use [base] instead once ready *)

let base =
  let ( + ) t t' = Bop (Add, t, t') in
  let ( - ) t t' = Bop (Sub, t, t') in
  let ( / ) t t' = Bop (Div, t, t') in
  let ( * ) t t' = Bop (Mul, t, t') in
  let ( $ ) f ts = App (f, ts) in
  let v var = Var var in
  let f float = Float float in
  [ Global (Uniform, TyVec 2, "u_resolution")
  ; Global (Uniform, TyVec 2, "u_mouse")
  ; Global (Uniform, TyFloat, "u_time")
  ; Global (Out, TyVec 4, "fragColor")
  ; Function
      { name = "sMin"
      ; desc = Some "Exponential Smooth Minimum"
      ; params = [ TyFloat, "a"; TyFloat, "b"; TyFloat, "k" ]
      ; ret_type = TyFloat
      ; body =
          [ Assign (None, "k", v "k" * f 1.0)
          ; Assign
              ( Some TyFloat
              , "r"
              , ("exp2" $ [ f (-1.0) * v "a" / v "k" ])
                + ("exp2" $ [ f (-1.0) * v "b" / v "k" ]) )
          ; Return (f (-1.0) * v "k" * ("log2" $ [ v "r" ]))
          ]
      }
  ; Function
      { name = "sdCircle"
      ; desc = Some "SDF for a circle"
      ; params = [ TyVec 2, "p"; TyFloat, "r" ]
      ; ret_type = TyFloat
      ; body = [ Return (("length" $ [ v "p" ]) - v "r") ]
      }
  ]
;;

let () =
  Js_of_ocaml.Js.export
    "glml"
    (object%js
       val description = "GLML Language Compiler"
       val shader = [%blob "./shader.frag"]
       val testing = string_of_decl_list base
    end)
;;
