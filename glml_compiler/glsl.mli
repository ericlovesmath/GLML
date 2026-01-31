type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVoid
  | TyVec of int
  | TyMat of int * int
[@@deriving sexp_of]

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

type term =
  | Float of float
  | Int of int
  | Bool of bool
  | Var of string
  | Bop of binary_op * term * term
  | If of term * term * term
  | App of string * term list
  | Swizzle of term * string
[@@deriving sexp_of]

type qualifier =
  | Uniform
  | Out
  | Const
[@@deriving sexp_of]

type stmt =
  | Decl of qualifier option * ty * string * term
  | Assign of term * term
  | Return of term option
  | Expr of term
  | IfStmt of term * stmt * stmt option
  | For of stmt * term * stmt * stmt
  | Block of stmt list
  | Break
[@@deriving sexp_of]

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

type t = Program of decl list [@@deriving sexp_of, of_string]

(** Converts [Glsl.t] into fragment shader as GLSL string *)
val to_shader : t -> string
