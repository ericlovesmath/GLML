open Core
open Sexplib.Sexp

(* TODO: Improve Lexer in general *)

type token =
  | TRUE
  | FALSE
  | EQ
  | ARROW
  | LPAREN
  | RPAREN
  | DOT
  | LANGLE
  | RANGLE
  | LBRACKET
  | RBRACKET
  | SEMI
  | COLON
  | COMMA
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | BAR
  | MATCH
  | WITH
  | LCURLY
  | RCURLY
  | BOOL
  | INT
  | FLOAT
  | TICK
  | VEC
  | MAT
  | ADD
  | SUB
  | DIV
  | MUL
  | HASH
  | LEQ
  | GEQ
  | PERCENT
  | LAND
  | LOR
  | EXTERN
  | NUMERIC of int
  | ID of string
[@@deriving sexp, equal]

type pos =
  { i : int
  ; line : int
  ; col : int
  }

let sexp_of_pos { i = _; line; col } = Atom [%string "%{line#Int}:%{col#Int}"]

type loc = pos * pos

let sexp_of_loc (l, r) = List [ sexp_of_pos l; Atom "-"; sexp_of_pos r ]

type t =
  { str : string
  ; len : int
  ; mutable pos : pos
  }

let of_string s = { str = s; len = String.length s; pos = { i = 0; line = 1; col = 1 } }
let eof t = t.pos.i = t.len

let advance c t =
  t.pos
  <- (if Char.equal c '\n'
      then { i = t.pos.i + 1; line = t.pos.line + 1; col = 1 }
      else { i = t.pos.i + 1; line = t.pos.line; col = t.pos.col + 1 })
;;

let peek t = if eof t then None else Some (String.get t.str t.pos.i)

let skip t =
  match peek t with
  | Some c -> advance c t
  | None -> ()
;;

let rec strip t =
  match peek t with
  | Some c when Char.is_whitespace c ->
    skip t;
    strip t
  | _ -> ()
;;

let read_while f t =
  let start_i = t.pos.i in
  let rec go () =
    match peek t with
    | Some c when f c ->
      skip t;
      go ()
    | _ -> ()
  in
  go ();
  String.sub t.str ~pos:start_i ~len:(t.pos.i - start_i)
;;

let read_lexeme (t : t) : token Or_error.t =
  let consume tok =
    skip t;
    Ok tok
  in
  match peek t with
  | None -> error_s [%message "lexer: unexpected EOF" (t.pos : pos)]
  | Some c ->
    (match c with
     | '(' -> consume LPAREN
     | '=' -> consume EQ
     | ':' -> consume COLON
     | ')' -> consume RPAREN
     | '{' -> consume LCURLY
     | '}' -> consume RCURLY
     | '<' ->
       skip t;
       (match peek t with
        | Some '=' -> consume LEQ
        | _ -> Ok LANGLE)
     | '>' ->
       skip t;
       (match peek t with
        | Some '=' -> consume GEQ
        | _ -> Ok RANGLE)
     | '[' -> consume LBRACKET
     | ']' -> consume RBRACKET
     | ';' -> consume SEMI
     | ',' -> consume COMMA
     | '.' -> consume DOT
     | '|' ->
       skip t;
       (match peek t with
        | Some '|' -> consume LOR
        | _ -> Ok BAR)
     | '&' ->
       skip t;
       (match peek t with
        | Some '&' -> consume LAND
        | _ -> error_s [%message "lexer: single & is invalid" (t.pos : pos)])
     | '\'' -> consume TICK
     | '+' -> consume ADD
     | '-' ->
       skip t;
       (match peek t with
        | Some '>' -> consume ARROW
        | _ -> Ok SUB)
     | '/' -> consume DIV
     | '*' -> consume MUL
     | '#' -> consume HASH
     | '%' -> consume PERCENT
     | c when Char.is_digit c -> Ok (NUMERIC (Int.of_string (read_while Char.is_digit t)))
     | c when Char.is_alpha c ->
       let s = read_while (fun c -> Char.is_alpha c || Char.equal '_' c) t in
       Ok
         (match s with
          | "true" -> TRUE
          | "false" -> FALSE
          | "let" -> LET
          | "in" -> IN
          | "if" -> IF
          | "then" -> THEN
          | "else" -> ELSE
          | "fun" -> FUN
          | "match" -> MATCH
          | "with" -> WITH
          | "bool" -> BOOL
          | "int" -> INT
          | "float" -> FLOAT
          | "vec" -> VEC
          | "mat" -> MAT
          | "extern" -> EXTERN
          | _ -> ID s)
     | char -> error_s [%message "lexer: invalid char" (char : char) (t.pos : pos)])
;;

let lex (t : t) : (token * loc) list Or_error.t =
  let open Or_error.Let_syntax in
  let rec loop acc =
    strip t;
    if eof t
    then Ok (List.rev acc)
    else (
      let start_pos = t.pos in
      let%bind lexeme = read_lexeme t in
      let end_pos = t.pos in
      loop ((lexeme, (start_pos, end_pos)) :: acc))
  in
  loop []
;;

let%expect_test "lexer" =
  let test s =
    s
    |> of_string
    |> lex
    |> Or_error.map ~f:(List.map ~f:fst)
    |> Or_error.sexp_of_t (List.sexp_of_t sexp_of_token)
    |> print_s
  in
  test "true false = -> ( ) . < >";
  test "{ } ; : , if then else let";
  test "in fun | match with { }";
  test "bool int float ' 10 string_var";
  test "+ - / * # <= >= % && ||";
  [%expect
    {|
    (Ok (TRUE FALSE EQ ARROW LPAREN RPAREN DOT LANGLE RANGLE))
    (Ok (LCURLY RCURLY SEMI COLON COMMA IF THEN ELSE LET))
    (Ok (IN FUN BAR MATCH WITH LCURLY RCURLY))
    (Ok (BOOL INT FLOAT TICK (NUMERIC 10) (ID string_var)))
    (Ok (ADD SUB DIV MUL HASH LEQ GEQ PERCENT LAND LOR))
    |}];
  test "let{x:int}=match|a->fun->(f<x>*2)";
  [%expect
    {|
    (Ok
     (LET LCURLY (ID x) COLON INT RCURLY EQ MATCH BAR (ID a) ARROW FUN ARROW
      LPAREN (ID f) LANGLE (ID x) RANGLE MUL (NUMERIC 2) RPAREN))
    |}];
  test "vec2 mat2x3";
  [%expect {| (Ok (VEC (NUMERIC 2) MAT (NUMERIC 2) (ID x) (NUMERIC 3))) |}]
;;
