open Core
open Sexplib.Sexp

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
  | REC
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
  | VEC of int
  | MAT of int * int
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
  | TYPE
  | NUMERIC of int
  | FLOAT_LIT of float
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

let init_loc =
  let p = { i = 0; line = 0; col = 0 } in
  p, p
;;

let merge_loc (p1_start, _) (_, p2_end) = p1_start, p2_end
let loc_end (_, p_end) = p_end, p_end

type t =
  { str : string
  ; len : int
  ; mutable pos : pos
  }

let init s = { str = s; len = String.length s; pos = { i = 0; line = 1; col = 1 } }
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

let rec strip t =
  match peek t with
  | Some c when Char.is_whitespace c ->
    skip t;
    strip t
  | Some '/' ->
    let start_pos = t.pos in
    skip t;
    (match peek t with
     | Some '/' ->
       let _ = read_while (fun c -> not (Char.equal c '\n')) t in
       strip t
     | _ -> t.pos <- start_pos)
  | _ -> ()
;;

(* TODO: Make [vecs] and [mats] generalizable sizes *)
let vecs = String.Map.of_alist_exn [ "vec2", VEC 2; "vec3", VEC 3; "vec4", VEC 4 ]

let mats =
  String.Map.of_alist_exn
    [ "mat2", MAT (2, 2)
    ; "mat2x2", MAT (2, 2)
    ; "mat2x3", MAT (2, 3)
    ; "mat2x4", MAT (2, 4)
    ; "mat3", MAT (3, 3)
    ; "mat3x2", MAT (3, 2)
    ; "mat3x3", MAT (3, 3)
    ; "mat3x4", MAT (3, 4)
    ; "mat4", MAT (4, 4)
    ; "mat4x2", MAT (4, 2)
    ; "mat4x3", MAT (4, 3)
    ; "mat4x4", MAT (4, 4)
    ]
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
     | c when Char.is_digit c ->
       let int_part = read_while Char.is_digit t in
       (match peek t with
        | Some '.' ->
          skip t;
          let frac_part = read_while Char.is_digit t in
          Ok (FLOAT_LIT (Float.of_string (int_part ^ "." ^ frac_part)))
        | _ -> Ok (NUMERIC (Int.of_string int_part)))
     | c when Char.is_alpha c ->
       let s = read_while (fun c -> Char.is_alphanum c || String.mem "_'" c) t in
       (match s with
        | "true" -> Ok TRUE
        | "false" -> Ok FALSE
        | "let" -> Ok LET
        | "rec" -> Ok REC
        | "in" -> Ok IN
        | "if" -> Ok IF
        | "then" -> Ok THEN
        | "else" -> Ok ELSE
        | "fun" -> Ok FUN
        | "match" -> Ok MATCH
        | "with" -> Ok WITH
        | "bool" -> Ok BOOL
        | "int" -> Ok INT
        | "float" -> Ok FLOAT
        | "extern" -> Ok EXTERN
        | "type" -> Ok TYPE
        | s when Map.mem vecs s -> Map.find_or_error vecs s
        | s when Map.mem mats s -> Map.find_or_error mats s
        | _ -> Ok (ID s))
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
    |> init
    |> lex
    |> Or_error.map ~f:(List.map ~f:fst)
    |> Or_error.sexp_of_t (List.sexp_of_t sexp_of_token)
    |> print_s
  in
  test "true false = -> ( ) . < >";
  test "{ } ; : , if then else let";
  test "in fun | match with { }";
  test "bool int float ' 10 string_var vec mat";
  test "+ - / * # <= >= % && || extern let type";
  test "1.23 0.45 6. -1.";
  [%expect
    {|
    (Ok (TRUE FALSE EQ ARROW LPAREN RPAREN DOT LANGLE RANGLE))
    (Ok (LCURLY RCURLY SEMI COLON COMMA IF THEN ELSE LET))
    (Ok (IN FUN BAR MATCH WITH LCURLY RCURLY))
    (Ok (BOOL INT FLOAT TICK (NUMERIC 10) (ID string_var) (ID vec) (ID mat)))
    (Ok (ADD SUB DIV MUL HASH LEQ GEQ PERCENT LAND LOR EXTERN LET TYPE))
    (Ok ((FLOAT_LIT 1.23) (FLOAT_LIT 0.45) (FLOAT_LIT 6) SUB (FLOAT_LIT 1)))
    |}];
  test "let{x:int}=match|a->fun->(f<x>*2)";
  [%expect
    {|
    (Ok
     (LET LCURLY (ID x) COLON INT RCURLY EQ MATCH BAR (ID a) ARROW FUN ARROW
      LPAREN (ID f) LANGLE (ID x) RANGLE MUL (NUMERIC 2) RPAREN))
    |}];
  test
    {|
    // top comment
    x + 2 //inline comment
    x + 3
    // outer comment
    y - x
    // bottom comment
    |};
  [%expect {| (Ok ((ID x) ADD (NUMERIC 2) (ID x) ADD (NUMERIC 3) (ID y) SUB (ID x))) |}]
;;
