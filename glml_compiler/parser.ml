open Core
open Stlc
open Lexer
open Chomp
open Chomp.Let_syntax
open Chomp.Infix_syntax

let ident_p : string t =
  satisfy_map (function
    | ID s -> Some s
    | _ -> None)
;;

let between brace_type p =
  let l, r =
    match brace_type with
    | `Paren -> LPAREN, RPAREN
    | `Curly -> LCURLY, RCURLY
    | `Angle -> LANGLE, RANGLE
  in
  tok l *> p <* tok r
;;

let rec ty_p =
  fun st ->
  let ty_p = ty_arrow_p <|> ty_atom_p in
  (ty_p <|> between `Paren ty_p) st

and ty_atom_p = fun st -> ty_singles_p st

and ty_singles_p =
  satisfy_map (function
    | BOOL -> Some TyBool
    | INT -> Some TyInt
    | _ -> None)

and ty_arrow_p =
  fun st ->
  (let%bind l = ty_atom_p <|> between `Paren ty_p in
   let%bind _ = tok ARROW in
   let%bind r = ty_p in
   return (TyArrow (l, r)))
    st
;;

let%expect_test "ty parse tests" =
  let test s =
    s
    |> Lexer.of_string
    |> Lexer.lex
    |> Or_error.map ~f:(run ty_p)
    |> Or_error.join
    |> Or_error.sexp_of_t sexp_of_ty
    |> print_s
  in
  test "bool";
  test "((  int) )";
  test "int -> bool";
  [%expect
    {|
    (Ok TyBool)
    (Error ((chomp_error satisfy_map_fail) (contexts ())))
    (Ok (TyArrow TyInt TyBool))
    |}];
  test "";
  test "()";
  [%expect
    {|
    (Error ((chomp_error satisfy_eof) (contexts ())))
    (Error ((chomp_error satisfy_map_fail) (contexts ())))
    |}]
;;

let rec term_p =
  fun st ->
  (let%bind t = t_atom_p <|> between `Paren term_p in
   t_app_p t <|> return t)
    st

and t_atom_p =
  fun st ->
  let t_singles_p =
    satisfy_map (function
      | TRUE -> Some (Bool true)
      | FALSE -> Some (Bool false)
      | ID v -> Some (Var v)
      | _ -> None)
  in
  let t_commit_prefix_p =
    match%bind peek with
    | LET -> t_let_p
    | IF -> t_if_p
    | FUN -> t_abs_p
    | _ -> fail "commit: not a fixed prefix"
  in
  (t_singles_p <|> t_commit_prefix_p) st

and t_let_p =
  fun st ->
  (let%bind id = tok LET *> ident_p in
   let%bind bind = tok EQ *> term_p in
   let%bind body = tok IN *> term_p in
   return (Let (id, bind, body)))
    st

and t_if_p =
  fun st ->
  (let%bind c = tok IF *> term_p in
   let%bind t = tok THEN *> term_p in
   let%bind f = tok ELSE *> term_p in
   return (If (c, t, f)))
    st

and t_app_p t =
  let%bind ts = many (t_atom_p <|> between `Paren term_p) in
  return (List.fold_left ~f:(fun f x -> App (f, x)) ~init:t ts)

and t_abs_p =
  fun st ->
  (let%bind id = tok FUN *> ident_p in
   let%bind ty = tok COLON *> ty_p in
   let%bind t = tok ARROW *> term_p in
   return (Lam (id, ty, t)))
    st
;;

let%expect_test "term parse tests" =
  let test s =
    s
    |> Lexer.of_string
    |> Lexer.lex
    |> Or_error.map ~f:(run term_p)
    |> Or_error.join
    |> Or_error.sexp_of_t sexp_of_term
    |> print_s
  in
  test "false";
  test "if false then false else false";
  [%expect
    {|
    (Ok (Bool false))
    (Ok (If (Bool false) (Bool false) (Bool false)))
    |}];
  test "f x y z";
  test "f (x y) z";
  [%expect
    {|
    (Ok (App (App (App (Var f) (Var x)) (Var y)) (Var z)))
    (Ok (App (App (Var f) (App (Var x) (Var y))) (Var z)))
    |}];
  test "fun x : bool -> x";
  [%expect {| (Ok (Lam x TyBool (Var x))) |}]
;;
