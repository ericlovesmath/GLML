open Core
open Stlc
open Lexer
open Chomp
open Chomp.Let_syntax
open Chomp.Infix_syntax

let ident_p =
  satisfy_map (function
    | ID s -> Some s
    | _ -> None)
  <??> "ident"
;;

let num_p =
  satisfy_map (function
    | NUMERIC i -> Some i
    | _ -> None)
  <??> "num"
;;

let commas p = sep_by1 (tok COMMA) p

let between brace_type p =
  let l, r =
    match brace_type with
    | `Paren -> LPAREN, RPAREN
    | `Curly -> LCURLY, RCURLY
    | `Angle -> LANGLE, RANGLE
    | `Bracket -> LBRACKET, RBRACKET
  in
  tok l *> p <* tok r <??> "between"
;;

let ty_vec_p =
  let%bind _ = tok VEC in
  let%bind n = num_p in
  return (TyVec n) <??> "ty_vec"
;;

let ty_mat_p =
  let%bind _ = tok MAT in
  let%bind n = num_p in
  let%bind m = tok (ID "x") *> num_p <|> return n in
  return (TyMat (n, m)) <??> "ty_mat"
;;

let ty_singles_p =
  satisfy_map (function
    | BOOL -> Some TyBool
    | INT -> Some TyInt
    | FLOAT -> Some TyFloat
    | _ -> None)
  <??> "ty_single"
;;

let ty_atom_p = ty_singles_p <|> ty_vec_p <|> ty_mat_p

let rec ty_arrow_p =
  fun st ->
  (let%bind l = ty_atom_p <|> between `Paren ty_p in
   let%bind _ = tok ARROW in
   let%bind r = ty_p in
   return (TyArrow (l, r)) <??> "ty_arrow")
    st

and ty_p =
  fun st ->
  let ty_p = ty_arrow_p <|> ty_atom_p in
  (ty_p <|> between `Paren ty_p <??> "ty") st
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
  test "float";
  test "int";
  test "bool";
  test "vec2";
  test "mat3x2";
  test "mat3";
  test "float -> int";
  [%expect
    {|
    (Ok TyFloat)
    (Ok TyInt)
    (Ok TyBool)
    (Ok (TyVec 2))
    (Ok (TyMat 3 2))
    (Ok (TyMat 3 3))
    (Ok (TyArrow TyFloat TyInt))
    |}];
  test "(vec4)";
  test "(mat3x2->vec2)->(vec2->int)";
  [%expect
    {|
    (Ok (TyVec 4))
    (Ok (TyArrow (TyArrow (TyMat 3 2) (TyVec 2)) (TyArrow (TyVec 2) TyInt)))
    |}];
  test "";
  test "()";
  [%expect
    {|
    (Error ((chomp_error satisfy_eof) (contexts (between ty))))
    (Error
     ((chomp_error "satisfy_fail on token RPAREN at 1:2")
      (contexts ("between at 1:1" "ty at 1:1"))))
    |}]
;;

let term_number_p =
  let%bind sign = tok SUB *> return (-1) <|> tok ADD *> return 1 <|> return 1 in
  let%bind n = num_p in
  (let%bind m = tok DOT *> (num_p <|> return 0) in
   let unsigned_float = Float.of_string [%string "%{n#Int}.%{m#Int}"] in
   let float = Float.of_int sign *. unsigned_float in
   return (Float float) <??> "term_float")
  <|> (return (Int (sign * n)) <??> "term_int")
;;

let rec term_let_p =
  fun st ->
  (tok LET
   *> commit
        (let%bind id = ident_p in
         let%bind bind = tok EQ *> term_p in
         let%bind body = tok IN *> term_p in
         return (Let (id, bind, body)))
   <??> "term_let")
    st

and term_if_p =
  fun st ->
  (tok IF
   *> commit
        (let%bind c = term_p in
         let%bind t = tok THEN *> term_p in
         let%bind f = tok ELSE *> term_p in
         return (If (c, t, f)))
   <??> "term_if")
    st

and term_app_p term =
  let%bind ts = many (term_atom_p <|> between `Paren term_p) in
  return (List.fold_left ~f:(fun f x -> App (f, x)) ~init:term ts) <??> "term_app"

and term_lam_p =
  fun st ->
  (tok FUN
   *> tok LPAREN
   *> commit
        (let%bind id = ident_p in
         let%bind ty = tok COLON *> ty_p in
         let%bind t = tok RPAREN *> tok ARROW *> term_p in
         return (Lam (id, ty, t)))
   <??> "term_lam")
    st

and term_mat_p =
  fun st ->
  ((let%bind terms = between `Angle (commas (between `Angle (commas term_p))) in
    let n = List.length terms in
    let m = List.length (List.hd_exn terms) in
    if List.for_all terms ~f:(fun ts -> List.length ts = m)
    then return (Mat (n, m, List.concat terms))
    else fail "matrix contains rows of unequal size")
   <??> "term_mat")
    st

and term_vec_p =
  fun st ->
  (let%bind terms = between `Angle (commas term_p) in
   return (Vec (List.length terms, terms)) <??> "term_index")
    st

and term_index_p term =
  let%bind indices = many1 (between `Bracket num_p) in
  return (List.fold_left indices ~init:term ~f:(fun acc i -> Index (acc, i)))
  <??> "term_index"

(* NOTE: Builtins should not be special forms, but right now they are not curried so.. *)
and term_builtin_p =
  fun st ->
  (let%bind _ = tok HASH in
   let%bind builtin =
     satisfy_map (function
       | ID s -> Option.try_with (fun () -> Glsl.builtin_of_string s)
       | _ -> None)
   in
   let%bind args = between `Paren (commas term_p) in
   return (Builtin (builtin, args)) <??> "term_builtin")
    st

(* NOTE: We are reparsing the first term every time we call this but [chainl1] doesn't
   fit well into our system of passing in the term. If this is a bottleneck, maybe consider
   rewriting with proper postfix. *)
and term_bop_p =
  fun st ->
  (let bop_tok (t : token) (op : Glsl.binary_op) : (term -> term -> term) t =
     tok t *> return (fun l r -> Bop (op, l, r))
   in
   let bop_mul_p =
     bop_tok MUL Glsl.Mul <|> bop_tok DIV Glsl.Div <|> bop_tok PERCENT Glsl.Mod
   in
   let bop_add_p = bop_tok ADD Glsl.Add <|> bop_tok SUB Glsl.Sub in
   let bop_rel_p =
     bop_tok LANGLE Glsl.Lt
     <|> bop_tok RANGLE Glsl.Gt
     <|> bop_tok LEQ Glsl.Leq
     <|> bop_tok GEQ Glsl.Geq
   in
   let bop_eq_p = bop_tok EQ Glsl.Eq in
   let bop_and_p = bop_tok LAND Glsl.And in
   let bop_or_p = bop_tok LOR Glsl.Or in
   let rec term_factor_p st =
     (let%bind t = term_atom_p <|> between `Paren term_p in
      term_index_p t <|> term_app_p t <|> return t <??> "term_factor")
       st
   and term_mul_expr_p st = (chainl1 term_factor_p bop_mul_p <??> "term_mul") st
   and term_add_expr_p st = (chainl1 term_mul_expr_p bop_add_p <??> "term_add") st
   and term_rel_expr_p st = (chainl1 term_add_expr_p bop_rel_p <??> "term_rel") st
   and term_eq_expr_p st = (chainl1 term_rel_expr_p bop_eq_p <??> "term_eq") st
   and term_and_expr_p st = (chainl1 term_eq_expr_p bop_and_p <??> "term_and") st
   and term_or_expr_p st = (chainl1 term_and_expr_p bop_or_p <??> "term_or") st in
   term_or_expr_p)
    st

and term_atom_p =
  fun st ->
  let term_singles_p =
    satisfy_map (function
      | TRUE -> Some (Bool true)
      | FALSE -> Some (Bool false)
      | ID v -> Some (Var v)
      | _ -> None)
    <??> "term_single"
  in
  (term_builtin_p
   <|> term_singles_p
   <|> term_number_p
   <|> term_let_p
   <|> term_if_p
   <|> term_lam_p
   <|> term_mat_p
   <|> term_vec_p
   <??> "term_atom")
    st

and term_p =
  fun st ->
  (let%bind t = term_atom_p <|> term_bop_p <|> between `Paren term_p in
   term_index_p t <|> term_app_p t <|> return t <??> "term")
    st
;;

(* TODO: Pretty print [Stlc.term] for nicer output / testing *)
(* TODO: Exhausive Term Tests *)
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
  test "variable_name";
  test "-13.4";
  test "33";
  test "false";
  test "<1, 2, 3>";
  test "<<1, 2>, <3, 4>, <5, 6>>";
  test "fun (x : bool) -> x";
  test "f x y";
  test "let bind = true in bind";
  test "if true then x else y";
  test "1 + 2 * 44 % 10 && true";
  test "v[0]";
  test "#min(1, 2)";
  [%expect
    {|
    (Ok (Var variable_name))
    (Ok (Float -13.4))
    (Ok (Int 33))
    (Ok (Bool false))
    (Ok (Vec 3 ((Int 1) (Int 2) (Int 3))))
    (Ok (Mat 3 2 ((Int 1) (Int 2) (Int 3) (Int 4) (Int 5) (Int 6))))
    (Ok (Lam x TyBool (Var x)))
    (Ok (App (App (Var f) (Var x)) (Var y)))
    (Ok (Let bind (Bool true) (Var bind)))
    (Ok (If (Bool true) (Var x) (Var y)))
    (Error ((chomp_error run_stream_not_fully_consumed) (contexts ())))
    (Ok (Index (Var v) 0))
    (Ok (Builtin Min ((Int 1) (Int 2))))
    |}];
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
  test "fun (x : bool) -> x";
  [%expect {| (Ok (Lam x TyBool (Var x))) |}]
;;

(* TODO: Parser + Tests for [Stlc.t] *)
