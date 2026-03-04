open Core
open Stlc
open Lexer
open Chomp
open Chomp.Let_syntax
open Chomp.Infix_syntax

let with_term_loc p = with_loc p >>| fun (desc, loc) -> ({ desc; loc } : term)
let with_top_loc p = with_loc p >>| fun (desc, loc) -> ({ desc; loc } : top)

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
    |> Lexer.init
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
    (Ok float)
    (Ok int)
    (Ok bool)
    (Ok (vec 2))
    (Ok (mat 3 2))
    (Ok (mat 3 3))
    (Ok (float -> int))
    |}];
  test "(vec4)";
  test "(mat3x2->vec2)->(vec2->int)";
  [%expect
    {|
    (Ok (vec 4))
    (Ok (((mat 3 2) -> (vec 2)) -> ((vec 2) -> int)))
    |}];
  test "";
  test "()";
  [%expect
    {|
    (Error ((chomp_error satisfy_eof) (contexts (between ty))))
    (Error
     ((chomp_error "satisfy_fail on <RPAREN> (1:2 - 1:3)")
      (contexts ("between (1:1 - 1:2)" "ty (1:1 - 1:2)"))))
    |}]
;;

let float_p =
  satisfy_map (function
    | FLOAT_LIT f -> Some f
    | _ -> None)
  <??> "float"
;;

let term_number_p =
  with_term_loc
    (let%bind sign = tok SUB *> return (-1) <|> tok ADD *> return 1 <|> return 1 in
     (let%bind f = float_p in
      return (Float (Float.of_int sign *. f)) <??> "term_float")
     <|>
     let%bind n = num_p in
     return (Int (sign * n)) <??> "term_int")
;;

let term_unsigned_number_p =
  with_term_loc
    ((let%bind f = float_p in
      return (Float f) <??> "term_ufloat")
     <|>
     let%bind n = num_p in
     return (Int n) <??> "term_uint")
;;

let bop_levels =
  let bop t op =
    tok t
    *> return (fun (l : term) (r : term) ->
      ({ desc = Bop (op, l, r); loc = Lexer.merge_loc l.loc r.loc } : term))
  in
  [ bop MUL Mul <|> bop DIV Div <|> bop PERCENT Mod
  ; bop ADD Add <|> bop SUB Sub
  ; bop LANGLE Lt <|> bop RANGLE Gt <|> bop LEQ Leq <|> bop GEQ Geq
  ; bop EQ Eq
  ; bop LAND And
  ; bop LOR Or
  ]
;;

let param_p =
  with_loc
    (between
       `Paren
       (let%bind id = ident_p in
        let%bind ty = tok COLON *> ty_p in
        return (id, ty)))
  <??> "param"
;;

let make_lambdas params (body : term) =
  match params with
  | [] -> body.desc
  | _ ->
    let body_term, _ =
      List.fold_right
        params
        ~init:(body, body.loc)
        ~f:(fun ((id, ty), param_loc) (acc_term, acc_loc) ->
          let combined_loc = Lexer.merge_loc param_loc acc_loc in
          let lam_term : term = { desc = Lam (id, ty, acc_term); loc = combined_loc } in
          lam_term, combined_loc)
    in
    body_term.desc
;;

let rec term_let_p =
  fun st ->
  (with_term_loc
     (tok LET
      *> commit
           (let%bind id = ident_p in
            let%bind params = many param_p in
            let%bind rhs = tok EQ *> term_p in
            let rhs_desc = make_lambdas params rhs in
            let%bind body = tok IN *> term_p in
            return (Let (id, { desc = rhs_desc; loc = rhs.loc }, body))))
   <??> "term_let")
    st

and term_if_p =
  fun st ->
  (with_term_loc
     (tok IF
      *> commit
           (let%bind c = term_p in
            let%bind t = tok THEN *> term_p in
            let%bind f = tok ELSE *> term_p in
            return (If (c, t, f))))
   <??> "term_if")
    st

and term_lam_p =
  fun st ->
  (with_term_loc
     (tok FUN
      *> commit
           (let%bind params = many1 param_p in
            let%bind t = tok ARROW *> term_p in
            return (make_lambdas params t))))
    st

and term_mat_p =
  fun st ->
  (with_term_loc
     ((let%bind terms = between `Angle (commas (between `Angle (commas term_p))) in
       let n = List.length terms in
       let m = List.length (List.hd_exn terms) in
       if List.for_all terms ~f:(fun ts -> List.length ts = m)
       then return (Mat (n, m, List.concat terms))
       else fail "matrix contains rows of unequal size")
      <??> "term_mat"))
    st

and term_vec_p =
  fun st ->
  (with_term_loc
     (let%bind terms = between `Angle (commas term_p) in
      return (Vec (List.length terms, terms)) <??> "term_vec"))
    st

(* NOTE: Builtins should not be special forms, but right now they are not curried so.. *)
and term_builtin_p =
  fun st ->
  (with_term_loc
     (let%bind _ = tok HASH in
      let%bind builtin =
        satisfy_map (function
          | ID s -> Option.try_with (fun () -> Glsl.builtin_of_string s)
          | _ -> None)
      in
      let%bind args = between `Paren (commas term_p) in
      return (Builtin (builtin, args)) <??> "term_builtin"))
    st

and term_atom_p =
  fun st ->
  let term_singles_p =
    with_term_loc
      (satisfy_map (function
         | TRUE -> Some (Bool true)
         | FALSE -> Some (Bool false)
         | ID v -> Some (Var v)
         | _ -> None)
       <??> "term_single")
  in
  (term_builtin_p <|> term_singles_p <|> term_mat_p <|> term_vec_p <??> "term_atom") st

and term_head_p =
  fun st ->
  (term_let_p
   <|> term_if_p
   <|> term_lam_p
   <|> term_atom_p
   <|> term_number_p
   <|> between `Paren term_p
   <??> "term_head")
    st

and term_postfix_p =
  fun st ->
  let postfix_chain head_p op_p =
    let%bind head = head_p in
    let%bind ops = many op_p in
    return (List.fold_left ops ~init:head ~f:(fun t op -> op t))
  in
  let index_op_p =
    with_loc (between `Bracket num_p)
    >>| fun (i, loc) (t : term) ->
    ({ desc = Index (t, i); loc = Lexer.merge_loc t.loc loc } : term)
  in
  let term_arg_p =
    (* NOTE: intentionally excludes signed literals to avoid cases like [f -5] *)
    let first_arg = term_atom_p <|> term_unsigned_number_p <|> between `Paren term_p in
    postfix_chain first_arg index_op_p <??> "term_arg"
  in
  let app_op_p =
    term_arg_p
    >>| fun (a : term) (t : term) ->
    ({ desc = App (t, a); loc = Lexer.merge_loc t.loc a.loc } : term)
  in
  let op_p = index_op_p <|> app_op_p in
  (postfix_chain term_head_p op_p <??> "term_postfix_chain") st

and term_p =
  fun st -> (List.fold_left bop_levels ~init:term_postfix_p ~f:chainl1 <??> "term") st
;;

let%expect_test "term parse tests" =
  let test s =
    s
    |> Lexer.init
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
  test "1 * 2 + true && 44 % 10";
  test "v[0]";
  test "#min(1, 2)";
  [%expect
    {|
    (Ok variable_name)
    (Ok -13.4)
    (Ok 33)
    (Ok false)
    (Ok (vec3 1 2 3))
    (Ok (mat3x2 1 2 3 4 5 6))
    (Ok (lambda (x bool) x))
    (Ok (app (app f x) y))
    (Ok (let bind true bind))
    (Ok (if true x y))
    (Ok (&& (+ (* 1 2) true) (% 44 10)))
    (Ok (index v 0))
    (Ok (min 1 2))
    |}];
  test "-113.0";
  test "-113";
  test "f x y z";
  test "f (x y) z";
  test "2 * -13 - 10";
  test "f 5";
  test "fun (u : vec2) -> < f 10.0 a , 0.0, 0.0 >";
  test "f - 5";
  test "f (-5)";
  [%expect
    {|
    (Ok -113.)
    (Ok -113)
    (Ok (app (app (app f x) y) z))
    (Ok (app (app f (app x y)) z))
    (Ok (- (* 2 -13) 10))
    (Ok (app f 5))
    (Ok (lambda (u (vec 2)) (vec3 (app (app f 10.) a) 0. 0.)))
    (Ok (- f 5))
    (Ok (app f -5))
    |}];
  test "let f (x : bool) (y : bool) = x && y in f";
  test "let f = fun (x : bool) (y : bool) -> x && y in f";
  [%expect
    {|
    (Ok (let f (lambda (x bool) (lambda (y bool) (&& x y))) f))
    (Ok (let f (lambda (x bool) (lambda (y bool) (&& x y))) f))
    |}]
;;

let top_let_p =
  with_top_loc
    (tok LET
     *> commit
          (let%bind id = ident_p in
           let%bind params = many param_p in
           let%bind rhs = tok EQ *> term_p in
           let rhs_desc = make_lambdas params rhs in
           return (Define (id, { desc = rhs_desc; loc = rhs.loc })))
     <??> "top_let")
;;

let top_extern_p =
  with_top_loc
    (tok HASH
     *> commit
          (let%bind _ = tok EXTERN in
           let%bind ty = ty_p in
           let%bind v = ident_p in
           return (Extern (ty, v)))
     <??> "top_extern")
;;

let glml_p = many1 (top_let_p <|> top_extern_p) >>| fun tops -> Program tops

let%expect_test "glml parse tests" =
  let test s =
    s
    |> Lexer.init
    |> Lexer.lex
    |> Or_error.map ~f:(run glml_p)
    |> Or_error.join
    |> Or_error.sexp_of_t sexp_of_t
    |> print_s
  in
  test
    {|
    #extern float u_time
    let toplevel = 1 + 2
    let main = 1 + 2
    let f = fun (x : bool) (y : bool) -> x && y
    let main (u : vec2) = f <1, 2> + u
    |};
  [%expect
    {|
    (Ok
     (Program
      ((Extern float u_time) (Define toplevel (+ 1 2)) (Define main (+ 1 2))
       (Define f (lambda (x bool) (lambda (y bool) (&& x y))))
       (Define main (lambda (u (vec 2)) (+ (app f (vec2 1 2)) u))))))
    |}]
;;
