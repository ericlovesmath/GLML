open Core
open Stlc
open Lexer
open Chomp
open Chomp.Let_syntax
open Chomp.Infix_syntax

(* TODO: write a proper sexp_of for these types *)
let rec strip_desc_tag =
  let open Sexplib.Sexp in
  function
  | List [ List [ Atom "desc"; desc ] ] -> strip_desc_tag desc
  | List xs -> List (List.map xs ~f:strip_desc_tag)
  | Atom a -> Atom a
;;

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
     ((chomp_error "satisfy_fail on <RPAREN> (1:2 - 1:3)")
      (contexts ("between (1:1 - 1:2)" "ty (1:1 - 1:2)"))))
    |}]
;;

(* TODO: I want to allow [0.] to be a float, but [0. 5.] is parsed as
   [0.5 .] right now. Maybe I should just make float literals be part
   of the lexer. *)
let term_number_p =
  with_term_loc
    (let%bind sign = tok SUB *> return (-1) <|> tok ADD *> return 1 <|> return 1 in
     let%bind n = num_p in
     (let%bind m = tok DOT *> num_p in
      let unsigned_float = Float.of_string [%string "%{n#Int}.%{m#Int}"] in
      let float = Float.of_int sign *. unsigned_float in
      return (Float float) <??> "term_float")
     <|> (return (Int (sign * n)) <??> "term_int"))
;;

let term_unsigned_number_p =
  with_term_loc
    (let%bind n = num_p in
     (let%bind m = tok DOT *> num_p in
      let f = Float.of_string [%string "%{n#Int}.%{m#Int}"] in
      return (Float f) <??> "term_ufloat")
     <|> (return (Int n) <??> "term_uint"))
;;

let bop_levels =
  let bop t op =
    tok t
    *> return (fun (l : term) (r : term) ->
      ({ desc = Bop (op, l, r); loc = fst l.loc, snd r.loc } : term))
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
          let combined_loc = fst param_loc, snd acc_loc in
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
    ({ desc = Index (t, i); loc = fst t.loc, snd loc } : term)
  in
  let term_arg_p =
    (* NOTE: intentionally excludes signed literals to avoid cases like [f -5] *)
    let first_arg = term_atom_p <|> term_unsigned_number_p <|> between `Paren term_p in
    postfix_chain first_arg index_op_p <??> "term_arg"
  in
  let app_op_p =
    term_arg_p
    >>| fun (a : term) (t : term) ->
    ({ desc = App (t, a); loc = fst t.loc, snd a.loc } : term)
  in
  let op_p = index_op_p <|> app_op_p in
  (postfix_chain term_head_p op_p <??> "term_postfix_chain") st

and term_p =
  fun st -> (List.fold_left bop_levels ~init:term_postfix_p ~f:chainl1 <??> "term") st
;;

(* TODO: Pretty print [Stlc.term] for nicer output / testing *)
let%expect_test "term parse tests" =
  let test s =
    s
    |> Lexer.of_string
    |> Lexer.lex
    |> Or_error.map ~f:(run term_p)
    |> Or_error.join
    |> Or_error.sexp_of_t sexp_of_term
    |> strip_desc_tag
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
    (Ok
     (Bop And (Bop Add (Bop Mul (Int 1) (Int 2)) (Bool true))
      (Bop Mod (Int 44) (Int 10))))
    (Ok (Index (Var v) 0))
    (Ok (Builtin Min ((Int 1) (Int 2))))
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
    (Ok (Float -113))
    (Ok (Int -113))
    (Ok (App (App (App (Var f) (Var x)) (Var y)) (Var z)))
    (Ok (App (App (Var f) (App (Var x) (Var y))) (Var z)))
    (Ok (Bop Sub (Bop Mul (Int 2) (Int -13)) (Int 10)))
    (Ok (App (Var f) (Int 5)))
    (Ok
     (Lam u (TyVec 2)
      (Vec 3 ((App (App (Var f) (Float 10)) (Var a)) (Float 0) (Float 0)))))
    (Ok (Bop Sub (Var f) (Int 5)))
    (Ok (App (Var f) (Int -5)))
    |}];
  test "let f (x : bool) (y : bool) = x && y in f";
  test "let f = fun (x : bool) (y : bool) -> x && y in f";
  [%expect
    {|
    (Ok (Let f (Lam x TyBool (Lam y TyBool (Bop And (Var x) (Var y)))) (Var f)))
    (Ok (Let f (Lam x TyBool (Lam y TyBool (Bop And (Var x) (Var y)))) (Var f)))
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
    |> Lexer.of_string
    |> Lexer.lex
    |> Or_error.map ~f:(run glml_p)
    |> Or_error.join
    |> Or_error.sexp_of_t sexp_of_t
    |> strip_desc_tag
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
      ((Extern TyFloat u_time) (Define toplevel (Bop Add (Int 1) (Int 2)))
       (Define main (Bop Add (Int 1) (Int 2)))
       (Define f (Lam x TyBool (Lam y TyBool (Bop And (Var x) (Var y)))))
       (Define main
        (Lam u (TyVec 2)
         (Bop Add (App (Var f) (Vec 2 ((Int 1) (Int 2)))) (Var u)))))))
    |}]
;;
