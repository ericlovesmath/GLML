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

let constructor_p =
  satisfy_map (function
    | CONSTRUCTOR s -> Some s
    | _ -> None)
  <??> "constructor"
;;

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
  let%bind n =
    satisfy_map (function
      | VEC n -> Some n
      | _ -> None)
  in
  return (TyVec n) <??> "ty_vec"
;;

let ty_mat_p =
  let%bind n, m =
    satisfy_map (function
      | MAT (n, m) -> Some (n, m)
      | _ -> None)
  in
  return (TyMat (n, m)) <??> "ty_mat"
;;

let ty_singles_p =
  satisfy_map (function
    | BOOL -> Some TyBool
    | INT -> Some TyInt
    | FLOAT -> Some TyFloat
    | ID s -> Some (TyName s)
    | TYVAR v -> Some (TyVar v)
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
    |> Compiler_error.bind ~f:(Chomp.run ~pass:"parser" ty_p)
    |> Compiler_error.to_or_error
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
  test "'a";
  test "record_or_variant";
  [%expect
    {|
    (Ok float)
    (Ok int)
    (Ok bool)
    (Ok (vec 2))
    (Ok (mat 3 2))
    (Ok (mat 3 3))
    (Ok (float -> int))
    (Ok 'a)
    (Ok record_or_variant)
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
    (Error ("parser: ((chomp_error satisfy_eof) (contexts (between ty)))" ()))
    (Error
     ( "parser: ((chomp_error satisfy_map_fail)\
      \n (contexts (\"between (1:1 - 1:2)\" \"ty (1:1 - 1:2)\")))" ()))
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
        let%bind ty = optional (tok COLON *> ty_p) in
        return (id, ty))
     <|> (ident_p >>| fun id -> id, None))
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

(* TODO: Hardcoded to maximum 1000 loops for now *)
let recur_p = tok REC *> return (Rec 1000) <|> return Nonrec <??> "recur"

let rec term_let_p =
  fun st ->
  (with_term_loc
     (tok LET
      *> commit
           (let%bind recur = recur_p in
            let%bind id = ident_p in
            let%bind params = many param_p in
            let%bind return_ty = optional (tok COLON *> ty_p) in
            let%bind rhs = tok EQ *> term_p in
            let rhs_desc = make_lambdas params rhs in
            let%bind body = tok IN *> term_p in
            return (Let (recur, id, return_ty, { desc = rhs_desc; loc = rhs.loc }, body)))
     )
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

(* TODO: All branches of math must be a constructor right now, which is limiting *)
and term_match_p =
  fun st ->
  (with_term_loc
     (tok MATCH
      *> commit
           (let%bind scrutinee = term_p in
            let%bind _ = tok WITH in
            let%bind cases =
              many1
                (let%bind _ = tok BAR in
                 let%bind ctor = constructor_p in
                 let%bind vars =
                   (* [Const (x, y)] or [Const x] or [Const] *)
                   between `Paren (commas ident_p)
                   <|> (ident_p >>| fun v -> [ v ])
                   <|> return []
                 in
                 let%bind _ = tok ARROW in
                 let%bind body = term_p in
                 return (ctor, vars, body))
            in
            return (Match (scrutinee, cases))))
   <??> "term_match")
    st

and term_mat_p =
  fun st ->
  (with_term_loc
     ((let%bind terms = between `Bracket (commas (between `Bracket (commas term_p))) in
       let n = List.length terms in
       let m = List.length (List.hd_exn terms) in
       if List.for_all terms ~f:(fun ts -> List.length ts = m)
       then return (Mat (n, m, List.concat terms))
       else fail "matrix contains rows of unequal size")
      <??> "term_mat"))
    st

and term_record_p =
  fun st ->
  (with_term_loc
     (let%map fields =
        between
          `Curly
          (commas
             (let%bind id = ident_p in
              let%bind _ = tok EQ in
              let%bind t = term_p in
              return (id, t)))
      in
      Record fields)
   <??> "term_record")
    st

and term_vec_p =
  fun st ->
  (with_term_loc
     (let%bind terms = between `Bracket (commas term_p) in
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

and term_variant_p =
  fun st ->
  (with_term_loc
     (let%bind ctor = constructor_p in
      let%bind args =
        between `Paren (commas term_p)
        <|> (term_atom_p <|> term_unsigned_number_p >>| fun t -> [ t ])
        <|> return []
      in
      return (Variant (ctor, args)))
   <??> "term_variant")
    st

and term_atom_p =
  fun st ->
  let term_singles_p =
    with_term_loc
      (satisfy_map (function
         | TRUE -> Some (Bool true)
         | FALSE -> Some (Bool false)
         | CONSTRUCTOR v -> Some (Variant (v, []))
         | ID v -> Some (Var v)
         | _ -> None)
       <??> "term_single")
  in
  (term_builtin_p
   <|> term_singles_p
   <|> term_mat_p
   <|> term_vec_p
   <|> term_record_p
   <??> "term_atom")
    st

and term_unary_neg_p =
  fun st ->
  (let%bind _, loc = with_loc (tok SUB) in
   commit
     (let%map (e : term) = term_postfix_p in
      let neg_one : term = { desc = Int (-1); loc } in
      let loc = Lexer.merge_loc loc e.loc in
      ({ desc = Bop (Mul, neg_one, e); loc } : term))
   <??> "term_unary_neg")
    st

and term_head_p =
  fun st ->
  (term_variant_p
   <|> term_atom_p
   <|> term_number_p
   <|> term_unary_neg_p
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
  let dot_op_p =
    let%bind _ = tok DOT in
    let%map res, loc =
      with_loc (ident_p >>| Either.first <|> (num_p >>| Either.second))
    in
    fun (t : term) : term ->
      let desc =
        match res with
        | First f -> Field (t, f)
        | Second i -> Index (t, i)
      in
      { desc; loc = Lexer.merge_loc t.loc loc }
  in
  let term_arg_p =
    (* NOTE: intentionally excludes signed literals to avoid cases like [f -5] *)
    term_atom_p <|> term_unsigned_number_p <|> between `Paren term_p <??> "term_arg"
  in
  let app_op_p =
    term_arg_p
    >>| fun (a : term) (t : term) ->
    ({ desc = App (t, a); loc = Lexer.merge_loc t.loc a.loc } : term)
  in
  let op_p = dot_op_p <|> app_op_p in
  (postfix_chain term_head_p op_p <??> "term_postfix_chain") st

and term_p =
  fun st ->
  (term_let_p
   <|> term_if_p
   <|> term_lam_p
   <|> term_match_p
   <|> List.fold_left bop_levels ~init:term_postfix_p ~f:chainl1
   <??> "term")
    st
;;

let test s =
  s
  |> Lexer.init
  |> Lexer.lex
  |> Compiler_error.bind ~f:(Chomp.run ~pass:"parser" term_p)
  |> Compiler_error.to_or_error
  |> Or_error.sexp_of_t sexp_of_term
  |> print_s
;;

let%expect_test "term parse tests" =
  test "variable_name";
  test "-13.4";
  test "33";
  test "false";
  test "[1, 2, 3]";
  test "[[1, 2], [3, 4], [5, 6]]";
  test "fun (x : bool) -> x";
  test "f x y";
  test "let bind = true in bind";
  test "let rec f (x : float) : bool = f x in f";
  test "if true then x else y";
  test "1 * 2 + true && 44 % 10";
  test "v.0";
  test "#min(1, 2)";
  test "#exp2(1.)";
  test "Constr";
  test "Constr x";
  test "Constr (x, 2.0)";
  test "match x with | Constr x -> a | Alt b -> b";
  [%expect
    {|
    (Ok variable_name)
    (Ok -13.4)
    (Ok 33)
    (Ok false)
    (Ok (vec3 1 2 3))
    (Ok (mat3x2 1 2 3 4 5 6))
    (Ok (lambda (x (bool)) x))
    (Ok (app (app f x) y))
    (Ok (let bind true bind))
    (Ok (let (rec 1000) f (: bool) (lambda (x (float)) (app f x)) f))
    (Ok (if true x y))
    (Ok (&& (+ (* 1 2) true) (% 44 10)))
    (Ok (index v 0))
    (Ok (min 1 2))
    (Ok (exp2 1.))
    (Ok (Variant Constr))
    (Ok (Variant Constr x))
    (Ok (Variant Constr x 2.))
    (Ok (match x (Constr (x) a) (Alt (b) b)))
    |}];
  test "-113.0";
  test "-113.";
  test "-113";
  test "f x y z";
  test "f (x y) z";
  test "2 * -13 - 10";
  test "f 5";
  test "fun (u : vec2) -> [ f 10.0 a , 0.0, 0.0 ]";
  test "f - 5";
  test "f (-5)";
  test "-var";
  test "- -x";
  test "-5";
  test "1 - -x";
  [%expect
    {|
    (Ok -113.)
    (Ok -113.)
    (Ok -113)
    (Ok (app (app (app f x) y) z))
    (Ok (app (app f (app x y)) z))
    (Ok (- (* 2 -13) 10))
    (Ok (app f 5))
    (Ok (lambda (u ((vec 2))) (vec3 (app (app f 10.) a) 0. 0.)))
    (Ok (- f 5))
    (Ok (app f -5))
    (Ok (* -1 var))
    (Ok (* -1 (* -1 x)))
    (Ok -5)
    (Ok (- 1 (* -1 x)))
    |}];
  test "let f (x : bool) (y : bool) = x && y in f";
  test "let f = fun (x : bool) (y : bool) -> x && y in f";
  test "let f x y = x && y in f";
  test "let rec f x = f x in f";
  test "let rec f (x : 'b) : 'a = f x in f";
  test "let f (x : float) : float = x in f";
  [%expect
    {|
    (Ok (let f (lambda (x (bool)) (lambda (y (bool)) (&& x y))) f))
    (Ok (let f (lambda (x (bool)) (lambda (y (bool)) (&& x y))) f))
    (Ok (let f (lambda (x ()) (lambda (y ()) (&& x y))) f))
    (Ok (let (rec 1000) f (lambda (x ()) (app f x)) f))
    (Ok (let (rec 1000) f (: 'a) (lambda (x ('b)) (app f x)) f))
    (Ok (let f (: float) (lambda (x (float)) x) f))
    |}]
;;

let%expect_test "regression test, sequential non-parenthesized terms" =
  test "let x = 1.0 in x + 1.0";
  test "1.0 + let x = 1.0 in x";
  test "1.0 + (let x = 1.0 in x)";
  test "f let x = 1.0 in x";
  test "f (let x = 1.0 in x)";
  test "[1.0, 2.0] let x = 1.0 in x";
  test "[1.0, let x = 2.0 in x]";
  test "if true then 1.0 else 2.0 + 3.0";
  [%expect
    {|
    (Ok (let x 1. (+ x 1.)))
    (Error
     ("parser: ((chomp_error run_stream_not_fully_consumed) (contexts ()))" ()))
    (Ok (+ 1. (let x 1. x)))
    (Error
     ("parser: ((chomp_error run_stream_not_fully_consumed) (contexts ()))" ()))
    (Ok (app f (let x 1. x)))
    (Error
     ("parser: ((chomp_error run_stream_not_fully_consumed) (contexts ()))" ()))
    (Ok (vec2 1. (let x 2. x)))
    (Ok (if true 1. (+ 2. 3.)))
    |}]
;;

let top_let_p =
  with_top_loc
    (tok LET
     *> commit
          (let%bind recur = recur_p in
           let%bind id = ident_p in
           let%bind params = many param_p in
           let%bind return_ty = optional (tok COLON *> ty_p) in
           let%bind rhs = tok EQ *> term_p in
           let rhs_desc = make_lambdas params rhs in
           return (Define (recur, id, return_ty, { desc = rhs_desc; loc = rhs.loc })))
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

let top_record_p =
  fun st ->
  (with_top_loc
     (let%bind _ = tok TYPE in
      let%bind id = ident_p in
      let%bind _ = tok EQ in
      let%bind fields =
        between
          `Curly
          (commas
             (let%bind f_id = ident_p in
              let%bind _ = tok COLON in
              let%bind f_ty = ty_p in
              return (f_id, f_ty)))
      in
      return (TypeDef (id, RecordDecl fields)))
   <??> "top_record")
    st
;;

let top_variant_p =
  with_top_loc
    (let%bind _ = tok TYPE in
     let%bind id = ident_p in
     let%bind _ = tok EQ in
     let%bind ctors =
       let ctor_p =
         let%bind ctor = constructor_p in
         let%bind args = tok OF *> sep_by1 (tok MUL) ty_p <|> return [] in
         return (ctor, args)
       in
       let%bind _ = optional (tok BAR) in
       sep_by1 (tok BAR) ctor_p
     in
     return (TypeDef (id, VariantDecl ctors)))
  <??> "top_variant"
;;

let glml_p =
  many1 (top_let_p <|> top_extern_p <|> top_record_p <|> top_variant_p)
  >>| fun tops -> Program tops
;;

let parse tokens = Chomp.run ~pass:"parser" glml_p tokens

let test s =
  s
  |> Lexer.init
  |> Lexer.lex
  |> Compiler_error.bind ~f:parse
  |> Compiler_error.to_or_error
  |> Or_error.sexp_of_t sexp_of_t
  |> print_s
;;

let%expect_test "glml parse tests" =
  test
    {|
    #extern float u_time

    type point = { x : float, y : int }

    type shape = Circle of int * float | Triangle

    let a_struct = { x = 0.0, y = 0 }
    let toplevel = 1 + 2
    let main = 1 + 2
    let f = fun (x : bool) (y : bool) -> x && y
    let main (u : vec2) = f [1, 2] + u
    let rec g (x : float) : float = g x
    |};
  [%expect
    {|
    (Ok
     (Program
      ((Extern float u_time) (TypeDef point (RecordDecl ((x float) (y int))))
       (TypeDef shape (VariantDecl ((Circle (int float)) (Triangle ()))))
       (Define Nonrec a_struct (record (x 0.) (y 0)))
       (Define Nonrec toplevel (+ 1 2)) (Define Nonrec main (+ 1 2))
       (Define Nonrec f (lambda (x (bool)) (lambda (y (bool)) (&& x y))))
       (Define Nonrec main (lambda (u ((vec 2))) (+ (app f (vec2 1 2)) u)))
       (Define (Rec 1000) g (: float) (lambda (x (float)) (app g x))))))
    |}]
;;

let%expect_test "regression test, toplevel let parsing after record" =
  test
    {|
    let f (x : float) =
      let g (y : float) = x + y in
      [ g 1.0, 0.0, 0.0 ]

    let main (u : vec2) = f 10.0
    |};
  [%expect
    {|
    (Ok
     (Program
      ((Define Nonrec f
        (lambda (x (float))
         (let g (lambda (y (float)) (+ x y)) (vec3 (app g 1.) 0. 0.))))
       (Define Nonrec main (lambda (u ((vec 2))) (app f 10.))))))
    |}]
;;
