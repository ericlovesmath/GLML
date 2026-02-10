open Core
module Glsl = Glsl
module Stlc = Stlc
module Uniquify = Uniquify
module Typecheck = Typecheck
module Translate = Translate

let compile_source src = Glsl.to_shader (Glsl.of_string src)

(* TODO: Move this to compile_source *)
let compile_stlc (t : Stlc.t) : string Or_error.t =
  let open Or_error.Let_syntax in
  let t = Uniquify.uniquify t in
  let%bind ctx = Typecheck.typecheck t in
  let%bind glsl = Translate.translate ctx t in
  return (Glsl.to_shader glsl)
;;

(* TODO: Potentially ephemeral tests, does not pass *)
let%expect_test "simple tests for compile_stlc" =
  let test s =
    s
    |> Stlc.of_string
    |> compile_stlc
    |> Or_error.sexp_of_t String.sexp_of_t
    |> Sexp.to_string_hum
    |> print_endline
  in
  test "((fun x : float -> (+ x 1.0)) 10.0)";
  [%expect "hi"]
;;
