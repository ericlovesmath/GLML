open Core
open Glml_compiler

let examples_dir = "../examples"

let%expect_test "compile examples" =
  let glml_files = Stdlib.Sys.readdir examples_dir in
  Array.iter glml_files ~f:(fun file ->
    let content = In_channel.read_all (Filename.concat examples_dir file) in
    match compile content with
    | Ok _ -> print_s [%message "pass" (file : string)]
    | Error err -> print_s [%message "fail" (file : string) (err : Error.t)]);
  [%expect
    {|
    (pass (file checkerboard.glml))
    (pass (file mouse_circle.glml))
    (pass (file rainbow.glml))
    |}]
;;
