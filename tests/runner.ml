open Core
open Glml_compiler

let test ?dump source =
  match compile ?dump source with
  | Error err -> print_endline (Compiler_error.to_string_hum ~source err)
  | Ok glsl ->
    print_endline glsl;
    (match Glsl_validator.validate_glsl glsl with
     | None -> ()
     | Some err -> print_endline ("\n\n>>> glslangValidator Error: " ^ err))
;;

let test_term s = test ("let main (coord : vec2) = " ^ s)

let%expect_test "Check glslangValidator status" =
  Glsl_validator.glslang_validator_exists ()
  |> Option.value ~default:"[glslValidator] is ready and will run on all tests!"
  |> String.append "STATUS: "
  |> print_endline;
  [%expect {| STATUS: [glslValidator] is ready and will run on all tests! |}]
;;
