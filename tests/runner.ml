open Core
open Glml_compiler

let test ?(dump : Passes.t list = []) source =
  let dump =
    List.map dump ~f:(fun p ->
      ( p
      , fun s ->
          printf "\n===== %s =====\n" (Passes.to_string p);
          print_s s;
          print_endline "" ))
    |> Passes.Map.of_alist_exn
  in
  match compile ~dump source with
  | Error err -> print_endline (Compiler_error.to_string_hum ~source err)
  | Ok glsl ->
    print_endline glsl;
    (match Glsl_validator.validate_glsl glsl with
     | None -> ()
     | Some err -> print_endline ("\n\n>>> glslangValidator Error: " ^ err))
;;

let test_term ?(dump : Passes.t list = []) s =
  test ~dump ("let main (coord : vec2) = let c = (" ^ s ^ ") in [c.0, c.1, c.2, 1.0]")
;;

let%expect_test "Check glslangValidator status" =
  Glsl_validator.glslang_validator_exists ()
  |> Option.value ~default:"[glslValidator] is ready and will run on all tests!"
  |> String.append "STATUS: "
  |> print_endline;
  [%expect {| STATUS: [glslValidator] is ready and will run on all tests! |}]
;;
