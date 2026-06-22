open Core
open Core_bench

(* Benchmark compiling each example shader, with and without the optimizer *)

let examples_dir = "../examples"

let test ~name ~optimize source =
  Bench.Test.create ~name (fun () -> Glml_compiler.compile ~optimize source)
;;

let () =
  let files = Stdlib.Sys.readdir examples_dir in
  Array.sort files ~compare:String.compare;
  let tests =
    Array.to_list files
    |> List.concat_map ~f:(fun file ->
      let source = In_channel.read_all (Filename.concat examples_dir file) in
      [ test ~name:(file ^ " [opt]") ~optimize:true source
      ; test ~name:(file ^ " [noopt]") ~optimize:false source
      ])
  in
  Command_unix.run (Bench.make_command tests)
;;
