open Core

let compile _ = "TODO: hook up compiler"

let compile_command =
  Command.basic
    ~summary:"Compile the source language to GLSL"
    ~readme:(fun () ->
      "This command takes a source file and outputs optimized GLSL code.")
    (let open Command.Let_syntax in
     let%map_open input_file = anon ("filename" %: string)
     and output_file =
       flag "-o" (optional string) ~doc:"FILE output file path (default: stdout)"
     in
     fun () ->
       let result = compile input_file in
       match output_file with
       | Some path -> Out_channel.write_all path ~data:result
       | None -> print_endline result)
;;

let list_passes_command =
  Command.basic
    ~summary:"List all available passes"
    (let open Command.Let_syntax in
     return (fun () -> print_endline "TODO: list passes here"))
;;

let main_command =
  Command.group
    ~summary:"GLML: A functional compiler targetting GLSL"
    [ "compile", compile_command; "list-passes", list_passes_command ]
;;

let () = Command_unix.run main_command
