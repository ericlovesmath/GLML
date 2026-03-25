open Core
module Glml = Glml_compiler

let compile_command =
  Command.basic
    ~summary:"Compile the source language to GLSL"
    ~readme:(fun () ->
      "This command takes a source file and outputs optimized GLSL code.")
    (let open Command.Let_syntax in
     let%map_open input_file = anon ("filename" %: string)
     and output_file =
       flag "-o" (optional string) ~doc:"FILE output file path (default: stdout)"
     and dump_passes =
       flag "-p" (listed string) ~doc:"PASSES name of pass to dump or 'all'"
     and dump_dir =
       flag "-d" (optional string) ~doc:"DIR directory to dump pass files into"
     in
     fun () ->
       let handler pass sexp =
         let pass = Glml.Passes.to_string pass in
         let data = Sexp.to_string_hum sexp in
         match dump_dir with
         | None -> Printf.printf "=== %s ===\n%s\n\n" pass data
         | Some dir ->
           if not (Sys_unix.is_directory_exn dir) then Core_unix.mkdir_p dir;
           let filename = sprintf "%s/%s.sexp" dir pass in
           Printf.printf "Dumping %s\n" filename;
           Out_channel.write_all filename ~data
       in
       let dump =
         let passes =
           if List.mem dump_passes "all" ~equal:String.equal
           then Glml.Passes.all
           else List.map ~f:Glml.Passes.of_string dump_passes
         in
         passes
         |> List.map ~f:(fun pass -> pass, handler pass)
         |> Glml.Passes.Map.of_alist_exn
       in
       let result =
         input_file
         |> In_channel.read_all
         |> Glml.compile ~dump
         |> Glml.Compiler_error.ok_exn
       in
       match output_file with
       | Some path -> Out_channel.write_all path ~data:result
       | None -> print_endline result)
;;

let list_passes_command =
  Command.basic
    ~summary:"List all available passes"
    (Command.Param.return (fun () ->
       Glml.Passes.all
       |> List.map ~f:Glml.Passes.to_string
       |> String.concat ~sep:", "
       |> print_endline))
;;

let main_command =
  Command.group
    ~summary:"GLML: A functional compiler targetting GLSL"
    [ "compile", compile_command; "list-passes", list_passes_command ]
;;

let () = Command_unix.run main_command
