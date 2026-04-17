open Core

let glslang_validator_exists =
  let cached =
    lazy
      (match Core_unix.system "glslangValidator --version >/dev/null 2>&1" with
       | Ok () -> None
       | Error _ ->
         Some "[glslangValidator --version] failed to run, validator will not be run!")
  in
  fun () -> Lazy.force cached
;;

let validate_glsl shader_source =
  if Option.is_some (glslang_validator_exists ())
  then None
  else (
    let path, fd = Core_unix.mkstemp "shader_frag" in
    Core_unix.close fd;
    Exn.protect
      ~f:(fun () ->
        Out_channel.write_all path ~data:shader_source;
        let cmd = sprintf "glslangValidator -S frag %s 2>&1" path in
        let ic = Core_unix.open_process_in cmd in
        let output = In_channel.input_all ic in
        match Core_unix.close_process_in ic with
        | Ok () -> None
        | Error _ ->
          output
          |> String.strip
          |> String.substr_replace_all ~pattern:path ~with_:"<input>"
          |> Option.some)
      ~finally:(fun () -> Core_unix.unlink path))
;;
