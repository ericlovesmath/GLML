open Core

let no_mangle = [ "main" ]
let counter = ref 0

let fresh name =
  if List.mem no_mangle name ~equal:String.equal
  then name
  else (
    let id = !counter in
    counter := id + 1;
    Printf.sprintf "%s_%d" name id)
;;

let reset () = counter := 0
