open Core

let counter = ref 0

let fresh name =
  let id = !counter in
  counter := id + 1;
  Printf.sprintf "%s_%d" name id
;;

let reset () = counter := 0
