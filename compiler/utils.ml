open Core

let no_mangle = [ "main" ]
let counter = ref 0
let used_names = String.Hash_set.create ()

(** Patches [name] to be valid GLSL names *)
let sanitize name = String.substr_replace_all name ~pattern:"'" ~with_:"_prime"

let fresh name =
  if List.mem no_mangle name ~equal:String.equal
  then name
  else (
    let name = sanitize name in
    let rec gen_sym () =
      let v =
        let id = !counter in
        counter := id + 1;
        Printf.sprintf "%s_%d" name id
      in
      if Hash_set.mem used_names v then gen_sym () else v
    in
    let v = gen_sym () in
    Hash_set.add used_names v;
    v)
;;

let reset () =
  Hash_set.clear used_names;
  counter := 0
;;

let%expect_test "sanitizing and fresh names" =
  reset ();
  print_endline (fresh "var");
  print_endline (fresh "var2");
  print_endline (fresh "var");
  print_endline (fresh "var'");
  print_endline (fresh "var'");
  print_endline (fresh "var''");
  print_endline (fresh "var'");
  [%expect
    {|
    var_0
    var2_1
    var_2
    var_prime_3
    var_prime_4
    var_prime_prime_5
    var_prime_6
    |}]
;;
