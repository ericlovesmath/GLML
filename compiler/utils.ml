open Core

let no_mangle = [ "main" ]
let used_names : String.Hash_set.t = String.Hash_set.create ()
let counters : int String.Table.t = String.Table.create ()

(** GLSL ES 3.00 keywords and reserved words that cannot be used as identifiers *)
let glsl_keywords : String.Set.t =
  String.Set.of_list
    [ "attribute" ; "const" ; "uniform" ; "varying" ; "break" ; "continue" ; "do"
    ; "for" ; "while" ; "void" ; "bool" ; "int" ; "uint" ; "float" ; "bvec2" ; "bvec3"
    ; "bvec4" ; "ivec2" ; "ivec3" ; "ivec4" ; "uvec2" ; "uvec3" ; "uvec4" ; "vec2"
    ; "vec3" ; "vec4" ; "mat2" ; "mat3" ; "mat4" ; "mat2x2" ; "mat2x3" ; "mat2x4"
    ; "mat3x2" ; "mat3x3" ; "mat3x4" ; "mat4x2" ; "mat4x3" ; "mat4x4" ; "in" ; "out"
    ; "inout" ; "struct" ; "true" ; "false" ; "lowp" ; "mediump" ; "highp" ; "precision"
    ; "invariant" ; "discard" ; "return" ; "if" ; "else" ; "switch" ; "case" ; "default"
    ; "layout" ; "centroid" ; "flat" ; "smooth" ; "union" ; "enum" ; "typedef" ; "template"
    ; "this" ; "packed" ; "goto" ; "inline" ; "noinline" ; "volatile" ; "public" ; "static"
    ; "extern" ; "external" ; "interface" ; "long" ; "short" ; "double" ; "half" ; "fixed"
    ; "unsigned" ; "superp" ; "input" ; "output" ; "filter"
    ]
  [@ocamlformat "disable"]
;;

(** Patches [name] to be valid GLSL names.
    Replaces ['] and potential generation of [__] *)
let sanitize name =
  let name = String.substr_replace_all name ~pattern:"'" ~with_:"_prime" in
  let name = String.substr_replace_all name ~pattern:"__" ~with_:"_u" in
  if String.is_suffix name ~suffix:"_" then name ^ "x" else name
;;

let fresh name =
  if List.mem no_mangle name ~equal:String.equal
  then name
  else (
    let base = sanitize name in
    if not (Hash_set.mem used_names base || Set.mem glsl_keywords base)
    then (
      Hash_set.add used_names base;
      base)
    else (
      let n = Hashtbl.find_or_add counters base ~default:(Fn.const 0) in
      let rec go () =
        Hashtbl.set counters ~key:base ~data:(n + 1);
        let v = Printf.sprintf "%s_%d" base n in
        if Hash_set.mem used_names v
        then go ()
        else (
          Hash_set.add used_names v;
          v)
      in
      go ()))
;;

let reset () =
  Hash_set.clear used_names;
  Hashtbl.clear counters
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
    var
    var2
    var_0
    var_prime
    var_prime_0
    var_prime_prime
    var_prime_1
    |}];
  print_endline (fresh "foo_");
  print_endline (fresh "_");
  print_endline (fresh "__foo");
  print_endline (fresh "foo__bar");
  print_endline (fresh "_foo");
  [%expect
    {|
    foo_x
    _x
    _ufoo
    foo_ubar
    _foo
    |}];
  print_endline (fresh "union");
  print_endline (fresh "union");
  print_endline (fresh "input");
  print_endline (fresh "output");
  print_endline (fresh "float");
  [%expect
    {|
    union_0
    union_1
    input_0
    output_0
    float_0
    |}]
;;
