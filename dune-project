(lang dune 3.17)

(name glml)

(generate_opam_files true)

(source
 (github glml-lang/GLML))

(authors "Eric Lee <me@ericchanlee.com>")

(maintainers "Eric Lee <me@ericchanlee.com>")

(license MIT)

(documentation https://www.glml-lang.com/)

(package
 (name glml)
 (synopsis "Functional DSL targetting GLSL")
 (description "Functional DSL targetting GLSL")
 (depends
   ocaml
   js_of_ocaml
   js_of_ocaml-ppx
   core
   topological_sort
   core_bench
   ppx_jane)
 (tags ("graphics" "shader" "DSL" "GLSL" "GLML" "compiler" "playground")))
