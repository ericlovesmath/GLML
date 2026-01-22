(lang dune 3.17)

(name GLML)

(generate_opam_files true)

(source
 (github ericlovesmath/GLML))

(authors "Eric Lee <me@ericchanlee.com>")

(maintainers "Eric Lee <me@ericchanlee.com>")

(license MIT)

(documentation https://github.com/ericlovesmath/GLML)

(package
 (name GLML)
 (synopsis "Functional DSL targetting GLSL")
 (description "Functional DSL targetting GLSL")
 (depends ocaml)
 (tags ("graphics" "shader" "DSL" "GLSL" "GLML" "compiler" "playground")))
