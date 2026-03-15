Currently a mess, will organize later (if ever)

# Compilation Passes (Currently Working On)

- Button to show GLSL output on mobile
- Use ternary to replace some `if`'s in GLSL
- Read Futhark's monomorphization and defunctionalization code for partial application
- Vim/Emacs syntax file
- Better benchmarking tests
- Better pipeline tests
- Defunctionalization?
- Implicit error field added to every function to propagate error color back
- Move mod to nonbinop version as that is a gentype, add more builtins
- Allow clicking through all passes in GLSL output window
- main is mangled so make sure that doesn't happen, must be fixed to coord vec2
- add types to monomorphorphization stage
- Add builtin GLSL function callers or GLSL extern libraries
- Add user definable constrained functions
- Add support for LSP hover or something like an [inspect]

# Remaining Compilation Passes

- Typechecking (Church-typed LC -> HM -> Size dependent)
- Closure Conversion (turn closures into explicit struct passing)
- Lowering (ADTs/matches into tagged unions/switches)
- Typeclasses for polymorphic functions
- Swizzle syntax or some kind of rank polymorphism
- Function `inlining` / `specialize` (but likely everything is specialized)
- Dead code elimination
- Constant folding/propagation
- Doc strings

Interesting Ideas

- `let%glsl` ppx to embed DSL?
- `wasm_of_ocaml` build?
- Local renderer without web

# Idea Dump

- Do I separate camera and make it specific like a 3D ShaderToy for Raymarching in the web playground?
- Write Nix derivation for Javascript and OCaml bindings
- Emit on compilation what data needs to be passed from host

# Resources

- Janet to GLSL Compiler: https://ianthehenry.com/posts/bauble/building-bauble/
- Articles on SDFs: https://iquilezles.org/articles/
- Size-Dependent Types: https://futhark-lang.org/publications/array21.pdf
- Futhark In-place Records: https://futhark-lang.org/blog/2017-03-06-futhark-record-system.html
- Futhark Size-Dependent Types: https://www.di.ens.fr/~pouzet/bib/array23.pdf
- Writing Nix Derivations: https://github.com/justinwoo/nix-shorts/blob/master/posts/your-first-derivation.md
