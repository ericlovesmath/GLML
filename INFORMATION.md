# Compilation Passes (Currently Working On)

- Button to show GLSL output
- Lambda lifting
- Use ternary to replace some `if`'s in GLSL
- Read Futhark's monomorphization and defunctionalization code
- Monomorphization (Specialization)

# Remaining Compilation Passes

- Typechecking (Church-typed LC -> HM -> Size dependent)
- Closure Conversion (turn closures into explicit struct passing)
- Lambda Lifting (Higher order functions into toplevel)
- Lowering (ADTs/matches into tagged unions/switches)
- Recursion Elimination (Convert tailcalls into while loops)
- Typeclasses for polymorphic functions
- Swizzle syntax
- Function `inlining` / `specialize`
- Dead code elimination
- Constant folding/propagation
- Sourcemaps?

Interesting Ideas

- `let%glsl` ppx to embed DSL?
- `wasm_of_ocaml`, but would have to give up on `extern js`
- Remove Jane Street dependency for `bonsai` and just use `js_of_ocaml` with a javascript framework

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
