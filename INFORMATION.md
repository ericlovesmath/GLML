# Compilation Passes (Currently Working On)

- Button to show GLSL output
- Use ternary to replace some `if`'s in GLSL
- Read Futhark's monomorphization and defunctionalization code
- Monomorphization (Specialization)
- Vim syntax file
- Better pipeline tests
- Better benchmarking tests
- Defunctionalization?
- Implicit error field added to every function to propagate error color back
- Move mod to nonbinop version as that is a gentype, add more builtins
- Rewrite examples to not have to explicitly type everything

# Remaining Compilation Passes

- Typechecking (Church-typed LC -> HM -> Size dependent)
- Closure Conversion (turn closures into explicit struct passing)
- Lowering (ADTs/matches into tagged unions/switches)
- Typeclasses for polymorphic functions

    - TCO must have a hard capped loop so the browser doesn't just casually crash

- Swizzle syntax or some kind of rank polymorphism
- Function `inlining` / `specialize` (but likely everything is specialized)
- Dead code elimination
- Constant folding/propagation
- Doc strings

Interesting Ideas

- `let%glsl` ppx to embed DSL?
- `wasm_of_ocaml`, but would have to give up on `extern js`
- Remove Jane Street dependency for `bonsai` and just use `js_of_ocaml` with a javascript framework
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
