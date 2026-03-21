# Information Dump

This page contains a dump of current TODO items and unorganized thoughts and tasks. This will likely stay a mess.

## Tasks (Currently Working On)

- Button to show GLSL output on mobile
- Allow clicking through all passes in GLSL output window
- Defunctionalization?
- BUG: main is mangled so make sure that doesn't happen, must be fixed to coord vec2
- Add builtin GLSL function callers or GLSL extern libraries
- Add user definable constrained functions?
- Add mdbook and split off [play.glml-lang.com]
- Copy pasting doesn't seem to work in vim mode, maybe its the ctrl+enter runner?
- Vim mode some keys are wrong, like enter
- Syntax highlighting for mdbook
- uintBitsToFloat instead of fat structs to represent variants, instead storing as uvec4 in raw bits
- Add match support for non variants, as well as fallthrough cases and `when` clauses

## Remaining Tasks

- Implicit error field added to every function to propagate error color back
- Add support for LSP hover or something like an [inspect]
- Typechecking (Church-typed LC -> HM -> Size dependent)
- Closure Conversion (turn closures into explicit struct passing)
- Lowering (ADTs/matches into tagged unions/switches)
- Swizzle syntax or some kind of rank polymorphism
- Function `inlining` / `specialize` (but likely everything is specialized)
- Dead code elimination
- Constant folding/propagation (Sparse conditional constant propagation)
- Doc strings
- Read Futhark's monomorphization and defunctionalization code for partial application
- Better benchmarking tests
- Better pipeline tests
- Buffer passing (e.g. for Game of Life)
- Example of rendering clouds
- Add sliders in playground to change values
- Update blog posts
- Set up vim/treesitter plugsin
- Set up Github org more
- Vim/Emacs syntax file
- Add language reference and examples to mdbook

## Potentially Interesting Thoughts

- `wasm_of_ocaml` build? `Core` seems to cause `Error: Base_am_testing not implemented`
- Local renderer without web / tests with [glslViewer](https://github.com/patriciogonzalezvivo/glslViewer)
- Do I separate camera and make it specific like a 3D ShaderToy for Raymarching in the web playground?
- Write Nix derivation for Javascript and OCaml bindings
- Emit on compilation what data needs to be passed from host

## Resources

- Janet to GLSL Compiler: https://ianthehenry.com/posts/bauble/building-bauble/
- Articles on SDFs: https://iquilezles.org/articles/
- Size-Dependent Types: https://futhark-lang.org/publications/array21.pdf
- Futhark In-place Records: https://futhark-lang.org/blog/2017-03-06-futhark-record-system.html
- Futhark Size-Dependent Types: https://www.di.ens.fr/~pouzet/bib/array23.pdf
- Writing Nix Derivations: https://github.com/justinwoo/nix-shorts/blob/master/posts/your-first-derivation.md
- Typing Haskell in Haskell: [https://web.cecs.pdx.edu/~mpj/thih/thih.pdf]
- Type Inference with Constrained Types: [https://www.cs.tufts.edu/~nr/cs257/archive/martin-odersky/hmx.pdf]
- Demystifying Typeclasses: [https://okmij.org/ftp/Computation/typeclass.html]
