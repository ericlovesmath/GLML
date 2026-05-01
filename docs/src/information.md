# Information Dump

This page contains a dump of current TODO items and unorganized thoughts and tasks. This will likely stay a mess.

## Todo Tasks / Bugs

- Shorten names of function outputs on monomorphization with long types
- Allow binary operations and pipes to be used as curried functions
- Nested destructing, removing mat pattern case
- User defined constraints for broadcasting/others, eg: `let f (x : a) (y : b) : c where (a b +> c) = x + y`
- Lava lamp-like example
- Example storing functions and hotswapping functions during executions to justify DFns in variants
- Sanity check that mats are truly vec of vecs!
- More Demos: New examples with HOFs, IQ Palette, Beaver Logo
- Type annotations for arbitrary terms
- Promotion with ints/floats (true coercion enforced rather than constraint)
- Type aliases with parameters
- Curried builtin functions for partial application
- Auto lift `f : float -> float` to work over vecs?
- Update GLML screenshot
- Add builtin GLSL function callers or GLSL extern libraries
- `uintBitsToFloat` instead of fat structs to represent variants, instead storing as uvec4 in raw bits
- Reuse fields with same type for structs / defunctionalization
- Tuples and static arrays
- `when` clause for match statements
- Nested destructing
- add types to new passes like `specialize_params`
- Pathtracing example
- Potentially some kind of recursive types like `type list['a] = Nil | Cons of 'a * list['a]`
- Passing constraints to mono is a bit weird, they should concretize that in `typecheck` step
- Merging specialize and mono passes since they interplay like they're supposed to be the same pass
- Mutual recursion
- Make logo with GLML
- Add a guide or overview to playground
- Export to shadertoy?
- Refactor Makefile if needed to shared playground build files, since we rebuild playground on serve
- Add common GLSL builtins: `#radians` / `#degrees`, `#refract`, `#faceforward`, `#dFdx`, `#dFdy`, `#fwidth`,`#matrixCompMult`, `#transpose`, `#inverse`, `#determinant`

## Long term Todo Tasks

- Implicit error field added to every function to propagate error color back
- Add support for LSP hover or something like an [inspect]
- Typechecking (Church-typed LC -> HM -> Size dependent)
- Swizzle syntax or some kind of rank polymorphism
- Function `inlining` / `specialize` (but likely everything is specialized)
- Dead code elimination
- Constant folding/propagation (Sparse conditional constant propagation)
- Doc strings or emission of helpful comments
- Better benchmarking tests
- Buffer passing (e.g. for Game of Life)
- Example of rendering clouds (volumetric rendering)
- Add sliders in playground to change values
- Update blog posts
- Set up vim/treesitter plugins
- Set up Github org more
- Vim/Emacs syntax file
- Add language reference and examples to mdbook
- TUI pass explorer with minttea (bug with conflicting minttea and Core naming preventing this)

## Potentially Interesting Thoughts

- `wasm_of_ocaml` build? `Core` seems to cause `Error: Base_am_testing not implemented`
- Do I separate camera and make it specific like a 3D ShaderToy for Raymarching in the web playground?
- Write Nix derivation for Javascript and OCaml bindings
- Emit on compilation what data needs to be passed from host
- Indexing vectors by arbitrary terms?
- Differentiate int and float division explicitly
- Have `int <: float` be a true subtype (currently can't assign `let (x : option[float]) = Some 5`)
- WebGPU backend for computer shaders and SSBOs?

## Resources

- [Janet to GLSL Compiler](https://ianthehenry.com/posts/bauble/building-bauble/)
- [Articles on SDFs](https://iquilezles.org/articles/)
- [Size-Dependent Types](https://futhark-lang.org/publications/array21.pdf)
- [Futhark In-place Records](https://futhark-lang.org/blog/2017-03-06-futhark-record-system.html)
- [Futhark Size-Dependent Types](https://www.di.ens.fr/~pouzet/bib/array23.pdf)
- [Writing Nix Derivations](https://github.com/justinwoo/nix-shorts/blob/master/posts/your-first-derivation.md)
- [Typing Haskell in Haskell]([https://web.cecs.pdx.edu/~mpj/thih/thih.pdf])
- [Type Inference with Constrained Types]([https://www.cs.tufts.edu/~nr/cs257/archive/martin-odersky/hmx.pdf])
- [Demystifying Typeclasses]([https://okmij.org/ftp/Computation/typeclass.html])
