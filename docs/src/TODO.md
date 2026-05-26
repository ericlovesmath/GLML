# Information Dump

This page contains a dump of current TODO items and unorganized thoughts and tasks. This will likely stay a mess.

## Tasks / Bugs

- Inline/fold effectful while loops
- Inline `main_pure` function into `main` potentially 
- `vec<_, float>)`
- Get rid of vec3 or matnxn syntax
- Allow binary operations and pipes to be used as curried functions
- Type annotations for arbitrary terms
- Type aliases with parameters
- Curried builtin functions for partial application
- Auto lift `f : float -> float` to work over vecs?
- Add builtin GLSL function callers or GLSL extern libraries
- `uintBitsToFloat` instead of fat structs to represent variants, instead storing as uvec4 in raw bits
- Reuse fields with same type for structs / defunctionalization
- static arrays
- `when` clause for match statements
- Add types to new passes like `specialize_params`
- Potentially some kind of recursive types like `type list['a] = Nil | Cons of 'a * list['a]`
- Mutual recursion
- Add a guide or overview to playground
- Refactor Makefile if needed to shared playground build files, since we rebuild playground on serve
- Add common GLSL builtins: `#radians` / `#degrees`, `#refract`, `#faceforward`, `#dFdx`, `#dFdy`, `#fwidth`,`#matrixCompMult`, `#transpose`, `#inverse`, `#determinant`

## Example Ideas

- Raymarched volumetric clouds
- Buffer passing uses (e.g. Game of Life)
- Lava lamp-like example
- Example storing functions and hotswapping functions during executions to justify DFns in variants
- Pathtracing
- Make logo with GLML (finish beaver)

## Long Term Tasks

- Update GLML screenshot
- Implicit error field added to every function to propagate error color back
- Add support for LSP hover or something, at least a simple [inspect] for the playground
- Size dependent types
- Swizzle syntax or some kind of rank polymorphism
- Function `inlining` / `specialize` (but likely everything is specialized)
- Doc strings or emission of helpful comments
- Better benchmarking tests
- Add sliders in playground to change values
- Update blog posts
- Set up Neovim/Emacs/Treesitter plugins
- Set up Github organization

## Potentially Interesting Thoughts

- `wasm_of_ocaml` build? `Core` seems to cause `Error: Base_am_testing not implemented`
- Emit on compilation what data needs to be passed from host
- Indexing vectors by arbitrary terms?
- Differentiate int and float division explicitly
- WebGPU backend for computer shaders and SSBOs?
- Export to shadertoy?

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
- [Compiling Pattern Matching to Good Decision Trees](https://www.cs.tufts.edu/comp/150FP/archive/luc-maranget/jun08.pdf)
- [Compiler Pattern Matching](https://compiler.club/compiling-pattern-matching/)
- [Sparse Conditional Constant Propagation](https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/sccp/)
