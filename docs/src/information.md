# Information Dump

This page contains a dump of current TODO items and unorganized thoughts and tasks. This will likely stay a mess.

## Bugs

```glml
// Strange [translate] bug??
type sdf = vec2 -> float
let constant (r : float) : sdf = fun p -> r
let union (f : sdf) _ r = f r
let dup f g x = f (g x) (g x)
let scene : sdf = dup union constant 0.3
let main (coord : vec2) = [0, 0, 0]
```

- main is mangled so make sure that doesn't happen, must be fixed to coord vec2

## Todo Tasks

- More Demos: New examples with HOFs, IQ Palette, Beaver Logo
- Type annotations for arbitrary terms
- Promotion with ints/floats (true coercion enforced rather than constraint)
- Type aliases with parameters
- Curried builtin functions for partial application
- Auto lift `f : float -> float` to work over vecs?
- Update GLML screenshot
- Button to show GLSL output on mobile
- Add builtin GLSL function callers or GLSL extern libraries
- Add user definable constrained functions?
- `uintBitsToFloat` instead of fat structs to represent variants, instead storing as uvec4 in raw bits
- Reuse fields with same type for structs / defunctionalization
- Tuples and static arrays
- `when` clause for match statements
- matching on `struct`s
- Nested destructing
- `mat` should be exactly a `vec` of `vec`s, not a new form which makes it ambiguous and prevents non-float vecs
- add types to new passes like `specialize_params`, also do some general refactoring?
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
- Better pipeline tests
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
