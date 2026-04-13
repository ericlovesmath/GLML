# Information Dump

This page contains a dump of current TODO items and unorganized thoughts and tasks. This will likely stay a mess.

## Bugs

```glml
let add (x : float) (y : float) = x + y

let addn (n : float) = fun (x : float) -> add n x

let main (coord : vec2) =
  // The below fails to typecheck when [0.] and [1.] are ints
  let f = addn 0. in
  let r = f 1. in
  [r, 0, 0]
```

```glml
// Does not typecheck because return type wants type of last type in series of arrows
let palette (a : vec3) (b : vec3) (c : vec3) (d : vec3) : (float -> vec3) =
  fun t -> a + b * #cos(6.28318 * (c * t + d))
```
- main is mangled so make sure that doesn't happen, must be fixed to coord vec2

## Todo Tasks

- Curried builtin functions for partial application
- Auto Lift `f : float -> float` to work over vecs?
- Update GLML screenshot
- Move examples to toplevel consts
- Pipe as syntactic sugar for application
- `let x = u_time` compiles `const float x = u_time`, but that's not allowed though since `u_time` is an extern
- Button to show GLSL output on mobile
- Allow clicking through all passes in GLSL output window
- Add builtin GLSL function callers or GLSL extern libraries
- Add user definable constrained functions?
- uintBitsToFloat instead of fat structs to represent variants, instead storing as uvec4 in raw bits
- Reuse fields with same type for structs / defunctionalization
- Validate if extern variables are in a constant term on output, which might not be possible?
- Tuples and static arrays
- Vectors can't be toplevel constants right now
- `when` clause for match statements
- matching on `vec`s, `mat`s and `struct`s
- Nested destructing
- Destructing in `let` bindings
- `mat` should be exactly a `vec` of `vec`s, not a new form which makes it ambigious and prevents non-float vecs
- add types to new passes like `specialize_params`, also do some general refactoring?
- Pathtracing example
- Potentially some kind of recursive types like `type list['a] = Nil | Cons of 'a * list['a]`
- Passing constraints to mono is a bit weird, they should concertize that in `typecheck` step
- Merging specialize and mono passes since they interplay like they're supposed to be the same pass
- Type aliases
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
- Read Futhark's monomorphization and defunctionalization code for partial application
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
- Local renderer without web / tests with [glslViewer](https://github.com/patriciogonzalezvivo/glslViewer)
- Do I separate camera and make it specific like a 3D ShaderToy for Raymarching in the web playground?
- Write Nix derivation for Javascript and OCaml bindings
- Emit on compilation what data needs to be passed from host
- Indexing vectors by arbitrary terms?
- Differentiate int and float division explicitly
- Have `int <: float` be a true subtype (currently can't assign `let (x : option[float) = Some 5`)
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
