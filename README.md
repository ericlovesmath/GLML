# GLML (OpenGL Meta Language)

GLML is an early stage functional domain-specific language for writing fragment shaders, targeting GLSL. Right now, it is primarily based on Hindley-Milner typing and features typeclasses for operator overloading. The long-term goal is to evolve into an ML-style language with [size-dependent types](https://futhark-lang.org/publications/fhpnc23.pdf).

Docs: [www.glml-lang.com](https://www.glml-lang.com/playground)

**Try GLML Playground:** [www.glml-lang.com/playground](https://www.glml-lang.com/playground)

<p align="center">
    <img src="./docs/src/playground.png" width="60%">
</p>

## Build

If Nix is installed, `nix develop` provides the necessary libraries. Check [dune-project](dune-project) to note dependencies, but all of the packages should support OCaml 5.3.0.
```bash
# Nix:
nix develop

# Opam:
opam switch create . 5.3.0
opam install . --deps-only
```

## Usage

- **CLI**: `dune exec GLML -- compile <file>`
- **Web Playground**: `make serve`
