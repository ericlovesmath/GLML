# GLML (OpenGL Meta Language)

GLML is an early stage functional domain-specific language for writing fragment shaders, targeting GLSL. The goal is an ML-style language with [size-dependent types](https://futhark-lang.org/publications/fhpnc23.pdf).

See [INFORMATION.md](INFORMATION.md) for a working roadmap and referenced resources.

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
- **Web**: `make serve`
