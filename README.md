# GLML (OpenGL Meta Language)

GLML is an early stage functional domain-specific language for writing fragment shaders, targeting GLSL. It allows you to write shaders using an ML-style language with [size-dependent types](https://futhark-lang.org/publications/fhpnc23.pdf).

## Build

If Nix is installed, `nix develop` provides the necessary libraries. If not, note that the web playground uses some dependencies from [Jane Street's opam repository](https://github.com/janestreet/opam-repository) for [bonsai](https://github.com/janestreet/bonsai/issues). The compiler itself does not depend on bonsai and can be compiled without Jane Street's opam repository.

```bash
# Nix:
nix develop

# Opam (to compile web playground):
opam switch create . 5.2.0 --repositories=default,janestreet=git+https://github.com/janestreet/opam-repository.git
opam install . --deps-only
```

## Usage

- **CLI**: `dune exec GLML -- compile <file>`
- **Web**: `make serve` and visit `http://localhost:8000/web/index.html`
