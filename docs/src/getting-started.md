# Getting Started

## Web Playground

The fastest way to try GLML is the online playground at [glml-lang.com/playground](https://www.glml-lang.com/playground). Paste any GLML program or select one of the examples and see the compiled GLSL output immediately. Programs can be run with `ctrl-enter`.

## Building from Source

### With Nix (recommended)

```bash
git clone https://github.com/glml-lang/glml
cd glml
nix develop         # enters a shell with all dependencies
```

### With opam

Requires OCaml 5.3.0+:

```bash
git clone https://github.com/glml-lang/glml
cd glml
opam switch create . 5.3.0
opam install . --deps-only
eval $(opam env)
```

## Compiling a Shader

Compile a `.glml` file to GLSL and print to stdout:

```bash
dune exec GLML -- build examples/rainbow.glml
```

Write the output to a file:

```bash
dune exec GLML -- build examples/rainbow.glml -o shader.glsl
```

## Inspecting Compiler Passes

GLML has a multi-pass pipeline. You can dump the AST at any pass for debugging:

```bash
# Dump all passes to stdout
dune exec GLML -- build examples/rainbow.glml -p all

# Dump a specific pass
dune exec GLML -- build examples/rainbow.glml -p typecheck

# Dump all passes to files in a directory
dune exec GLML -- build examples/rainbow.glml -p all -d /tmp/passes

# List available pass names
dune exec GLML -- list-passes
```

## Running Tests

```bash
make test       # run all expect tests
dune promote    # accept new output after intentional changes
```
