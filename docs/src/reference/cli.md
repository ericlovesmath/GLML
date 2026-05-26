# CLI

## Install with Nix

Run directly from the flake without cloning:

```bash
nix run github:glml-lang/glml -- build <example.glml>
```

Or build the CLI locally:

```bash
nix build github:glml-lang/glml
./result/bin/glml build example.glml

git clone https://github.com/glml-lang/glml
nix run ".#glml" -- build <example.glml>
```

For development, clone and enter the dev shell:

```bash
git clone https://github.com/glml-lang/glml
cd glml
nix develop
```

## Install with opam

Requires OCaml 5.3.0+:

```bash
git clone https://github.com/glml-lang/glml
cd glml
opam switch create . 5.3.0
opam install . --deps-only
eval $(opam env)
```

## Usage

```bash
# binary by default built in `./_build/default/bin/main.exe`
make bin

glml build example.glml                        # output to stdout
glml build example.glml -o shader.glsl         # output to file
glml build example.glml -p all                 # dump all passes to stdout
glml build example.glml -p typecheck           # one pass
glml build example.glml -p all -d /tmp/passes  # to files

# Disable inlining / constant folding / deadcode elim
glml build example.glml -no-optimize

# List available passes to dump
glml list-passes
```

## Tests

```bash
make test           # run all expect tests
dune promote        # accept new output after intentional changes
```
