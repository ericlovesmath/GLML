.PHONY: clean bin js playground serve test benchmark

PROFILE := dev
ifdef RELEASE
	PROFILE := release
endif

DUNE_FLAGS := --profile $(PROFILE)

all: bin js playground

clean:
	dune clean
	rm -rf dist

bin:
    # Use dune exec GLML -- <args> to run cli
    # Alternatively use ./_build/default/bin/main.exe
	dune build $(DUNE_FLAGS) _build/default/bin/main.exe

js:
	dune build $(DUNE_FLAGS) _build/default/jsoo/main.bc.js

playground:
	dune build $(DUNE_FLAGS) _build/default/playground/main.bc.js
	mkdir -p dist
	cp playground/index.html dist
	cp playground/style.css dist
	cp -f _build/default/playground/main.bc.js dist

serve: playground
	cd dist; python3 -m http.server

test:
	dune runtest

benchmark:
	cd benchmark; ./runner.sh
