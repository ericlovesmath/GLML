.PHONY: clean bin js web serve test benchmark

PROFILE := dev
ifdef RELEASE
	PROFILE := release
endif

DUNE_FLAGS := --profile $(PROFILE)

all: bin js web

clean:
	dune clean
	rm -rf dist

bin:
    # Use dune exec GLML -- <args> to run cli
    # Alternatively use ./_build/default/bin/main.exe
	dune build $(DUNE_FLAGS) _build/default/bin/main.exe

js:
	dune build $(DUNE_FLAGS) _build/default/jsoo/main.bc.js

web:
	dune build $(DUNE_FLAGS) _build/default/web/main.bc.js
	mkdir -p dist
	cp web/index.html dist
	cp web/style.css dist
	cp -f _build/default/web/main.bc.js dist

serve: web
	cd dist; python3 -m http.server

test:
	dune runtest

benchmark:
	cd benchmark; ./runner.sh
