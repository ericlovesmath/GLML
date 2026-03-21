.PHONY: clean bin js playground serve test benchmark website

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

playground: js
	mkdir -p playground/public
	cp -f _build/default/jsoo/main.bc.js playground/public/
	cd playground && npm install && npm run build

serve: playground
	cd playground && npm run dev

website: clean playground
	cd docs && mdbook build;
	cp -r docs/book dist
	cp -r playground/dist dist/playground

test:
	dune runtest

benchmark:
	cd benchmark; ./runner.sh
