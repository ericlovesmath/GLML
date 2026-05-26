.PHONY: all clean bin js playground-deps playground serve test benchmark website

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
	dune build $(DUNE_FLAGS) _build/default/bin/main.exe

js:
	dune build $(DUNE_FLAGS) _build/default/jsoo/main.bc.js

playground-deps: js
	mkdir -p playground/public
	cp -f _build/default/jsoo/main.bc.js playground/public/
	cd playground && npm install

playground: playground-deps
	cd playground && npm run build

serve: playground-deps
	cd playground && npm run dev

website: clean playground
	dune build $(DUNE_FLAGS) _build/default/docs/preprocessor/main.exe
	cd docs && mdbook build
	cp -r docs/book dist
	cp -r playground/dist dist/playground

test:
	dune runtest

benchmark:
	cd benchmark; ./runner.sh
