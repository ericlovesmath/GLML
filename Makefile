.PHONY: all clean bin js playground-deps serve test benchmark website

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

website: playground-deps
	cd playground && npm run build
	rm -rf dist
	mkdir -p dist
	cp -r playground/dist/. dist/

serve: playground-deps
	cd playground && npm run dev

test:
	dune runtest

benchmark:
	cd benchmark; ./runner.sh
