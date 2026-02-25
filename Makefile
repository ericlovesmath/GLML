.PHONY: clean bin js web serve test benchmark

all: bin js web

clean:
	dune clean
	rm -rf dist

bin:
    # Use dune exec GLML -- <args> to run cli
    # Alternatively use ./_build/default/bin/main.exe
	dune build _build/default/bin/main.exe

js:
	dune build _build/default/jsoo/main.bc.js

web:
	dune build --profile release _build/default/web/main.bc.js
	mkdir -p dist
	cp web/index.html dist
	cp web/style.css dist
	cp -f _build/default/web/main.bc.js dist

serve: web
	@echo "========================================"
	@echo "  Playground: http://localhost:8000"
	@echo "========================================"
	cd dist; python3 -m http.server

test:
	dune runtest

benchmark:
	cd benchmark; ./runner.sh
