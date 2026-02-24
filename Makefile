.PHONY: clean bin web serve test benchmark

all: bin web

clean:
	dune clean
	rm -rf dist

bin:
    # Use dune exec GLML -- <args> to run cli
    # Alternatively use ./_build/default/bin/main.exe
	dune build _build/default/bin/main.exe

web:
	dune build _build/default/jsoo/main.bc.js
	dune build _build/default/web/main.bc.js

serve: web
	mkdir -p dist
	cp web/index.html dist
	cp -f _build/default/web/main.bc.js dist
	@echo "========================================"
	@echo "  Playground: http://localhost:8000"
	@echo "========================================"
	cd dist; python3 -m http.server

test:
	dune runtest

benchmark:
	cd benchmark; ./runner.sh
