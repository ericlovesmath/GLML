.PHONY: clean build web serve test

all: build web

clean:
	dune clean

build:
    # Use dune exec GLML -- <args> to run cli
    # Alternatively use ./_build/default/bin/main.exe
	dune build _build/default/bin/main.exe

web:
	dune build _build/default/jsoo/main.bc.js
	dune build _build/default/web/main.bc.js

serve: web
	@echo "========================================================="
	@echo "  Playground: http://localhost:8000/web/index.html"
	@echo "========================================================="
	python3 -m http.server

test:
	dune runtest
