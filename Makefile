.PHONY: clean build web serve test

# Default command if you just type 'make'
all: clean build

clean:
	dune clean

build:
	dune build _build/default/bin/main.exe

web:
	dune build _build/default/jsoo/main.bc.js
	dune build _build/default/web/main.bc.js

serve: web
	@echo "========================================================="
	@echo "  Shader: http://localhost:8000/web/glml.html"
	@echo "  Codemirror: http://localhost:8000/web/codemirror.html"
	@echo "========================================================="
	python3 -m http.server

test:
	dune runtest
