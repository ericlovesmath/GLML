.PHONY: clean build web test

# Default command if you just type 'make'
all: clean build

clean:
	dune clean

build:
	dune build _build/default/bin/main.exe
	dune build _build/default/jsoo/main.bc.js

web:
	dune build _build/default/web/main.bc.js

test:
	dune runtest
