_build/default/bin/main.exe:
	@dune build
	ln -sf _build/default/bin/main.exe main
	chmod -H +x main

all: build

build: _build/default/bin/main.exe

test:
	@python test/run-tests.py

clean:
	@dune clean

.PHONY: all clean test
