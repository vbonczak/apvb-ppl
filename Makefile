all:
	dune build

clean:
	dune clean

test:
	dune exec src/test/test.exe tests/dump.mlppl

parse:
	dune exec src/test/parse.exe tests/dump.mlppl

.PHONY: clean
