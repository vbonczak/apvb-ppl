all:
	dune build

clean:
	dune clean

test:
	dune exec src/test/test.exe tests/dump.mlppl

.PHONY: clean