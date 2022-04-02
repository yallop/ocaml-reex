all:
	dune build -p reex,reex_match

clean:
	dune clean

test:
	dune runtest

.PHONY: all clean test
