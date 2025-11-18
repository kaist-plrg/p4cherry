MAIN = p4cherry

# Compile

.PHONY: build build-p4

EXEMAIN = p4/_build/default/bin/main.exe
EXETEST = p4/_build/default/bin/test.exe

build: build-p4

build-p4:
	rm -f ./$(MAIN)
	opam switch 5.1.0
	cd p4 && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXEMAIN) ./$(MAIN)

build-p4-release:
	rm -f ./$(MAIN)
	opam switch 5.1.0
	cd p4 && opam exec -- dune build --profile release bin/main.exe && echo
	ln -f $(EXEMAIN) ./$(MAIN)

# Format

.PHONY: fmt

fmt:
	opam switch 5.1.0
	cd p4 && opam exec dune fmt

# Tests

.PHONY: test promote coverage 

test:
	echo "#### Running (dune runtest)"
	opam switch 5.1.0
	cd p4 && dune clean && opam exec -- dune runtest && echo OK || (echo "####>" Failure running dune test. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

promote:
	opam switch 5.1.0
	cd p4 && opam exec -- dune promote

coverage:
	echo "#### Running (dune runtest --instrument-with bisect_ppx --force)"
	opam switch 5.1.0
	cd p4 && dune clean && find . -name '*.coverage' | xargs rm -f && opam exec -- dune runtest --instrument-with bisect_ppx --force
	cd p4 && bisect-ppx-report html && bisect-ppx-report summary

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(MAIN)
	cd p4 && dune clean
