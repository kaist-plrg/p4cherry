MAIN = p4cherry
TEST = p4cherry-test

# Compile

.PHONY: build

EXEMAIN = _build/default/bin/main.exe
EXETEST = _build/default/bin/test.exe

build:
	rm -f ./$(MAIN)
	dune build && echo
	ln -f $(EXEMAIN) ./$(MAIN)
	ln -f $(EXETEST) ./$(TEST)

debug:
	rm -f ./$(MAIN)
	dune build --profile release && echo
	ln -f $(EXEMAIN) ./$(MAIN)
	ln -f $(EXETEST) ./$(TEST)

# Format

.PHONY: fmt

fmt:
	dune build @fmt --auto-promote

# Tests

.PHONY: test-parser test-typecheck test

test-parser: build
	./$(TEST) parse -i test/arch test/program/well-typed > status/parser.log 2> status/parser.err

test-typecheck: build
	./$(TEST) typecheck -i test/arch -p test/program/well-typed > status/typecheck-pos.log 2> status/typecheck-pos.err
	./$(TEST) typecheck -i test/arch -n test/program/ill-typed > status/typecheck-neg.log 2> status/typecheck-neg.err

test: test-parser test-typecheck

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(NAME) ./$(TEST)
	dune clean
