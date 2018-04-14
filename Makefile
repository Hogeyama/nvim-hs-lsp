all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack build --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build --pedantic
	stack build --test --no-run-tests

.PHONY: test
test:
	stack test

.PHONY: lint
lint:
	stack exec -- hlint src
	stack exec -- weeder src
