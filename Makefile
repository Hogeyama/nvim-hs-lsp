all: stack-all
stack-all: stack-setup stack-build stack-test stack-lint
cabal-all: cabal-setup cabal-build cabal-test cabal-lint

# stack
#######

.PHONY: stack-setup
stack-setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack build --copy-compiler-tool hlint weeder

.PHONY: stack-build
stack-build:
	stack build --pedantic
	stack build --test --no-run-tests

.PHONY: stack-test
stack-test:
	stack test

.PHONY: stack-lint
stack-lint:
	stack exec -- hlint src
	stack exec -- weeder src

.PHONY: stack-clean
stack-clean:
	stack clean

# cabal
#######

.PHONY: cabal-setup
cabal-setup:
	cabal new-update
	cabal new-install hpack hlint weeder
	hpack
	cabal new-build --dependencies-only

.PHONY: cabal-build
cabal-build:
	cabal new-build

.PHONY: cabal-test
cabal-test:
	cabal new-test

.PHONY: cabal-lint
cabal-lint:
	hlint src
	weeder src

.PHONY: cabal-clean
cabal-clean:
	cabal new-clean

