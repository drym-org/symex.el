# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

EMACS=emacs
CASK ?= cask

PROJECT_FILES=`${CASK} files`

help:
	@echo "clean - remove all build artifacts"
	@echo "install - install package dependencies in .cask/"
	@echo "lint - check style with package-lint"
	@echo "lint+less - lint piped to less"
	@echo "lint-no-noise - lint with typically noisy warnings filtered out"
	@echo "lint-noiseless - lint-no-noise piped to less"
	@echo "checkdoc - check docstrings"
	@echo "build - byte compile the package"
	@echo "test - run tests"

clean-core:
	cd symex-core && ${CASK} clean-elc

install-core:
	cd symex-core && ${CASK} install

build-core:
	cd symex-core && ${CASK} build

clean-symex:
	cd symex && ${CASK} clean-elc

install-symex:
	cd symex && ${CASK} install

build-symex:
	cd symex && ${CASK} build

clean-ide:
	cd symex-ide && ${CASK} clean-elc

install-ide:
	cd symex-ide && ${CASK} install

build-ide:
	cd symex-ide && ${CASK} build

clean-evil:
	cd symex-evil && ${CASK} clean-elc

install-evil:
	cd symex-evil && ${CASK} install

build-evil:
	cd symex-evil && ${CASK} build

clean-rigpa:
	cd symex-rigpa && ${CASK} clean-elc

install-rigpa:
	cd symex-rigpa && ${CASK} install

build-rigpa:
	cd symex-rigpa && ${CASK} build

clean: clean-core clean-symex clean-ide clean-evil clean-rigpa

install: install-core install-symex install-ide install-evil install-rigpa

build: build-core build-symex build-ide build-evil build-rigpa

lint-core:
	cd symex-core && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "package-lint.el"  \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

lint-symex:
	cd symex && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "package-lint.el"  \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

lint-ide:
	cd symex-ide && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "package-lint.el"  \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

lint-evil:
	cd symex-evil && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "package-lint.el"  \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

lint-rigpa:
	cd symex-rigpa && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "package-lint.el"  \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

lint: lint-core lint-symex lint-ide lint-evil lint-rigpa

lint+less:
	@$(MAKE) -f $(THIS_FILE) lint 2>&1 | less

lint-no-noise:
	@$(MAKE) -f $(THIS_FILE) lint 2>&1 | grep -v "start with.*prefix" |grep -v "lexical-binding" |grep -v "non-snapshot.*racket" |grep -v "Version.*header is missing" |grep -v "Package-Version"

lint-noiseless:
	@$(MAKE) -f $(THIS_FILE) lint-no-noise 2>&1 | less

checkdoc-core:
	cd symex-core && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "../dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

checkdoc-symex:
	cd symex && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "../dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

checkdoc-ide:
	cd symex-ide && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "../dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

checkdoc-evil:
	cd symex-evil && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "../dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

checkdoc-rigpa:
	cd symex-rigpa && ${CASK} exec $(EMACS) -Q --batch  \
	                      -l "../dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

# Run with make -k to ignore errors
checkdoc: checkdoc-core checkdoc-symex checkdoc-ide checkdoc-evil checkdoc-rigpa

test: build
	${CASK} exec ert-runner

.PHONY: help clean install build lint checkdoc test clean-core install-core build-core clean-symex install-symex build-symex clean-ide install-ide build-ide clean-evil install-evil build-evil clean-rigpa install-rigpa build-rigpa
