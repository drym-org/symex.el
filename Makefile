# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

EMACS=emacs
CASK ?= cask

INIT_PACKAGE_EL="(progn  \
  (require 'package)  \
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)  \
  (package-initialize)  \
  (unless package-archive-contents \
     (package-refresh-contents)))"

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

lint:
	${CASK} exec $(EMACS) -Q --batch  \
	                      --eval $(INIT_PACKAGE_EL)  \
	                      -l "package-lint.el"  \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

lint+less:
	@$(MAKE) -f $(THIS_FILE) lint 2>&1 | less

lint-no-noise:
	@$(MAKE) -f $(THIS_FILE) lint 2>&1 | grep -v "start with.*prefix" |grep -v "lexical-binding" |grep -v "non-snapshot.*racket" |grep -v "Version.*header is missing" |grep -v "Package-Version"

lint-noiseless:
	@$(MAKE) -f $(THIS_FILE) lint-no-noise 2>&1 | less

checkdoc:
	${CASK} exec $(EMACS) -Q --batch  \
	                      --eval $(INIT_PACKAGE_EL)  \
	                      -l "dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

test: build
	${CASK} exec ert-runner

.PHONY: help clean install build lint lint+less lint-no-noise lint-noiseless checkdoc test clean-core install-core build-core clean-symex install-symex build-symex clean-ide install-ide build-ide clean-evil install-evil build-evil clean-rigpa install-rigpa build-rigpa
