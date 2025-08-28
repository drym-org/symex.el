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
	@echo "build - byte compile the package"
	@echo "lint - check style with package-lint"
	@echo "checkdoc - check docstrings"

clean:
	cd ci && rm -rf ci-init

install:
	cd ci && emacs --batch --quick --load ci-install.el

build:
	cd ci && emacs --batch --quick --load ci-build.el

lint:
	cd ci && emacs --batch --quick --load ci-lint.el

checkdoc:
	cd ci && emacs --batch --quick --load ci-checkdoc.el

.PHONY: help clean install build lint checkdoc
