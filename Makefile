# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

EMACS=emacs

export CI_PROJECT=symex
export CI_PACKAGES=symex-core symex symex-ide symex-evil symex-rigpa

help:
	@echo "clean - remove all build artifacts"
	@echo "install - install package dependencies in .cask/"
	@echo "build - byte compile the package"
	@echo "lint - check style with package-lint"
	@echo "checkdoc - check docstrings"

clean:
	cd emacs-ci && rm -rf ci-init

bootstrap:
	cd emacs-ci && emacs --batch --quick --load bootstrap.el

install:
	cd emacs-ci && emacs --batch --quick --load install.el

build:
	cd emacs-ci && emacs --batch --quick --load build.el

lint:
	cd emacs-ci && emacs --batch --quick --load lint.el

checkdoc:
	cd emacs-ci && emacs --batch --quick --load checkdoc.el

build-docs:
	cd symex/doc && texi2any --html --output symex_html symex.texi && mkdir -p symex_html/figures && cp figures/* symex_html/figures/

.PHONY: help clean install build lint checkdoc build-docs
