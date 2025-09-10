# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

EMACS=emacs

export CI_PROJECT=symex
export CI_PACKAGES=symex-core symex symex-ide symex-evil symex-rigpa

help:
	@echo "Run common development actions."
	@echo
	@echo "    Usage: make <target>"
	@echo "    where <target> is one of:"
	@echo
	@echo "help - show this menu"
	@echo "clean - remove all build artifacts"
	@echo "setup-ci - clone emacs-ci to run project CI actions such as linting"
	@echo "bootstrap - install Straight.el"
	@echo "install - install package dependencies"
	@echo "build - byte compile the package"
	@echo "lint - check style with package-lint"
	@echo "checkdoc - check docstrings"
	@echo "build-docs - build HTML docs at symex/doc/symex_html/"
	@echo
	@echo "**All of these actions (aside from docs) take effect and are contained inside the emacs-ci/ folder --- they do not affect the system Emacs configuration.**"

setup-ci:
	git clone https://github.com/countvajhula/emacs-ci.git

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

.PHONY: help setup-ci clean bootstrap install build lint checkdoc build-docs
