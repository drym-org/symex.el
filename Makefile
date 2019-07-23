EMACS=emacs
CASK ?= cask

INIT_PACKAGE_EL="(progn  \
  (require 'package)  \
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)  \
  (setq my-package-dir \"$(PWD)\")  \
  (add-to-list 'load-path my-package-dir)  \
  (let ((default-directory my-package-dir))  \
    (normal-top-level-add-subdirs-to-load-path))  \
  (package-initialize))"

PROJECT_FILES=`${CASK} files`

lint:
	${CASK} exec $(EMACS) -Q --batch --eval $(INIT_PACKAGE_EL) -l "package-lint.el" -f "package-lint-batch-and-exit" ${PROJECT_FILES}

checkdoc:
	${CASK} exec $(EMACS) -Q --batch --eval $(INIT_PACKAGE_EL) -l "build-utils.el" --eval '(flycheck/batch-checkdoc ".")'

build :
	${CASK} build

clean :
	${CASK} clean-elc

install:
	${CASK} install

test: build
	${CASK} exec ert-runner

.PHONY:	lint checkdoc build clean install test
