;; ci-helpers.el
;; This script provides shared functions and constants for the CI process.
;; -*- lexical-binding: t -*-

(defvar ci-packages
  (let ((packages-env (getenv "CI_PACKAGES")))
    (if (or (not packages-env) (string-empty-p packages-env))
        ;; If the env var is not set or is empty, signal an error.
        (error "The CI_PACKAGES environment variable is not set. Please set it to a space-separated list of package names.")
      ;; Otherwise, split it by whitespace into a list.
      (split-string packages-env)))
  "A list of all packages to be checked in the CI process.
This MUST be set via the CI_PACKAGES environment variable
to a space-separated list of package names.")

(defconst ci-project-name
  (let ((project-name-env (getenv "CI_PROJECT")))
    (if (or (not project-name-env) (string-empty-p project-name-env))
        (error "The CI_PROJECT environment variable is not set.")
      project-name-env))
  "The common project name/prefix to be used by the linter.
This MUST be set via the CI_PROJECT environment variable.")

(defun ci-load-straight ()
  "Load the straight.el library from the local CI installation.
This must be called at the beginning of any script that
needs to use straight.el's functions."
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (expand-file-name "ci-init"))))
    (unless (file-exists-p bootstrap-file)
      (error "straight.el not found. Run ci-install.el first."))
    (load bootstrap-file nil 'nomessage)))

(defun ci-get-load-path-args (pkg-name &optional extra-dirs)
  "Return a list of \"-L /path\" arguments for PKG-NAME.
This includes the package's build directory and all its dependencies.
Optional EXTRA-DIRS can be provided to add more directories to the path."
  (let* ((build-dir (straight--build-dir pkg-name))
         (deps-dirs (mapcar #'straight--build-dir
                            (straight--flatten (straight-dependencies pkg-name))))
         (all-dirs (append extra-dirs deps-dirs (list build-dir))))
    (mapcan (lambda (dir) (list "-L" dir)) all-dirs)))

(defun ci-get-package-main-file (pkg-name)
  "Return the absolute path to the main .el file for PKG-NAME."
  (expand-file-name (concat pkg-name ".el") (straight--build-dir pkg-name)))

(defun ci-get-package-all-files (pkg-name)
  "Return absolute paths to all .el files for PKG-NAME, including subdirs.
This is used for byte-compilation."
  (directory-files-recursively (straight--build-dir pkg-name) "\\.el$"))

(defun ci-get-package-source-files (pkg-name)
  "Return absolute paths to the source .el files for PKG-NAME.
This uses the straight.el build directory and excludes generated files."
  (let* ((all-files (ci-get-package-all-files pkg-name)))
    ;; Exclude generated autoload files from checks.
    (cl-remove-if (lambda (file) (string-match-p "-autoloads\\.el$" file))
                  all-files)))

(provide 'ci-helpers)

