;; ci-lint.el
;; This script runs package-lint on the packages.
;; It must be run *after* ci-install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Load the shared CI helper functions and constants.
(require 'ci-helpers (expand-file-name "ci-helpers.el"))
(ci-load-straight)

;; Install the forked linter.
;; This uses a customized version of package-lint that:
;;  1. recognizes the package suite pattern
;;  2. doesn't check availability of dependencies on package archives
;;     as these packages are not on MELPA (and the installation and
;;     building of these dependencies is already checked in preceding
;;     CI checks, i.e., ci-install.el and ci-build.el).
(straight-use-package
 '(package-lint :host github :repo "countvajhula/package-lint" :branch "control-which-checks-to-run"))


;; --- The Linter Tool ---
(defun ci-lint-package (pkg-name)
  "Run package-lint on PKG-NAME, print all output,
and return a shell-friendly exit code."
  (let* ((linter-dir (straight--build-dir "package-lint"))
         (load-path-args (ci-get-load-path-args pkg-name (list linter-dir)))
         (main-file (ci-get-package-main-file pkg-name))
         (files-to-lint (ci-get-package-source-files pkg-name))
         (output-buffer (generate-new-buffer " *lint-output*")))

    (message (format "--- Linting %s ---" pkg-name))
    (unwind-protect
        (let* ((args (append '("-Q" "--batch")
                             load-path-args
                             ;; Set all necessary linter variables.
                             (list "--eval"
                                   (format "(setq package-lint-prefix %S
                                                  package-lint-main-file %S
                                                  package-lint-check-installable nil)"
                                           ci-project-name
                                           main-file))
                             '("-l" "package-lint")
                             '("-f" "package-lint-batch-and-exit")
                             files-to-lint))
               (exit-code (apply #'call-process
                                 (executable-find "emacs") nil output-buffer nil args)))
          (with-current-buffer output-buffer
            (princ (buffer-string)))
          exit-code)
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))


;; --- Main Execution ---
(let ((exit-code 0))
  (dolist (pkg ci-packages)
    (let ((status (ci-lint-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Linting failed for %s with status %d" pkg status))
        (setq exit-code status))))
  (if (zerop exit-code)
      (message "\nAll packages passed linting.")
    (kill-emacs exit-code)))

