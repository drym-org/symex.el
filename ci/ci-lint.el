;; ci-lint.el
;; This script runs package-lint on the packages.
;; It must be run *after* ci-install.el has successfully completed.
;; -*- lexical-binding: t -*-
;;
;; Note: some flags passed to checkdoc and lint from a calling script
;; (e.g., this one) typically presuppose dynamic binding, but in the
;; present case we're running those tools as subprocesses, so they
;; should use the default dynamic binding, even though this script
;; uses lexical binding.

(defvar straight-base-dir (expand-file-name "ci-init"))

;; --- Load the existing straight.el installation ---
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        straight-base-dir)))
  (unless (file-exists-p bootstrap-file)
    (error "straight.el not found. Run ci-install.el first."))
  (load bootstrap-file nil 'nomessage))

;; --- Install the forked linter ---
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
  (let* ((source-dir (expand-file-name pkg-name "../"))
         ;; Set the main file in the package so that package-lint
         ;; can parse dependencies, etc.
         (main-file (expand-file-name (concat pkg-name ".el") source-dir))
         (deps-dirs (mapcar #'straight--build-dir
                            (straight--flatten (straight-dependencies pkg-name))))
         ;; The linter needs its own build dir on the load-path too.
         (linter-dir (straight--build-dir "package-lint"))
         (load-path-args (mapcan (lambda (dir) (list "-L" dir))
                                 (append deps-dirs (list source-dir linter-dir))))
         (files-to-lint (directory-files source-dir t "\\.el$"))
         (output-buffer (generate-new-buffer " *lint-output*")))

    (message (format "--- Linting %s ---" pkg-name))
    (unwind-protect
        (let* ((args (append '("-Q" "--batch")
                             load-path-args
                             ;; Set all necessary linter variables.
                             (list "--eval"
                                   (format "(setq package-lint-prefix \"symex\"
                                                 package-lint-main-file %S
                                                 package-lint-check-installable nil)"
                                           main-file))
                             '("-l" "package-lint")
                             '("-f" "package-lint-batch-and-exit")
                             files-to-lint))
               (exit-code (apply #'call-process
                                 (executable-find "emacs") nil output-buffer t args)))
          (with-current-buffer output-buffer
            (princ (buffer-string)))
          exit-code)
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))


;; --- Main Execution ---
(let ((packages-to-check '("symex-core"
                           "symex"
                           "symex-ide"
                           "symex-evil"))
                           ;; "symex-rigpa"
      (exit-code 0))
  (dolist (pkg packages-to-check)
    (let ((status (ci-lint-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Linting failed for %s with status %d" pkg status))
        (setq exit-code status))))
  (if (zerop exit-code)
      (message "\nAll packages passed linting.")
    (kill-emacs exit-code)))
