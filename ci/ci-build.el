;; ci-build.el
;; This script byte-compiles the packages.
;; It must be run *after* ci-install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Load the shared CI helper functions and constants.
(require 'ci-helpers (expand-file-name "ci-helpers.el"))
(ci-load-straight)


;; --- The Direct Compilation Tool ---
(defun ci-compile-package (pkg-name)
  "Compile PKG-NAME using `batch-byte-compile`, print all output,
and return a shell-friendly exit code."
  (let* ((load-path-args (ci-get-load-path-args pkg-name))
         ;; For compilation, we want all .el files, including autoloads.
         (files-to-compile (ci-get-package-all-files pkg-name))
         (output-buffer (generate-new-buffer " *compilation-output*")))

    (message (format "--- Compiling %s ---" pkg-name))
    (unwind-protect
        (let* ((args (append '("-Q" "--batch")
                             load-path-args
                             '("--eval" "(setq byte-compile-error-on-warn t)")
                             '("-f" "batch-byte-compile")
                             files-to-compile))
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
    (let ((status (ci-compile-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Compilation failed for %s with status %d" pkg status))
        (setq exit-code status))))
  (if (zerop exit-code)
      (message "\nAll packages compiled successfully.")
    (kill-emacs exit-code)))
