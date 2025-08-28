;; ci-build.el
;; This script byte-compiles the packages.
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
;; We don't need to bootstrap, just load the library to get its functions.
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        straight-base-dir)))
  (unless (file-exists-p bootstrap-file)
    (error "straight.el not found. Run ci-install.el first."))
  (load bootstrap-file nil 'nomessage))


;; --- The Direct Compilation Tool ---
(defun ci-compile-package (pkg-name)
  "Compile PKG-NAME using `batch-byte-compile`, print all output,
and return a shell-friendly exit code."
  (let* ((build-dir (directory-file-name (straight--build-dir pkg-name)))
         (deps-dirs (mapcar #'straight--build-dir
                            (straight--flatten (straight-dependencies pkg-name))))
         (load-path-args (mapcan (lambda (dir) (list "-L" dir))
                                 (append deps-dirs (list build-dir))))
         (files-to-compile (directory-files-recursively build-dir "\\.el$"))
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
(let ((packages-to-check '("symex-core"
                           "symex"
                           "symex-ide"
                           "symex-evil"))
                           ;; "symex-rigpa"
      (exit-code 0))
  (dolist (pkg packages-to-check)
    (let ((status (ci-compile-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Compilation failed for %s with status %d" pkg status))
        (setq exit-code status))))
  (if (zerop exit-code)
      (message "\nAll packages compiled successfully.")
    (kill-emacs exit-code)))
