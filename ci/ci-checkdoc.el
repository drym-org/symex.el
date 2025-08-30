;; ci-checkdoc.el
;; This script runs checkdoc on the packages.
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


;; --- The Checkdoc Tool (Multi-Process Version) ---
(defun ci-checkdoc-package (pkg-name)
  "Run checkdoc on PKG-NAME in a separate process, print all output,
and return a shell-friendly exit code based on whether output was generated."
  (let* ((repo-root (expand-file-name ".."))
         (source-dir (expand-file-name pkg-name repo-root))
         (deps-dirs (mapcar #'straight--build-dir
                            (straight--flatten (straight-dependencies pkg-name))))
         (load-path-args (mapcan (lambda (dir) (list "-L" dir))
                                 (append deps-dirs (list source-dir))))
         ;; Get only the top-level .el files from the source directory.
         (files-to-check (directory-files source-dir t "\\.el$"))
         (output-buffer (generate-new-buffer " *checkdoc-output*"))
         ;; This simple program just runs checkdoc on all files. We will
         ;; inspect the output in the parent process.
         (program `(progn
                     (require 'checkdoc)
                     ;; Disable the noisy, experimental verb check.
                     (setq checkdoc-verb-check-experimental-flag nil)
                     (dolist (file ',files-to-check)
                       (checkdoc-file file)))))

    (message (format "--- Running checkdoc on %s ---" pkg-name))
    (unwind-protect
        (let* ((args (append '("-Q" "--batch")
                             load-path-args
                             (list "--eval" (format "%S" program))))
               ;; Run the process, capturing all output into a single buffer.
               (exit-code (apply #'call-process
                                 (executable-find "emacs") nil
                                 output-buffer ; Capture stdout and stderr here.
                                 nil args))
               (output-text (with-current-buffer output-buffer (buffer-string))))

          ;; Print whatever the subprocess produced.
          (princ output-text)

          ;; Determine failure based on EITHER a non-zero exit code (Lisp error)
          ;; OR the presence of any output (checkdoc warnings).
          (if (or (not (zerop exit-code))
                  (string-match-p "\\S-" output-text))
              1 ; Failure
            0)) ; Success

      ;; Cleanup: Kill the temporary buffer.
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))


;; --- Main Execution ---
(let ((packages-to-check '("symex-core"
                           "symex"
                           "symex-ide"
                           "symex-evil"
                           "symex-rigpa"))
      (exit-code 0))
  (dolist (pkg packages-to-check)
    (let ((status (ci-checkdoc-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Checkdoc failed for %s with status %d" pkg status))
        (setq exit-code status))))
  (if (zerop exit-code)
      (message "\nAll packages passed checkdoc.")
    (progn
      (message "\nCheckdoc process completed with some errors.")
      (kill-emacs exit-code))))
