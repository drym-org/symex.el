;; ci-checkdoc.el
;; This script runs checkdoc on the packages.
;; It must be run *after* ci-install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Load the shared CI helper functions and constants.
(require 'ci-helpers (expand-file-name "ci-helpers.el"))
(ci-load-straight)


;; --- The Checkdoc Tool (Multi-Process Version) ---
(defun ci-checkdoc-package (pkg-name)
  "Run checkdoc on PKG-NAME and return its output and status.
This function is silent. The return value is a cons cell of the
form (STATUS . FILTERED-OUTPUT), where STATUS is 0 for
success and 1 for failure."
  (let* ((load-path-args (ci-get-load-path-args pkg-name))
         ;; Use the helper to get a clean list of source files to check.
         (files-to-check (ci-get-package-source-files pkg-name))
         (output-buffer (generate-new-buffer " *checkdoc-output*"))
         (program `(progn
                     (require 'checkdoc)
                     (setq checkdoc-verb-check-experimental-flag nil)
                     (dolist (file ',files-to-check)
                       (checkdoc-file file)))))

    (unwind-protect
        (let* ((args (append '("-Q" "--batch")
                             load-path-args
                             (list "--eval" (format "%S" program))))
               ;; Capture all output (stdout and stderr) into a single buffer.
               (exit-code (apply #'call-process
                                 (executable-find "emacs") nil
                                 output-buffer ; Destination for all output.
                                 nil args))
               (output-text (with-current-buffer output-buffer (buffer-string)))
               ;; Filter the full output to remove harmless "Followed link..." messages.
               (filtered-output
                (string-join
                 (cl-remove-if (lambda (line) (string-prefix-p "Followed link to" line))
                               (split-string output-text "\n" t))
                 "\n")))

          ;; Return a cons cell of (STATUS . FILTERED-OUTPUT)
          (cons (if (or (not (zerop exit-code))
                        (string-match-p "\\S-" filtered-output))
                    1 ; Failure
                  0)  ; Success
                filtered-output))

      ;; Cleanup: Kill the temporary buffer.
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))


;; --- Main Execution ---
(let ((exit-code 0))
  (dolist (pkg ci-packages)
    (message (format "\n--- Running checkdoc on %s ---" pkg))
    (let* ((result (ci-checkdoc-package pkg))
           (status (car result))
           ;; The output is now the pre-filtered combined output.
           (output (cdr result)))

      ;; Print the captured (and filtered) output.
      (when (string-match-p "\\S-" output)
        (princ output)
        (princ "\n"))

      (unless (zerop status)
        (message (format "!!! Checkdoc failed for %s with status %d" pkg status))
        (setq exit-code status))))

  (if (zerop exit-code)
      (message "\nAll packages passed checkdoc.")
    (progn
      (message "\nCheckdoc process completed with some errors.")
      (kill-emacs exit-code))))
