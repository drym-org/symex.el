;; Modified from flycheck build scripts at: https://github.com/flycheck/flycheck/

(require 'subr-x)

(defun flycheck/checkdoc-get-current-errors ()
  "Get the current checkdoc errors.
Return a list of all error messages from checkdoc, and erase the
error message buffer, so that the next checkdoc check starts
fresh without previous errors.
Each error is just a string with the complete human-readable
location and error message."
  (with-current-buffer checkdoc-diagnostic-buffer
    (unwind-protect
        (progn
          (goto-char (point-min))
          ;; Skip over the checkdoc header
          (re-search-forward (rx line-start "***" (1+ not-newline)
                                 ": checkdoc-current-buffer"))
          (forward-line 1)
          (let ((text (buffer-substring-no-properties (point) (point-max))))
            (and (not (string-empty-p text))
                 (split-string text "\n"))))
      (kill-buffer))))

(defun flycheck/checkdoc-file (filename)
  "Run checkdoc on FILENAME and return a list of errors.
Each error is just a string with the complete human-readable
location and error message."
  (with-temp-buffer
    ;; Visit the file to make sure that the filename is set, as some checkdoc
    ;; lints only apply for buffers with filenames
    (insert-file-contents filename 'visit)
    (set-buffer-modified-p nil)
    ;; Switch to Emacs Lisp mode to give checkdoc the proper syntax table, etc.
    (delay-mode-hooks (emacs-lisp-mode))
    (setq delay-mode-hooks nil)
    (let ((checkdoc-arguments-in-order-flag nil))
      (checkdoc-current-buffer 'take-notes))
    (flycheck/checkdoc-get-current-errors)))

(defun flycheck/collect-el-files (directory &optional recursive)
  "Collect all Emacs Lisp files in DIRECTORY.
If RECURSIVE is given and non-nil collect files recursively."
  (let ((fn-re (rx ".el" eos)))
    (if recursive
        (directory-files-recursively directory fn-re)
      (directory-files directory 'full fn-re))))

(defun flycheck/batch-checkdoc (directory)
  "Run checkdoc on all source files and exit."
  (let ((errors (seq-mapcat #'flycheck/checkdoc-file
                            (flycheck/collect-el-files directory))))
    (seq-do (lambda (err) (message "%s" err)) errors)
    (kill-emacs (if errors 1 0))))
