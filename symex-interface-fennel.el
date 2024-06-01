;;; symex-interface-fennel.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex.el

;; This program is "part of the world," in the sense described at
;; https://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.
;;
;; This work transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:

;; Interface for the Fennel language

;;; Code:

(require 'fennel-mode nil 'noerror)
(require 'symex-interop)


(declare-function fennel-find-definition  "ext:fennel-mode")
(declare-function fennel-repl "ext:fennel-mode")

(defun symex-eval-fennel ()
  "Eval last sexp.

Accounts for different point location in evil vs Emacs mode."
  (interactive)
  (lisp-eval-last-sexp))

(defun symex-eval-definition-fennel ()
  "Eval entire containing definition."
  (lisp-eval-defun))

(defun symex-eval-pretty-fennel ()
  "Evaluate symex and render the result in a useful string form."
  (interactive)
  (lisp-eval-last-sexp))

(defun symex-eval-thunk-fennel ()
  "Evaluate symex as a 'thunk,' i.e. as a function taking no arguments."
  (interactive)
  nil)

(defun symex-eval-print-fennel ()
  "Eval symex and print result in buffer."
  (interactive)
  nil)

(defun symex-describe-symbol-fennel ()
  "Describe symbol at point."
  (interactive)
  (fennel-find-definition))

(defun symex-repl-fennel ()
  "Go to REPL."
  (let ((original-window (selected-window)))
    (fennel-repl fennel-program)
    (unless (eq original-window (selected-window))
      ;; if the REPL window is currently being created
      ;; then don't attempt to go to the bottom
      (goto-char (point-max))
      (symex-enter-lowest))))

(defun symex-run-fennel ()
  "Evaluate buffer."
  (error "Not implemented"))


(provide 'symex-interface-fennel)
;;; symex-interface-fennel.el ends here
