;;; symex-interface-scheme.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex.el

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
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
;;
;; Interface for the Scheme language
;;

;;; Code:


(require 'geiser-mode)


(defun symex-eval-scheme ()
  "Eval Scheme symex."
  (interactive)
  (geiser-eval-last-sexp nil))

(defun symex-eval-definition-scheme ()
  "Eval entire containing definition."
  (geiser-eval-definition nil))

(defun symex-eval-pretty-scheme ()
  "Evaluate symex and render the result in a useful string form."
  (interactive)
  (symex-eval-scheme))

(defun symex-eval-thunk-scheme ()
  "Evaluate symex as a 'thunk,' i.e. as a function taking no arguments."
  (interactive)
  (message "eval as thunk currently not supported for Scheme"))

(defun symex-eval-print-scheme ()
  "Eval symex and print result in buffer."
  (interactive)
  nil)

(defun symex-describe-symbol-scheme ()
  "Describe symbol at point."
  (interactive)
  (geiser-doc-symbol-at-point))

(defun symex-repl-scheme ()
  "Go to REPL."
  (geiser-mode-switch-to-repl nil))

(defun symex-run-scheme ()
  "Evaluate buffer."
  (geiser-eval-buffer nil))


(provide 'symex-interface-scheme)
;;; symex-interface-scheme.el ends here
