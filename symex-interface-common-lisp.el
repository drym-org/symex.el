;;; symex-interface-common-lisp.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Interface for the Common Lisp language
;;

;;; Code:

(require 'slime)
(require 'slime-repl)

(defun symex-eval-common-lisp ()
  "Eval last sexp.

Accounts for different point location in evil vs Emacs mode."
  (interactive)
  (slime-eval-last-expression))

(defun symex-eval-definition-common-lisp ()
  "Eval entire containing definition."
  (slime-eval-defun))

(defun symex-eval-pretty-common-lisp ()
  "Evaluate symex and render the result in a useful string form."
  (interactive)
  (symex-eval-common-lisp))

(defun symex-eval-thunk-common-lisp ()
  "Evaluate symex as a 'thunk,' i.e. as a function taking no arguments."
  (interactive)
  ;; can use slime-interactive-eval
  (message "eval as thunk currently not supported for Common Lisp"))

(defun symex-eval-print-common-lisp ()
  "Eval symex and print result in buffer."
  (interactive)
  (call-interactively 'slime-eval-print-last-expression))

(defun symex-describe-symbol-common-lisp ()
  "Describe symbol at point."
  (interactive)
  (call-interactively 'slime-documentation))

(defun symex-repl-common-lisp ()
  "Go to REPL."
  (slime-repl))

(defun symex-run-common-lisp ()
  "Evaluate buffer."
  (slime-eval-buffer))


(provide 'symex-interface-common-lisp)
;;; symex-interface-common-lisp.el ends here
