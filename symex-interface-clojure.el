;;; symex-interface-clojure.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Interface for the Clojure language
;;

;;; Code:

(require 'cider)

(defun symex-eval-clojure ()
  "Eval last sexp.

Accounts for different point location in evil vs Emacs mode."
  (interactive)
  (cider-eval-last-sexp))

(defun symex-eval-definition-clojure ()
  "Eval entire containing definition."
  (cider-eval-defun-at-point nil))

(defun symex-eval-pretty-clojure ()
  "Evaluate symex and render the result in a useful string form."
  (interactive)
  (symex-eval-clojure))

(defun symex-eval-thunk-clojure ()
  "Evaluate symex as a 'thunk,' i.e. as a function taking no arguments."
  (interactive)
  (message "eval as thunk currently not supported for Clojure"))

(defun symex-eval-print-clojure ()
  "Eval symex and print result in buffer."
  (interactive)
  (cider-eval-print-last-sexp))

(defun symex-describe-symbol-clojure ()
  "Describe symbol at point."
  (interactive)
  (cider-doc nil))

(defun symex-repl-clojure ()
  "Go to REPL."
  (cider-switch-to-repl-buffer))

(defun symex-run-clojure ()
  "Evaluate buffer."
  (cider-eval-buffer))


(provide 'symex-interface-clojure)
;;; symex-interface-clojure.el ends here
