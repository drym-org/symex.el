;;; symex-interface-common-lisp.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Interface for the Common Lisp language

;;; Code:

(require 'slime      nil 'noerror)
(require 'slime-repl nil 'noerror)
(require 'sly        nil 'noerror)
(require 'sly-repl   nil 'noerror)
(require 'symex-interop)
(require 'symex-custom)

;; Make the bytecompiler aware of slime
(declare-function slime-eval-last-expression "ext:slime")
(declare-function slime-eval-defun           "ext:slime")
(declare-function slime-eval-buffer          "ext:slime")
(declare-function slime-repl                 "ext:slime-repl")
(declare-function slime-eval-print-last-expression "ext:slime")
(declare-function slime-documentation        "ext:slime")

;; Make the bytecompiler aware of sly
(declare-function sly-eval-last-expression "ext:sly")
(declare-function sly-eval-defun           "ext:sly")
(declare-function sly-eval-buffer          "ext:sly")
(declare-function sly-repl                 "ext:sly-repl")
(declare-function sly-eval-print-last-expression "ext:sly")
(declare-function sly-documentation        "ext:sly")

(defun symex-eval-common-lisp ()
  "Eval last sexp.

Accounts for different point location in evil vs Emacs mode."
  (interactive)
  (when (eq symex-common-lisp-backend 'sly)
    (sly-eval-last-expression))
  (slime-eval-last-expression))

(defun symex-eval-definition-common-lisp ()
  "Eval entire containing definition."
  (when (eq symex-common-lisp-backend 'sly)
    (sly-eval-defun))
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
  (call-interactively
   (if (eq symex-common-lisp-backend 'sly)
       #'sly-eval-print-last-expression
       #'slime-eval-print-last-expression)))

(defun symex-describe-symbol-common-lisp ()
  "Describe symbol at point."
  (interactive)
  (call-interactively
   (if (eq symex-common-lisp-backend 'sly)
       #'sly-documentation
       #'slime-documentation)))

(defun symex-repl-common-lisp ()
  "Go to REPL."
  ;; this already goes to the active repl prompt
  ;; so there's no need to move point there
  (if (eq symex-common-lisp-backend 'sly)
      (sly-repl)
      (slime-repl))
  (symex-enter-lowest))

(defun symex-run-common-lisp ()
  "Evaluate buffer."
  (when (eq symex-common-lisp-backend 'sly)
    (sly-eval-buffer))
  (slime-eval-buffer))


(provide 'symex-interface-common-lisp)
;;; symex-interface-common-lisp.el ends here
