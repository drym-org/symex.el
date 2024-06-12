;;; symex-interface-builtins.el --- Builtin symex interface extensions -*- lexical-binding: t; -*-

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

;; Builtin symex interface extensions

;;; Code:

(require 'symex-interface-elisp)
(require 'symex-interface-racket)
(require 'symex-interface-scheme)
(require 'symex-interface-clojure)
(require 'symex-interface-common-lisp)
(require 'symex-interface-arc)
(require 'symex-interface-fennel)

(defun symex-register-builtin-interfaces ()
  "Register built-in interfaces."
  (symex-interface-register-elisp)
  (symex-interface-register-racket)
  (symex-interface-register-scheme)
  (symex-interface-register-clojure)
  (symex-interface-register-common-lisp)
  (symex-interface-register-arc)
  (symex-interface-register-fennel))

(provide 'symex-interface-builtins)
;;; symex-interface-builtins.el ends here
