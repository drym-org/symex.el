;;; symex-rigpa.el --- Use Symex with Rigpa -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/drym-org/symex.el
;; Version: 0.0
;; Package-Requires: ((emacs "25.1") (symex "2.0"))
;; Keywords: lisp, convenience, languages

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

;; Use Symex with Rigpa.

;;; Code:

(require 'rigpa)
(require 'symex)

;; These override the definitions in the symex package, so this
;; package (symex-evil) should be loaded after symex
(defun symex-escape-higher ()
  "Exit symex mode via an \"escape\"."
  (interactive)
  (rigpa-enter-higher-level))

(defun symex-enter-lower ()
  "Exit symex mode via an \"enter\"."
  (interactive)
  (rigpa-enter-lower-level))

(defun symex-enter-lowest ()
  "Enter the lowest (manual) editing level."
  (interactive)
  (rigpa-enter-lowest-level))

;;;###autoload
(defun symex-rigpa-initialize ()
  "Rigpa interconnects for Symex."
  nil)


(provide 'symex-rigpa)
;;; symex-rigpa.el ends here
