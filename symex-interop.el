;;; symex-interop.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Interoperability with editing workflows, specifically towards supporting
;; either evil-based or vanilla Emacs editing in conjunction with
;; symex mode.
;;

;;; Code:

(defun symex-escape-higher ()
  "Exit symex mode via an 'escape'."
  (interactive)
  (cond ((and (boundp 'epistemic-mode)
              epistemic-mode)
         (when (fboundp 'eem-enter-higher-level)
            (eem-enter-higher-level)))
        ((and (boundp 'evil-mode)
              evil-mode)
         (evil-normal-state))
        (t (evil-emacs-state))))

(defun symex-enter-lower ()
  "Exit symex mode via an 'enter'."
  (interactive)
  (cond ((and (boundp 'epistemic-mode)
              epistemic-mode)
         (when (fboundp 'eem-enter-lower-level)
           (eem-enter-lower-level)))
        ((and (boundp 'evil-mode)
              evil-mode)
         (evil-insert-state))
        (t (evil-emacs-state))))

(defun symex-enter-lowest ()
  "Enter the lowest (manual) editing level."
  (interactive)
  (cond ((and (boundp 'epistemic-mode)
              epistemic-mode)
         (when (fboundp 'eem-enter-lowest-level)
           (eem-enter-lowest-level)))
        ((and (boundp 'evil-mode)
              evil-mode)
         (evil-insert-state))
        (t (evil-emacs-state))))


(provide 'symex-interop)
;;; symex-interop.el ends here
