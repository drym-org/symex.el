;;; symex-evil.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/drym-org/symex.el

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

;; Evil-related things

;;; Code:

;; NOTE: this module is part of symex-mode
(require 'evil nil :no-error)
(require 'symex-motions)
(require 'symex-lithium)

(defun symex-select-nearest-advice (&rest _)
  "Advice to select the nearest symex."
  (when symex-editing-mode
    (symex-user-select-nearest)))

(defun symex-initialize-evil ()
  "Initialize evil interop.

This includes the repeat command (uses evil-repeat), and preserving
the evil state at symex after running evil commands which may attempt
to set the state to Normal."
  (when (fboundp 'undo-tree-undo)
    (advice-add #'undo-tree-undo :after #'symex-select-nearest-advice))
  (when (fboundp 'undo-tree-redo)
    (advice-add #'undo-tree-redo :after #'symex-select-nearest-advice)))

(defun symex-disable-evil ()
  "Disable evil interop."
  (when (fboundp 'undo-tree-undo)
    (advice-remove #'undo-tree-undo #'symex-select-nearest-advice))
  (when (fboundp 'undo-tree-redo)
    (advice-remove #'undo-tree-redo #'symex-select-nearest-advice)))

(provide 'symex-evil)
;;; symex-evil.el ends here
