;;; symex-interop.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Interoperability with editing workflows, specifically towards supporting
;; either evil-based or vanilla Emacs editing in conjunction with
;; symex mode.

;;; Code:

;; NOTE: this module is part of symex-core, for now. We should ideally
;; eliminate it, maybe by moving a lot of this into user config, or a
;; separate package for each integration, e.g., symex-rigpa,
;; symex-evil
(require 'symex-custom)
(require 'symex-primitives)

(defvar-local symex--original-scroll-margin nil)
(defvar-local symex--original-max-scroll-margin nil)

;; Note that these three functions are overridden
;; in packages like symex-evil and symex-rigpa
(defun symex-escape-higher ()
  "Exit symex mode via an \"escape\"."
  (interactive)
  (cond ((fboundp 'symex-user-defined-higher-mode)
         (symex-user-defined-higher-mode))))

(defun symex-enter-lower ()
  "Exit symex mode via an \"enter\"."
  (interactive)
  (cond ((fboundp 'symex-user-defined-lower-mode)
         (symex-user-defined-lower-mode))))

(defun symex-enter-lowest ()
  "Enter the lowest (manual) editing level."
  (interactive)
  (cond ((fboundp 'symex-user-defined-lowest-mode)
         (symex-user-defined-lowest-mode))))

(defun symex--set-scroll-margin ()
  "Set a convenient scroll margin for symex mode, after storing the original value."
  (unless symex--original-scroll-margin
    ;; only set these the first time symex mode is entered in the buffer
    ;; do they need to be buffer-local, though?
    (setq-local symex--original-scroll-margin scroll-margin)
    (setq-local symex--original-max-scroll-margin maximum-scroll-margin))
  (setq-local scroll-margin 9999)
  (setq-local maximum-scroll-margin 0.368))

(defun symex--restore-scroll-margin ()
  "Restore original `scroll-margin` (e.g. upon symex exit)."
  (setq-local scroll-margin symex--original-scroll-margin)
  (setq-local maximum-scroll-margin symex--original-max-scroll-margin))


(provide 'symex-interop)
;;; symex-interop.el ends here
