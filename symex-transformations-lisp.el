;;; symex-transformations-lisp.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Standard mutative operations to be performed on Lisp symexes.

;;; Code:

(require 'cl-lib)

(require 'paredit)
(require 'symex-primitives)
(require 'symex-primitives-lisp)
(require 'symex-utils)
(require 'symex-traversals)
(require 'symex-interop)

;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSFORMATIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-lisp-clear ()
  "Helper to clear contents of symex."
  (cond ((symex-lisp-string-p) (save-excursion (kill-sexp)
                                               (insert "\"\"")))
        ((or (symex-empty-list-p)
             (symex--special-empty-list-p)
             ;; for consistency with treatment of ()
             ;; we also don't do anything for atoms
             (symex-lisp-atom-p))
         ;; nothing needs to be done
         nil)
        (t (kill-region (1+ (point))
                        (1- (symex-lisp--get-end-point 1))))))

(defun symex-lisp-replace ()
  "Replace contents of symex."
  (symex-lisp-clear)
  (forward-char (symex--form-offset)))

(defun symex-lisp-append-after ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (forward-sexp)  ; selected symexes will have the cursor on the starting paren
  (insert " "))

(defun symex-lisp-open-line-after ()
  "Open new line after symex."
  (interactive)
  (forward-sexp)
  (if (symex-inline-comment-p)
      (progn (end-of-line)
             (newline-and-indent))
    (newline-and-indent)))

(defun symex-lisp-open-line-before ()
  "Open new line before symex."
  (interactive)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  (move-end-of-line 1)
  (unless (or (symex--current-line-empty-p)
              (save-excursion (backward-char)
                              (symex-left-p)))
    (insert " ")))

(defun symex-lisp-insert-before ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (insert " ")
  (backward-char))

(defun symex-lisp-insert-at-beginning ()
  "Insert at beginning of symex."
  (interactive)
  (when (or (symex-left-p)
            (symex-lisp-string-p))
    (forward-char)))

(defun symex-lisp-insert-at-end ()
  "Insert at end of symex."
  (interactive)
  (if (or (symex-left-p)
          (symex-lisp-string-p))
      (progn (forward-sexp)
             (backward-char))
    (forward-sexp)))

(defun symex-lisp--emit-backward ()
  "Emit backward."
  (when (and (symex-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex--go-up)  ; need to be inside the symex to emit and capture
      (paredit-backward-barf-sexp 1))
    (symex--go-forward)
    (when (symex-empty-list-p)
      (fixup-whitespace)
      (re-search-forward symex--re-left)
      (symex--go-down))))

(defun symex-lisp-emit-backward (count)
  "Emit backward COUNT times."
  (dotimes (_ count)
    (symex-lisp--emit-backward)))

(defun symex-lisp--emit-forward ()
  "Emit forward."
  (when (and (symex-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex--go-up)  ; need to be inside the symex to emit and capture
      (paredit-forward-barf-sexp 1))
    (when (symex-empty-list-p)
      (symex--go-forward)
      (fixup-whitespace)
      (re-search-backward symex--re-left))))

(defun symex-lisp-emit-forward (count)
  "Emit forward COUNT times."
  (dotimes (_ count)
    (symex-lisp--emit-forward)))

(defun symex-lisp--capture-backward ()
  "Capture from behind."
  (when (and (symex-left-p)
             ;; paredit captures 1 ((|2 3)) -> (1 (2 3)) but we don't
             ;; want to in this case since point indicates the inner
             ;; symex, which cannot capture, rather than the outer
             ;; one. We avoid this by employing a guard condition here.
             (not (symex--point-at-first-symex-p)))
    (if (symex-empty-list-p)
        (forward-char)
      (symex--go-up))  ; need to be inside the symex to emit and capture
    (paredit-backward-slurp-sexp 1)
    (fixup-whitespace)
    (symex--go-down)))

(defun symex-lisp-capture-backward (count)
  "Capture from behind, COUNT times."
  (dotimes (_ count)
    (symex-lisp--capture-backward)))

(defun symex-lisp--capture-forward ()
  "Capture from the front."
  (when (and (symex-left-p)
             (not (save-excursion
                    (symex-other)
                    (forward-char)
                    (symex-lisp--point-at-end-p))))
    (save-excursion
      (if (symex-empty-list-p)
          (forward-char)
        (symex--go-up))  ; need to be inside the symex to emit and capture
      (paredit-forward-slurp-sexp 1))))

(defun symex-lisp-capture-forward (count)
  "Capture from the front, COUNT times."
  (dotimes (_ count)
    (symex-lisp--capture-forward)))

(defun symex-lisp-split ()
  "Split symex into two."
  (paredit-split-sexp)
  (forward-char))

(defun symex-lisp-join ()
  "Merge symexes at the same level."
  (save-excursion
    (when (symex--go-forward)
      (paredit-join-sexps))))

(defun symex-lisp-swallow ()
  "Swallow the head of the symex.

This consumes the head of the symex, putting the rest of its contents
in the parent symex."
  (save-excursion
    (symex--go-up)
    (symex--go-forward)
    (paredit-splice-sexp-killing-backward)))

(defun symex-lisp-swallow-tail ()
  "Swallow the tail of the symex.

This consumes the tail of the symex, putting the head
in the parent symex."
  (save-excursion
    (symex--go-up)
    (symex--go-forward)
    (paredit-splice-sexp-killing-forward)
    (symex--go-backward)))

(defun symex-lisp-append-newline (count)
  "Append COUNT newlines after symex."
  (save-excursion
    (forward-sexp)
    (newline-and-indent count)
    (fixup-whitespace))
  (symex--same-line-tidy-affected))

(provide 'symex-transformations-lisp)
;;; symex-transformations-lisp.el ends here
