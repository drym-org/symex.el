;;; symex-transformations.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6.1") (lispy "0.26.0") (paredit "24") (evil-cleverparens "20170718.413") (dash-functional "2.15.0") (evil "20180914.1216") (smartparens "20181007.1501") (racket-mode "20181030.1345") (geiser "0.10") (evil-surround "20180102.1401") (hydra "20180918.1529"))

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.

;;; Commentary:
;;
;; Standard mutative operations to be performed on symexes.
;;

;;; Code:


(require 'paredit)
(require 'lispy)
(require 'evil)
(require 'evil-surround)
(require 'smartparens)
(require 'symex-utils)
(require 'symex-misc)

;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSFORMATIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-delete ()
  "Delete symex."
  (interactive)
  (sp-kill-sexp nil)
  (cond ((or (current-line-empty-p)  ; ^<>$
             (save-excursion (back-to-indentation)  ; ^(<>
                             (forward-char)
                             (lispy-right-p)))
         (progn (evil-previous-line)
                (symex-join-lines)))
        ((save-excursion (evil-last-non-blank)  ; (<>$
                         (lispy-left-p))
         (sp-next-sexp)
         (save-excursion
           (symex-join-lines t)))
        ((looking-at-p "\n")  ; (abc <>
         (evil-join (line-beginning-position)
                    (line-end-position)))
        (t (fixup-whitespace)))
  (symex-select-nearest)
  (symex-tidy))

(defun symex-change ()
  "Change symex."
  (interactive)
  (kill-sexp 1)
  (evil-insert-state))

(defun symex-replace ()
  "Replace contents of symex."
  (interactive)
  (let ((move (symex-go-in)))
    (if move
        (apply #'evil-change (evil-inner-paren))  ; TODO: dispatch on paren type
      (sp-kill-sexp nil)
      (evil-insert-state))))

(defun symex-clear ()
  "Clear contents of symex."
  (interactive)
  (let ((move (symex-go-in)))
    (if move
        (apply #'evil-delete (evil-inner-paren))  ; TODO: dispatch on paren type
      (sp-kill-sexp nil))
    (symex-select-nearest)
    (symex-tidy)))

(defun symex-spit-backward ()
  "Spit backward."
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-in)  ; need to be inside the symex to spit and slurp
      (paredit-backward-barf-sexp 1))
    (symex-go-forward)
    (when (symex-empty-list-p)
      (fixup-whitespace)
      (re-search-forward lispy-left)
      (symex-go-out))))

(defun symex-spit-forward ()
  "Spit forward."
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-in)  ; need to be inside the symex to spit and slurp
      (paredit-forward-barf-sexp 1))
    (when (symex-empty-list-p)
      (symex-go-forward)
      (fixup-whitespace)
      (re-search-backward lispy-left))))

(defun symex-slurp-backward ()
  "Slurp from behind."
  (interactive)
  (when (lispy-left-p)
    (if (symex-empty-list-p)
        (forward-char)
      (symex-go-in))  ; need to be inside the symex to spit and slurp
    (paredit-backward-slurp-sexp 1)
    (fixup-whitespace)
    (symex-go-out)))

(defun symex-slurp-forward ()
  "Slurp from the front."
  (interactive)
  (when (lispy-left-p)
    (save-excursion
      (if (symex-empty-list-p)
          (forward-char)
        (symex-go-in))  ; need to be inside the symex to spit and slurp
      (lispy-forward-slurp-sexp 1))))

(defun symex-join ()
  "Merge symexes at the same level."
  (interactive)
  (save-excursion
    (symex-go-forward)
    (paredit-join-sexps)))

(defun symex-join-lines (&optional backwards)
  "Join lines inside symex.

If BACKWARDS is true, then joins current symex to previous one, otherwise,
by default, joins next symex to current one."
  (interactive)
  (let ((original-column (current-column)))
    (save-excursion
      (if backwards
          (progn (evil-previous-line)
                 (if (current-line-empty-p)
                     (evil-join (line-beginning-position)
                                (1+ (line-beginning-position)))
                   (evil-join (line-beginning-position)
                              (line-end-position))))
        (forward-sexp)
        (evil-join (line-beginning-position)
                   (line-end-position))))
    (unless (= (current-column)
               original-column)
      (forward-char))))

(defun symex-yank ()
  "Yank (copy) symex."
  (interactive)
  (lispy-new-copy))

(defun symex-paste-before ()
  "Paste before symex."
  (interactive)
  (let ((extra-to-append
         (cond ((or (and (point-at-indentation-p)
                         (not (bolp)))
                    (save-excursion (forward-sexp)
                                    (eolp)))
                "\n")
               (t " "))))
    (with-undo-collapse
      (save-excursion
        (save-excursion
          (evil-paste-before nil nil)
          (forward-char)
          (insert extra-to-append))
        (symex-go-forward)
        (symex-tidy)))))

(defun symex-paste-after ()
  "Paste after symex."
  (interactive)
  (let ((extra-to-prepend
         (cond ((or (and (point-at-indentation-p)
                         (not (bolp)))
                    (save-excursion (forward-sexp)
                                    (eolp)))
                "\n")
               (t " "))))
    (with-undo-collapse
      (save-excursion
        (save-excursion
          (forward-sexp)
          (insert extra-to-prepend)
          (evil-paste-before nil nil)
          (forward-char))
        (symex-go-forward)
        (symex-tidy))
      (symex-go-forward))))

(defun symex-open-line-after ()
  "Open new line after symex."
  (interactive)
  (forward-sexp)
  (newline-and-indent)
  (evil-insert-state))

(defun symex-open-line-before ()
  "Open new line before symex."
  (interactive)
  (newline-and-indent)
  (evil-previous-line)
  (indent-according-to-mode)
  (evil-append-line 1))

(defun symex-append-after ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (forward-sexp)  ; selected symexes will have the cursor on the starting paren
  (evil-insert 1 nil nil))

(defun symex-insert-before ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (evil-insert 1 nil nil))

(defun symex-insert-at-beginning ()
  "Insert at beginning of symex."
  (interactive)
  (if (lispy-left-p)
      (evil-append 1 nil)
    (evil-insert 1 nil nil)))

(defun symex-insert-at-end ()
  "Insert at end of symex."
  (interactive)
  (if (lispy-left-p)
      (progn (forward-sexp)
             (backward-char)
             (evil-insert 1 nil nil))
    (progn (forward-sexp)
           (evil-insert 1 nil nil))))

(defun symex-create (type)
  "Create new symex (list).

New list delimiters are determined by the TYPE."
  (interactive)
  (save-excursion
    (cond ((equal type 'round)
           (insert "()"))
          ((equal type 'square)
           (insert "[]"))
          ((equal type 'curly)
           (insert "{}"))
          ((equal type 'angled)
           (insert "<>")))))

(defun symex-insert-newline ()
  "Insert newline and reindent symex."
  (interactive)
  (newline-and-indent)
  (symex-tidy))

(defun symex-append-newline ()
  "Append newline and reindent symex."
  (interactive)
  (save-excursion
    (forward-sexp)
    (newline-and-indent)
    (symex-tidy)))

(defun symex-swallow ()
  "Swallow symex, putting its contents in the parent symex."
  (interactive)
  (symex-go-in)
  (symex-go-forward)
  (paredit-splice-sexp-killing-backward))

(defun symex-wrap-round ()
  "Wrap with ()."
  (interactive)
  (paredit-wrap-round)
  (symex-go-out))

(defun symex-wrap-square ()
  "Wrap with []."
  (interactive)
  (paredit-wrap-square)
  (symex-go-out))

(defun symex-wrap-curly ()
  "Wrap with {}."
  (interactive)
  (paredit-wrap-curly)
  (evil-find-char-backward nil 123))

(defun symex-wrap-angled ()
  "Wrap with <>."
  (interactive)
  (paredit-wrap-angled)
  (evil-find-char-backward nil 60))

(defun symex-wrap ()
  "Wrap with containing symex."
  (interactive)
  (symex-wrap-round)
  (symex-insert-at-beginning))

(defun symex-shift-forward ()
  "Move symex forward in current tree level."
  (interactive)
  (forward-sexp)
  (condition-case nil
      (progn (transpose-sexps 1)
             (backward-sexp))
    (error (backward-sexp))))

(defun symex-shift-backward ()
  "Move symex backward in current tree level."
  (interactive)
  (let ((move (symex-go-backward)))
    (when move
      (symex-shift-forward)
      (symex-go-backward))))

(defun symex-change-delimiter ()
  "Change delimiter enclosing current symex, e.g. round -> square brackets."
  (interactive)
  (evil-surround-change (following-char)))

(defun symex-tidy ()
  "Auto-indent symex and fix any whitespace."
  (interactive)
  (fixup-whitespace)
  (when (save-excursion (looking-at-p "[[:space:]]"))
      (forward-char))
  (save-excursion
    (forward-sexp)
    (fixup-whitespace))
  (save-excursion
    (apply #'evil-indent
           (seq-take (evil-cp-a-form 1)
                     2)))
  (symex-select-nearest))

(defun symex-tidy-proper ()
  "Properly tidy things up."
  (interactive)
  (save-excursion
    (symex-execute-traversal (symex-make-circuit symex--traversal-preorder-in-tree))
    (let ((traversal (symex-make-circuit symex--traversal-postorder-in-tree)))
      (let ((result (symex-execute-traversal traversal
                                             nil
                                             #'symex-tidy)))
        (message "%s" result)
        result))))

(provide 'symex-transformations)
;;; symex-transformations.el ends here
