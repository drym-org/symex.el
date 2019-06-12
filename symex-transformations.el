;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSFORMATIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-delete ()
  "Delete symex"
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
  "Change symex"
  (interactive)
  (kill-sexp 1)
  (evil-insert-state))

(defun symex-replace ()
  "Replace contents of symex"
  (interactive)
  (let ((move (symex-go-in)))
    (if move
        (apply #'evil-change (evil-inner-paren))  ; TODO: dispatch on paren type
      (sp-kill-sexp nil)
      (evil-insert-state))))

(defun symex-spit-backward ()
  "Spit backward"
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
  "Spit forward"
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
  "Slurp from behind"
  (interactive)
  (when (lispy-left-p)
    (if (symex-empty-list-p)
        (forward-char)
      (symex-go-in))  ; need to be inside the symex to spit and slurp
    (paredit-backward-slurp-sexp 1)
    (fixup-whitespace)
    (symex-go-out)))

(defun symex-slurp-forward ()
  "Slurp from the front"
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
  "Join lines inside symex."
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
  "Paste before symex"
  (interactive)
  (cond ((or (and (point-at-indentation-p)
                  (not (bolp)))
             (save-excursion (forward-sexp)
                             (eolp)))
         (setq extra-to-append "\n"))
        (t (setq extra-to-append " ")))
  (with-undo-collapse
    (save-excursion
      (save-excursion
        (evil-paste-before nil nil)
        (forward-char)
        (insert extra-to-append))
      (symex-go-forward)
      (symex-tidy))))

(defun symex-paste-after ()
  "Paste after symex"
  (interactive)
  (cond ((or (and (point-at-indentation-p)
                  (not (bolp)))
             (save-excursion (forward-sexp)
                             (eolp)))
         (setq extra-to-prepend "\n"))
        (t (setq extra-to-prepend " ")))
  (with-undo-collapse
    (save-excursion
      (save-excursion
        (forward-sexp)
        (insert extra-to-prepend)
        (evil-paste-before nil nil)
        (forward-char))
      (symex-go-forward)
      (symex-tidy))
    (symex-go-forward)))

(defun symex-open-line-after ()
  "Open new line after symex"
  (interactive)
  (forward-sexp)
  (newline-and-indent)
  (evil-insert-state))

(defun symex-open-line-before ()
  "Open new line before symex"
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
  "Create new symex (list)."
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

(defun symex-swallow ()
  "Swallow symex, putting its contents in the parent symex."
  (interactive)
  (symex-go-in)
  (symex-go-forward)
  (paredit-splice-sexp-killing-backward))

(defun symex-wrap-round ()
  "Wrap with ()"
  (interactive)
  (paredit-wrap-round)
  (symex-go-out))

(defun symex-wrap-square ()
  "Wrap with []"
  (interactive)
  (paredit-wrap-square)
  (symex-go-out))

(defun symex-wrap-curly ()
  "Wrap with {}"
  (interactive)
  (paredit-wrap-curly)
  (evil-find-char-backward nil 123))

(defun symex-wrap-angled ()
  "Wrap with <>"
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

(provide 'symex-transformations)
