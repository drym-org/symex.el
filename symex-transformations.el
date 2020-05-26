;;; symex-transformations.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Standard mutative operations to be performed on symexes.
;;

;;; Code:


(require 'paredit)
(require 'lispy)
(require 'evil)
(require 'evil-surround)
(require 'evil-cleverparens)
(require 'smartparens)
(require 'symex-utils)
(require 'symex-misc)
(require 'symex-traversals)
(require 'symex-interop)

;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSFORMATIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun symex--do-while-traversing (operation traversal)
  "Traverse a symex using TRAVERSAL and do OPERATION at each step."
  (let ((result (symex-execute-traversal traversal
                                         nil
                                         operation)))
    (message "%s" result)
    (when result
      (symex--do-while-traversing operation
                                  traversal))))

(defun symex-eval-recursive ()
  "Evaluate a symex recursively.

Eval starting at the leaves and proceed down to the root, similarly
to how the Lisp interpreter does it (when it is following
'applicative-order evaluation')."
  (interactive)
  (save-excursion
    (symex-execute-traversal (symex-traversal
                              (circuit symex--traversal-preorder-in-tree))
                             nil
                             #'symex-evaluate)
    (symex--do-while-traversing #'symex-evaluate
                                symex--traversal-postorder-in-tree)))

(defun symex-delete ()
  "Delete symex."
  (interactive)
  (kill-sexp 1)
  (cond ((symex--current-line-empty-p)             ; ^<>$
         (progn (symex-go-backward)
                (symex-join-lines)
                (symex-go-forward)))
        ((save-excursion (back-to-indentation)     ; ^<>)
                         (forward-char)
                         (lispy-right-p))
         (progn (symex-go-backward)
                (symex-join-lines)))
        ((save-excursion (evil-last-non-blank)  ; (<>$
                         (lispy-left-p))
         (sp-next-sexp)
         (save-excursion
           (symex-join-lines t)))
        ((looking-at-p "\n")  ; (abc <>
         (evil-join (line-beginning-position)
                    (line-end-position)))
        ((save-excursion (forward-char)  ; ... <>)
                         (lispy-right-p))
         (symex-go-backward))
        (t (fixup-whitespace)))
  (symex-select-nearest)
  (symex-tidy))

(defun symex-change ()
  "Change symex."
  (interactive)
  (kill-sexp 1)
  (symex-enter-lowest))

(defun symex-replace ()
  "Replace contents of symex."
  (interactive)
  (let ((move (symex-go-up)))
    (if move
        (apply #'evil-change (evil-inner-paren))  ; TODO: dispatch on paren type
      (sp-kill-sexp nil)
      (symex-enter-lowest))))

(defun symex-clear ()
  "Clear contents of symex."
  (interactive)
  (let ((move (symex-go-up)))
    (if move
        (apply #'evil-delete (evil-inner-paren))  ; TODO: dispatch on paren type
      (sp-kill-sexp nil))
    (symex-select-nearest)
    (symex-tidy)))

(defun symex-emit-backward ()
  "Emit backward."
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-up)  ; need to be inside the symex to emit and capture
      (paredit-backward-barf-sexp 1))
    (symex-go-forward)
    (when (symex-empty-list-p)
      (fixup-whitespace)
      (re-search-forward lispy-left)
      (symex-go-down))))

(defun symex-emit-forward ()
  "Emit forward."
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-up)  ; need to be inside the symex to emit and capture
      (paredit-forward-barf-sexp 1))
    (when (symex-empty-list-p)
      (symex-go-forward)
      (fixup-whitespace)
      (re-search-backward lispy-left))))

(defun symex-capture-backward ()
  "Capture from behind."
  (interactive)
  (when (lispy-left-p)
    (if (symex-empty-list-p)
        (forward-char)
      (symex-go-up))  ; need to be inside the symex to emit and capture
    (paredit-backward-slurp-sexp 1)
    (fixup-whitespace)
    (symex-go-down)))

(defun symex-capture-forward ()
  "Capture from the front."
  (interactive)
  (when (lispy-left-p)
    (save-excursion
      (if (symex-empty-list-p)
          (forward-char)
        (symex-go-up))  ; need to be inside the symex to emit and capture
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
    (if backwards
        (progn (evil-previous-line)
               (if (symex--current-line-empty-p)
                   (evil-join (line-beginning-position)
                              (1+ (line-beginning-position)))
                 (evil-join (line-beginning-position)
                            (line-end-position))))
      (save-excursion (forward-sexp)
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
         (cond ((or (and (symex--point-at-indentation-p)
                         (not (bolp)))
                    (save-excursion (forward-sexp)
                                    (eolp)))
                "\n")
               (t " "))))
    (symex--with-undo-collapse
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
         (cond ((or (and (symex--point-at-indentation-p)
                         (not (bolp)))
                    (save-excursion (forward-sexp)
                                    (eolp)))
                "\n")
               (t " "))))
    (symex--with-undo-collapse
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
  (symex-enter-lowest))

(defun symex-open-line-before ()
  "Open new line before symex."
  (interactive)
  (newline-and-indent)
  (evil-previous-line)
  (indent-according-to-mode)
  (evil-move-end-of-line)
  (symex-enter-lowest))

(defun symex-append-after ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (forward-sexp)  ; selected symexes will have the cursor on the starting paren
  (symex-enter-lowest))

(defun symex-insert-before ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (symex-enter-lowest))

(defun symex-insert-at-beginning ()
  "Insert at beginning of symex."
  (interactive)
  (if (lispy-left-p)
      (progn (forward-char)
             (symex-enter-lowest))
    (symex-enter-lowest)))

(defun symex-insert-at-end ()
  "Insert at end of symex."
  (interactive)
  (if (lispy-left-p)
      (progn (forward-sexp)
             (backward-char)
             (symex-enter-lowest))
    (progn (forward-sexp)
           (symex-enter-lowest))))

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
  "Swallow symex.

This consumes the head of the symex, putting the rest of its contents
in the parent symex."
  (interactive)
  (symex-go-up)
  (symex-go-forward)
  (paredit-splice-sexp-killing-backward))

(defun symex-swallow-tail ()
  "Swallow-tail symex.

This consumes the tail of the symex, putting the head
in the parent symex."
  (interactive)
  (symex-go-up)
  (symex-go-forward)
  (paredit-splice-sexp-killing-forward)
  (symex-go-backward)
  (symex-tidy))

(defun symex-splice ()
  "Splice or 'clip' symex.

If the symex is a nested list, this operation eliminates the symex,
putting its contents in the parent symex.  If the symex is an atom,
then no action is taken."
  (interactive)
  (when (lispy-left-p)
    (symex-go-up)
    (paredit-splice-sexp-killing-backward)))

(defun symex-wrap-round ()
  "Wrap with ()."
  (interactive)
  (paredit-wrap-round)
  (symex-go-down))

(defun symex-wrap-square ()
  "Wrap with []."
  (interactive)
  (paredit-wrap-square)
  (symex-go-down))

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

(defun symex-comment ()
  "Comment out symex."
  (interactive)
  (mark-sexp)
  (comment-dwim nil))

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
    (symex-execute-traversal (symex-traversal
                              (circuit symex--traversal-preorder-in-tree))
                             nil
                             #'symex-tidy)
    (symex--do-while-traversing #'symex-tidy
                                symex--traversal-postorder-in-tree)))

(provide 'symex-transformations)
;;; symex-transformations.el ends here
