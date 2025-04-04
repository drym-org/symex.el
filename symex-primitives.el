;;; symex-primitives.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Primitive navigations using third party libraries.  This layer of primitives
;; can be swapped out without changing the higher-level functionality provided
;; by the Symex DSL.  In the future it may make sense to directly operate on
;; an annotated representation of the AST here containing a mapping to buffer
;; positions and other metadata relevant for editing purposes.

;;; Code:


(require 'symex-primitives-lisp)
(require 'symex-ts)

;;; User Interface

(defun symex--adjust-point ()
  "Helper to adjust point to indicate the correct symex."
  (if (symex-ts-available-p)
      (symex-ts--adjust-point)
    (symex-lisp--adjust-point)))

;;; Predicates

(defun symex--point-at-root-symex-p ()
  "Check if point is at a root symex."
  (if (symex-ts-available-p)
      ;; note that tree-sitter has a global
      ;; root for the whole file -- that's
      ;; not the one we mean here, but
      ;; rather, top level definitions
      (symex-ts--at-tree-root-p)
    (symex-lisp--point-at-root-symex-p)))

(defun symex--point-at-first-symex-p ()
  "Check if point is at the first symex at some level."
  (if (symex-ts-available-p)
      (symex-ts--at-first-p)
    (symex-lisp--point-at-first-symex-p)))

(defun symex--point-at-last-symex-p ()
  "Check if point is at the last symex at some level."
  (if (symex-ts-available-p)
      (symex-ts--at-last-p)
    (symex-lisp--point-at-last-symex-p)))

(defun symex--point-at-final-symex-p ()
  "Check if point is at the last symex in the buffer."
  (if (symex-ts-available-p)
      (symex-ts--at-final-p)
    (symex-lisp--point-at-final-symex-p)))

(defun symex--point-at-initial-symex-p ()
  "Check if point is at the first symex in the buffer."
  (if (symex-ts-available-p)
      (symex-ts--at-initial-p)
    (symex-lisp--point-at-initial-symex-p)))

(defun symex--point-at-start-p ()
  "Check if point is at the start of a symex."
  (if (symex-ts-available-p)
      (symex-ts--point-at-start-p)
    (symex-lisp--point-at-start-p)))

(defun symex--following-line-empty-p ()
  "Check if the line following the current expression is empty."
  (save-excursion
    (symex-select-end 1)
    (forward-line)
    (symex--current-line-empty-p)))

(defun symex--previous-p ()
  "Check if a preceding symex exists at this level."
  (if (symex-ts-available-p)
      (symex-ts--previous-p)
    (symex-lisp--previous-p)))

(defun symex--next-p ()
  "Check if a succeeding symex exists at this level."
  (if (symex-ts-available-p)
      (symex-ts--next-p)
    (symex-lisp--next-p)))

(defun symex-atom-p ()
  "Check if the selected symex is an atom."
  (if (symex-ts-available-p)
      (symex-ts-atom-p)
    (symex-lisp-atom-p)))

;;; Navigation

(defun symex--go-forward (&optional count)
  "Forward symex.

Go forward COUNT times, defaulting to one.

This is a Lisp motion primitive.  It is an internal utility that avoids
any user-level concerns such as symex selection via advice.  This
should be used in all internal operations _above_ the primitive layer
\(e.g. favoring it over Emacs internal utilities like `forward-sexp`)
that are not primarily user-directed."
  (interactive)
  (if (symex-ts-available-p)
      (symex-ts-move-next-named-sibling count)
    (symex-lisp--forward count)))

(defun symex--go-backward (&optional count)
  "Backward symex.

Go backward COUNT times, defaulting to one.

This is a Lisp motion primitive.  It is an internal utility that avoids
any user-level concerns such as symex selection via advice.  This
should be used in all internal operations _above_ the primitive layer
\(e.g. favoring it over Emacs internal utilities like `forward-sexp`)
that are not primarily user-directed."
  (interactive)
  (if (symex-ts-available-p)
      (symex-ts-move-prev-named-sibling count)
    (symex-lisp--backward count)))

(defun symex--go-up (&optional count)
  "Enter higher symex level.

Enter COUNT times, defaulting to one.

This is a Lisp motion primitive.  It is an internal utility that avoids
any user-level concerns such as symex selection via advice.  This
should be used in all internal operations _above_ the primitive layer
\(e.g. favoring it over Emacs internal utilities like `forward-sexp`)
that are not primarily user-directed."
  (interactive)
  (if (symex-ts-available-p)
      (symex-ts-move-child count)
    (symex-lisp--go-up count)))

(defun symex--go-down (&optional count)
  "Exit to lower symex level.

Exit COUNT times, defaulting to one.

This is a Lisp motion primitive.  It is an internal utility that avoids
any user-level concerns such as symex selection via advice.  This
should be used in all internal operations _above_ the primitive layer
\(e.g. favoring it over Emacs internal utilities like `forward-sexp`)
that are not primarily user-directed."
  (interactive)
  (if (symex-ts-available-p)
      (symex-ts-move-parent count)
    (symex-lisp--go-down count)))

;;; Transformations

(defun symex--indent (count)
  "Indent COUNT expressions."
  (let* ((start (point))
         (end (condition-case nil
                  (symex--get-end-point count)
                ;; if empty, end = start
                (error start))))
    (indent-region start end)))

(defun symex--tidy (count)
  "Auto-indent COUNT symexes and fix any whitespace."
  ;; Note that this does not fix leading whitespace
  ;; (e.g. via `symex--fix-leading-whitespace`)
  ;; as that apparently destroys the indentation clues
  ;; the major mode needs to properly indent the code
  ;; in tree-sitter.
  ;; We could potentially have this part be delegated
  ;; to a Lisp-specific indent utility, but it could
  ;; be argued that indenting leading whitespace
  ;; is a concern of the _preceding_ expression, which,
  ;; this does get handled by this function via fixing
  ;; trailing whitespace.

  ;; fixing leading whitespace in lisp, for now
  ;; probably find a better/uniform way later
  (unless (symex-ts-available-p)
    (symex--fix-leading-whitespace))
  ;; fix trailing whitespace (indent region doesn't)
  (symex--fix-trailing-whitespace count)
  (symex--indent count)
  (symex-select-nearest))

(defun symex--remove (count &optional include-whitespace include-separator)
  "Delete COUNT symexes.

This is a low-level utility that simply removes the indicated text
from the buffer.

See `symex--get-end-point' for more on INCLUDE-WHITESPACE and
INCLUDE-SEPARATOR."
  ;; TODO: instead of having the count at the primitive level, have
  ;; each delete operation push onto a (yet to be implemented)
  ;; traversal memory stack. If the traversal is within a larger
  ;; traversal, the stacks should implicitly compose so that the
  ;; nested traversal accumulates and pushes onto the containing
  ;; traversal stack. Then, we can put the entire contents of the
  ;; stack into the paste buffer in e.g. symex-delete (after popping
  ;; the contents to get them in the right order)
  (let ((last-command nil)  ; see symex-yank re: last-command
        (start (point))
        (end (symex--get-end-point count include-whitespace include-separator)))
    (when (> end start)
      (kill-region start end)
      t)))

(defun symex--reset-after-delete ()
  "Tidy after deletion and select the appropriate symex."
  (if (symex-ts-available-p)
      (symex-ts--reset-after-delete)
    (symex-lisp--reset-after-delete)))

(defun symex-prim-delete (what)
  "Delete WHAT symex.

WHAT could be `this', `next', or `previous'."
  (let ((result))
    (condition-case nil
        (cond ((eq 'this what)
               (setq result (symex--remove 1 t t)))
              ((eq 'previous what)
               (when (symex--previous-p)
                 (symex--go-backward)
                 (setq result (symex--remove 1 t t))))
              ((eq 'next what)
               (when (symex--next-p)
                 (save-excursion
                   (symex--go-forward)
                   (setq result (symex--remove 1 t t)))))
              (t (error "Invalid argument for primitive delete!")))
      ;; if unable to delete, return nil instead of
      ;; raising an error. nil is used in the evaluator
      ;; to mean failed, so the traversal would stop there
      ;; as expected.
      (error nil))
    (when result
      (symex--reset-after-delete)
      ;; should we return the actual motion we took?
      result)))

(defun symex-prim-paste (where)
  "Paste WHERE.

WHERE could be either `before' or `after'.

This is the implementation of `paste' used in the DSL."
  ;; TODO: we might want to introduce delete and paste
  ;; counts into the DSL
  (symex--paste 1 where))

(defun symex--paste (count direction)
  "Paste before or after symex, COUNT times, according to DIRECTION.

DIRECTION should be either the symbol `before' or `after'."
  (interactive)
  (let* ((start (symex--get-starting-point))
         (end (condition-case nil
                  (symex--get-end-point 1 nil t)
                (error start)))
         (padding (symex--paste-padding start end (eq direction' before))))
    (goto-char (if (eq direction 'before)
                   start
                 end))
    (dotimes (_ count)
      (when (eq direction 'after)
        (insert padding)
        (indent-according-to-mode))
      (yank)
      (when (eq direction 'before)
        (insert padding)
        (indent-according-to-mode))))
  t)

;;; Utilities

(defvar symex--mode-mapping
  '((inferior-emacs-lisp-mode . emacs-lisp-mode))
  "Mapping of special modes to ordinary modes for symex transformations.")

(defun symex--map-major-mode (mode)
  "Map a major MODE to a more suitable one for symex transformations.

This maps special modes like Elisp REPLs to their ordinary
counterparts used in a source buffer, as using a REPL major mode for a
transformation would produce unexpected results due to the presence of
mode entry side effects, read-only regions, and other special regions
like prompts."
  (or (cdr (assoc mode symex--mode-mapping))
      mode))

(defmacro symex--transform-in-isolation (start end &rest body)
  "Transform a region in a temporary buffer and replace the original with it.

Copies the region from START to END into a temporary buffer, executes
BODY, and pastes the result back into the source buffer, replacing the
original."
  (declare (indent 2))
  (let ((original-major-mode (gensym))
        (mapped-major-mode (gensym))
        (text-to-transform (gensym))
        (result (gensym)))
    `(let ((,result)
           (,text-to-transform (buffer-substring ,start ,end)))
       ;; TODO: consider using `replace-region-contents'
       (delete-region ,start ,end)
       (let (,original-major-mode)
         ;; In using a temp buffer to do the transformation here, we need to
         ;; ensure that it uses the syntax table of the original buffer, since
         ;; otherwise it doesn't necessarily treat characters the same way
         ;; as the original buffer does, separating, for example, characters like
         ;; `?` and `#` from the rest of the symbol during recursive indentation.
         ;;
         ;; The with-temp-buffer macro doesn't see the original syntax table
         ;; when it is lexically defined here, not sure why. Defining a
         ;; lexical scope here and then setting it dynamically via `setq`
         ;; seems to work
         (setq ,original-major-mode major-mode)
         (let ((,mapped-major-mode (symex--map-major-mode ,original-major-mode)))
           (with-temp-buffer
             (funcall ,mapped-major-mode)
             (insert ,text-to-transform)
             (goto-char 0)
             ,@body
             (setq ,result (buffer-string)))))
       (save-excursion (insert ,result))
       (indent-region (point)
                      (+ (point) (length ,result)))
       (symex-select-nearest))))

(defmacro symex-save-excursion (&rest body)
  "Execute BODY while preserving position in the tree.

Like `save-excursion', but in addition to preserving the point
position, this also preserves the structural position in the tree, for
languages where point position doesn't uniquely identify a tree
location (e.g. non-symex-based languages like Python)."
  (declare (indent 0))
  (let ((offset (gensym))
        (result (gensym)))
    `(let ((,offset (symex--point-height-offset)))
       (let ((,result
              (save-excursion
                ,@body)))
         (symex-select-nearest)
         (symex--go-up ,offset)
         ,result))))

(defun symex--point-height-offset ()
  "Compute the height offset of the current symex.

This is measured from the lowest symex indicated by point.

This will always be zero for symex-oriented languages such as Lisp,
but in languages like Python where the same point position could
correspond to multiple hierarchy levels, this function computes the
difference from the lowest such level."
  (if (symex-ts-available-p)
      (symex-ts--point-height-offset)
    (symex-lisp--point-height-offset)))

(defun symex--get-starting-point ()
  "Get the point value at the start of the current symex."
  (if (symex-ts-available-p)
      (symex-ts--get-starting-point)
    (symex-lisp--get-starting-point)))

(defun symex--get-end-point (count &optional include-whitespace include-separator)
  "Get the point value after COUNT symexes.

If the containing expression terminates earlier than COUNT
symexes, returns the end point of the last one found.

If INCLUDE-WHITESPACE is non-nil, it includes trailing whitespace at
the end of the last symex.

If INCLUDE-SEPARATOR is non-nil, it includes any trailing separators
such as commas (Treesitter-only --- this isn't relevant for Lisp where
there are no separators besides whitespace).

Whitespace in treesitter is counted *after* the separator."
  (if (symex-ts-available-p)
      (symex-ts--get-end-point count
                               include-whitespace
                               include-separator)
    ;; separator not relevant for lisp
    (symex-lisp--get-end-point count include-whitespace)))

(defun symex--paste-padding (start end &optional before)
  "Determine paste padding needed for current point position.

Padding is dependent on whether we are pasting BEFORE the current
symex or after it.  START and END are the bounds of the current symex
that is the context for the paste."
  (if (symex-ts-available-p)
      (symex-ts--padding start end before)
    (symex-lisp--padding before)))

(defun symex-copy (&optional count)
  "Copy COUNT symexes."
  (let ((count (or count 1)))
    (let ((start (symex--get-starting-point))
          (end (symex--get-end-point count)))
      (buffer-substring start end))))

(defun symex-select-end (count &optional include-whitespace include-separator)
  "Select endpoint of COUNT symexes starting nearest to point.

See `symex--get-end-point' for more on INCLUDE-WHITESPACE and
INCLUDE-SEPARATOR."
  (goto-char (symex--get-end-point count include-whitespace include-separator))
  (point))

(defun symex-select-nearest ()
  "Select symex nearest to point."
  (if (symex-ts-available-p)
      (symex-ts-set-current-node-from-point)
    (symex-lisp-select-nearest))
  (point))

(defun symex--fix-leading-whitespace ()
  "Fix leading whitespace."
  ;; fix leading whitespace
  (fixup-whitespace)
  ;; fixup may move point into the whitespace - restore it
  (when (looking-at-p "[[:space:]]")
    (symex--go-to-next-non-whitespace-char)))

(defun symex--fix-trailing-whitespace (count)
  "Fix trailing whitespace after COUNT symexes."
  (condition-case nil
      (save-excursion
        (symex-select-end count nil t)
        (fixup-whitespace))
    (error nil)))

(defun symex-last-non-blank ()
  "Go to last non-blank character on line."
  (end-of-line)
  (skip-chars-backward " \t")
  (unless (bolp) (backward-char)))

(defun symex--primitive-enter ()
  "Take necessary actions as part of entering Symex mode, at a primitive level."
  (if (symex-ts-available-p)
      (symex-ts-enter)
    (symex-lisp-enter)))

(defun symex--primitive-exit ()
  "Take necessary actions as part of exiting Symex mode, at a primitive level."
  (if (symex-ts-available-p)
      (symex-ts-exit)
    (symex-lisp-exit)))


(provide 'symex-primitives)
;;; symex-primitives.el ends here
