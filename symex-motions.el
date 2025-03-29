;;; symex-motions.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Ways to move around in trees.

;;; Code:


(require 'symex-custom)
(require 'symex-primitives)
(require 'symex-dsl)
(require 'symex-traversals)
(require 'symex-tree)
(require 'symex-interop)
(require 'symex-ui)

;; TODO: these "selection" functions aren't motions; maybe they ought
;; to be filed in another module (`symex-user`?)
(defun symex-user-select-nearest ()
  "Select symex nearest to point.

This user-level interface does the most intuitive thing from the perspective
of the user, and isn't necessarily deterministic. It may select the next
expression, the previous one, or the containing one, depending on context.
For the deterministic version used at the primitive level, see
`symex-select-nearest`.

This also may entail hooks and advice, which would be absent in the
primitive version."
  (interactive)
  (symex-select-nearest))

(defun symex-select-nearest-in-line ()
  "Select symex nearest to point that's on the current line."
  (interactive)
  (unless (symex--current-line-empty-p)
    (let ((original-pos (point)))
      (symex-select-nearest)
      (unless (= (line-number-at-pos)
                 (line-number-at-pos original-pos))
        (goto-char original-pos)))))

(defun symex--selection-side-effects (&rest _)
  "Things to do as part of symex selection, e.g. after navigations."
  (when symex-highlight-p
    (symex--update-overlay)))

(defmacro symex-define-motion (name
                               args
                               docstring
                               interactive-decl
                               &rest
                               body)
  "Define a symex motion."
  (declare (indent defun))
  (eldoc-add-command name)
  (let ((result (gensym)))
    `(defun ,name ,args
       ,docstring
       ,interactive-decl
       (let ((,result (progn ,@body)))
         (symex-user-select-nearest)
         ,result))))

(symex-define-motion symex-go-forward (count)
  "Move forward COUNT symexes."
  (interactive "p")
  ;; TODO: these should be compiled
  ;; so that the actual executed traversal
  ;; is (move N 0) rather than
  ;; (move 1 0) executed N times
  (symex-eval
   (symex-traversal (circuit (move forward)
                             count))))

(symex-define-motion symex-go-backward (count)
  "Move backward COUNT symexes."
  (interactive "p")
  (symex-eval
   (symex-traversal (circuit (move backward)
                             count))))

(symex-define-motion symex-go-up (count)
  "Move up COUNT symexes."
  (interactive "p")
  (symex-eval
   (symex-traversal (circuit (move up)
                             count))))

(symex-define-motion symex-go-down (count)
  "Move down COUNT symexes."
  (interactive "p")
  (symex-eval
   (symex-traversal (circuit (move down)
                             count))))

(symex-define-motion symex-goto-first ()
  "Select first symex at present level."
  (interactive)
  (symex-eval symex--traversal-goto-first)
  (point))

(symex-define-motion symex-goto-last ()
  "Select last symex at present level."
  (interactive)
  (symex-eval symex--traversal-goto-last)
  (point))

(symex-define-motion symex-goto-lowest ()
  "Select lowest symex."
  (interactive)
  (symex-eval symex--traversal-goto-lowest)
  (point))

(symex-define-motion symex-goto-highest ()
  "Select highest symex."
  (interactive)
  (symex-eval symex--traversal-goto-highest)
  (point))

(symex-define-motion symex-traverse-forward (count)
  "Traverse symex as a tree, using pre-order traversal.

Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-eval symex--traversal-preorder)))

(symex-define-motion symex-traverse-forward-more (count)
  "Traverse symex as a tree, using pre-order traversal.

Moves more steps at a time.  Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-traverse-forward 3)))

(symex-define-motion symex-traverse-forward-in-tree (count)
  "Traverse symex forward using pre-order traversal, stopping at end of tree.

Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-eval symex--traversal-preorder-in-tree)))

(symex-define-motion symex-traverse-forward-skip (count)
  "Traverse symex as a tree, skipping forward.

Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-eval symex--traversal-skip-forward)))

(symex-define-motion symex-traverse-backward (count)
  "Traverse symex as a tree, using converse post-order traversal.

Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-eval symex--traversal-postorder)))

(symex-define-motion symex-traverse-backward-more (count)
  "Traverse symex as a tree, using pre-order traversal.

Moves more steps at a time.  Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-traverse-backward 3)))

(symex-define-motion symex-traverse-backward-in-tree (count)
  "Traverse symex backward using post-order traversal, stopping at root of tree.

Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-eval symex--traversal-postorder-in-tree)))

(symex-define-motion symex-traverse-backward-skip (count)
  "Traverse symex as a tree, skipping backwards.

Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-eval symex--traversal-skip-backward)))

(symex-define-motion symex-climb-branch (count)
  "Climb up.

Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-eval symex--traversal-climb-branch)))

(symex-define-motion symex-descend-branch (count)
  "Descend the tree.

Executes the motion COUNT times."
  (interactive "p")
  (dotimes (_ count)
    (symex-eval symex--traversal-descend-branch)))

(symex-define-motion symex-soar-backward (count)
  "Leap backwards, crossing to a neighboring tree.

At the moment, if a neighboring branch in the current tree is
available in that direction, we leap to it.  In a future version of
symex, this may be changed to always go to a neighboring tree,
ignoring local branches.

Leaps COUNT times, defaulting to once."
  (interactive "p")
  (dotimes (_ count)
    (symex--leap-backward t)))

(symex-define-motion symex-soar-forward (count)
  "Leap forward, crossing to a neighboring tree.

At the moment, if a neighboring branch in the current tree is
available in that direction, we leap to it.  In a future version of
symex, this may be changed to always go to a neighboring tree,
ignoring local branches.

Leaps COUNT times, defaulting to once."
  (interactive "p")
  (dotimes (_ count)
    (symex--leap-forward t)))

(symex-define-motion symex-leap-backward (count)
  "Leap backward to a neighboring branch, preserving height and position.

Leaps COUNT times, defaulting to once."
  (interactive "p")
  (dotimes (_ count)
    (symex--leap-backward)))

(symex-define-motion symex-leap-forward (count)
  "Leap forward to a neighboring branch, preserving height and position.

Leaps COUNT times, defaulting to once."
  (interactive "p")
  (dotimes (_ count)
    (symex--leap-forward)))

;; These two aren't defined as motions since their post-motion selection
;; strategy is different.
(defun symex-next-visual-line (&optional count)
  "Coordinate navigation to move down.

This moves down COUNT lines in terms of buffer coordinates, rather than
structurally in terms of the tree."
  (interactive "p")
  (next-line count)
  (symex-select-nearest-in-line))

(eldoc-add-command 'symex-next-visual-line)

(defun symex-previous-visual-line (&optional count)
  "Coordinate navigation to move up.

This moves up COUNT lines in terms of buffer coordinates, rather than
structurally in terms of the tree."
  (interactive "p")
  (previous-line count)
  (symex-select-nearest-in-line))

(eldoc-add-command 'symex-previous-visual-line)

(defun symex-select-nearest-advice (&rest _)
  "Advice to select the nearest symex."
  (when symex-editing-mode
    (symex-user-select-nearest)))

(provide 'symex-motions)
;;; symex-motions.el ends here
