;;; symex-tree.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; High-level insights about the tree and navigating it

;;; Code:


(require 'symex-evaluator)
(require 'symex-computations)
(require 'symex-traversals)

;; buffer-local branch memory stack
(defvar-local symex--branch-memory nil)

;;;;;;;;;;;;;;;;;;;;;
;;; MISCELLANEOUS ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun symex-remaining-length ()
  "Compute the remaining length of the containing symex.

This *includes* the current symex."
  (1+  ; length, so not zero-based like index
   (symex-save-excursion
     (symex-eval symex--traversal-goto-last
                 symex--computation-traversal-length))))

(defun symex-preceding-length ()
  "Compute the length of the symex before the current one.

This does *not* include the current symex."
  (symex-save-excursion
    (symex-eval symex--traversal-goto-first
                symex--computation-traversal-length)))

(defun symex-index ()
  "Get relative (from start of containing symex) index of current symex."
  (symex-save-excursion
    (symex-eval symex--traversal-goto-first
                symex--computation-traversal-length)))

(defun symex-height ()
  "Get height (above root) of current symex."
  (symex-save-excursion
    (symex-eval symex--traversal-goto-lowest
                symex--computation-traversal-length)))

(defun symex--tree-index ()
  "Index of current tree."
  (symex-save-excursion
   (symex-goto-lowest)
   (symex-index)))

(defun symex--leap-forward (&optional soar)
  "Leap forward to a neighboring branch, preserving height and position.

If SOAR is true, leap between trees too, otherwise, stay in the
current tree.

See the documentation on `symex-leap-backward` for details regarding
the implementation -- the only difference is that this uses a preorder
traversal instead of a postorder traversal."
  (let ((traverse (if soar
                      symex--traversal-preorder
                    symex--traversal-preorder-in-tree)))
    (symex-eval
     (symex-traversal
       (maneuver (loop traverse
                       (lambda (acc)
                         (and (= (symex--move-x acc) 0)
                              (= (symex--move-y acc) 0))))))
     symex--computation-node-distance)))

(defun symex--leap-backward (&optional soar)
  "Leap backward to a neighboring branch, preserving height and position.

If SOAR is true, leap between trees too, otherwise, stay in the
current tree.

This is implemented as a postorder traversal from the starting
position, keeping track of changes to the height (nesting level from
root) and index (position along branch) while traversing, and stopping
when both the height and index delta returns to zero, based on a
computation that keeps track of this delta while traversing.

Since we only track height and index _deltas_ and don't actually
measure them anywhere, we do this traversal in O(n)."
  (let ((traverse (if soar
                      symex--traversal-postorder
                    symex--traversal-postorder-in-tree)))
    (symex-eval
     (symex-traversal
       (maneuver (loop traverse
                       (lambda (acc)
                         (and (= (symex--move-x acc) 0)
                              (= (symex--move-y acc) 0))))))
     symex--computation-node-distance)))

(defun symex--remember-branch-position (position)
  "Remember branch POSITION when descending the tree.

This pushes the current position onto a stack, which is popped
while ascending."
  (push position symex--branch-memory))

(defun symex--recall-branch-position ()
  "Recall position on the branch."
  (pop symex--branch-memory))

(defun symex--clear-branch-memory ()
  "Clear the branch memory stack.

Technically, branch memory is tree-specific, and stored branch
positions are no longer relevant on a different tree than the one on
which they were recorded.  To be conservative and err on the side of
determinism here, we clear branch memory upon entering symex mode,
since may enter at arbitrary points in the code, i.e. on arbitrary
trees.

TODO: Yet, hypothetically if there were two identical trees next to
one another, then the positions from one would naturally carry over to
the other and in some sense this would be the most intuitive.  Thus,
an alternative could be to retain branch memory across trees so that
we attempt to climb each tree as if it were the last tree
climbed, which may in practice end up being more intuitive than
assuming no knowledge of the tree at all.

This may be worth exploring as a defcustom."
  (setq symex--branch-memory nil))

(defun symex--forget-branch-positions ()
  "Forget any stored branch positions when moving to a different tree."
  (setq symex--branch-memory nil))

(defun symex--go-up-with-memory (count)
  "Go up COUNT times, recalling previous positions along branches."
  (let ((position))
    (symex-eval
     (symex-traversal
       ;; ideally, we need a version of `circuit' here that works
       ;; like a venture rather than a maneuver, that is, which
       ;; repeats as many times as possible up to count, considering
       ;; it successful if it is done at least once.
       (circuit
        (maneuver
         (effect (setq position (symex--recall-branch-position))
                 (move up))
         (lambda ()
           ;; As a fallback case, a symex traversal can be any lambda.
           ;; We use one here because otherwise, if we just used
           ;; `circuit' directly, `position' would evaluate statically
           ;; to nil, and it wouldn't have access to its dynamic value
           ;; read from the branch memory stack
           (symex-eval
            (symex-traversal
              (circuit (move forward)
                       ;; popping empty stack is nil,
                       ;; so use 0 there instead
                       (or position 0))))))
        count)))))

(defun symex--go-down-with-memory (count)
  "Go down COUNT times, remembering branch positions along the way."
  (let ((position))
    (symex-eval
     (symex-traversal
       (circuit
        (precaution
         (move down)
         (beforehand (lambda ()
                       (setq position (symex-index))))
         (afterwards (lambda ()
                       (symex--remember-branch-position position))))
        count)))))

(provide 'symex-tree)
;;; symex-tree.el ends here
