;;; symex-ts.el --- Primitive navigations using Tree Sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Simon Pugnet <simon@polaris64.net>
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

;; This module defines a set of movement primitives which use Tree
;; Sitter to navigate a buffer's abstract syntax tree.
;;
;; A hydra is also provided, however this is intended for debugging
;; purposes.  It allows the primitives to be used directly without the
;; rest of Symex.

;;; Code:

(require 'treesit nil :no-error)
(require 'symex-custom)
(require 'symex-data)
(require 'symex-utils)

;; TODO: Emacs can't find these when you try to visit their definitions,
;; and they also raise byte-compile warnings, though they are locally
;; defined (but inside the `symex-ts--init-treesit-builtin' function)
(declare-function symex-ts--node-end-position nil)
(declare-function symex-ts--get-next-sibling nil)
(declare-function symex-ts--get-prev-sibling nil)
(declare-function symex-ts--count-named-children nil)
(declare-function symex-ts--get-named-descendant-for-position-range nil)
(declare-function symex-ts--get-nth-named-child nil)
(declare-function symex-ts--get-next-named-sibling nil)
(declare-function symex-ts--get-prev-named-sibling nil)
(declare-function symex-ts--root-node nil)
(declare-function symex-ts--node-eq nil)
(declare-function symex-ts--get-parent nil)
(declare-function symex-ts--node-start-position nil)

;; The use of these functions in this module is guarded by checks for
;; the presence of tree-sitter in higher-level functions, so these
;; should never get called on older versions of Emacs (<29.1) that
;; don't have treesitter. But melpazoid still complains about their
;; use as it doesn't know that they won't be called, so we declare
;; these functions here and suppress those checks.
(declare-function treesit-node-child-count nil)
(declare-function treesit-node-descendant-for-range nil)
(declare-function treesit-node-child nil)
(declare-function treesit-node-parent nil)
(declare-function treesit-node-prev-sibling nil)
(declare-function treesit-node-at nil)
(declare-function treesit-node-end nil)
(declare-function treesit-node-eq nil)
(declare-function treesit-node-start nil)
(declare-function treesit-buffer-root-node nil)
(declare-function treesit-parser-add-notifier nil)
(declare-function treesit-parser-remove-notifier nil)
(declare-function treesit-parser-list nil)
(declare-function treesit-node-string nil)
(declare-function treesit-node-type nil)
(declare-function treesit-node-next-sibling nil)

(defun symex-ts--current-ts-library ()
  "Return a symbol to show what type of tree sitter library is available.

`internal' means that Emacs has been compiled with native tree
sitter support.  `external' means that the `elisp-tree-sitter'
package is being used instead."
  (if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
      'internal
    'external))

(defun symex-ts--init ()
  "Initialise tree sitter support for Symex.el.

If treesitter isn't available, this doesn't do anything."
  (message "Initialising Symex-TS...")
  (when (eq (symex-ts--current-ts-library) 'internal)
    (symex-ts--init-treesit-builtin)))

(defun symex-ts--init-treesit-builtin ()
  "Initialise Symex tree sitter support via built-in tree-sitter library."
  (message "Initialising Symex tree sitter support using Emacs built-in library...")
  (defun symex-ts--count-named-children (node)
    "Return the number of named children of NODE.

If NODE is nil, return nil."
    (treesit-node-child-count node t))
  (defun symex-ts--get-named-descendant-for-position-range (node beg end)
    "Return the smallest named node that covers buffer positions BEG to END.

The returned node is a descendant of NODE.
Return nil if there is no such node.
If NODE is nil, return nil."
    (treesit-node-descendant-for-range node beg end t))
  (defun symex-ts--get-next-named-sibling (node)
    "Return the next named sibling of NODE.

Return nil if there is no next sibling. If NODE is nil, return
nil."
    (treesit-node-next-sibling node t))
  (defun symex-ts--get-next-sibling (node)
    "Return the next sibling of NODE.

Return nil if there is no next sibling. If NODE is nil, return
nil."
    (treesit-node-next-sibling node))
  (defun symex-ts--get-nth-named-child (node n)
    "Return the Nth named child of NODE.

Return nil if there is no Nth child. If NODE is nil, return nil.

N could be negative, e.g., -1 represents the last child."
    (treesit-node-child node n t))
  (defalias 'symex-ts--get-parent #'treesit-node-parent)
  (defun symex-ts--get-prev-named-sibling (node)
    "Return the previous named sibling of NODE.

Return nil if there is no previous sibling. If NODE is nil,
return nil."
    (treesit-node-prev-sibling node t))
  (defun symex-ts--get-prev-sibling (node)
    "Return the previous sibling of NODE.

Return nil if there is no previous sibling. If NODE is nil,
return nil."
    (treesit-node-prev-sibling node))
  (defalias 'symex-ts--node-at #'treesit-node-at)
  (defalias 'symex-ts--node-end-position #'treesit-node-end)
  (defalias 'symex-ts--node-eq #'treesit-node-eq)
  (defalias 'symex-ts--node-start-position #'treesit-node-start)
  (defalias 'symex-ts--root-node #'treesit-buffer-root-node)
  t)

(defun symex-ts-available-p ()
  "Predicate to show if tree sitter support is available to Symex."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (> (length (treesit-parser-list)) 0)
       ;; We use the Lisp primitives for Clojure
       ;; even though Emacs 29 provides tree-sitter APIs
       ;; for it, since the Lisp primitives in Symex are
       ;; more mature than the Tree Sitter ones at the
       ;; present time.
       ;; TODO: I don't think this is needed anymore,
       ;; as clojure-mode doesn't use tree-sitter anyway
       ;; and clojure-ts-mode cannot use the lisp parser anyway
       (not (member major-mode symex-lisp-modes))))

(defvar-local symex-ts--current-node nil "The current Tree Sitter node.")

(defun symex-ts--set-current-node (node)
  "Set the current node to NODE and update internal references."
  (setq-local symex-ts--current-node node)
  (goto-char (symex-ts--node-start-position node)))

(defun symex-ts--get-topmost-node (node)
  "Return the highest node in the tree starting from NODE.

The returned node is the highest possible node that has the same
start position as NODE."
  (let ((node-start-pos (symex-ts--node-start-position node))
        (parent (symex-ts--get-parent node)))
    (if parent
        (let ((parent-pos (symex-ts--node-start-position parent)))
          (if (and (eq node-start-pos parent-pos)
                   ;; don't consider root
                   (not (symex-ts--node-eq parent (symex-ts--root-node))))
              (symex-ts--get-topmost-node parent)
            node))
      node)))

(defun symex-ts--get-nth-sibling-from-node (src-node traversal-fn n)
  "Return the N-th sibling node from SRC-NODE.

TRAVERSAL-FN should be a function which returns the next node in
the chain.  For example, to get the node two positions prior to
SRC-NODE, use `symex-ts--get-prev-named-sibling'.

If N traversals cannot be completed (e.g. if N is 3 but there are
only two more nodes), the last node is returned instead."
  (let ((next-node (funcall traversal-fn src-node)))
    (if (or (eq n 1) (not next-node))
        src-node
      (symex-ts--get-nth-sibling-from-node next-node traversal-fn (1- n)))))

(defun symex-ts--node-has-sibling-p (node)
  "Check if NODE has a sibling."
  (or (symex-ts--get-prev-named-sibling node)
      (symex-ts--get-next-named-sibling node)))

(defun symex-ts--descend-to-child-with-sibling (node)
  "Descend from NODE to the first child recursively.

Recursion will end when the child node has a sibling or is a
leaf."
  (let ((child (symex-ts--get-nth-named-child node 0)))
    (if child
        (if (or (symex-ts--node-has-sibling-p child)
                (not (= (symex-ts--node-start-position node)
                        (symex-ts--node-start-position child))))
            child
          (symex-ts--descend-to-child-with-sibling child))
      nil)))

(defun symex-ts--ascend-to-parent-with-sibling (node &optional initial)
  "Ascend from NODE to parent recursively.

Recursion will end when the parent node has a sibling or is the
root.

The INITIAL node is used to ensure that a parent is selected even if
it doesn't have siblings if it changes point (TODO: clarify)."
  (let ((parent (symex-ts--get-parent node))
        (initial (or initial node)))
    (if parent
        (cond ((or (symex-ts--node-eq parent (symex-ts--root-node))  ; don't visit root node
                   ;; visit node if it either has no siblings or changes point,
                   ;; for symmetry with "descend" behavior
                   (and (not (= (symex-ts--node-start-position node)
                                (symex-ts--node-start-position parent)))
                        (not (symex-ts--node-eq node initial))))
               node)
              ((symex-ts--node-has-sibling-p parent) parent)
              (t (symex-ts--ascend-to-parent-with-sibling parent node)))
      node)))

(defun symex-ts-current-node-sexp ()
  "Print the current node as an s-expression."
  (interactive)
  (message (treesit-node-string symex-ts--current-node)))

(defun symex-ts-current-node-type ()
  "Print the type of the current node."
  (interactive)
  (message (treesit-node-type symex-ts--current-node)))

(defun symex-ts-set-current-node-from-point ()
  "Set the current node to the top-most node at point."
  (cond ((and (not symex-ts--current-node)
              (looking-at-p symex--re-whitespace)
              (looking-back symex--re-non-whitespace (line-beginning-position)))
         (re-search-backward symex--re-whitespace)
         (forward-char))
        (t (unless (symex--go-to-next-non-whitespace-char)
             (symex--go-to-previous-non-whitespace-char))))
  (symex-ts--set-current-node (symex-ts-get-topmost-node-at-point)))

(defun symex-ts-get-current-node ()
  "Return the current node.
Automatically set it to the node at point if necessary."
  (unless (and symex-ts--current-node
               (not (treesit-node-check symex-ts--current-node 'outdated)))
    (symex-ts-set-current-node-from-point))
  symex-ts--current-node)

(defun symex-ts-get-topmost-node-at-point ()
  "Return the top-most node at the current point."
  (let ((root (symex-ts--root-node))
        (p (point)))
    (symex-ts--get-topmost-node (symex-ts--get-named-descendant-for-position-range root p p))))

(defun symex-ts--selected-p ()
  "Check if a symex is currently selected."
  symex-ts--current-node)

;;; User Interface

(defun symex-ts--adjust-point ()
  "Helper to adjust point to indicate the correct symex."
  nil)

(defun symex-ts--notify-of-changes (_ranges _parser &rest _args)
  "Take action when notified of any changes to the contents of the buffer.

While in Symex mode, if there are any changes in the buffer (e.g., due
to a mutative operation like delete) and if the selected node is no
longer valid, then refresh to select a new current node near point.

Note that, technically, this doesn't \"notify\" anyone, but rather,
*handles* changes that it is *being notified of* by Emacs. We use the
term to match its use in builtin treesitter APIs by Emacs, as in
`treesit-parser-add-notifier'."
  (when (and symex-ts--current-node
             (treesit-node-check symex-ts--current-node 'outdated))
    (symex-ts-set-current-node-from-point)))

(defun symex-ts-add-notifier ()
  "Register the change notifier."
  (dolist (parser (treesit-parser-list))
    (treesit-parser-add-notifier parser
                                 #'symex-ts--notify-of-changes)))

(defun symex-ts-remove-notifier ()
  "Unregister the change notifier."
  (dolist (parser (treesit-parser-list))
    (treesit-parser-remove-notifier parser
                                    #'symex-ts--notify-of-changes)))

;;; Predicates

(defmacro symex-ts-save-excursion (&rest body)
  "Execute BODY while preserving position in the tree.

Like `save-excursion`, but in addition to preserving the point
position, this also preserves the structural position in the tree, for
languages where point position doesn't uniquely identify a tree
location (e.g. non-symex-based languages like Python).

This is tree-sitter specific and meant for internal, primitive use."
  (declare (indent 0))
  (let ((offset (gensym))
        (result (gensym)))
    `(let ((,offset (symex-ts--point-height-offset)))
       (let ((,result
              (save-excursion
                ,@body)))
         (symex-ts-set-current-node-from-point)
         (symex-ts-move-child ,offset)
         ,result))))

(defmacro symex-ts--if-stuck (do-what operation &rest body)
  "Attempt OPERATION and if it fails, then do DO-WHAT."
  (let ((orig (gensym))
        (cur (gensym)))
    `(let ((,orig (symex-ts-get-current-node)))
       ,operation
       (let ((,cur (symex-ts-get-current-node)))
         (if (symex-ts--node-eq ,cur ,orig)
             ,do-what
           ,@body)))))

(defun symex-ts--at-root-p ()
  "Check whether the current node is the global root node."
  (let ((root (symex-ts--root-node))
        (cur (symex-ts-get-current-node)))
    (symex-ts--node-eq cur root)))

(defun symex-ts--at-tree-root-p ()
  "Check whether the current node is the root node of a tree.

Note that this does not consider global root to be a tree root."
  (let ((root (symex-ts--root-node))
        (cur (symex-ts-get-current-node)))
    (let ((parent (symex-ts--get-parent cur)))
      (or (not parent) (symex-ts--node-eq parent root)))))

(defun symex-ts--at-first-p ()
  "Check if the current node is the first one at some level."
  (symex-ts--if-stuck t
                      (symex-ts-move-prev-named-sibling)
                      (symex-ts-move-next-named-sibling)
                      nil))

(defun symex-ts--at-last-p ()
  "Check if the current node is at the last one at some level."
  (symex-ts--if-stuck t
                      (symex-ts-move-next-named-sibling)
                      (symex-ts-move-prev-named-sibling)
                      nil))

(defun symex-ts--at-final-p ()
  "Check if the current node is at the last one in the buffer."
  (and (symex-ts--at-tree-root-p)
       (symex-ts--at-last-p)))

(defun symex-ts--at-initial-p ()
  "Check if the current node is at the first one in the buffer."
  (and (symex-ts--at-tree-root-p)
       (symex-ts--at-first-p)))

(defun symex-ts--point-at-start-p ()
  "Check if point is at the start of a node."
  (let ((cur (symex-ts-get-current-node)))
    (= (point) (symex-ts--node-start-position cur))))

(defun symex-ts--previous-p ()
  "Check if a preceding symex exists at this level."
  (symex-ts-save-excursion (symex-ts-move-prev-named-sibling)))

(defun symex-ts--next-p ()
  "Check if a succeeding symex exists at this level."
  (symex-ts-save-excursion (symex-ts-move-next-named-sibling)))

(defun symex-ts-atom-p ()
  "Check if the selected symex is an atom.

It could include both identifiers as well as empty lists or forms."
  (= 0 (symex-ts--count-named-children symex-ts--current-node)))

;;; Navigations

(defun symex-ts--move-with-count (fn move-delta &optional count)
  "Move the point from the current node if possible.

Movement is defined by FN, which should be a function which
returns the appropriate neighbour node.

MOVE-DELTA is a Symex \"move\" describing the desired x and y
point movement (e.g. `(move -1 0)' for a move \"upward\").

Move COUNT times, defaulting to 1.

Return a Symex move (list with x,y node offsets tagged with
`move') or nil if no movement was performed."
  (let ((target-node nil)
        (move symex--move-zero)
        (cursor (symex-ts-get-current-node)))
    (dotimes (_ (or count 1))
      (let ((new-node (funcall fn cursor)))
        (when (and new-node (not (symex-ts--node-eq new-node cursor)))
          (setq move (symex--move-+ move move-delta))
          (setq cursor new-node
                target-node cursor))))
    (when target-node (symex-ts--set-current-node target-node))

    ;; Return the Symex move that was executed, or nil to signify that
    ;; the movement failed
    (when (not (symex--are-moves-equal-p move symex--move-zero)) move)))

(defun symex-ts-move-prev-sibling (&optional count)
  "Move the point to the current node's previous sibling if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'symex-ts--get-prev-sibling (symex-make-move -1 0) count))

(defun symex-ts-move-prev-named-sibling (&optional count)
  "Move the point to the current node's previous sibling if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'symex-ts--get-prev-named-sibling (symex-make-move -1 0) count))

(defun symex-ts-move-next-sibling (&optional count)
  "Move the point to the current node's next sibling if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'symex-ts--get-next-sibling (symex-make-move 1 0) count))

(defun symex-ts-move-next-named-sibling (&optional count)
  "Move the point to the current node's next sibling if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'symex-ts--get-next-named-sibling (symex-make-move 1 0) count))

(defun symex-ts-move-parent (&optional count)
  "Move the point to the current node's parent if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'symex-ts--ascend-to-parent-with-sibling (symex-make-move 0 -1) count))

(defun symex-ts-move-child (&optional count)
  "Move the point to the current node's first child if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'symex-ts--descend-to-child-with-sibling (symex-make-move 0 1) count))

;;; Utilities

(defconst symex-ts-separators '(","))

(defun symex-ts--get-starting-point ()
  "Get the point value at the start of the current symex."
  (symex-ts--node-start-position (symex-ts-get-current-node)))

(defun symex-ts--get-end-point-helper (count &optional include-separator)
  "Helper to get the point value after COUNT symexes.

If the containing expression terminates earlier than COUNT
symexes, returns the end point of the last one found.

Note that this mutates point - it should not be called directly.

See `symex--get-end-point' for more on INCLUDE-SEPARATOR."
  (symex-ts-move-next-named-sibling (1- count))
  (let ((next (treesit-node-next-sibling symex-ts--current-node)))
    (if (and include-separator
             (member (treesit-node-type next)
                     symex-ts-separators))
        (symex-ts--node-end-position next)
      (symex-ts--node-end-position symex-ts--current-node))))

(defun symex-ts--get-end-point (count &optional include-whitespace include-separator)
  "Get the point value after COUNT symexes.

If the containing expression terminates earlier than COUNT
symexes, returns the end point of the last one found.

See `symex--get-end-point' for more on INCLUDE-WHITESPACE and
INCLUDE-SEPARATOR."
  (symex-ts-save-excursion
    (let ((endpoint (symex-ts--get-end-point-helper count
                                                    include-separator)))
      (if include-whitespace
          (progn (goto-char endpoint)
                 (if (and (not (eobp))
                          (symex-whitespace-p))
                     (1+ endpoint)
                   endpoint))
        endpoint))))

(defun symex-ts--point-height-offset-helper (orig-pos)
  "A helper to compute the height offset of the current symex.

The height offset is determined as soon as point differs from the
original point position ORIG-POS upon repeatedly going down."
  (cond ((symex-ts--at-tree-root-p)
         (if (= orig-pos (point))
             0
           -1))
        ((not (= (point) orig-pos)) -1)
        (t (symex-ts-move-parent)
           (1+ (symex-ts--point-height-offset-helper orig-pos)))))

(defun symex-ts--point-height-offset ()
  "Compute the height offset of the current symex.

This is measured from the lowest symex indicated by point."
  ;; TODO: probably make this a tree-sitter utility instead, so that
  ;; it uses tree-sitter APIs to determine point-height offset instead
  ;; of doing it at the level of traversals.
  ;; don't attempt to calculate offset at the "real" root
  ;; since offsets are typically computed while ignoring it
  ;; i.e. they are wrt. "tree root"
  (cond ((symex-ts--at-root-p) 0)
        ;; at the "tree root" of the first symex in the buffer,
        ;; point-height offset would need to account for "true" root
        ;; but since we've now disallowed root selection at the
        ;; primitive level, we don't need to treat this initial
        ;; node as special, after all, as this initial symex is now
        ;; the lowest selectable node. Still, we retain a distinct
        ;; case for it here as it would be impacted if we ever change
        ;; the behavior wrt root selection
        ((symex-ts--at-initial-p) 0)
        ;; aside from the above special cases, compute point-height
        ;; offset by just descending as long as point does not change,
        ;; and counting the number of steps taken
        (t (let* ((orig-pos (point))
                  (offset (symex-ts--point-height-offset-helper orig-pos)))
             ;; return to original tree position
             ;; before returning the result
             (goto-char orig-pos)
             (symex-ts-set-current-node-from-point)
             (symex-ts-move-child offset)
             offset))))

(defun symex-ts--reset-after-delete ()
  "Tidy things up after deletion.

If the deletion results in an empty line it will be removed."
  (when (symex--current-line-empty-p)
    (if (symex--previous-line-empty-p)
        (symex--join-to-non-whitespace)
      (symex--delete-whole-line))))

(defun symex-ts--padding ()
  "Determine paste padding needed for current point position.

START and END are the bounds of the current symex that is the context
for the paste."
  (let* ((start (symex-ts--get-starting-point))
         (end (symex-ts--get-end-point 1))
         (indent-start (save-excursion (back-to-indentation) (point)))
         (block-node (or (not (= (line-number-at-pos start)
                                 (line-number-at-pos end)))
                         (and (= start indent-start)
                              (= end (line-end-position))))))
    (if block-node "\n" " ")))

(defun symex-ts-enter ()
  "Take necessary tree-sitter related actions upon entering Symex mode."
  (symex-ts-add-notifier))

(defun symex-ts-exit ()
  "Take necessary tree-sitter related actions upon exiting Symex mode."
  (setq-local symex-ts--current-node nil)
  (symex-ts-remove-notifier))


(provide 'symex-ts)
;;; symex-ts.el ends here
