;;; symex-ts.el --- Primitive navigations using Tree Sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Simon Pugnet <simon@polaris64.net>
;; URL: https://github.com/countvajhula/symex.el

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
;; purposes. It allows the primitives to be used directly without the
;; rest of Symex.

;;; Code:

(require 'tree-sitter)


(defface symex-ts-current-node-face
  '((t :inherit highlight :extend nil))
  "Face used to highlight the current tree node."
  :group 'symex-faces)


(defvar symex-ts--current-node nil "The current Tree Sitter node.")

(defvar symex-ts--current-overlay nil "The current overlay which highlights the current node.")


(defun symex-ts--delete-overlay ()
  "Delete the highlight overlay."
  (when symex-ts--current-overlay
    (delete-overlay symex-ts--current-overlay)))

(defun symex-ts--update-overlay (node)
  "Update the highlight overlay to match the start/end position of NODE."
  (when symex-ts--current-overlay
    (delete-overlay symex-ts--current-overlay))
  (setq-local symex-ts--current-overlay (make-overlay (tsc-node-start-position node) (tsc-node-end-position node)))
  (overlay-put symex-ts--current-overlay 'face 'symex-ts-current-node-face))

(defun symex-ts--set-current-node (node)
  "Set the current node to NODE and update internal references."
  (setq-local symex-ts--current-node node)
  (goto-char (tsc-node-start-position node))
  (symex-ts--update-overlay symex-ts--current-node))

(defun symex-ts--get-topmost-node (node)
  "Return the highest node in the tree starting from NODE.

The returned node is the highest possible node that has the same
start position as NODE."
  (let ((node-start-pos (tsc-node-start-position node))
        (parent (tsc-get-parent node)))
    (if parent
        (let ((parent-pos (tsc-node-start-position parent)))
          (if (eq node-start-pos parent-pos)
              (symex-ts--get-topmost-node parent)
            node))
      node)))

(defun symex-ts--get-nth-sibling-from-node (src-node traversal-fn n)
  "Return the N-th sibling node from SRC-NODE.

TRAVERSAL-FN should be a function which returns the next node in
the chain. For example, to get the node two positions prior to
SRC-NODE, use `tsc-get-prev-named-sibling'.

If N traversals cannot be completed (e.g. if N is 3 but there are
only two more nodes), the last node is returned instead."
  (let ((next-node (funcall traversal-fn src-node)))
    (if (or (eq n 1) (not next-node))
        src-node
      (symex-ts--get-nth-sibling-from-node next-node traversal-fn (1- n)))))

(defun symex-ts--descend-to-child-with-sibling (node)
  "Descend from NODE to the first child recursively.

Recursion will end when the child node has a sibling or is a
leaf."
  (let ((child (tsc-get-nth-named-child node 0)))
    (if child
        (if (or (tsc-get-prev-named-sibling child) (tsc-get-next-named-sibling child))
            child
          (symex-ts--descend-to-child-with-sibling child))
      node)))

(defun symex-ts--ascend-to-parent-with-sibling (node)
  "Ascend from NODE to parent recursively.

Recursion will end when the parent node has a sibling or is the
root."
  (let ((parent (tsc-get-parent node)))
    (if parent
        (if (or (tsc-get-prev-named-sibling parent) (tsc-get-next-named-sibling parent))
            parent
          (symex-ts--ascend-to-parent-with-sibling parent))
      node)))

(defun symex-ts--move-with-count (fn move-delta &optional count)
  "Move the point from the current node if possible.

Movement is defined by FN, which should be a function which
returns the appropriate neighbour node.

MOVE-DELTA is a Symex \"move\" describing the desired x and y
point movement (e.g. `(move -1 0)' for a move \"upward\").

Move COUNT times, defaulting to 1.

Return a Symex move (list with x,y node offsets tagged with
'move) or nil if no movement was performed."
  (let ((target-node nil)
        (move symex--move-zero)
        (cursor (symex-ts-get-current-node)))
    (dotimes (_ (or count 1))
      (let ((new-node (funcall fn cursor)))
        (when (and new-node (not (eq new-node cursor)))
          (setq move (symex--add-moves (list move move-delta)))
          (setq cursor new-node
                target-node cursor))))
    (when target-node (symex-ts--set-current-node target-node))

    ;; Return the Symex move that was executed, or nil to signify that
    ;; the movement failed
    (when (not (symex--are-moves-equal-p move symex--move-zero)) move)))

(defun symex-ts--after-tree-modification ()
  "Handle any tree modification."
  (symex-ts--delete-overlay)
  (setq-local symex-ts--current-node nil))


(defun symex-ts-current-node-sexp ()
  "Print the current node as an s-expression."
  (interactive)
  (message (tsc-node-to-sexp symex-ts--current-node)))

(defun symex-ts-get-current-node ()
  "Return the current node.
Automatically set it to the node at point if necessary."
  (unless symex-ts--current-node
    (symex-ts-set-current-node-from-point))
  symex-ts--current-node)

(defun symex-ts-set-current-node-from-point ()
  "Set the current node to the top-most node at point."
  (symex-ts--set-current-node (symex-ts-get-topmost-node-at-point)))

(defun symex-ts-get-topmost-node-at-point ()
  "Return the top-most node at the current point."
  (let ((root (tsc-root-node tree-sitter-tree))
        (p (point)))
    (symex-ts--get-topmost-node (tsc-get-named-descendant-for-position-range root p p))))


(defun symex-ts-move-prev-sibling (&optional count)
  "Move the point to the current node's previous sibling if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'tsc-get-prev-named-sibling (symex-make-move -1 0) count))

(defun symex-ts-move-next-sibling (&optional count)
  "Move the point to the current node's next sibling if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'tsc-get-next-named-sibling (symex-make-move 1 0) count))

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


(provide 'symex-ts)
;;; symex-ts.el ends here
