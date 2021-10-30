;;; symex-ts.el --- Primitive navigations using Tree Sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Simon Pugnet <simon@polaris64.net>
;; URL: https://github.com/countvajhula/symex.el

;;; Commentary:

;; This module defines a set of movement primitives which use Tree
;; Sitter to navigate a buffer's abstract syntax tree.
;;
;; A hydra is also provided, however this is intended for debugging
;; purposes. It allows the primitives to be used directly without the
;; rest of Symex.

;;; Code:

(require 'hydra)
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
  "Update the highlight overlay to match the start/end position
of NODE."
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
  "Return the highest node in the tree starting from NODE that
has the same start position."
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
  "Descend from NODE to first child recursively until the child
node has a sibling or is a leaf."
  (let ((child (tsc-get-nth-named-child node 0)))
    (if child
        (if (or (tsc-get-prev-named-sibling child) (tsc-get-next-named-sibling child))
            child
          (symex-ts--descend-to-child-with-sibling child))
      node)))

(defun symex-ts--ascend-to-parent-with-sibling (node)
  "Ascend from NODE to parent recursively until the parent node has
a sibling or is the root."
  (let ((parent (tsc-get-parent node)))
    (if parent
        (if (or (tsc-get-prev-named-sibling parent) (tsc-get-next-named-sibling parent))
            parent
          (symex-ts--ascend-to-parent-with-sibling parent))
      node)))

(defun symex-ts--add-move (init-move move-delta)
  "Add MOVE-DELTA to INIT-MOVE, each being a list with two integer
elements."
  (list (+ (nth 0 init-move) (nth 0 move-delta))
        (+ (nth 1 init-move) (nth 1 move-delta))))

(defun symex-ts--move-with-count (fn move-delta &optional count)
  "Move the point from the current node if possible.

Movement is defined by FN, which should be a function which
returns the appropriate neighbour node.

Move COUNT times, defaulting to 1.

Return a Symex move (list with x,y node offsets tagged with
'move) or nil if no movement was performed."
  (let ((target-node nil)
        (move '(0 0))
        (cursor (symex-ts-get-current-node)))
    (dotimes (_ (or count 1))
      (let ((new-node (funcall fn cursor)))
        (when (and new-node (not (eq new-node cursor)))
          (setq move (symex-ts--add-move move move-delta))
          (setq cursor new-node
                target-node cursor))))
    (when target-node (symex-ts--set-current-node target-node))

    ;; Return the Symex move that was executed, or nil to signify that
    ;; the movement failed
    (when (not (equal '(0 0) move)) (flatten-list (list 'move move)))))

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
  (symex-ts--move-with-count #'tsc-get-prev-named-sibling '(-1 0) count))

(defun symex-ts-move-next-sibling (&optional count)
  "Move the point to the current node's next sibling if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'tsc-get-next-named-sibling '(1 0) count))

(defun symex-ts-move-parent (&optional count)
  "Move the point to the current node's parent if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'symex-ts--ascend-to-parent-with-sibling '(0 -1) count))

(defun symex-ts-move-child (&optional count)
  "Move the point to the current node's first child if possible.

Move COUNT times, defaulting to 1."
  (interactive "p")
  (symex-ts--move-with-count #'symex-ts--descend-to-child-with-sibling '(0 1) count))

(defun symex-ts-delete-node-backward (&optional count)
  "Delete COUNT nodes backward from the current node."
  (interactive "p")
  (let* ((count (or count 1))
         (node (symex-ts-get-current-node))
         (end-pos (tsc-node-end-position node))
         (start-pos (tsc-node-start-position
                     (if (> count 1)
                         (symex-ts--get-nth-sibling-from-node node #'tsc-get-prev-named-sibling count)
                       node))))
    (delete-region start-pos end-pos))
  (symex-ts--after-tree-modification))

(defun symex-ts-delete-node-forward (&optional count)
  "Delete COUNT nodes forward from the current node."
  (interactive "p")
  (let* ((count (or count 1))
         (node (symex-ts-get-current-node))
         (start-pos (tsc-node-start-position node))
         (end-pos (tsc-node-end-position
                   (if (> count 1)
                       (symex-ts--get-nth-sibling-from-node node #'tsc-get-next-named-sibling count)
                     node))))
    (delete-region start-pos end-pos))
  (symex-ts--after-tree-modification))


(defun symex-ts--hydra-exit ()
  "Handle Hydra exit."
  (symex-ts--delete-overlay))

(defhydra hydra-symex-ts (:post (symex-ts--hydra-exit))
  "Symex-TS"
  ("d" symex-ts-current-node-sexp "DEBUG NODE")

  ("h" symex-ts-move-prev-sibling "prev")
  ("l" symex-ts-move-next-sibling "next")
  ("j" symex-ts-move-parent "parent")
  ("k" symex-ts-move-child "child")

  ("X" symex-ts-delete-node-backward "delete node (backward)")
  ("x" symex-ts-delete-node-forward "delete node (forward)"))

(defun symex-ts-launch ()
  "Start the Symex-TS hydra."
  (interactive)

  ;; Set the current node to the top-most node at point
  (symex-ts-set-current-node-from-point)

  ;; Launch hydra
  (hydra-symex-ts/body))


(provide 'symex-ts)
;; symex-ts.el ends here
