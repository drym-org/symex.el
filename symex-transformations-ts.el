;;; symex-transformations-ts.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Standard mutative operations to be performed on symexes via Tree
;; Sitter.

;;; Code:

(require 'symex-utils)
(require 'symex-ts)
(require 'symex-primitives)

(defun symex-ts--change-notifier (_ranges _parser &rest _args)
  "Notify of any changes to the contents of the buffer."
  (when (and symex-editing-mode
             (treesit-node-check symex-ts--current-node 'outdated))
    (symex-ts-set-current-node-from-point)))

(defun symex-ts-clear ()
  "Clear contents of symex."
  (when symex-ts--current-node
    (let ((child-count (symex-ts--count-named-children symex-ts--current-node)))

      ;; If the node has children, delete them. Otherwise, just delete
      ;; the current node using `symex-ts-delete-node-forward'.
      (if (> child-count 0)
          (let ((first-child (symex-ts--get-nth-named-child symex-ts--current-node 0))
                (last-child (symex-ts--get-nth-named-child symex-ts--current-node (1- child-count))))
            (when (and first-child last-child)
              (kill-region (symex-ts--node-start-position first-child) (symex-ts--node-end-position last-child))))
        (symex-ts-delete-node-forward 1)))))

(defun symex-ts-comment (&optional count)
  "Comment out COUNT expressions."
  (when (symex-ts-available-p)
    (let* ((count (or count 1))
           (node (symex-ts-get-current-node))
           (start-pos (symex-ts--node-start-position node))
           (end-pos (symex-ts--get-end-point count t t)))
      (save-excursion (set-mark start-pos)
                      (goto-char end-pos)
                      (comment-dwim nil))
      (symex-ts-set-current-node-from-point))))

(defun symex-ts--reset-after-delete ()
  "Tidy things up after deletion.

If the deletion results in an empty line it will be removed."
  (when (symex--current-line-empty-p)
    (if (symex--previous-line-empty-p)
        (symex--join-to-non-whitespace)
      (symex--delete-whole-line))))

(defun symex-ts-delete-node-forward (&optional count)
  "Delete COUNT nodes forward from the current node."
  (interactive "p")
  ;; TODO: this is no longer used outside of this module
  (let* ((count (or count 1))
         (node (symex-ts-get-current-node))
         (start-pos (symex-ts--node-start-position node))
         (end-pos (symex-ts--node-end-position
                   (if (> count 1)
                       (symex-ts--get-nth-sibling-from-node
                        node
                        #'symex-ts--get-next-named-sibling count)
                     node))))

    ;; Delete the node's region
    (kill-region start-pos end-pos))
  t)

(defun symex-ts-insert-at-beginning ()
  "Insert at beginning of symex."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (symex-ts--node-start-position (symex-ts-get-current-node)))))

(defun symex-ts-insert-at-end ()
  "Insert at end of symex."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (symex-ts--node-end-position (symex-ts-get-current-node)))))

(defun symex-ts-insert-before ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (symex-ts--node-start-position (symex-ts-get-current-node)))
    (insert " ")
    (backward-char)))

(defun symex-ts-append-after ()
  "Append after the end of the symex.

Since non-Lisp languages don't really have a syntactic distinction
between the inside and the outside of expressions, this is just an
alias for inserting at the end."
  (interactive)
  (symex-ts-insert-at-end))

(defun symex-ts-open-line-after ()
  "Open new line after symex."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (symex-ts--get-end-point 1 nil t))
    (newline-and-indent)))

(defun symex-ts-open-line-before ()
  "Open new line before symex."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (symex-ts--node-start-position (symex-ts-get-current-node)))
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)
    (move-end-of-line 1)))

(defun symex-ts-replace ()
  "Replace contents of symex."
  (when symex-ts--current-node
    (let* ((child-count (symex-ts--count-named-children symex-ts--current-node))

           ;; Get new position for insertion: if the node has children
           ;; then the start of the first child node, otherwise the
           ;; current point.
           (new-pos (if (> child-count 0)
                        (symex-ts--node-start-position (symex-ts--get-nth-named-child symex-ts--current-node 0))
                      (point))))

      (symex-ts-clear)
      (goto-char new-pos))))

(defun symex-ts--append-newline ()
  "Append COUNT newlines after symex."
  (let ((end (symex--get-end-point 1)))
    (symex-save-excursion
      (goto-char end)
      (newline-and-indent 1)
      (fixup-whitespace))))

(defun symex-ts-append-newline (count)
  "Append COUNT newlines after symex."
  (dotimes (_ count)
    (symex-ts--append-newline)))

(defun symex-ts--not-implemented ()
  "Features that are not implemented for treesitter."
  (message "Not implemented for tree-sitter."))


(provide 'symex-transformations-ts)
;;; symex-transformations-ts.el ends here
