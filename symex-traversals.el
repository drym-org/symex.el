;;; symex-traversals.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Common traversals for symexes.
;;

;;; Code:


(require 'symex-data)
(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-dsl)

;;;;;;;;;;;;;;;;;;
;;; TRAVERSALS ;;;
;;;;;;;;;;;;;;;;;;

(deftraversal symex--traversal-goto-first
  (circuit (move backward))
  "Go to first symex at present level.")

(deftraversal symex--traversal-goto-last
  (circuit (move forward))
  "Go to last symex at present level.")

(deftraversal symex--traversal-goto-outermost
  (circuit (move out))
  "Go to outermost (root) symex in present tree.")

(defun symex-goto-first ()
  "Select first symex at present level."
  (interactive)
  (symex-execute-traversal symex--traversal-goto-first)
  (point))

(defun symex-goto-last ()
  "Select last symex at present level."
  (interactive)
  (symex-execute-traversal symex--traversal-goto-last)
  (point))

(defun symex-goto-outermost ()
  "Select outermost symex."
  (interactive)
  (symex-execute-traversal symex--traversal-goto-outermost)
  (point))

(defun symex-goto-innermost ()
  "Select innermost symex."
  (interactive)
  (symex-execute-traversal (symex-traversal
                            (maneuver (move in)
                                      (circuit (protocol (circuit (move forward))
                                                         (move in))))))
  (point))

(deftraversal symex--traversal-preorder
  (protocol (protocol (move in)
                      (move forward))
            (detour (precaution (move out)
                                (afterwards (not (at final))))
                    (move forward)))
  "Pre-order tree traversal, continuing to other trees.")

(deftraversal symex--traversal-preorder-in-tree
  (protocol (protocol (move in)
                      (move forward))
            (detour (precaution (move out)
                                (afterwards (not (at root))))
                    (move forward)))
  "Pre-order tree traversal.")

(deftraversal symex--traversal-postorder
  (protocol (maneuver (move backward)
                      (circuit (maneuver (move in)
                                         (circuit (move forward)))))
            (move out))
  "Post-order tree traversal, continuing to other trees.")

(deftraversal symex--traversal-postorder-in-tree
  (protocol (precaution (maneuver (move backward)
                                  (circuit (maneuver (move in)
                                                     (circuit (move forward)))))
                        (beforehand (not (at root))))
            (move out))
  "Post-order tree traversal.")

(defun symex-traverse-forward ()
  "Traverse symex as a tree, using pre-order traversal."
  (interactive)
  (let ((result (symex-execute-traversal symex--traversal-preorder)))
    (message "%s" result)
    result))

(defun symex-traverse-forward-in-tree ()
  "Traverse symex forward using pre-order traversal, stopping at end of tree."
  (interactive)
  (let ((result (symex-execute-traversal symex--traversal-preorder-in-tree)))
    (message "%s" result)
    result))

(defun symex-traverse-backward ()
  "Traverse symex as a tree, using converse post-order traversal."
  (interactive)
  (let ((result (symex-execute-traversal symex--traversal-postorder)))
    (message "%s" result)
    result))

(defun symex-traverse-backward-in-tree ()
  "Traverse symex backward using post-order traversal, stopping at root of tree."
  (interactive)
  (let ((result (symex-execute-traversal symex--traversal-postorder-in-tree)))
    (message "%s" result)
    result))


(provide 'symex-traversals)
;;; symex-traversals.el ends here
