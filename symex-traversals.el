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

(defun symex-goto-first ()
  "Select first symex at present level."
  (interactive)
  (let ((traversal
         (symex-compile-traversal
          (circuit
           (move backward)))))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-last ()
  "Select last symex at present level."
  (interactive)
  (let ((traversal
         (symex-compile-traversal
          (circuit
           (move forward)))))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-outermost ()
  "Select outermost symex."
  (interactive)
  (let ((traversal
         (symex-compile-traversal
          (circuit
           (move out)))))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-innermost ()
  "Select innermost symex."
  (interactive)
  (let ((traversal
         (symex-compile-traversal
          (maneuver
           (move in)
           (circuit
            (protocol
             (circuit
              (move forward))
             (move in)))))))
    (symex-execute-traversal traversal))
  (point))

(deftraversal symex--traversal-preorder
  (protocol
   (protocol
    (move in)
    (move forward))
   (detour
    (precaution
     (move out)
     :after (lambda ()
              (not (symex--point-at-final-symex-p))))
    (move forward)))
  "Pre-order tree traversal, continuing to other trees.")

(deftraversal symex--traversal-preorder-in-tree
  (protocol
   (protocol
    (move in)
    (move forward))
   (detour
    (precaution
     (move out)
     :after (lambda ()
              (not (symex--point-at-root-symex-p))))
    (move forward)))
  "Pre-order tree traversal.")

(defvar symex--traversal-postorder
  (let* ((traverse-in
          (symex-compile-traversal
            (circuit
             (maneuver
              (move in)
              (circuit
               (move forward))))))
         (traverse-backwards-and-in
          (symex-compile-traversal
           (maneuver (move backward)
                     traverse-in))))
    (symex-compile-traversal (protocol traverse-backwards-and-in
                                       (move out))))
  "Post-order tree traversal, continuing to other trees.")

(defvar symex--traversal-postorder-in-tree
  (let* ((traverse-in
          (symex-compile-traversal
           (circuit
            (maneuver
             (move in)
             (circuit
              (move forward))))))
         (traverse-backwards-and-in
          (symex-compile-traversal
           (precaution
            (maneuver
             (move backward)
             traverse-in)
            :before (lambda ()
                      (not (symex--point-at-root-symex-p)))))))
    (symex-compile-traversal (protocol traverse-backwards-and-in
                                       (move out))))
  "Post-order tree traversal.")

(defun symex-traverse-forward ()
  "Traverse symex as a tree, using pre-order traversal."
  (interactive)
  (let ((traversal symex--traversal-preorder))
    (let ((result (symex-execute-traversal traversal)))
      (message "%s" result)
      result)))

(defun symex-traverse-forward-in-tree ()
  "Traverse symex forward using pre-order traversal, stopping at end of tree."
  (interactive)
  (let ((traversal symex--traversal-preorder-in-tree))
    (let ((result (symex-execute-traversal traversal)))
      (message "%s" result)
      result)))

(defun symex-traverse-backward ()
  "Traverse symex as a tree, using converse post-order traversal."
  (interactive)
  (let ((traversal symex--traversal-postorder))
    (let ((result (symex-execute-traversal traversal)))
      (message "%s" result)
      result)))

(defun symex-traverse-backward-in-tree ()
  "Traverse symex backward using post-order traversal, stopping at root of tree."
  (interactive)
  (let ((traversal symex--traversal-postorder-in-tree))
    (let ((result (symex-execute-traversal traversal)))
      (message "%s" result)
      result)))


(provide 'symex-traversals)
;;; symex-traversals.el ends here
