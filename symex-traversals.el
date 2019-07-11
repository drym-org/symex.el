;;; symex-traversals.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6.1") (lispy "0.26.0") (paredit "24") (evil-cleverparens "20170718.413") (dash-functional "2.15.0") (evil "20180914.1216") (smartparens "20181007.1501") (racket-mode "20181030.1345") (geiser "0.10") (evil-surround "20180102.1401") (hydra "20180918.1529"))

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
(require 'symex-misc)

;;;;;;;;;;;;;;;;;;
;;; TRAVERSALS ;;;
;;;;;;;;;;;;;;;;;;

(defun symex-goto-first ()
  "Select first symex at present level."
  (interactive)
  (let ((traversal
         (symex-make-circuit
          symex--move-go-backward)))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-last ()
  "Select last symex at present level."
  (interactive)
  (let ((traversal
         (symex-make-circuit
          symex--move-go-forward)))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-outermost ()
  "Select outermost symex."
  (interactive)
  (let ((traversal
         (symex-make-circuit
          symex--move-go-out)))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-innermost ()
  "Select innermost symex."
  (interactive)
  (let ((traversal
         (symex-make-maneuver
          symex--move-go-in
          (symex-make-circuit
           (symex-make-protocol
            (symex-make-circuit
             symex--move-go-forward)
            symex--move-go-in)))))
    (symex-execute-traversal traversal))
  (point))

(defvar symex--traversal-preorder
  (symex-make-protocol
   (symex-make-protocol
    symex--move-go-in
    symex--move-go-forward)
   (symex-make-detour
    (symex-make-precaution
     symex--move-go-out
     :post-condition (lambda ()
                       (not (point-at-final-symex?))))
    symex--move-go-forward))
  "Pre-order tree traversal, continuing to other trees.")

(defvar symex--traversal-preorder-in-tree
  (symex-make-protocol
   (symex-make-protocol
    symex--move-go-in
    symex--move-go-forward)
   (symex-make-detour
    (symex-make-precaution
     symex--move-go-out
     :post-condition (lambda ()
                       (not (point-at-root-symex?))))
    symex--move-go-forward))
  "Pre-order tree traversal.")

(defvar symex--traversal-postorder
  (let* ((postorder-in
          (symex-make-circuit
           (symex-make-maneuver
            symex--move-go-in
            (symex-make-circuit
             symex--move-go-forward))))
         (postorder-backwards-in
          (symex-make-maneuver symex--move-go-backward
                               postorder-in)))
    (symex-make-protocol postorder-backwards-in
                         symex--move-go-out))
  "Post-order tree traversal, continuing to other trees.")

(defvar symex--traversal-postorder-in-tree
  (let* ((postorder-in
          (symex-make-circuit
           (symex-make-maneuver
            symex--move-go-in
            (symex-make-circuit
             symex--move-go-forward))))
         (postorder-backwards-in-tree
          (symex-make-precaution
           (symex-make-maneuver
            symex--move-go-backward
            postorder-in)
           :pre-condition (lambda ()
                            (not (point-at-root-symex?))))))
    (symex-make-protocol postorder-backwards-in-tree
                         symex--move-go-out))
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

(defun symex-index ()  ; TODO: may be better framed as a computation
  "Get relative (from start of containing symex) index of current symex."
  (interactive)
  (save-excursion
    (symex-select-nearest)
    (let ((original-location (point)))
      (let ((current-location (symex-goto-first))
            (result 0))
        (while (< current-location original-location)
          (symex-go-forward)
          (setq current-location (point))
          (setq result (1+ result)))
        result))))

(defun symex-switch-branch-backward ()
  "Switch branch backward."
  (interactive)
  (let ((index (symex-index))
        (closest-index -1)
        (best-branch-position (point)))
    (defun switch-backward ()
      (if (point-at-root-symex?)
          (goto-char best-branch-position)
        (symex-go-out)
        (if-stuck (switch-backward)
                  (symex-go-backward)
                  (if-stuck (switch-backward)
                            (symex-go-in)
                            (symex-go-forward index)
                            (let ((current-index (symex-index)))
                              (when (and (< current-index
                                            index)
                                         (> current-index
                                            closest-index))
                                (setq closest-index current-index)
                                (setq best-branch-position (point))))))))
    (switch-backward)))

(defun symex-switch-branch-forward ()
  "Switch branch forward."
  (interactive)
  (let ((index (symex-index)))
    (symex-go-out)
    (symex-go-forward)
    (symex-go-in)
    (symex-go-forward index)))


(provide 'symex-traversals)
;;; symex-traversals.el ends here
