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
          move-go-backward)))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-last ()
  "Select last symex at present level."
  (interactive)
  (let ((traversal
         (symex-make-circuit
          move-go-forward)))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-outermost ()
  "Select outermost symex."
  (interactive)
  (let ((traversal
         (symex-make-circuit
          move-go-out)))
    (symex-execute-traversal traversal))
  (point))

(defun symex-goto-innermost ()
  "Select innermost symex."
  (interactive)
  (let ((traversal
         (symex-make-maneuver
          move-go-in
          (symex-make-circuit
           (symex-make-protocol
            (symex-make-circuit
             move-go-forward)
            move-go-in)))))
    (symex-execute-traversal traversal))
  (point))

;; TODO: is there a way to "monadically" build the tree data structure
;; (or ideally, do an arbitrary structural computation) as part of this traversal?
;; key is, it has to be inferrable from inputs and outputs alone, i.e. specifically
;; from the result of invocation of e.g. forward-symex
(defun symex-traverse-forward (&optional flow)
  "Traverse symex as a tree, using pre-order traversal.

If FLOW is true, continue from one tree to another.  Otherwise, stop at end of
current rooted tree."
  (interactive)
  (let ((exit-until-root
         (symex-make-precaution
          move-go-out
          :post-condition (lambda ()
                            (not (point-at-root-symex?)))))
        (exit-until-end-of-buffer
         (symex-make-precaution
          move-go-out
          :post-condition (lambda ()
                            (not (point-at-final-symex?))))))
    (let ((traversal
           (symex-make-protocol
            (symex-make-protocol
             move-go-in
             move-go-forward)
            (symex-make-detour
             (if flow
                 exit-until-end-of-buffer
               exit-until-root)
             move-go-forward))))
      (let ((result (symex-execute-traversal traversal)))
        (message "%s" result)
        result))))

(defun symex-traverse-backward (&optional flow)
  "Traverse symex as a tree, using converse post-order traversal.

If FLOW is true, continue from one tree to another.  Otherwise, stop at root of
current tree."
  (interactive)
  (let* ((postorder-in
          (symex-make-circuit
           (symex-make-maneuver
            move-go-in
            (symex-make-circuit
             move-go-forward))))
         (postorder-backwards-in
          (symex-make-maneuver move-go-backward
                               postorder-in))
         (postorder-backwards-in-tree
          (symex-make-precaution
           (symex-make-maneuver
            move-go-backward
            postorder-in)
           :pre-condition (lambda ()
                            (not (point-at-root-symex?))))))
    (let* ((traversal
            (symex-make-protocol
             (if flow
                 postorder-backwards-in
               postorder-backwards-in-tree)
             move-go-out)))
      (let ((result (symex-execute-traversal traversal)))
        (message "%s" result)
        result))))

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
                                (setq best-branch-position (point)))))))))
  (switch-backward))

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
