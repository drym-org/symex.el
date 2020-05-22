;;; symex-primitives.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Primitive navigations using third party libraries.  This layer of primitives
;; can be swapped out without changing the higher-level functionality provided
;; by the Symex DSL.  In the future it may make sense to directly operate on
;; an annotated representation of the AST here containing a mapping to buffer
;; positions and other metadata relevant for editing purposes.
;;

;;; Code:


(require 'lispy)
(require 'paredit)
(require 'symex-data)

;;;;;;;;;;;;;;;;;;
;;; PRIMITIVES ;;;
;;;;;;;;;;;;;;;;;;

(defmacro symex-if-stuck (do-what operation &rest body)
  "Attempt OPERATION and if it fails, then do DO-WHAT."
  `(let ((orig-pt (point)))
     ,operation
     (if (= orig-pt (point))
         ,do-what
       ,@body)))

(defun symex--point-at-root-symex-p ()
  "Check if point is at a root symex."
  (save-excursion
    (symex-if-stuck t
                    (symex--exit)
                    nil)))

(defun symex--point-at-first-symex-p ()
  "Check if point is at the first symex at some level."
  (save-excursion
    (symex-if-stuck t
                    (symex--backward)
                    nil)))

(defun symex--point-at-last-symex-p ()
  "Check if point is at the last symex at some level."
  (save-excursion
    (symex-if-stuck t
                    (symex--forward)
                    nil)))

(defun symex--point-at-final-symex-p ()
  "Check if point is at the last symex in the buffer."
  (save-excursion
    (symex-if-stuck (progn (symex-if-stuck t
                                           (symex--exit)
                                           nil))
                    (symex--forward)
                    nil)))

(defun symex--point-at-initial-symex-p ()
  "Check if point is at the first symex in the buffer."
  (save-excursion
    (condition-case nil
        (progn (backward-sexp 1)
               (not (thing-at-point 'sexp)))
      (error nil))))

(defun symex-comment-line-p ()
  "Check if we're currently at the start of a comment line."
  (and (lispy-bolp)
       (looking-at-p ";")))

(defun symex-empty-list-p ()
  "Check if we're looking at an empty list."
  (save-excursion
    (and (lispy-left-p)
         (progn (forward-char 2) ;; need to go forward by 2 for some reason
                (lispy-right-p)))))

(defun symex--forward-one ()
  "Forward one symex."
  (let ((original-location (point))
        (result 0))
    (if (thing-at-point 'sexp)
        (condition-case nil
            (progn (forward-sexp 2)
                   (setq result 2))
          (error (condition-case nil
                     (progn (forward-sexp 1)
                            (setq result 1))
                   (error (setq result 0)))))
      (condition-case nil
          (progn (forward-sexp 1)
                 (setq result 1))
        (error (setq result 0))))
    (condition-case nil
        (progn (backward-sexp 1)
               (setq result (1- result)))
      (error nil))
    (let ((current-location (point)))
      (when (= original-location current-location)
        ;; happens at end of buffer
        (setq result 0)))
    result))

(defun symex--forward (&optional count)
  "Forward symex.

Go forward COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-forward` instead)."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (_ count)
      (let ((res (symex--forward-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move result 0))))

(defun symex--backward-one ()
  "Backward one symex."
  (let ((result 0))
    (unless (symex--point-at-initial-symex-p)
      (condition-case nil
          (progn (backward-sexp 1)
                 (setq result (1+ result)))
        (error nil)))
    result))

(defun symex--backward (&optional count)
  "Backward symex.

Go backward COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-backward` instead)."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (_ count)
      (let ((res (symex--backward-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move (- result) 0))))

(defun symex--enter-one ()
  "Enter one level."
  (let ((result 1))
    (cond ((symex-comment-line-p)
           (lispy-flow 1))
          ((and (lispy-left-p)
                (not (symex-empty-list-p)))
           (forward-char))
          ;; one-off - better to recognize #( as a delimiter
          ;; at the AST level
          ((looking-at (concat "[#'`]" lispy-left))
           (forward-char 2))
          (t (setq result 0)))
    result))

(defun symex--enter (&optional count)
  "Enter higher symex level.

Enter COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-up` instead)."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (_ count)
      (let ((res (symex--enter-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move 0 result))))

(defun symex--exit-one ()
  "Exit one level."
  (condition-case nil
      (progn (paredit-backward-up 1)
             (when (looking-back "[#'`]" (line-beginning-position))
               ;; one-off - better to recognize #( as a delimiter
               ;; at the AST level
               (backward-char))
             1)
    (error 0)))

(defun symex--exit (&optional count)
  "Exit to lower symex level.

Exit COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-down` instead)."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (_ count)
      (let ((res (symex--exit-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move 0 (- result)))))


(provide 'symex-primitives)
;;; symex-primitives.el ends here
