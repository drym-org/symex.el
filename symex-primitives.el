;;; symex-primitives.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Primitive navigations using third party libraries.  This layer of primitives
;; can be swapped out without changing the higher-level functionality provided
;; by the Symex DSL.  In the future it may make sense to directly operate on
;; an annotated representation of the AST here containing a mapping to buffer
;; positions and other metadata relevant for editing purposes.

;;; Code:


(require 'symex-primitives-lisp)
(require 'symex-ts)

(defun symex--forward (&optional count)
  "Forward symex.

Go forward COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-forward` instead)."
  (interactive)
  (if tree-sitter-mode
      (symex-ts-move-next-sibling count)
    (symex-lisp--forward count)))

(defun symex--backward (&optional count)
  "Backward symex.

Go backward COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-backward` instead)."
  (interactive)
  (if tree-sitter-mode
      (symex-ts-move-prev-sibling count)
    (symex-lisp--backward count)))

(defun symex--enter (&optional count)
  "Enter higher symex level.

Enter COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-up` instead)."
  (interactive)
  (if tree-sitter-mode
      (symex-ts-move-child count)
    (symex-lisp--enter count)))

(defun symex--exit (&optional count)
  "Exit to lower symex level.

Exit COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-down` instead)."
  (interactive)
  (if tree-sitter-mode
      (symex-ts-move-parent count)
    (symex-lisp--exit count)))


(provide 'symex-primitives)
;;; symex-primitives.el ends here
