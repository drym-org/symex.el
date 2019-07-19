;;; symex-dsl.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Syntax specification for the Symex DSL
;;

;;; Code:


(defun expand-traversal (traversal)
  "expand traversal"
  `(traversal-expander ,traversal))

(defmacro protocol-expander (&rest options)
  `(symex-make-protocol
    ,@(mapcar 'expand-traversal options)))

(defmacro maneuver-expander (&rest phases)
  `(symex-make-maneuver
    ,@(mapcar 'expand-traversal phases)))

(defmacro detour-expander (reorientation traversal)
  `(symex-make-detour (traversal-expander ,reorientation)
                      (traversal-expander ,traversal)))

(defmacro circuit-expander (traversal &optional times)
  `(symex-make-circuit (traversal-expander ,traversal)
                       ,times))

(defun rewrite-precaution-component (arg)
  "Rewrite DSL syntax to Lisp syntax."
  (cond ((not (symbolp arg)) arg)
        ((equal ":before" (symbol-name arg))
         ':pre-condition)
        ((equal ":after" (symbol-name arg))
         ':post-condition)))

(defmacro precaution-expander (traversal &rest args)
  `(symex-make-precaution (traversal-expander ,traversal) ,@(mapcar 'rewrite-precaution-component args)))

(defmacro move-expander (direction)
  (cond ((equal "forward" (symbol-name direction))
         '(symex-make-move 1 0))
        ((equal "backward" (symbol-name direction))
         '(symex-make-move -1 0))
        ((equal "in" (symbol-name direction))
         '(symex-make-move 0 1))
        ((equal "out" (symbol-name direction))
         '(symex-make-move 0 -1))))

(defmacro traversal-expander (traversal)
  (cond ((not (listp traversal)) traversal)
        ((equal "protocol" (symbol-name (car traversal)))
         `(protocol-expander ,@(cdr traversal)))
        ((equal "maneuver" (symbol-name (car traversal)))
         `(maneuver-expander ,@(cdr traversal)))
        ((equal "detour" (symbol-name (car traversal)))
         `(detour-expander ,@(cdr traversal)))
        ((equal "circuit" (symbol-name (car traversal)))
         `(circuit-expander ,@(cdr traversal)))
        ((equal "precaution" (symbol-name (car traversal)))
         `(precaution-expander ,@(cdr traversal)))
        ((equal "move" (symbol-name (car traversal)))
         `(move-expander ,@(cdr traversal)))))

(defmacro deftraversal (name traversal &optional docstring)
  "Define a symex traversal."
  `(defvar ,name (traversal-expander ,traversal) ,docstring))


(provide 'symex-dsl)
;;; symex-dsl.el ends here
