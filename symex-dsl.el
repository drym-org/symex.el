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


(defun symex--compile-traversal-helper (traversal)
  "Helper function to compile a TRAVERSAL.

This is useful for mapping a compiler macro over a list of
traversal specifications."
  `(symex-compile-traversal ,traversal))

(defmacro symex--compile-protocol (&rest options)
  "Compile a protocol from Symex DSL -> Lisp.

OPTIONS - see underlying Lisp implementation."
  `(symex-make-protocol
    ,@(mapcar 'symex--compile-traversal-helper options)))

(defmacro symex--compile-maneuver (&rest phases)
  "Compile a maneuver from Symex DSL -> Lisp.

PHASES - see underlying Lisp implementation."
  `(symex-make-maneuver
    ,@(mapcar 'symex--compile-traversal-helper phases)))

(defmacro symex--compile-detour (reorientation traversal)
  "Compile a detour from Symex DSL -> Lisp.

REORIENTATION - see underlying Lisp implementation.
TRAVERSAL - see underlying Lisp implementation."
  `(symex-make-detour (symex-compile-traversal ,reorientation)
                      (symex-compile-traversal ,traversal)))

(defmacro symex--compile-circuit (traversal &optional times)
  "Compile a circuit from Symex DSL -> Lisp.

TRAVERSAL - see underlying Lisp implementation.
TIMES - see underlying Lisp implementation."
  `(symex-make-circuit (symex-compile-traversal ,traversal)
                       ,times))

(defun symex--rewrite-precaution-component (arg)
  "Rewrite DSL syntax to Lisp syntax in a protocol specification.

ARG - an argument provided to the protocol definition."
  (cond ((not (symbolp arg)) arg)
        ((or (equal ':before arg)
             (equal ':beforehand arg))
         ':pre-condition)
        ((or (equal ':after arg)
             (equal ':afterwards arg))
         ':post-condition)))

(defmacro symex--compile-precaution (traversal &rest args)
  "Compile a precaution from Symex DSL -> Lisp.

TRAVERSAL - see underlying Lisp implementation.
ARGS - arguments provided in the precaution specification.  This may
include the keyword arguments :before and :after together with
the corresponding boolean functions."
  `(symex-make-precaution (symex-compile-traversal ,traversal)
                          ,@(mapcar 'symex--rewrite-precaution-component args)))

(defmacro symex--compile-move (direction)
  "Compile a move from Symex DSL -> Lisp.

DIRECTION - the direction to move in, which could be one of:
forward, backward, in, or out."
  (cond ((equal 'forward direction)
         '(symex-make-move 1 0))
        ((equal 'backward direction)
         '(symex-make-move -1 0))
        ((equal 'in direction)
         '(symex-make-move 0 1))
        ((equal 'out direction)
         '(symex-make-move 0 -1))))

(defmacro symex-compile-traversal (traversal)
  "Compile a traversal from Symex DSL -> Lisp.

TRAVERSAL could be any traversal specification, e.g. a maneuver,
a detour, a move, etc., which is specified using the Symex DSL."
  (cond ((not (listp traversal)) traversal)  ; e.g. a variable containing a traversal
        ((equal 'protocol (car traversal))
         `(symex--compile-protocol ,@(cdr traversal)))
        ((equal 'maneuver (car traversal))
         `(symex--compile-maneuver ,@(cdr traversal)))
        ((equal 'detour (car traversal))
         `(symex--compile-detour ,@(cdr traversal)))
        ((equal 'circuit (car traversal))
         `(symex--compile-circuit ,@(cdr traversal)))
        ((equal 'precaution (car traversal))
         `(symex--compile-precaution ,@(cdr traversal)))
        ((equal 'move (car traversal))
         `(symex--compile-move ,@(cdr traversal)))))

(defmacro deftraversal (name traversal &optional docstring)
  "Define a symex traversal using the Symex DSL.

NAME is the name of the traversal.  The defined traversal will be
assigned to a variable with this name.
TRAVERSAL is the specification of the traversal in the Symex DSL.
This can be thought of as the 'program' written in the DSL, which
will be compiled into Lisp and can be executed when needed.
An optional DOCSTRING will be used as documentation for the variable
NAME to which the traversal is assigned."
  `(defvar ,name (symex-compile-traversal ,traversal) ,docstring))


(provide 'symex-dsl)
;;; symex-dsl.el ends here
