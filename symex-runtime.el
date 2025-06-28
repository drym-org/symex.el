;;; symex-runtime.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Features specific to each language platform, like evaluation,
;; REPLs, documentation lookups.

;;; Code:

(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-traversals)
(require 'symex-tree)
(require 'symex-interface)

(defun symex--evaluate ()
  "Evaluate symex."
  (let ((start (point))
        (end (symex--get-end-point 1)))
    ;; selected symexes will have the cursor on the starting paren
    (goto-char end)
    (funcall (symex-interface-get-method :eval))
    (goto-char start)))

(defun symex-evaluate (count)
  "Evaluate COUNT symexes."
  (interactive "p")
  (save-excursion
    (let ((count (min count
                      (symex-remaining-length))))
      (dotimes (_ count)
        (symex--evaluate)
        (symex--go-forward)))))

(defun symex-eval-recursive ()
  "Evaluate a symex recursively.

Eval starting at the leaves and proceed down to the root, similarly
to how the Lisp interpreter does it (when it is following
\"applicative-order evaluation\")."
  (interactive)
  (save-excursion
    (symex-eval
     (symex-traversal
       (circuit symex--traversal-preorder-in-tree)))
    (symex--do-while-traversing #'symex--evaluate
                                symex--traversal-postorder-in-tree)))

(defun symex-evaluate-remaining ()
  "Evaluate the remaining symexes at this level."
  (interactive)
  (save-excursion
    (symex--do-while-traversing #'symex--evaluate
                                (symex-make-move 1 0))))

(defun symex-evaluate-definition ()
  "Evaluate entire containing symex definition."
  (interactive)
  (funcall (symex-interface-get-method :eval-definition)))

(defun symex-evaluate-pretty ()
  "Evaluate Symex and transform output into a useful string representation."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (funcall (symex-interface-get-method :eval-pretty))))

(defun symex-eval-print ()
  "Eval symex and print result in buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (funcall (symex-interface-get-method :eval-print))))

(defun symex-evaluate-thunk ()
  "Evaluate symex as a thunk.

This treats the symex as a thunk -- i.e. a function that takes no
arguments -- by (transparently) wrapping it in parens and then
executing it."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (funcall (symex-interface-get-method :eval-thunk))))

(defun symex-describe ()
  "Lookup doc on symex."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (funcall (symex-interface-get-method :describe-symbol))))

(defun symex-repl ()
  "Go to REPL."
  (interactive)
  (funcall (symex-interface-get-method :repl)))

(defun symex-run ()
  "Send to REPL."
  (interactive)
  (funcall (symex-interface-get-method :run)))

(provide 'symex-runtime)
;;; symex-runtime.el ends here
