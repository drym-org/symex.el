;;; symex-context.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Generic context setting in terms of predicates and actions.

;;; Code:

(defvar contextualize-contexts
  (make-hash-table :test 'equal)
  "A context is a pair (condition, handler).

When `contextualize-set-context` is called with
the category name, all contexts registered for that
category are checked for the condition, which if met
calls the corresponding handler.

The condition and the handlers are defined by the
client rather than in contextualize.")

(defvar symex-category 'symex
  "An identifier to pull up any contexts relevant for Symex.")

(defun symex-contextualize-elisp-p ()
  "Check if the context is Elisp."
  (and (member major-mode symex-elisp-modes)
       'elisp))

(defun symex-contextualize-racket-p ()
  "Check if the context is Racket."
  (and (member major-mode symex-racket-modes)
       'racket))

(defun symex-contextualize-common-lisp-p ()
  "Check if the context is Common Lisp."
  (and (equal major-mode 'lisp-mode)
       'lisp))

(defun symex-contextualize-scheme-p ()
  "Check if the context is Scheme."
  (and (equal major-mode 'scheme-mode)
       'scheme))

(defun symex-contextualize-clojure-p ()
  "Check if the context is Clojure."
  (and (member major-mode symex-clojure-modes)
       'clojure))

(defun symex-contextualize-arc-p ()
  "Check if the context is Arc."
  (and (equal major-mode 'arc-mode)
       'arc))

(defun symex-contextualize (language)
  "Set the environment for Symex for LANGUAGE."
  (cond ((eq 'elisp language)
         (fset symex-eval-function #'symex-eval-elisp))
        ((eq 'racket language)
         (fset symex-eval-function #'symex-eval-racket))
        ((eq 'lisp language)
         (fset symex-eval-function #'symex-eval-common-lisp))
        ((eq 'scheme language)
         (fset symex-eval-function #'symex-eval-scheme))
        ((eq 'clojure language)
         (fset symex-eval-function #'symex-eval-clojure))
        ((eq 'arc language)
         (fset symex-eval-function #'symex-eval-arc))))

(defun contextualize-register-context (category context)
  "Register a new CONTEXT for Symex."
  (let ((contexts (gethash category
                           contextualize-contexts
                           nil)))
    (puthash category
             (push context contexts)
             contextualize-contexts)))

(defun symex-initialize-contexts ()
  "Register all built-in contexts."
  (let ((contexts
         (list (cons #'symex-contextualize-elisp-p
                     #'symex-contextualize)
               (cons #'symex-contextualize-racket-p
                     #'symex-contextualize)
               (cons #'symex-contextualize-common-lisp-p
                     #'symex-contextualize)
               (cons #'symex-contextualize-scheme-p
                     #'symex-contextualize)
               (cons #'symex-contextualize-clojure-p
                     #'symex-contextualize)
               (cons #'symex-contextualize-arc-p
                     #'symex-contextualize))))
    (dolist (context contexts)
      (contextualize-register-context symex-category
                                      context))))

(defun symex-disable-contexts ()
  "De-register any symex-related contexts."
  (contextualize-remove-category symex-category))

(defun contextualize-remove-category (category)
  "Unregister CATEGORY."
  (remhash category contextualize-contexts))

(defun contextualize-predicate (context)
  "Get the predicate for CONTEXT."
  (car context))

(defun contextualize-handler (context)
  "Get the handler for CONTEXT."
  (cdr context))

(defun contextualize-list-categories ()
  "List all categories currently registered."
  (hash-table-keys contextualize-contexts))

(defun contextualize-list-contexts (category)
  "List all contexts in CATEGORY."
  (gethash category contextualize-contexts))

(defun contextualize-set-context (category)
  "Set context for CATEGORY.

Loop through all contexts configured for CATEGORY,
check the condition for each and call the corresponding
handler if the condition is met."
  (dolist (ctx (gethash category contextualize-contexts))
    (let ((p (contextualize-predicate ctx))
          (h (contextualize-handler ctx)))
      (let ((result (funcall p)))
        (when result (funcall h result))))))


(provide 'symex-context)
;;; symex-context.el ends here
