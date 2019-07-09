;;; symex-interface-elisp.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Interface for the Elisp language
;;

;;; Code:


(require 'evil)


(defun symex-eval-elisp ()
  "Eval Elisp symex."
  (interactive)
  (eval-last-sexp nil))

(defun symex-eval-definition-elisp ()
  "Eval entire containing definition."
  (eval-defun nil))

(defun symex-eval-pretty-elisp ()
  "Evaluate symex and render the result in a useful string form."
  (interactive)
  (symex-eval-elisp))

(defun symex-eval-print-elisp ()
  "Eval symex and print result in buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-print-last-sexp)))

(defun symex-describe-symbol-elisp ()
  "Describe symbol at point."
  (interactive)
  (describe-symbol (symbol-at-point)))

(defun symex-repl-elisp ()
  "Enter elisp REPL, context-aware.

If there is only one window, open REPL in a new window.  Otherwise
open in current window."
  (interactive)
  (when (= (length (window-list))
           1)
    (progn (evil-window-vsplit)
           (evil-window-right 1)))
  (ielm))


(provide 'symex-interface-elisp)
;;; symex-interface-elisp.el ends here
