;;; symex-interface-elisp.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Interface for the Elisp language

;;; Code:

(require 'evil)
(require 'symex-interop)
(require 'symex-interface)

(defun symex-eval-elisp ()
  "Eval Elisp symex."
  (eval-last-sexp nil))

(defun symex-eval-definition-elisp ()
  "Eval entire containing definition."
  (eval-defun nil))

(defun symex-eval-print-elisp ()
  "Eval symex and print result in buffer."
  (eval-print-last-sexp))

(defun symex-describe-symbol-elisp ()
  "Describe symbol at point."
  (describe-symbol (symbol-at-point)))

(defun symex-repl-elisp ()
  "Enter elisp REPL, context-aware.

If the REPL is already visible, switch to that window.  Otherwise,
if there is only one window, open REPL in a new window.  Otherwise
open in most recently used other window."
  (let ((window (get-buffer-window "*ielm*")))
    (cond (window (select-window window))
          ((= 1 (length (window-list)))
           (evil-window-vsplit)
           (evil-window-right 1)
           (ielm))
          (t (evil-window-mru)  ; better LRU
             (ielm)))
    (goto-char (point-max))
    (symex-enter-lowest)))

(defun symex-switch-to-scratch-buffer-elisp ()
  "Switch to scratch buffer."
  (let ((buffer-name "*scratch*"))
    (switch-to-buffer (or (get-buffer buffer-name)
                          (symex--new-scratch-buffer buffer-name)))))

(defvar symex-elisp-modes (list 'lisp-interaction-mode
                                'emacs-lisp-mode
                                'inferior-emacs-lisp-mode))

(defun symex-interface-register-elisp ()
  "Register the Emacs Lisp runtime interface."
  (symex-interface-extend
   symex-elisp-modes
   (list
    :eval #'symex-eval-elisp
    :eval-definition #'symex-eval-definition-elisp
    :eval-pretty #'symex-eval-elisp
    :eval-print #'symex-eval-print-elisp
    :describe-symbol #'symex-describe-symbol-elisp
    :repl #'symex-repl-elisp
    :run #'eval-buffer
    :switch-to-scratch-buffer #'symex-switch-to-scratch-buffer-elisp)))

(provide 'symex-interface-elisp)
;;; symex-interface-elisp.el ends here
