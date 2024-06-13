;;; symex-interface-fennel.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Interface for the Fennel language

;;; Code:

(require 'fennel-mode nil 'noerror)
(require 'symex-interop)


(declare-function fennel-find-definition  "ext:fennel-mode")
(declare-function fennel-repl "ext:fennel-mode")

(defun symex-eval-thunk-fennel ()
  "Evaluate symex as a 'thunk,' i.e. as a function taking no arguments."
  (let ((thunk-code (string-join
                     `("("
                       ,(buffer-substring (symex--get-starting-point)
                                          (point))
                       ")"))))
    (lisp-eval-string thunk-code)))

(defun symex-repl-fennel ()
  "Go to REPL."
  (let ((original-window (selected-window)))
    (fennel-repl fennel-program)
    (unless (eq original-window (selected-window))
      ;; if the REPL window is currently being created
      ;; then don't attempt to go to the bottom
      (goto-char (point-max))
      (symex-enter-lowest))))

(defun symex-run-fennel ()
  "Evaluate buffer."
  (lisp-eval-region (point-min) (point-max)))

(defun symex-switch-to-scratch-buffer-fennel ()
  "Switch to scratch buffer."
  (let ((buffer-name "*scratch - Fennel*"))
    (switch-to-buffer (or (get-buffer buffer-name)
                          (symex--new-scratch-buffer buffer-name)))))

(defvar symex-fennel-modes (list 'fennel-mode))

(defun symex-interface-register-fennel ()
  "Register the Fennel runtime interface."
  (symex-interface-extend
   symex-fennel-modes
   (list
    :eval #'lisp-eval-last-sexp
    :eval-definition #'lisp-eval-defun
    :eval-thunk #'symex-eval-thunk-fennel
    :describe-symbol #'fennel-find-definition
    :repl #'symex-repl-fennel
    :run #'symex-run-fennel
    :switch-to-scratch-buffer #'symex-switch-to-scratch-buffer-fennel)))

(provide 'symex-interface-fennel)
;;; symex-interface-fennel.el ends here
