;;; symex-interface-racket.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Interface for the Racket language

;;; Code:

(require 'racket-mode nil 'noerror)
(require 'subr-x)
(require 'symex-interop)
(require 'symex-primitives)
(require 'symex-interface)

;; from racket-mode - avoid byte-compile warnings
(defvar racket-repl-buffer-name)
(defvar racket-xp-mode)
(defvar racket--repl-output-mark)

(declare-function racket-edit-switch-to-repl "ext:racket-mode")
(declare-function racket-send-last-sexp "ext:racket-mode")
(declare-function racket-send-definition "ext:racket-mode")
(declare-function racket-xp-describe "ext:racket-mode")
(declare-function racket-repl-describe "ext:racket-mode")
(declare-function racket-run "ext:racket-mode")

(defun symex-eval-pretty-racket ()
  "Evaluate symex and render the result in a useful string form."
  (let ((pretty-code (string-join
                      `("(let ([result "
                        ,(buffer-substring (symex--get-starting-point)
                                           (point))
                        "])"
                        " (cond [(stream? result) (stream->list result)]
                                  [(sequence? result) (sequence->list result)]
                                  [else result]))"))))
    (let ((original-repl-buffer-name racket-repl-buffer-name))
      (symex--with-temp-buffer
        (setq racket-repl-buffer-name original-repl-buffer-name)
        (insert pretty-code)
        (racket--send-region-to-repl (point-min) (point-max))))))

(defun symex-eval-thunk-racket ()
  "Evaluate symex as a \"thunk,\" i.e. as a function taking no arguments."
  (let ((thunk-code (string-join
                     `("("
                       ,(buffer-substring (symex--get-starting-point)
                                          (point))
                       ")"))))
    (let ((original-repl-buffer-name racket-repl-buffer-name))
      (symex--with-temp-buffer
        (setq racket-repl-buffer-name original-repl-buffer-name)
        (insert thunk-code)
        (racket--send-region-to-repl (point-min) (point-max))))))

(defun symex-describe-symbol-racket ()
  "Describe symbol at point."
  (let ((original-window (selected-window)))
    (cond (racket-xp-mode (racket-xp-describe))
          ((derived-mode-p 'racket-repl-mode) (racket-repl-describe))
          (t (error "Enable racket-xp-mode or start the REPL!")))
    (select-window original-window)))

(defun symex-repl-racket ()
  "Go to REPL."
  (let ((original-window (selected-window)))
    (racket-edit-switch-to-repl)
    (unless (eq original-window (selected-window))
      ;; if the REPL window is currently being created
      ;; then don't attempt to go to the bottom
      (goto-char (point-max))
      (symex-enter-lowest))))

(defvar symex-racket-modes (list 'racket-mode
                                 'racket-repl-mode))

(defun symex-interface-register-racket ()
  "Register the Racket runtime interface."
  (symex-interface-extend
   symex-racket-modes
   (list
    :eval #'racket-send-last-sexp
    :eval-definition #'racket-send-definition
    :eval-pretty #'symex-eval-pretty-racket
    :eval-thunk #'symex-eval-thunk-racket
    :describe-symbol #'symex-describe-symbol-racket
    :repl #'symex-repl-racket
    :run #'racket-run)))

(provide 'symex-interface-racket)
;;; symex-interface-racket.el ends here
