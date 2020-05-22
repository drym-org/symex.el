;;; symex-interface-racket.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Interface for the Racket language
;;

;;; Code:

(require 'racket-mode)
(require 'subr-x)

(defun symex--racket-send-to-repl (code)
  "Internal function to send CODE to the Racket REPL for evaluation.

Before sending the code (in string form), calls `racket-repl' and
`racket--repl-forget-errors'.  Also inserts a ?\n at the process
mark so that output goes on a fresh line, not on the same line as
the prompt.

Afterwards call `racket--repl-show-and-move-to-end'."
  (racket-repl t)
  (racket--repl-forget-errors)
  (let ((proc (get-buffer-process racket--repl-buffer-name)))
    (with-racket-repl-buffer
      (save-excursion
        (goto-char (process-mark proc))
        (insert ?\n)
        (set-marker (process-mark proc) (point))))
    (comint-send-string proc code)
    (comint-send-string proc "\n"))
  (racket--repl-show-and-move-to-end))

(defun symex-eval-racket ()
  "Eval last sexp.

Accounts for different point location in evil vs Emacs mode."
  (interactive)
  (save-excursion
    (when (equal evil-state 'normal)
      (forward-char))
    (racket-send-last-sexp)))

(defun symex-eval-definition-racket ()
  "Eval entire containing definition."
  (racket-send-definition nil))

(defun symex-eval-pretty-racket ()
  "Evaluate symex and render the result in a useful string form."
  (interactive)
  (let ((pretty-code (string-join
                      `("(let ([result "
                        ,(buffer-substring (racket--repl-last-sexp-start)
                                           (point))
                        "])"
                        " (cond [(stream? result) (stream->list result)]
                                  [(sequence? result) (sequence->list result)]
                                  [else result]))"))))
    (symex--racket-send-to-repl pretty-code)))

(defun symex-eval-thunk-racket ()
  "Evaluate symex as a 'thunk,' i.e. as a function taking no arguments."
  (interactive)
  (let ((thunk-code (string-join
                      `("("
                        ,(buffer-substring (racket--repl-last-sexp-start)
                                           (point))
                        ")"))))
    (symex--racket-send-to-repl thunk-code)))

(defun symex-eval-print-racket ()
  "Eval symex and print result in buffer."
  (interactive)
  nil)

(defun symex-describe-symbol-racket ()
  "Describe symbol at point."
  (interactive)
  (racket-describe nil))

(defun symex-repl-racket ()
  "Go to REPL."
  (racket-repl))

(defun symex-run-racket ()
  "Evaluate buffer."
  (racket-run nil))


(provide 'symex-interface-racket)
;;; symex-interface-racket.el ends here
