;;; symex-interface-racket.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Interface for the Racket language
;;

;;; Code:


(require 'racket-mode)


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
    (racket--send-to-repl pretty-code)))

(defun symex-describe-symbol-racket ()
  "Describe symbol at point."
  (interactive)
  (racket-describe nil))

(defun symex-repl-racket ()
  "Go to REPL."
  (racket-repl))


(provide 'symex-interface-racket)
;;; symex-interface-racket.el ends here
