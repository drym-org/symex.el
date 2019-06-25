;;; symex-interface-scheme.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
;; Interface for the Scheme language
;;

;;; Code:


(require 'geiser-mode)


(defun symex-eval-scheme ()
  "Eval Scheme symex."
  (interactive)
  (geiser-eval-last-sexp nil))

(defun symex-eval-definition-scheme ()
  "Eval entire containing definition."
  (geiser-eval-definition nil))

(defun symex-eval-pretty-scheme ()
  "Evaluate symex and render the result in a useful string form."
  (interactive)
  (symex-eval-scheme))

(defun symex-describe-symbol-scheme ()
  "Describe symbol at point."
  (interactive)
  (geiser-doc-symbol-at-point))

(defun symex-repl-scheme ()
  "Go to REPL."
  (geiser-mode-switch-to-repl))


(provide 'symex-interface-scheme)
;;; symex-interface-scheme.el ends here
