;;; symex-interface-elisp.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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
