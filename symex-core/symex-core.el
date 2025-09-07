;;; symex-core.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/drym-org/symex.el
;; Version: 2.0
;; Package-Requires: ((emacs "25.1") (paredit "24") (seq "2.22"))
;; Keywords: lisp, convenience, languages

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

;; Symex mode (pronounced sym-ex, as in symbolic expression) is a vim-
;; inspired way of editing Lisp code as trees.  Entering symex mode
;; allows you to reason about your code in terms of its structure,
;; similar to other tools like paredit and lispy.  But while in those
;; packages the tree representation is implicit, symex mode models
;; the tree structure explicitly so that tree navigations and operations
;; can be described using an expressive DSL, and invoked in a vim-
;; style modal interface.
;;
;; Under the hood, Symex mode uses paredit and Tree-Sitter for parsing
;; the code syntax tree.

;;; Code:

(require 'paredit)

(require 'symex-motions)
(require 'symex-tree)
(require 'symex-transformations)
(require 'symex-primitives)
(require 'symex-custom)
(require 'symex-ts)

;;;###autoload
(define-minor-mode symex-lisp-mode
  "A minor mode to balance parentheses while editing Lisp buffers."
  :lighter " symex-lisp"
  :group 'symex
  :keymap (let ((symex-map (make-sparse-keymap)))
            (define-key
             symex-map
             (kbd "(")
             #'paredit-open-round)

            (define-key
             symex-map
             (kbd ")")
             #'paredit-close-round)

            (define-key
             symex-map
             (kbd "[")
             #'paredit-open-square)

            (define-key
             symex-map
             (kbd "]")
             #'paredit-close-square)

            (define-key
             symex-map
             (kbd "<backspace>")
             #'paredit-backward-delete)

            (define-key
             symex-map
             (kbd "<DEL>")
             #'paredit-backward-delete)

            (define-key
             symex-map
             (kbd "\"")
             #'paredit-doublequote)

            symex-map))

;;;###autoload
(defun symex-initialize ()
  "Initialize symex mode.

This registers `symex-lisp-mode' in all recognized Lisp modes to
ensure that parentheses remain balanced in these modes. It also
initializes the tree-sitter provider by aliasing Symex functions to
the tree-sitter functions provided by Emacs. This extra step to
defining these functions is necessary (instead of just using the
functions directly) as tree-sitter isn't available on < Emacs 29, but
Symex's Lisp support should still be usable on these older versions."
  (when symex-ensure-structure-p
    (dolist (mode-name (symex-get-lisp-modes))
      (let ((mode-hook (intern (concat (symbol-name mode-name)
                                       "-hook"))))
        (add-hook mode-hook #'symex-lisp-mode))))
  (symex-ts--init))

(defun symex-disable ()
  "Disable symex.

This unregisters the symex minor mode from all lisp-related hooks, and
removes any advice corresponding to configured features.

If you are changing symex customizations to enable or disable certain
features, you may need to call this function after making such changes
and prior to calling `symex-initialize` again, in order for the former
configuration to be disabled and the new one adopted."
  (when symex-ensure-structure-p
    (dolist (mode-name (symex-get-lisp-modes))
      (let ((mode-hook (intern (concat (symbol-name mode-name)
                                       "-hook"))))
        (remove-hook mode-hook #'symex-lisp-mode)))))

;;;###autoload
(define-minor-mode symex-core-mode
  "An evil way to edit Lisp symbolic expressions as trees."
  :lighter " symex-core"
  :global t
  :group 'symex
  (if symex-core-mode
      (symex-initialize)
    (symex-disable)))


(provide 'symex-core)
;;; symex-core.el ends here
