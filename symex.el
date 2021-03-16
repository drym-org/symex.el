;;; symex.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/symex.el
;; Version: 0.9
;; Package-Requires: ((emacs "24.4") (lispy "0.26.0") (paredit "24") (evil-cleverparens "20170718.413") (dash "2.18.0") (evil "1.2.14") (smartparens "1.11.0") (evil-surround "1.0.4") (hydra "0.15.0") (seq "2.22") (undo-tree "0.7.5"))
;; Keywords: lisp, evil

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
;; Symex mode (pronounced sym-ex, as in symbolic expression) is a vim-
;; inspired way of editing Lisp code as trees.  Entering symex mode
;; allows you to reason about your code in terms of its structure,
;; similar to other tools like paredit and lispy.  But while in those
;; packages the tree representation is implicit, symex mode models
;; the tree structure explicitly so that tree navigations and operations
;; can be described using an expressive DSL, and invoked in a vim-
;; style modal interface implemented with a Hydra.
;;
;; At the moment, symex mode uses paredit, lispy, and evil-cleverparens
;; to provide much of its low level functionality.
;; In the future, this layer of primitives may be replaced with a layer
;; that explicitly uses the abstract syntax tree, for greater precision.
;;

;;; Code:

(require 'undo-tree)
(require 'lispy)
(require 'paredit)
(require 'cl-lib)
(require 'dash)

(require 'symex-data)
(require 'symex-computations)
(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-traversals)
(require 'symex-transformations)
(require 'symex-misc)
(require 'symex-interop)
(require 'symex-ui)
(require 'symex-hydra)
(require 'symex-evil)

;;;;;;;;;;;;;;;;;;;;;
;;; CONFIGURATION ;;;
;;;;;;;;;;;;;;;;;;;;;

(defgroup symex nil
  "A language for editing symbolic expressions."
  :group 'lisp)

(defcustom symex-highlight-p nil
  "Whether selected symexes should be highlighted."
  :type 'boolean
  :group 'symex)

(defcustom symex-refocus-p t
  "Whether to refocus on the selected symex when it's close to the edge of the screen."
  :type 'boolean
  :group 'symex)

(defcustom symex-remember-branch-positions-p t
  "Whether movement in the vertical direction should remember branch positions."
  :type 'boolean
  :group 'symex)

(defcustom symex-modal-backend 'evil
  "Whether to use hydra or evil as the backend for the modal interface."
  :type 'symbol
  :group 'symex)

(defvar symex-elisp-modes (list 'lisp-interaction-mode
                                'emacs-lisp-mode
                                'inferior-emacs-lisp-mode))

(defvar symex-racket-modes (list 'racket-mode
                                 'racket-repl-mode))

(defvar symex-lisp-modes (append symex-elisp-modes
                                 symex-racket-modes
                                 (list 'scheme-mode 'arc-mode)))

;;;###autoload
(define-minor-mode symex-mode
  "An evil way to edit Lisp symbolic expressions as trees."
  :lighter " symex"
  :keymap (let ((symex-map (make-sparse-keymap)))
            (define-key
              symex-map
              (kbd "(")
              'paredit-open-round)

            (define-key
              symex-map
              (kbd ")")
              'paredit-close-round)

            (define-key
              symex-map
              (kbd "[")
              'paredit-open-square)

            (define-key
              symex-map
              (kbd "]")
              'paredit-close-square)

            (define-key
              symex-map
              (kbd "<backspace>")
              'paredit-backward-delete)

            (define-key
              symex-map
              (kbd "<DEL>")
              'paredit-backward-delete)

            (define-key
              symex-map
              (kbd "\"")
              'paredit-doublequote)

            symex-map))

(defun symex--ensure-minor-mode ()
  "Enable symex minor mode if it isn't already enabled."
  (unless symex-mode
    (symex-mode)))

(defun symex--adjust-point ()
  "Adjust point context from the Emacs to the Vim interpretation.

If entering symex mode from Insert or Emacs mode, then translate point
so it indicates the appropriate symex in Symex mode.  This is necessary
because in Emacs, the symex preceding point is indicated.  In Vim, the
symex 'under' point is indicated.  We want to make sure to select the
right symex when we enter Symex mode."
  (interactive)
  (when (or (not (boundp 'evil-mode))
            (and (boundp 'evil-mode) (not evil-mode))
            (member evil-previous-state '(insert emacs)))
    (unless (bobp)
      (let ((just-inside-symex-p (save-excursion (backward-char)
                                                 (lispy-left-p))))
        (unless just-inside-symex-p
          (backward-char))))))

(defun symex--enter-mode ()
  "Load the modal interface."
  (cond ((eq symex-modal-backend 'hydra) (hydra-symex/body))
        ((eq symex-modal-backend 'evil) (evil-symex-state))))

(defun symex-enter-mode ()
  "Take necessary action upon symex mode entry."
  (symex--ensure-minor-mode)
  (symex--adjust-point)
  (when symex-remember-branch-positions-p
    (symex--clear-branch-memory))
  (symex-select-nearest)
  (when symex-refocus-p
    ;; smooth scrolling currently not supported
    ;; may add it back in the future
    (symex--set-scroll-margin))
  (symex--enter-mode))

(defun symex--add-selection-advice ()
  "Add selection advice."
  (advice-add #'symex-go-forward :around #'symex-selection-motion-advice)
  (advice-add #'symex-go-backward :around #'symex-selection-motion-advice)
  (advice-add #'symex-go-up :around #'symex-selection-motion-advice)
  (advice-add #'symex-go-down :around #'symex-selection-motion-advice)
  (advice-add #'symex-goto-first :around #'symex-selection-advice)
  (advice-add #'symex-goto-last :around #'symex-selection-advice)
  (advice-add #'symex-goto-lowest :around #'symex-selection-advice)
  (advice-add #'symex-goto-highest :around #'symex-selection-advice)
  (advice-add #'symex-traverse-forward :around #'symex-selection-advice)
  (advice-add #'symex-traverse-backward :around #'symex-selection-advice)
  (advice-add #'symex-select-nearest :around #'symex-selection-advice))

(defun symex--remove-selection-advice ()
  "Remove selection advice."
  (advice-remove #'symex-go-forward #'symex-selection-motion-advice)
  (advice-remove #'symex-go-backward #'symex-selection-motion-advice)
  (advice-remove #'symex-go-up #'symex-selection-motion-advice)
  (advice-remove #'symex-go-down #'symex-selection-motion-advice)
  (advice-remove #'symex-goto-first #'symex-selection-advice)
  (advice-remove #'symex-goto-last #'symex-selection-advice)
  (advice-remove #'symex-goto-lowest #'symex-selection-advice)
  (advice-remove #'symex-goto-highest #'symex-selection-advice)
  (advice-remove #'symex-traverse-forward #'symex-selection-advice)
  (advice-remove #'symex-traverse-backward #'symex-selection-advice)
  (advice-remove #'symex-select-nearest #'symex-selection-advice))

;;;###autoload
(defun symex-initialize ()
  "Initialize symex mode.

This registers symex mode for use in all recognized Lisp modes, and also
advises functions to enable or disable features based on user configuration."
  ;; enable the symex minor mode in all recognized lisp modes
  (dolist (mode-name symex-lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (add-hook mode-hook 'symex-mode)))
  ;; advise functions to enable or disable configured features
  (when symex-remember-branch-positions-p
    (advice-add #'symex-go-down :around #'symex--remember-branch-position)
    (advice-add #'symex-go-up :around #'symex--return-to-branch-position)
    (advice-add #'symex-go-backward :around #'symex--forget-branch-positions)
    (advice-add #'symex-go-forward :around #'symex--forget-branch-positions))
  (symex--add-selection-advice))

(defun symex-disable ()
  "Disable symex.

This unregisters the symex minor mode from all lisp-related hooks, and
removes any advice corresponding to configured features.

If you are changing symex customizations to enable or disable certain
features, you may need to call this function after making such changes
and prior to calling `symex-initialize` again, in order for the former
configuration to be disabled and the new one adopted."
  (dolist (mode-name symex-lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (remove-hook mode-hook 'symex-mode)))
  ;; remove all advice
  (advice-remove #'symex-go-down #'symex--remember-branch-position)
  (advice-remove #'symex-go-up #'symex--return-to-branch-position)
  (advice-remove #'symex-go-backward #'symex--forget-branch-positions)
  (advice-remove #'symex-go-forward #'symex--forget-branch-positions)
  (symex--remove-selection-advice))

;;;###autoload
(defun symex-mode-interface ()
  "The main entry point for editing symbolic expressions using symex mode.

Enter the symex evil state and show a hydra menu for accessing various
features."
  (interactive)
  (symex-enter-mode))


(provide 'symex)
;;; symex.el ends here
