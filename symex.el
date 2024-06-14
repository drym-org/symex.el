;;; symex.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/drym-org/symex.el
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (tsc "0.15.2") (tree-sitter "0.15.2") (paredit "24") (evil "1.2.14") (evil-surround "1.0.4") (seq "2.22"))
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

(require 'symex-evil)
(require 'symex-interop)
(require 'symex-context)
(require 'symex-misc)
(require 'symex-primitives)
(require 'symex-custom)
(require 'tree-sitter)
(require 'symex-ts)

(defvar symex--original-blink-cursor-state nil)

;;;###autoload
(define-minor-mode symex-mode
  "An evil way to edit Lisp symbolic expressions as trees."
  :lighter " symex"
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

(defun symex--ensure-minor-mode ()
  "Enable symex minor mode if it isn't already enabled."
  (unless symex-mode
    (symex-mode)))

(defun symex--enter-mode ()
  "Load the modal interface."
  (unless (symex--rigpa-enabled-p)
    ;; the minor mode needs to be enabled prior to entering the
    ;; evil state or the keybindings won't take effect. So we
    ;; can't do it in the state entry hook, which would
    ;; otherwise be preferable
    (symex-enable-editing-minor-mode))
  (evil-symex-state))

(defun symex-enter-mode ()
  "Take necessary action upon symex mode entry."
  (symex--ensure-minor-mode)
  (symex--adjust-point-on-entry)
  (when symex-remember-branch-positions-p
    (symex--clear-branch-memory))
  (when symex-toggle-blink-cursor
    (setq symex--original-blink-cursor-state blink-cursor-mode)
    (blink-cursor-mode -1))
  (symex-user-select-nearest)
  (when symex-refocus-p
    ;; smooth scrolling currently not supported
    ;; may add it back in the future
    (symex--set-scroll-margin))
  (contextualize-set-context symex-category)
  (symex--enter-mode))

;;; Major modes in which symex should be active.
(defvar symex-elisp-modes (list 'lisp-interaction-mode
                                'emacs-lisp-mode
                                'inferior-emacs-lisp-mode))

(defvar symex-racket-modes (list 'racket-mode
                                 'racket-repl-mode))

(defvar symex-clojure-modes (list 'clojure-mode
                                  'clojurescript-mode
                                  'clojurec-mode))

(defvar symex-common-lisp-modes (list 'lisp-mode
                                      'slime-repl-mode
                                      'sly-mrepl-mode))

(defvar symex-lisp-modes (append symex-elisp-modes
                                 symex-racket-modes
                                 symex-clojure-modes
                                 symex-common-lisp-modes
                                 (list 'scheme-mode
                                       'arc-mode)))

(defun symex-modal-provider-initialize ()
  "Initialize the modal interface provider."
  (symex-evil-initialize))

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
  (when (fboundp 'undo-tree-undo)
    (advice-add #'undo-tree-undo :after #'symex-select-nearest-advice))
  (when (fboundp 'undo-tree-redo)
    (advice-add #'undo-tree-redo :after #'symex-select-nearest-advice))
  (symex--add-selection-advice)
  ;; initialize modal interface frontend
  (symex-modal-provider-initialize)
  ;; initialize user context inference
  (symex-initialize-contexts))

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
  (when (fboundp 'undo-tree-undo)
    (advice-remove #'undo-tree-undo #'symex-select-nearest-advice))
  (when (fboundp 'undo-tree-redo)
    (advice-remove #'undo-tree-redo #'symex-select-nearest-advice))
  (symex--remove-selection-advice)
  ;; unregister any user contexts
  (symex-disable-contexts))

;;;###autoload
(defun symex-mode-interface ()
  "The main entry point for editing symbolic expressions using symex mode.

Enter the symex evil state, activating symex keybindings."
  (interactive)
  (symex-enter-mode))


(provide 'symex)
;;; symex.el ends here
