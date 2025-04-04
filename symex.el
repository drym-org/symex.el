;;; symex.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/drym-org/symex.el
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (paredit "24") (seq "2.22") (lithium "0.0"))
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

(require 'symex-lithium)
(require 'symex-interop)
(when (symex--evil-installed-p)
  (require 'symex-evil))
(require 'symex-motions)
(require 'symex-tree)
(require 'symex-interface-builtins)
(require 'symex-transformations)
(require 'symex-primitives)
(require 'symex-ui)
(require 'symex-custom)
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
  (symex-editing-mode-enter))

;; TODO: put a lot of this in entry hooks
(defun symex-enter-mode ()
  "Take necessary action upon symex mode entry."
  (symex--adjust-point-on-entry)
  (when symex-remember-branch-positions-p
    (symex--clear-branch-memory))
  (when symex-toggle-blink-cursor
    (setq symex--original-blink-cursor-state blink-cursor-mode)
    (blink-cursor-mode -1))
  (symex-user-select-nearest)
  (symex--primitive-enter)
  (when symex-refocus-p
    ;; smooth scrolling currently not supported
    ;; may add it back in the future
    (symex--set-scroll-margin))
  (symex--enter-mode))

(defun symex-exit-mode ()
  "Take necessary action upon symex mode exit."
  (when symex--original-blink-cursor-state
    (blink-cursor-mode 1))
  (when symex-refocus-p
    (symex--restore-scroll-margin))
  (symex--delete-overlay)
  (symex--primitive-exit))

(defun symex-modal-provider-initialize ()
  "Initialize the modal interface provider."
  (symex-lithium-initialize))

;;;###autoload
(defun symex-initialize ()
  "Initialize symex mode.

This registers symex mode for use in all recognized Lisp modes, and also
advises functions to enable or disable features based on user configuration."

  (symex-register-builtin-interfaces)
  ;; enable the symex minor mode in all recognized lisp modes
  (dolist (mode-name (symex-get-lisp-modes))
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (add-hook mode-hook 'symex-mode)))
  ;; any side effects that should happen as part of selection,
  ;; e.g., update overlay
  (advice-add #'symex-user-select-nearest :after #'symex--selection-side-effects)
  (advice-add #'symex-select-nearest-in-line :after #'symex--selection-side-effects)
  ;; initialize modal interface frontend
  (symex-modal-provider-initialize)
  ;; initialize repeat command and other evil interop
  (when (symex--evil-installed-p)
    (symex-initialize-evil))
  (symex-ts--init))

(defun symex-disable ()
  "Disable symex.

This unregisters the symex minor mode from all lisp-related hooks, and
removes any advice corresponding to configured features.

If you are changing symex customizations to enable or disable certain
features, you may need to call this function after making such changes
and prior to calling `symex-initialize` again, in order for the former
configuration to be disabled and the new one adopted."
  (dolist (mode-name (symex-get-lisp-modes))
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (remove-hook mode-hook 'symex-mode)))
  ;; remove all advice
  (advice-remove #'symex-user-select-nearest #'symex--selection-side-effects)
  (advice-remove #'symex-select-nearest-in-line #'symex--selection-side-effects)
  (when (symex--evil-installed-p)
    (symex-disable-evil)))

;;;###autoload
(defun symex-mode-interface ()
  "The main entry point for editing symbolic expressions using symex mode.

Enter the symex evil state, activating symex keybindings."
  (interactive)
  (symex-enter-mode))


(provide 'symex)
;;; symex.el ends here
