;;; symex.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/drym-org/symex.el
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (paredit "24") (seq "2.22") (lithium "0.1.1"))
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

(require 'symex-motions)
(require 'symex-tree)
(require 'symex-custom)

;; TODO: for primitive-entry/exit
;; eliminate, in favor of a ts minor mode.
(require 'symex-primitives)

(require 'symex-lithium)
(require 'symex-repeat)
(require 'symex-runtime)
(require 'symex-ui)
(require 'symex-interop)

(defgroup symex-mode nil
  "Point-free modal UI for Symex."
  :group 'symex)

(defcustom symex-highlight-p t
  "Whether selected symexes should be highlighted."
  :type 'boolean
  :group 'symex-mode)

(defcustom symex-refocus-p t
  "Whether to refocus on the selected symex when it's near the screen's edge."
  :type 'boolean
  :group 'symex-mode)

(defun symex-mode-highlight-selected (&rest _)
  "Things to do as part of symex selection, e.g. after navigations."
  (when symex-highlight-p
    (symex--update-overlay)))

(defun symex--enter-mode ()
  "Load the modal interface."
  (symex-editing-mode-enter))

;; TODO: put a lot of this in entry hooks
(defun symex-enter-mode ()
  "Take necessary action upon symex mode entry."
  (add-hook 'symex-selection-hook
            #'symex-mode-highlight-selected)
  (symex--adjust-point-on-entry)
  (when symex-remember-branch-positions-p
    (symex--clear-branch-memory))
  (symex-user-select-nearest)
  ;; TODO: this concept of primitive entry should be removed.
  ;; it enables the change notifier mainly, and we should
  ;; instead do that via a symex-treesit-mode, perhaps
  (symex--primitive-enter)
  ;; enable parsing for repeat functionality
  (symex-repeat-enable)
  (when symex-refocus-p
    ;; smooth scrolling currently not supported
    ;; may add it back in the future
    (symex--set-scroll-margin))
  (unless (or (member major-mode (symex-get-lisp-modes))
              (symex-ts-available-p))
    (message "WARNING (Symex): Consider using a tree-sitter enabled major mode for %s."
             (buffer-name)))
  (symex--enter-mode))

(defun symex-exit-mode ()
  "Take necessary action upon symex mode exit."
  (remove-hook 'symex-selection-hook
               #'symex-mode-highlight-selected)
  (when symex-refocus-p
    (symex--restore-scroll-margin))
  (symex--delete-overlay)
  (symex--primitive-exit)
  ;; if we are exiting as part of a repeatable action
  ;; then don't suspend the symex repeat parser
  (unless (member symex--current-keys symex-repeatable-keys)
    (symex-repeat-disable)))

(defun symex-modal-provider-initialize ()
  "Initialize the modal interface provider."
  (symex-lithium-initialize))

;;;###autoload
(defun symex-modal-initialize ()
  "Initialize the modal interface."
  ;; any side effects that should happen as part of selection,
  ;; e.g., update overlay
  ;; initialize modal interface provider
  (symex-modal-provider-initialize)
  ;; initialize repeat command and evil interop
  (symex-repeat-initialize)
  ;; initialize runtime integrations with various language backends
  (symex-runtime-initialize))

(defun symex-modal-disable ()
  "Disable symex modal interface."
  ;; remove all advice
  (symex-repeat-teardown))

;;;###autoload
(defun symex-mode-interface ()
  "The main entry point for editing symbolic expressions using symex mode.

Enter the symex evil state, activating symex keybindings."
  (interactive)
  (symex-enter-mode))


(provide 'symex-mode)
;;; symex-mode.el ends here
