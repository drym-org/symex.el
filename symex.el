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

(require 'evil)
(require 'undo-tree)
(require 'lispy)
(require 'paredit)
(require 'evil-cleverparens)  ;; really only need cp-textobjects here
(require 'cl-lib)
(require 'dash)
(require 'hydra)

(require 'symex-data)
(require 'symex-computations)
(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-traversals)
(require 'symex-transformations)
(require 'symex-misc)
(require 'symex-interop)

(eval-when-compile              ; eventually sort out the dependency
  (defvar chimera-symex-mode)   ; order so this is unnecessary
  (declare-function chimera-hydra-portend-exit "ext:ignore"))

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

(evil-define-state symex
  "Symex state."
  :tag " <λ> "
  :message "-- SYMEX --"
  :enable (normal))

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
  (when (member evil-previous-state '(insert emacs))
    (unless (bobp)
      (let ((just-inside-symex-p (save-excursion (backward-char)
                                                 (lispy-left-p))))
        (unless just-inside-symex-p
          (backward-char))))))

(defun symex-enter-mode ()
  "Take necessary action upon symex mode entry."
  (unless (and (boundp 'rigpa-mode) rigpa-mode)
    (when (and (boundp 'evil-mode) evil-mode)
      (evil-symex-state)))
  (symex--ensure-minor-mode)
  (symex--adjust-point)
  (when symex-remember-branch-positions-p
    (symex--clear-branch-memory))
  (symex-select-nearest)
  (when symex-refocus-p
    ;; smooth scrolling currently not supported
    ;; may add it back in the future
    (symex--set-scroll-margin))
  (hydra-symex/body))

(defun symex-exit-mode ()
  "Take necessary action upon symex mode exit."
  (deactivate-mark)
  (when symex-refocus-p
    (symex--restore-scroll-margin))
  (when (and (boundp 'rigpa-mode) rigpa-mode)
    (chimera-hydra-portend-exit chimera-symex-mode t)))

(defun symex--toggle-highlight ()
  "Toggle highlighting of selected symex."
  (interactive)
  (if mark-active
      (deactivate-mark)
    (mark-sexp))
  (setq symex-highlight-p
        (not symex-highlight-p)))

(defun symex-hide-menu ()
  "Hide symex menu."
  (hydra-set-property 'hydra-symex :verbosity 0))

(defun symex-show-menu ()
  "Show symex menu."
  (hydra-set-property 'hydra-symex :verbosity 2))

(defun symex-toggle-menu ()
  "Show/hide the symex menu.

Note that hiding the menu still retains the symex editing mode,
and simply toggles whether the menu is visible or not.  To enter
and exit the symex modal interface, use `symex-mode-interface`
to enter, and any of the standard exits to exit."
  (interactive)
  (let ((visibility (hydra-get-property 'hydra-symex :verbosity)))
    (if (> visibility 0)
        (symex-hide-menu)
      (symex-show-menu))))

;; TOOD: it might make sense to symex-tidy as a formal followup,
;; possibly after every head, but at least after the transformations
;; likewise, we might want to disable and re-enable highlighting,
;; if active, on each command
(defhydra hydra-symex (:columns 4
                       :post (symex-exit-mode)
                       :after-exit (symex--signal-exit))
  "Symex mode"
  ("(" (lambda ()
         (interactive)
         (symex-create 'round)) "()")
  ("[" (lambda ()
         (interactive)
         (symex-create 'square)) "[]")
  ("{" (lambda ()
         (interactive)
         (symex-create 'curly)) "{}")
  ("<" (lambda ()
         (interactive)
         (symex-create 'angled)) "<>")
  ("h" symex-go-backward "previous")
  ("k" symex-go-up "up")
  ("j" symex-go-down "down")
  ("l" symex-go-forward "next")
  ("f" symex-traverse-forward "flow forward")
  ("b" symex-traverse-backward "flow backward")
  ("F" symex-traverse-forward-skip "skip forward")
  ("B" symex-traverse-backward-skip "skip backward")
  ("C-h" symex-leap-backward "leap backward")
  ("C-l" symex-leap-forward "leap forward")
  ("C-M-h" (lambda ()
             (interactive)
             (symex-leap-backward t)) "soar backward")
  ("C-M-l" (lambda ()
             (interactive)
             (symex-leap-forward t)) "soar forward")
  ("C-k" symex-climb-branch "climb branch")
  ("C-j" symex-descend-branch "descend branch")
  ("y" symex-yank "yank (copy)")
  ("p" symex-paste-after "paste after")
  ("P" symex-paste-before "paste before")
  ("x" symex-delete "delete")
  ("c" symex-change "change" :exit t)
  ("C" symex-clear "clear")
  ("s" symex-replace "replace" :exit t)
  ("S" symex-change-delimiter "change surrounding delimiter")
  ("H" symex-shift-backward "move backward")
  ("L" symex-shift-forward "move forward")
  ("K" paredit-raise-sexp "raise")
  ("C-S-j" symex-emit-backward "emit backward")
  ("C-(" symex-capture-backward "capture backward")
  ("C-S-h" symex-capture-backward "capture backward")
  ("C-{" symex-emit-backward "emit backward")
  ("C-S-l" symex-capture-forward "capture forward")
  ("C-}" symex-emit-forward "emit forward")
  ("C-S-k" symex-emit-forward "emit forward")
  ("C-)" symex-capture-forward "capture forward")
  ("z" symex-swallow "swallow head")
  ("Z" symex-swallow-tail "swallow tail")
  ("e" symex-evaluate "evaluate")
  ("E" symex-evaluate-pretty "pretty evaluate")
  ("d" symex-evaluate-definition "evaluate definition")
  ("M-e" symex-eval-recursive "evaluate recursively")
  ("T" symex-evaluate-thunk "evaluate as 'thunk'")
  (":" eval-expression "eval expression")
  ("t" symex-switch-to-scratch-buffer "scratch buffer" :exit t)
  ("M" symex-switch-to-messages-buffer "messages buffer")
  ("r" symex-repl "go to REPL" :exit t)
  ("R" symex-run "run buffer")
  ("X" symex-run "run buffer")
  ("|" lispy-split "split")
  ("m" symex-join "merge (join)")
  ("\\" symex-splice "clip/splice")
  (")" symex-wrap-round "wrap with ()")
  ("]" symex-wrap-square "wrap with []")
  ("}" symex-wrap-curly "wrap with {}")
  (">" symex-wrap-angled "wrap with <>")
  ("o" symex-open-line-after "open line after" :exit t)
  ("O" symex-open-line-before "open line before" :exit t)
  ("n" symex-insert-newline "newline")
  ("C-S-o" symex-append-newline "append newline")
  ("J" symex-join-lines "join lines")
  ("M-J" symex-collapse "collapse to single line")
  ("N" (lambda ()
         (interactive)
         (symex-join-lines t)) "join lines backwards")
  ("0" symex-goto-first "go to first")
  ("M-h" symex-goto-first "go to first")
  ("$" symex-goto-last "go to last")
  ("M-l" symex-goto-last "go to last")
  ("M-j" symex-goto-lowest "go to lowest")
  ("M-k" symex-goto-highest "go to highest")
  ("=" symex-tidy "tidy/indent")
  ("<tab>" symex-tidy "tidy/indent")
  ("M-=" symex-tidy-proper "tidy/indent properly")
  ("A" symex-append-after "append after symex" :exit t)
  ("a" symex-insert-at-end "append inside symex" :exit t)
  ("i" symex-insert-at-beginning "insert inside symex" :exit t)
  ("I" symex-insert-before "insert before symex" :exit t)
  ("w" symex-wrap "wrap with symex" :exit t)
  ("g" evil-jump-to-tag "go to definition")
  ("G" evil-jump-backward "return to previous location")
  (";" symex-comment "comment out")
  ("C-;" symex-eval-print "eval + print")
  ;; canonical action
  ("s-;" symex-evaluate "evaluate" :exit t)
  ;; configuration
  ("H-h" symex--toggle-highlight "toggle highlight")
  ("H-m" symex-toggle-menu "show/hide this menu")
  ;; explicit "pass through" so hydra persists
  ("u" undo-tree-undo nil)
  ("C-r" undo-tree-redo nil)
  ("C-e" symex--scroll-down nil)
  ("C-y" symex--scroll-up nil)
  ;; standard exits
  ("?" symex-describe "info")
  ("<return>" symex-enter-lower "enter lower" :exit t)
  ("C-<escape>" symex-enter-lower "enter lower" :exit t)
  ("<escape>" symex-escape-higher "escape higher" :exit t)
  ("C-g" symex-escape-higher "escape higher" :exit t))

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
    (advice-add #'symex-go-forward :around #'symex--forget-branch-positions)))

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
  (advice-remove #'symex-go-forward #'symex--forget-branch-positions))

;;;###autoload
(defun symex-mode-interface ()
  "The main entry point for editing symbolic expressions using symex mode.

Enter the symex evil state and show a hydra menu for accessing various
features."
  (interactive)
  (symex-enter-mode))


(provide 'symex)
;;; symex.el ends here
