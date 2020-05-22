;;; symex.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/symex.el
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6.1") (lispy "0.26.0") (paredit "24") (evil-cleverparens "20170718.413") (dash-functional "2.15.0") (evil "1.2.14") (smartparens "1.11.0") (racket-mode "20181030.1345") (geiser "0.10") (evil-surround "1.0.4") (hydra "0.15.0") (cider "0.21.0") (slime "2.24"))
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
(require 'lispy)
(require 'paredit)
(require 'evil-cleverparens)  ;; really only need cp-textobjects here
(require 'cl-lib)
(require 'dash-functional)
(require 'hydra)

(require 'symex-data)
(require 'symex-computations)
(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-traversals)
(require 'symex-transformations)
(require 'symex-misc)
(require 'symex-interop)

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

(defcustom symex-smooth-scroll-p nil
  "Whether refocusing should happen smoothly or abruptly."
  :type 'boolean
  :group 'symex)

(evil-define-state symex
  "Symex state."
  :tag " <λ> "
  :message "-- SYMEX --"
  :entry-hook (symex--ensure-minor-mode symex--adjust-point)
  :enable (normal))

(defvar symex-elisp-modes (list 'lisp-interaction-mode
                                'emacs-lisp-mode
                                'inferior-emacs-lisp-mode))

(defvar symex-racket-modes (list 'racket-mode
                                 'racket-repl-mode))

(defvar symex-lisp-modes (append symex-elisp-modes
                                 symex-racket-modes
                                 (list 'scheme-mode)))

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
    (let ((just-inside-symex-p (save-excursion (backward-char)
                                               (lispy-left-p))))
      (unless just-inside-symex-p
        (backward-char)))))

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

(defhydra hydra-symex (:idle 1.0
                       :columns 4
                       :color pink
                       :body-pre (progn (evil-symex-state)
                                        (symex-select-nearest))
                       :post (deactivate-mark))
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
  ("G" symex-switch-to-messages-buffer "messages buffer" :exit t)
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
  ("g" evil-jump-to-tag "Go to definition")
  (";" symex-comment "comment out")
  ("C-;" symex-eval-print "eval + print")
  ;; canonical action
  ("s-;" symex-evaluate "evaluate" :exit t)
  ;; configuration
  ("H-h" symex--toggle-highlight "toggle highlight")
  ("H-m" symex-toggle-menu "show/hide this menu")
  ;; escape hatches
  ("v" evil-visual-char nil :exit t)
  ("V" evil-visual-line nil :exit t)
  ("C-v" evil-visual-block nil :exit t)
  ;; standard exits
  ("?" symex-describe "info")
  ("<return>" symex-enter-lower "enter lower" :exit t)
  ("C-<escape>" symex-enter-lower "enter lower" :exit t)
  ("<escape>" symex-escape-higher "escape higher" :exit t)
  ("C-g" symex-escape-higher "escape higher" :exit t))


;;;###autoload
(defun symex-mode-interface ()
  "The main entry point for editing symbolic expressions using symex mode.

Enter the symex evil state and show a hydra menu for accessing various
features."
  (interactive)
  (hydra-symex/body))


(provide 'symex)
;;; symex.el ends here
