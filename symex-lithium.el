;;; symex-lithium.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/drym-org/symex.el

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

;; Lithium modal frontend to symex.

;;; Code:

(require 'lithium)
(require 'cl-lib)
(require 'repeat-ring)

(require 'symex-ui)
(require 'symex-motions)
(require 'symex-tree)
(require 'symex-runtime)
(require 'symex-transformations)
(require 'symex-interop)
(require 'symex-utils)

(declare-function symex-exit-mode "symex.el")

;; this is dynamically referenced in the lithium defining macros and
;; causes a byte compile warning here. The current approach seems
;; fine; not sure if there's an alternative that would avoid the
;; warning. But we just handle it in the usual way here, by declaring
;; the variable.
(defvar symex-editing-mode-map)

(defvar symex-lithium-repeatable-keys
  (list (read-kbd-macro "(" :need-vector)
        (read-kbd-macro "[" :need-vector)
        (read-kbd-macro ")" :need-vector)
        (read-kbd-macro "]" :need-vector)
        (read-kbd-macro "C-'" :need-vector)
        (read-kbd-macro "C-," :need-vector)
        (read-kbd-macro "`" :need-vector)
        (read-kbd-macro "C-`" :need-vector)
        (read-kbd-macro "p" :need-vector)
        (read-kbd-macro "P" :need-vector)
        (read-kbd-macro "x" :need-vector)
        (read-kbd-macro "X" :need-vector)
        (read-kbd-macro "D" :need-vector)
        (read-kbd-macro "C--" :need-vector)
        (read-kbd-macro "S" :need-vector)
        (read-kbd-macro "H" :need-vector)
        (read-kbd-macro "L" :need-vector)
        (read-kbd-macro "M-H" :need-vector)
        (read-kbd-macro "M-L" :need-vector)
        (read-kbd-macro "K" :need-vector) ; revisit kb
        (read-kbd-macro "C-S-j" :need-vector)
        (read-kbd-macro "C-(" :need-vector)
        (read-kbd-macro "C-S-h" :need-vector)
        (read-kbd-macro "C-{" :need-vector)
        (read-kbd-macro "C-S-l" :need-vector)
        (read-kbd-macro "C-}" :need-vector)
        (read-kbd-macro "C-S-k" :need-vector)
        (read-kbd-macro "C-)" :need-vector)
        (read-kbd-macro "z" :need-vector)
        (read-kbd-macro "Z" :need-vector)
        (read-kbd-macro "|" :need-vector)
        (read-kbd-macro "&" :need-vector)
        (read-kbd-macro "-" :need-vector)
        (read-kbd-macro ">" :need-vector)
        (read-kbd-macro "<" :need-vector)
        (read-kbd-macro "C->" :need-vector)
        (read-kbd-macro "C-<" :need-vector)
        (read-kbd-macro "C-S-o" :need-vector)
        (read-kbd-macro "J" :need-vector)
        (read-kbd-macro "M-J" :need-vector)
        (read-kbd-macro "M-<" :need-vector)
        (read-kbd-macro "M->" :need-vector)
        (read-kbd-macro "C-M-<" :need-vector)
        (read-kbd-macro "C-M->" :need-vector)
        (read-kbd-macro "=" :need-vector)
        (read-kbd-macro "<tab>" :need-vector)
        (read-kbd-macro "C-=" :need-vector)
        (read-kbd-macro "C-<tab>" :need-vector)
        (read-kbd-macro "M-=" :need-vector)
        (read-kbd-macro "M-<tab>" :need-vector)
        (read-kbd-macro ";" :need-vector)
        (read-kbd-macro "M-;" :need-vector)
        ;; ("c" symex-change :exit)
        ;; ("C" symex-change-remaining :exit)
        ;; ("s" symex-replace :exit)
        ;; ("o" symex-open-line-after :exit)
        ;; ("O" symex-open-line-before :exit)
        ;; ("A" symex-append-after :exit)
        ;; ("a" symex-insert-at-end :exit)
        ;; ("i" symex-insert-at-beginning :exit)
        ;; ("I" symex-insert-before :exit)
        ;; ("w" symex-wrap :exit)
        ;; ("W" symex-wrap-and-append :exit)
        )
  "Key sequences in Symex (Lithium) mode that are repeatable.")

(defvar symex-repeat-ring
  (repeat-ring-make (lambda (key-seq)
                      (and symex-editing-mode
                           (member key-seq
                                   symex-lithium-repeatable-keys))))
  "Repeat ring for use in Symex (Lithium) mode.")

(defun symex-repeat ()
  "Repeat the last key sequence entered while in Symex mode."
  (interactive)
  (repeat-ring-repeat-for-ring symex-repeat-ring))

;; TODO: others that could accept a count argument:
;; simple insert/append
(lithium-define-local-mode symex-editing-mode
  "Symex mode."
  (("1" digit-argument)
   ("2" digit-argument)
   ("3" digit-argument)
   ("4" digit-argument)
   ("5" digit-argument)
   ("6" digit-argument)
   ("7" digit-argument)
   ("8" digit-argument)
   ("9" digit-argument)
   ("h" symex-go-backward)
   ("j" symex-go-down)
   ("k" symex-go-up)
   ("l" symex-go-forward)
   ("gh" backward-char)
   ("gj" symex-next-visual-line)
   ("gk" symex-previous-visual-line)
   ("gl" forward-char)
   ("(" symex-create-round)
   ("[" symex-create-square)
   (")" symex-wrap-round)
   ("]" symex-wrap-square)
   ("C-'" symex-cycle-quote)
   ("C-," symex-cycle-unquote)
   ("`" symex-add-quoting-level)
   ("C-`" symex-remove-quoting-level)
   ("f" symex-traverse-forward)
   ("b" symex-traverse-backward)
   ("C-f" symex-traverse-forward-more)
   ("C-b" symex-traverse-backward-more)
   ("F" symex-traverse-forward-skip)
   ("B" symex-traverse-backward-skip)
   ("{" symex-leap-backward)
   ("}" symex-leap-forward)
   ("M-{" symex-soar-backward)
   ("M-}" symex-soar-forward)
   ("C-k" symex-climb-branch)
   ("C-j" symex-descend-branch)
   ("y" symex-yank)
   ("Y" symex-yank-remaining)
   ("p" symex-paste-after)
   ("P" symex-paste-before)
   ("x" symex-delete)
   ("X" symex-delete-backwards)
   ("D" symex-delete-remaining)
   ("c" symex-change :exit)
   ("C" symex-change-remaining :exit)
   ("C--" symex-clear)
   ("s" symex-replace :exit)
   ("S" symex-change-delimiter)
   ("H" symex-shift-backward)
   ("L" symex-shift-forward)
   ("M-H" symex-shift-backward-most)
   ("M-L" symex-shift-forward-most)
   ("K" symex-raise) ; revisit kb
   ("C-S-j" symex-emit-backward)
   ("C-(" symex-capture-backward)
   ("C-S-h" symex-capture-backward)
   ("C-{" symex-emit-backward)
   ("C-S-l" symex-capture-forward)
   ("C-}" symex-emit-forward)
   ("C-S-k" symex-emit-forward)
   ("C-)" symex-capture-forward)
   ("z" symex-swallow)
   ("Z" symex-swallow-tail)
   ("e" symex-evaluate)
   ("E" symex-evaluate-remaining)
   ("C-M-e" symex-evaluate-pretty)
   ("d" symex-evaluate-definition)
   ("M-e" symex-eval-recursive)
   ("T" symex-evaluate-thunk)
   ("r" symex-repl)
   ("R" symex-run)
   ("|" symex-split)
   ("&" symex-join)
   ("-" symex-splice)
   ("o" symex-open-line-after :exit)
   ("O" symex-open-line-before :exit)
   (">" symex-insert-newline)
   ("<" symex-join-lines-backwards)
   ("C->" symex-append-newline)
   ("C-<" symex-join-lines)
   ("C-S-o" symex-append-newline)
   ("J" symex-join-lines)
   ("M-J" symex-collapse)
   ("M-<" symex-collapse)
   ("M->" symex-unfurl)
   ("C-M-<" symex-collapse-remaining)
   ("C-M->" symex-unfurl-remaining)
   ("0" symex-goto-first)
   ("M-h" symex-goto-first)
   ("$" symex-goto-last)
   ("M-l" symex-goto-last)
   ("M-j" symex-goto-lowest)
   ("M-k" symex-goto-highest)
   ("=" symex-tidy)
   ("<tab>" symex-tidy)
   ("C-=" symex-tidy-remaining)
   ("C-<tab>" symex-tidy-remaining)
   ("M-=" symex-tidy-proper)
   ("M-<tab>" symex-tidy-proper)
   ("A" symex-append-after :exit)
   ("a" symex-insert-at-end :exit)
   ("i" symex-insert-at-beginning :exit)
   ("I" symex-insert-before :exit)
   ("w" symex-wrap :exit)
   ("W" symex-wrap-and-append :exit)
   (";" symex-comment)
   ("M-;" symex-comment-remaining)
   ("C-;" symex-eval-print) ; weird pre-offset (in both)
   ("s-;" symex-evaluate)
   ("H-h" symex--toggle-highlight) ; treats visual as distinct mode
   ("C-?" symex-describe)
   ("." symex-repeat)
   ;; escapes
   ("<return>" symex-enter-lower :exit)
   ("<escape>" symex-escape-higher :exit))
  :lighter " symex"
  :group 'symex)

(defun symex-lithium-initialize ()
  "Initialize lithium modal interface."
  ;; If for whatever reason the Lihium mode must exit, ensure
  ;; that any exit actions for symex mode are taken.
  (add-hook 'symex-editing-mode-pre-exit-hook #'symex-exit-mode)
  ;; Subscribe the symex repeat ring to key sequences entered
  ;; in the main Emacs command loop
  (repeat-ring-subscribe symex-repeat-ring))


(provide 'symex-lithium)
;;; symex-lithium.el ends here
