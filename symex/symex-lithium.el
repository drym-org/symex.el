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

(require 'cl-lib)
(require 'lithium)

(require 'symex-ui)
(require 'symex-repeat)
(require 'symex-core)

;; this is dynamically referenced in the lithium defining macros and
;; causes a byte compile warning here. The current approach seems
;; fine; not sure if there's an alternative that would avoid the
;; warning. But we just handle it in the usual way here, by declaring
;; the variable.
(defvar symex-editing-mode-map)

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
   ("X" symex-delete-backward)
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
   ("|" symex-split)
   ("&" symex-join)
   ("-" symex-splice)
   ("o" symex-open-line-after :exit)
   ("O" symex-open-line-before :exit)
   (">" symex-insert-newline)
   ("<" symex-join-lines-backward)
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
   ("a" symex-append-at-end :exit)
   ("i" symex-insert-at-beginning :exit)
   ("I" symex-insert-before :exit)
   ("w" symex-wrap :exit)
   ("W" symex-wrap-and-append :exit)
   (";" symex-comment)
   ("M-;" symex-comment-remaining)
   ("H-h" symex--toggle-highlight)
   ("." symex-repeat)
   ("C-." symex-repeat-pop)
   ("C-c ." symex-repeat-recent)
   ;; escapes
   ("<return>" symex-enter-lower)
   ("<escape>" symex-escape-higher))
  :lighter " symex"
  :group 'symex

  ;; Enable mode
  (when symex-editing-mode
    (add-hook 'post-command-hook #'symex-user-select-nearest-idempotent nil :local))
  ;; Disable mode
  (when (not symex-editing-mode)
    (remove-hook 'post-command-hook #'symex-user-select-nearest-idempotent t)))


(provide 'symex-lithium)
;;; symex-lithium.el ends here
