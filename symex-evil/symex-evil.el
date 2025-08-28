;;; symex-evil.el --- Use Symex with Evil -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/drym-org/symex.el
;; Version: 2.0
;; Package-Requires: ((emacs "25.1") (evil "1.2.14") (symex "2.0"))
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

;; Use Symex with Evil.

;;; Code:

(require 'evil)
(require 'undo-tree nil :no-error)
(require 'lithium)
(require 'symex)

(declare-function undo-tree-undo "ext:undo-tree")
(declare-function undo-tree-redo "ext:undo-tree")

(defun symex-select-nearest-advice (&rest _)
  "Advice to select the nearest symex."
  (when symex-editing-mode
    (symex-user-select-nearest)))

(defun symex--adjust-point ()
  "Helper to adjust point to indicate the correct symex."
  (unless (or (bobp)
              (bolp)
              (symex-lisp--point-at-start-p)
              (looking-back "[,'`]" (line-beginning-position))
              (save-excursion (backward-char)  ; just inside symex
                              (or (symex-left-p)
                                  ;; this is to exclude the case where
                                  ;; we're inside a string, "|abc"
                                  ;; which "inverts" the code structure
                                  ;; and causes unexpected behavior when
                                  ;; navigating using Emacs's built-in
                                  ;; primitive symex motions. Unlike normal
                                  ;; forms, opening and closing delimiters
                                  ;; are not distinguished for strings and
                                  ;; so we can't specifically check for
                                  ;; "open quote," with the result that
                                  ;; in the case "abc"|, we don't always
                                  ;; select the right symex the way we
                                  ;; would with (abc)|.
                                  (symex-lisp-string-p))))
    (condition-case nil
        (backward-char)
      (error nil))))

(defun symex--adjust-point-on-entry ()
  "Adjust point context from the Emacs to the Vim interpretation.

If entering symex mode from Insert or Emacs mode, then translate point
so it indicates the appropriate symex in Symex mode.  This is necessary
because in Emacs, the symex preceding point is indicated.  In Vim, the
symex \"under\" point is indicated.  We want to make sure to select the
right symex when we enter Symex mode."
  (interactive)
  (when (or (not evil-mode)
            (member evil-state '(insert emacs))
            (not (symex-ts-available-p)))
    (symex--adjust-point)))

;; These override the definitions in the symex package, so this
;; package (symex-evil) should be loaded after symex
(defun symex-escape-higher ()
  "Exit symex mode via an \"escape\"."
  (interactive)
  (cond (evil-mode (evil-normal-state))))

(defun symex-enter-lower ()
  "Exit symex mode via an \"enter\"."
  (interactive)
  (cond (evil-mode (evil-insert-state))))

(defun symex-enter-lowest ()
  "Enter the lowest (manual) editing level."
  (interactive)
  (cond (evil-mode (evil-insert-state))))

(evil-define-state symex
  "Symex state."
  :tag " <λ> "
  :message "-- SYMEX --")

;;;###autoload
(defun symex-evil-initialize ()
  "Evil interconnects for Symex."
  ;; It's necessary to override all these keys because enabling normal
  ;; state in symex evil state overrides Symex's handling of counts
  ;; (though it otherwise works fine). So we must leave normal state
  ;; disabled in symex state, which necessitates redefining the
  ;; relevant bindings in symex mode explicitly.
  (lithium-define-keys symex-editing-mode
    (("u" evil-undo)
     ("C-r" evil-redo)
     ("\"" evil-use-register)
     ("g;" evil-goto-last-change)
     ("g," evil-goto-last-change-reverse)
     ("gg" evil-goto-first-line)
     ("G" evil-goto-line)
     ("q" evil-record-macro)
     ("@" evil-execute-macro)
     ("m" evil-set-marker)
     ("'" evil-goto-mark-line)
     ("/" evil-search-forward)
     ("?" evil-search-backward)
     ("#" evil-search-word-backward)
     ("*" evil-search-word-forward)
     ("n" evil-search-next)
     ("N" evil-search-previous)
     ("C-d" evil-scroll-down)
     ("C-u" evil-scroll-up)
     ("C-]" evil-jump-to-tag)
     ("C-i" evil-jump-forward)
     ("C-o" evil-jump-backward)
     ("C-p" evil-paste-pop)
     ("C-n" evil-paste-pop-next)))
  ;; TODO: handle other undo systems?
  (when (and (eq 'undo-tree evil-undo-system)
             (fboundp #'undo-tree-undo))
    (advice-add #'undo-tree-undo
                :after #'symex-select-nearest-advice)
    (advice-add #'undo-tree-redo
                :after #'symex-select-nearest-advice))
  (add-hook 'symex-editing-mode-pre-entry-hook
            #'symex--adjust-point-on-entry)
  (add-hook 'symex-editing-mode-pre-entry-hook
            #'evil-symex-state))

(defun symex-evil-disable ()
  "Disable evil interop."
  (when (and (eq 'undo-tree evil-undo-system)
             (fboundp #'undo-tree-undo))
    (advice-remove #'undo-tree-undo
                   #'symex-select-nearest-advice)
    (advice-remove #'undo-tree-redo
                   #'symex-select-nearest-advice))
  (remove-hook 'symex-editing-mode-pre-entry-hook
               #'symex--adjust-point-on-entry)
  (remove-hook 'symex-editing-mode-pre-entry-hook
               #'evil-symex-state))

;;;###autoload
(define-minor-mode symex-evil-mode
  "A mode seamlessly integrating Symex with Evil."
  :lighter " symex-evil"
  :global t
  :group 'symex
  (if symex-evil-mode
      (symex-evil-initialize)
    (symex-evil-disable)))


(provide 'symex-evil)
;;; symex-evil.el ends here
