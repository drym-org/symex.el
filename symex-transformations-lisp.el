;;; symex-transformations-lisp.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex.el

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

;; Standard mutative operations to be performed on symexes.

;;; Code:

(require 'cl-lib)

(require 'paredit)
(require 'lispy)
(require 'evil)
(require 'evil-surround)
(require 'evil-cleverparens)  ;; really only need cp-textobjects here
(require 'symex-primitives)
(require 'symex-primitives-lisp)
(require 'symex-utils)
(require 'symex-traversals)
(require 'symex-interop)

;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSFORMATIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-lisp--delete (count)
  "Delete COUNT symexes."
  (interactive "p")
  (let ((last-command nil)  ; see symex-yank re: last-command
        (start (point))
        (end (symex--get-end-point count)))
    (kill-region start end))
  (cond ((or (symex--current-line-empty-p)         ; ^<>$
             (save-excursion (evil-last-non-blank) ; (<>$
                             (lispy-left-p))
             (looking-at-p "\n")) ; (abc <>
         (symex--join-to-next))
        ((save-excursion (back-to-indentation) ; ^<>)
                         (forward-char)
                         (lispy-right-p))
         ;; Cases 2 and 3 in issue #18
         ;; if the deleted symex is preceded by a comment line
         ;; or if the preceding symex is followed by a comment
         ;; on the same line, then don't attempt to join lines
         (let ((original-position (point)))
           (when (symex--go-backward)
             (let ((previous-symex-end-pos (symex--get-end-point 1)))
               (unless (symex--intervening-comment-line-p previous-symex-end-pos
                                                          original-position)
                 (goto-char previous-symex-end-pos)
                 ;; ensure that there isn't a comment on the
                 ;; preceding line before joining lines
                 (unless (condition-case nil
                             (progn (evil-find-char 1 ?\;)
                                    t)
                           (error nil))
                   (symex--join-to-match lispy-right)
                   (symex--adjust-point)))))))
        ((save-excursion (forward-char) ; ... <>)
                         (lispy-right-p))
         (symex--go-backward))
        (t (symex--go-forward)))
  (symex-select-nearest)
  (symex-tidy))

(defun symex-lisp--delete-backwards (count)
  "Delete COUNT symexes backwards."
  (interactive "p")
  (dotimes (_ count)
    (when (symex--go-backward)
      (symex-delete 1))))

(defun symex-lisp--change (count)
  "Change COUNT symexes."
  (interactive "p")
  (let ((start (point))
        (end (symex--get-end-point count)))
    (kill-region start end))
  (symex-enter-lowest))

(provide 'symex-transformations-lisp)
;;; symex-transformations-lisp.el ends here
