;;; symex-misc.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Miscellaneous Lisp editing-related features

;;; Code:


(require 'symex-custom)
(require 'symex-primitives)
(require 'symex-traversals)
(require 'symex-interop)
(require 'symex-ui)

;;;;;;;;;;;;;;;;;;;;;
;;; MISCELLANEOUS ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun symex-user-select-nearest ()
  "Select symex nearest to point.

This user-level interface does the most intuitive thing from the perspective
of the user, and isn't necessarily deterministic. It may select the next
expression, the previous one, or the containing one, depending on context.
For the deterministic version used at the primitive level, see
`symex-select-nearest`."
  (interactive)
  (symex-select-nearest)
  (when (and (symex-right-p)
             (looking-back symex--re-left
                           (line-beginning-position)))
    (symex-go-down 1)))

(defun symex-select-nearest-in-line ()
  "Select symex nearest to point that's on the current line."
  (interactive)
  (unless (symex--current-line-empty-p)
    (let ((original-pos (point)))
      (symex-select-nearest)
      (unless (= (line-number-at-pos)
                 (line-number-at-pos original-pos))
        (goto-char original-pos)))))

(defun symex-next-visual-line (&optional count)
  "Coordinate navigation to move down.

This moves down COUNT lines in terms of buffer coordinates, rather than
structurally in terms of the tree."
  (interactive "p")
  (next-line count)
  (symex-select-nearest-in-line))

(defun symex-previous-visual-line (&optional count)
  "Coordinate navigation to move up.

This moves up COUNT lines in terms of buffer coordinates, rather than
structurally in terms of the tree."
  (interactive "p")
  (previous-line count)
  (symex-select-nearest-in-line))

(defun symex-select-nearest-advice (&rest _)
  "Advice to select the nearest symex."
  (when symex-editing-mode
    (symex-user-select-nearest)))

(defun symex--selection-side-effects ()
  "Things to do as part of symex selection, e.g. after navigations."
  (interactive)
  (when symex-highlight-p
    (symex--update-overlay)))

(defun symex-selection-advice (orig-fn &rest args)
  "Attach symex selection side effects to a given function.

ORIG-FN could be any function that results in a symex being selected.
ARGS are the arguments that were passed to ORIG-FN (as any advice function
is expected to handle in Emacs)."
  (interactive)
  (let ((result (apply orig-fn args)))
    (symex--selection-side-effects)
    result))

(defun symex-selection-motion-advice (orig-fn count &rest args)
  "Attach symex selection side effects to a given function.

This is a version of `symex-selection-advice` that preserves a numeric
argument supplied by the user, and can be used when the underlying
function expects to receive one.

ORIG-FN could be any function that results in a symex being selected.
COUNT is the numeric argument provided via interactive invocation.
ARGS are the arguments that were passed to ORIG-FN (as any advice function
is expected to handle in Emacs)."
  (interactive "p")
  (let ((result (apply orig-fn count args)))
    (symex--selection-side-effects)
    result))

(defun symex--add-selection-advice ()
  "Add selection advice."
  (advice-add #'symex-go-forward :around #'symex-selection-motion-advice)
  (advice-add #'symex-go-backward :around #'symex-selection-motion-advice)
  (advice-add #'symex-go-up :around #'symex-selection-motion-advice)
  (advice-add #'symex-go-down :around #'symex-selection-motion-advice)
  (advice-add #'symex-traverse-forward :around #'symex-selection-motion-advice)
  (advice-add #'symex-traverse-backward :around #'symex-selection-motion-advice)
  (advice-add #'symex-traverse-forward-skip :around #'symex-selection-motion-advice)
  (advice-add #'symex-traverse-backward-skip :around #'symex-selection-motion-advice)
  (advice-add #'symex-leap-forward :around #'symex-selection-motion-advice)
  (advice-add #'symex-leap-backward :around #'symex-selection-motion-advice)
  (advice-add #'symex-soar-forward :around #'symex-selection-motion-advice)
  (advice-add #'symex-soar-backward :around #'symex-selection-motion-advice)
  (advice-add #'symex-goto-first :around #'symex-selection-advice)
  (advice-add #'symex-goto-last :around #'symex-selection-advice)
  (advice-add #'symex-goto-lowest :around #'symex-selection-advice)
  (advice-add #'symex-goto-highest :around #'symex-selection-advice)
  (advice-add #'symex-user-select-nearest :around #'symex-selection-advice))

(defun symex--remove-selection-advice ()
  "Remove selection advice."
  (advice-remove #'symex-go-forward #'symex-selection-motion-advice)
  (advice-remove #'symex-go-backward #'symex-selection-motion-advice)
  (advice-remove #'symex-go-up #'symex-selection-motion-advice)
  (advice-remove #'symex-go-down #'symex-selection-motion-advice)
  (advice-remove #'symex-traverse-forward #'symex-selection-motion-advice)
  (advice-remove #'symex-traverse-backward #'symex-selection-motion-advice)
  (advice-remove #'symex-traverse-forward-skip #'symex-selection-motion-advice)
  (advice-remove #'symex-traverse-backward-skip #'symex-selection-motion-advice)
  (advice-remove #'symex-leap-forward #'symex-selection-motion-advice)
  (advice-remove #'symex-leap-backward #'symex-selection-motion-advice)
  (advice-remove #'symex-soar-forward #'symex-selection-motion-advice)
  (advice-remove #'symex-soar-backward #'symex-selection-motion-advice)
  (advice-remove #'symex-goto-first #'symex-selection-advice)
  (advice-remove #'symex-goto-last #'symex-selection-advice)
  (advice-remove #'symex-goto-lowest #'symex-selection-advice)
  (advice-remove #'symex-goto-highest #'symex-selection-advice)
  (advice-remove #'symex-user-select-nearest #'symex-selection-advice))

(defun symex-exit-mode ()
  "Take necessary action upon symex mode exit."
  (when symex--original-blink-cursor-state
    (blink-cursor-mode 1))
  (when symex-refocus-p
    (symex--restore-scroll-margin))
  (symex--delete-overlay)
  (symex--primitive-exit))

(provide 'symex-misc)
;;; symex-misc.el ends here
