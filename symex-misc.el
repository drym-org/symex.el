;;; symex-misc.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex.el

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
;; Miscellaneous Lisp editing-related features
;;

;;; Code:


(require 'lispy)
(require 'evil)
(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-traversals)
(require 'symex-interface-elisp)
(require 'symex-interface-racket)
(require 'symex-interface-scheme)
(require 'symex-interface-clojure)
(require 'symex-interface-common-lisp)

;; These are customization or config variables defined elsewhere;
;; explicitly indicating them here to avoid byte compile warnings
(defvar symex-refocus-p)
(defvar symex-highlight-p)
(defvar symex-smooth-scroll-p)
(defvar symex-racket-modes)
(defvar symex-elisp-modes)

;;;;;;;;;;;;;;;;;;;;;
;;; MISCELLANEOUS ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun symex-evaluate ()
  "Evaluate Symex."
  (interactive)
  (let ((original-evil-state evil-state))
    (evil-emacs-state) ; so that which symex is meant has a standard interpretation
    (save-excursion
      (forward-sexp) ; selected symexes will have the cursor on the starting paren
      (cond ((member major-mode symex-racket-modes)
             (symex-eval-racket))
            ((member major-mode symex-elisp-modes)
             (symex-eval-elisp))
            ((equal major-mode 'scheme-mode)
             (symex-eval-scheme))
            ((equal major-mode 'clojure-mode)
             (symex-eval-clojure))
            ((equal major-mode 'lisp-mode)
             (symex-eval-common-lisp))
            (t (error "Symex mode: Lisp flavor not recognized!"))))
    ;; enter normal state here momentarily, as a workaround to prevent entry into
    ;; symex mode from being treated as "emacs context" since the entry into emacs
    ;; state is done here as an implementation detail and is not user-directed
    (evil-normal-state)
    (funcall (intern (concat "evil-" (symbol-name original-evil-state) "-state")))))

(defun symex-evaluate-definition ()
  "Evaluate entire containing symex definition."
  (interactive)
  (cond ((member major-mode symex-racket-modes)
         (symex-eval-definition-racket))
        ((member major-mode symex-elisp-modes)
         (symex-eval-definition-elisp))
        ((equal major-mode 'scheme-mode)
         (symex-eval-definition-scheme))
        ((equal major-mode 'clojure-mode)
         (symex-eval-definition-clojure))
        ((equal major-mode 'lisp-mode)
         (symex-eval-definition-common-lisp))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-evaluate-pretty ()
  "Evaluate Symex and transform output into a useful string representation."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((member major-mode symex-racket-modes)
           (symex-eval-pretty-racket))
          ((member major-mode symex-elisp-modes)
           (symex-eval-pretty-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-eval-pretty-scheme))
          ((equal major-mode 'clojure-mode)
           (symex-eval-pretty-clojure))
          ((equal major-mode 'lisp-mode)
           (symex-eval-pretty-common-lisp))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-eval-print ()
  "Eval symex and print result in buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (cond ((member major-mode symex-racket-modes)
           (symex-eval-print-racket))
          ((member major-mode symex-elisp-modes)
           (symex-eval-print-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-eval-print-scheme))
          ((equal major-mode 'clojure-mode)
           (symex-eval-print-clojure))
          ((equal major-mode 'lisp-mode)
           (symex-eval-print-common-lisp))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-evaluate-thunk ()
  "Evaluate Symex as a thunk.

This treats the symex as a thunk -- i.e. a function that takes no
arguments -- by (transparently) wrapping it in parens and then
executing it."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((member major-mode symex-racket-modes)
           (symex-eval-thunk-racket))
          ((member major-mode symex-elisp-modes)
           (symex-eval-thunk-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-eval-thunk-scheme))
          ((equal major-mode 'clojure-mode)
           (symex-eval-thunk-clojure))
          ((equal major-mode 'lisp-mode)
           (symex-eval-thunk-common-lisp))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-describe ()
  "Lookup doc on symex."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((member major-mode symex-racket-modes)
           (symex-describe-symbol-racket))
          ((member major-mode symex-elisp-modes)
           (symex-describe-symbol-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-describe-symbol-scheme))
          ((equal major-mode 'clojure-mode)
           (symex-describe-symbol-clojure))
          ((equal major-mode 'lisp-mode)
           (symex-describe-symbol-common-lisp))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-repl ()
  "Go to REPL."
  (interactive)
  (cond ((member major-mode symex-racket-modes)
         (symex-repl-racket))
        ((member major-mode symex-elisp-modes)
         (symex-repl-elisp))
        ((equal major-mode 'scheme-mode)
         (symex-repl-scheme))
        ((equal major-mode 'clojure-mode)
         (symex-repl-clojure))
        ((equal major-mode 'lisp-mode)
         (symex-repl-common-lisp))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-run ()
  "Send to REPL."
  (interactive)
  (cond ((member major-mode symex-racket-modes)
         (symex-run-racket))
        ((member major-mode symex-elisp-modes)
         (symex-run-elisp))
        ((equal major-mode 'scheme-mode)
         (symex-run-scheme))
        ((equal major-mode 'clojure-mode)
         (symex-run-clojure))
        ((equal major-mode 'lisp-mode)
         (symex-run-common-lisp))
        ((equal major-mode 'arc-mode)
         (symex-run-arc))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))  ; TODO: create in lisp interaction mode if missing

(defun symex-switch-to-messages-buffer ()
  "Switch to messages buffer while retaining focus in original window."
  (interactive)
  (switch-to-buffer-other-window "*Messages*")
  (evil-window-mru))

(defun symex-select-nearest ()
  "Select symex nearest to point."
  (interactive)
  (cond ((and (not (eobp))
              (save-excursion (forward-char) (lispy-right-p)))  ; |)
         (forward-char)
         (lispy-different))
        ((looking-at-p "[[:space:]\n]")  ; <> |<> or <> |$
         (condition-case nil
             (progn (re-search-forward "[^[:space:]\n]")
                    (backward-char))
           (error (symex-if-stuck (symex-go-backward)
                                  (symex-go-forward)))))
        ((thing-at-point 'sexp)  ; som|ething
         (beginning-of-thing 'sexp))
        (t (symex-if-stuck (symex-go-backward)
                           (symex-go-forward))))
  (point))

(defun symex-refocus (&optional smooth-scroll)
  "Move screen to put symex in convenient part of the view.

If SMOOTH-SCROLL is set, then scroll the view gently to aid in visual tracking."
  (interactive)
  ;; Note: window-text-height is not robust to zooming
  (let* ((window-focus-line-number (/ (window-text-height)
                                       3))
         (current-line-number (line-number-at-pos))
         (top-line-number (save-excursion (evil-window-top)
                                          (line-number-at-pos)))
         (window-current-line-number (- current-line-number
                                        top-line-number))
         (window-scroll-delta (- window-current-line-number
                                 window-focus-line-number))
         (window-upper-view-bound (/ (window-text-height)
                                     9))
         (window-lower-view-bound (* (window-text-height)
                                     (/ 4.0 6))))
    (unless (< window-upper-view-bound
               window-current-line-number
               window-lower-view-bound)
      (if smooth-scroll
          (dotimes (_ (/ (abs window-scroll-delta)
                         3))
            (condition-case nil
                (evil-scroll-line-down (if (> window-scroll-delta 0)
                                           3
                                         -3))
              (error nil))
            (sit-for 0.0001))
        (recenter window-focus-line-number)))))

(defun symex-index ()  ; TODO: may be better framed as a computation
  "Get relative (from start of containing symex) index of current symex."
  (interactive)
  (save-excursion
    (symex-select-nearest)
    (let ((original-location (point)))
      (let ((current-location (symex-goto-first))
            (result 0))
        (while (< current-location original-location)
          (symex-go-forward)
          (setq current-location (point))
          (setq result (1+ result)))
        result))))

(defun symex-depth ()  ; TODO: may be better framed as a computation
  "Get depth (from root) of current symex."
  (interactive)
  (save-excursion
    (symex-select-nearest)
    (let ((moves (symex-execute-traversal symex--traversal-goto-lowest)))
      (length moves))))

(defun symex-leap-backward ()
  "Leap backward to a neighboring branch, preserving the depth and position.

Note: This isn't the most efficient at the moment since it determines
the depth at every step of the traversal which itself is logarithmic
in the size of the tree, making the cost O(nlog(n)).

There are at least two possible ways in which we could implement this
'leap' feature: first, as a \"local\" traversal from the starting
position, keeping track of changes to the depth while traversing and
stopping when a suitable destination point is reached.  This would be
efficient since we would only need to determine the depth once, at the
start, making it O(n).  However, this approach would require some
notion of 'memory' to be built into the DSL semantics, which at
present it lacks (representing a theoretical limitation on the types
of traversals expressible in the DSL in its present form).

A second way to do it is in \"global\" terms -- rather than keeping
track of changing depth in the course of the traversal, instead,
determine always from a common reference point (the root) the current
depth. This allows us to circumvent the need for 'memory' since this
information could be computed afresh at each step.  This latter
approach is the one employed here."
  (interactive)
  (let ((depth (symex-depth))
        (index (symex-index)))
    (let ((find-neighboring-branch
           (symex-traversal
            (circuit (precaution symex--traversal-postorder
                                 (afterwards (not (lambda ()
                                                    (= (symex-depth)
                                                       depth)))))))))
      (symex-execute-traversal
       (symex-traversal
        (precaution (maneuver (decision (at first)
                                        find-neighboring-branch
                                        (maneuver symex--traversal-goto-first
                                                  find-neighboring-branch))
                              symex--traversal-postorder
                              symex--traversal-goto-first
                              (circuit (precaution (move forward)
                                                   (beforehand (lambda ()
                                                                 (< (symex-index)
                                                                    index))))))
                    (beforehand (not (at root)))))))))

(defun symex-leap-forward ()
  "Leap forward to a neighboring branch, preserving the depth and position.

See the documentation on `symex-leap-backward` for details regarding
the implementation."
  (interactive)
  (let ((depth (symex-depth))
        (index (symex-index)))
    (let ((find-neighboring-branch
           (symex-traversal
            (circuit (precaution symex--traversal-preorder
                                 (afterwards (not (lambda ()
                                                    (= (symex-depth)
                                                       depth)))))))))
      (symex-execute-traversal
       (symex-traversal
        (precaution (maneuver (decision (at last)
                                        find-neighboring-branch
                                        (maneuver symex--traversal-goto-last
                                                  find-neighboring-branch))
                              symex--traversal-preorder
                              (circuit (precaution (move forward)
                                                   (beforehand (lambda ()
                                                                 (< (symex-index)
                                                                    index))))))
                    (beforehand (not (at root)))))))))

(defun symex--selection-side-effects ()
  "Things to do as part of symex selection, e.g. after navigations."
  (interactive)
  (when symex-refocus-p
    (symex-refocus symex-smooth-scroll-p))
  (when symex-highlight-p
    (mark-sexp)))

(defun symex-selection-advice (orig-fn &rest args)
  "Attach symex selection side effects to a given function.

ORIG-FN could be any function that results in a symex being selected.
ARGS are the arguments that were passed to ORIG-FN (as any advice function
is expected to handle in Emacs)."
  (interactive)
  (let ((result (apply orig-fn args)))
    (symex--selection-side-effects)
    result))

(advice-add #'symex-go-forward :around #'symex-selection-advice)
(advice-add #'symex-go-backward :around #'symex-selection-advice)
(advice-add #'symex-go-up :around #'symex-selection-advice)
(advice-add #'symex-go-down :around #'symex-selection-advice)
(advice-add #'symex-goto-first :around #'symex-selection-advice)
(advice-add #'symex-goto-last :around #'symex-selection-advice)
(advice-add #'symex-goto-lowest :around #'symex-selection-advice)
(advice-add #'symex-goto-highest :around #'symex-selection-advice)
(advice-add #'symex-traverse-forward :around #'symex-selection-advice)
(advice-add #'symex-traverse-backward :around #'symex-selection-advice)
(advice-add #'symex-select-nearest :around #'symex-selection-advice)


(provide 'symex-misc)
;;; symex-misc.el ends here
