;;; symex-misc.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6.1") (lispy "0.26.0") (paredit "24") (evil-cleverparens "20170718.413") (dash-functional "2.15.0") (evil "20180914.1216") (smartparens "20181007.1501") (racket-mode "20181030.1345") (geiser "0.10") (evil-surround "20180102.1401") (hydra "20180918.1529"))

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.

;;; Commentary:
;;
;; Miscellaneous Lisp editing-related features
;;

;;; Code:


(require 'lispy)
(require 'evil)
(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-interface-elisp)
(require 'symex-interface-racket)
(require 'symex-interface-scheme)


;;;;;;;;;;;;;;;;;;;;;
;;; MISCELLANEOUS ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun symex-evaluate ()
  "Evaluate Symex."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((member major-mode symex-racket-modes)
           (symex-eval-racket))
          ((member major-mode symex-elisp-modes)
           (symex-eval-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-eval-scheme))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-evaluate-definition ()
  "Evaluate entire containing symex definition."
  (interactive)
  (cond ((member major-mode symex-racket-modes)
         (symex-eval-definition-racket))
        ((member major-mode symex-elisp-modes)
         (symex-eval-definition-elisp))
        ((equal major-mode 'scheme-mode)
         (symex-eval-definition-scheme))
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
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-eval-print ()
  "Eval symex and print result in buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-print-last-sexp)))

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
        (t (error "Symex mode: Lisp flavor not recognized!"))))

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
           (error (if-stuck (symex-go-backward)
                            (symex-go-forward)))))
        ((thing-at-point 'sexp)  ; som|ething
         (beginning-of-thing 'sexp))
        (t (if-stuck (symex-go-backward)
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

(advice-add 'symex-go-forward :around 'symex-selection-advice)
(advice-add 'symex-go-backward :around 'symex-selection-advice)
(advice-add 'symex-go-in :around 'symex-selection-advice)
(advice-add 'symex-go-out :around 'symex-selection-advice)
(advice-add 'symex-goto-first :around 'symex-selection-advice)
(advice-add 'symex-goto-last :around 'symex-selection-advice)
(advice-add 'symex-goto-outermost :around 'symex-selection-advice)
(advice-add 'symex-goto-innermost :around 'symex-selection-advice)
(advice-add 'symex-traverse-forward :around 'symex-selection-advice)
(advice-add 'symex-traverse-backward :around 'symex-selection-advice)
(advice-add 'symex-select-nearest :around 'symex-selection-advice)


(provide 'symex-misc)
;;; symex-misc.el ends here
