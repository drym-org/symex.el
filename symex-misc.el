;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;
;;; MISCELLANEOUS ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun symex-evaluate ()
  "Evaluate Symex"
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-eval-symex))
          ((member major-mode elisp-modes)
           (my-elisp-eval-symex))
          ((equal major-mode 'scheme-mode)
           (my-scheme-eval-symex))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-evaluate-definition ()
  "Evaluate top-level definition"
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-send-definition nil))
        ((member major-mode elisp-modes)
         (eval-defun nil))
        ((equal major-mode 'scheme-mode)
         (geiser-eval-definition nil))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-evaluate-pretty ()
  "Evaluate Symex and transform output into a useful string representation."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-eval-symex-pretty))
          ((member major-mode elisp-modes)
           (my-elisp-eval-symex))
          ((equal major-mode 'scheme-mode)
           (my-scheme-eval-symex))
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
    (cond ((equal major-mode 'racket-mode)
           (my-racket-describe-symbol))
          ((member major-mode elisp-modes)
           (my-elisp-describe-symbol))
          ((equal major-mode 'scheme-mode)
           (my-scheme-describe-symbol))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-repl ()
  "Go to REPL."
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-repl))
        ((member major-mode elisp-modes)
         (my-lisp-repl))
        ((equal major-mode 'scheme-mode)
         (geiser-mode-switch-to-repl))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-select-nearest ()
  "Select symex nearest to point"
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
  (symex-refocus)
  (point))

(defun symex-refocus (&optional smooth-scroll)
  "Move screen to put symex in convenient part of the view."
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
          (dotimes (i (/ (abs window-scroll-delta)
                         3))
            (condition-case nil
                (evil-scroll-line-down (if (> window-scroll-delta 0)
                                           3
                                         -3))
              (error nil))
            (sit-for 0.0001))
        (recenter window-focus-line-number)))))

(provide 'symex-misc)
