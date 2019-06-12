;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;
;;; PRIMITIVES ;;;
;;;;;;;;;;;;;;;;;;

(defmacro if-stuck (do-what operation &rest body)
  `(let ((orig-pt (point)))
     ,operation
     (if (= orig-pt (point))
         ,do-what
       ,@body)))

(defun point-at-root-symex? ()
  "Check if point is at a root symex."
  (interactive)
  (save-excursion
    (if-stuck t
              (symex-go-out)
              nil)))

(defun point-at-first-symex? ()
  "Check if point is at the first symex at some level."
  (interactive)
  (save-excursion
    (if-stuck t
              (symex-go-backward)
              nil)))

(defun point-at-last-symex? ()
  "Check if point is at the last symex at some level."
  (interactive)
  (save-excursion
    (if-stuck t
              (symex-go-forward)
              nil)))

(defun point-at-final-symex? ()
  "Check if point is at the last symex in the buffer."
  (interactive)
  (save-excursion
    (if-stuck (progn (if-stuck t
                               (symex-go-out)
                               nil))
              (symex-go-forward)
              nil)))

(defun point-at-initial-symex? ()
  "Check if point is at the first symex in the buffer."
  (interactive)
  (save-excursion
    (condition-case nil
        (progn (backward-sexp 1)
               (not (thing-at-point 'sexp)))
      (error nil))))

(defun symex-comment-line-p ()
  "Checks if we're currently at the start of a comment line."
  (and (lispy-bolp)
       (looking-at-p ";")))

(defun symex-empty-list-p ()
  "Checks if we're looking at an empty list."
  (save-excursion
    (and (lispy-left-p)
         (progn (forward-char 2) ;; need to go forward by 2 for some reason
                (lispy-right-p)))))

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

(defun symex--forward-one ()
  "Forward one symex"
  (let ((original-location (point))
        (result 0))
    (if (thing-at-point 'sexp)
        (condition-case nil
            (progn (forward-sexp 2)
                   (setq result 2))
          (error (condition-case nil
                     (progn (forward-sexp 1)
                            (setq result 1))
                   (error (setq result 0)))))
      (condition-case nil
          (progn (forward-sexp 1)
                 (setq result 1))
        (error (setq result 0))))
    (condition-case nil
        (progn (backward-sexp 1)
               (setq result (1- result)))
      (error nil))
    (let ((current-location (point)))
      (when (= original-location current-location)
        ;; happens at end of buffer
        (setq result 0)))
    (symex-refocus)
    result))

(defun symex-forward (&optional count)
  "Forward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (symex--forward-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move result 0))))

(defun symex--backward-one ()
  "Backward one symex."
  (let ((result 0))
    (when (not (point-at-initial-symex?))
      (condition-case nil
          (progn (backward-sexp 1)
                 (setq result (1+ result)))
        (error nil)))
    result))

(defun symex-backward (&optional count)
  "Backward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (symex--backward-one)))
        (setq result (+ res result))))
    (symex-refocus)
    (when (> result 0)
      (symex-make-move (- result) 0))))

(defun symex--enter-one ()
  "Enter one lower symex level."
  (let ((result 1))
    (cond ((symex-comment-line-p)
           (lispy-flow 1))
          ((and (lispy-left-p)
                (not (symex-empty-list-p)))
           (forward-char))
          (t (setq result 0)))
    result))

(defun symex-enter (&optional count)
  "Enter lower symex level."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (symex--enter-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move 0 result))))

(defun symex--exit-one ()
  "Exit one level."
  (condition-case nil
      (progn (paredit-backward-up 1)
             1)
    (error 0)))

(defun symex-exit (&optional count)
  "Exit to higher symex level"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (symex--exit-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move 0 (- result)))))

(provide 'symex-primitives)
