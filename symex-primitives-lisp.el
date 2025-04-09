;;; symex-primitives-lisp.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Primitive navigations using third party libraries.  This layer of primitives
;; can be swapped out without changing the higher-level functionality provided
;; by the Symex DSL.  In the future it may make sense to directly operate on
;; an annotated representation of the AST here containing a mapping to buffer
;; positions and other metadata relevant for editing purposes.

;;; Code:


(require 'thingatpt)
(require 'paredit)
(require 'symex-data)
(require 'symex-utils)
(require 'symex-interface)

(declare-function symex--ensure-minor-mode "symex.el")

;;;;;;;;;;;;;;;;;;
;;; PRIMITIVES ;;;
;;;;;;;;;;;;;;;;;;

;;; Configuration

;;; List major modes in which symex should be active.
(defun symex-get-lisp-modes ()
  "List modes that implement the symex interface."
  (mapcar #'car symex-interfaces))

;;; Parsing

;; From https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html
(defun symex-other ()
  "Move point to the other delimiter in a matching pair."
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))))

(defun symex-comment-line-p ()
  "Check if we're currently at the start of a comment line."
  (save-excursion
    (back-to-indentation)
    (looking-at-p ";")))

(defun symex-inline-comment-p ()
  "Check if there is an inline comment following point."
  (looking-at-p "[[:space:]]*;"))

(defun symex-lisp-string-p ()
  "Check if the symex is a string."
  (looking-at-p "\""))

(defun symex-empty-list-p ()
  "Check if we're looking at an empty list."
  (looking-at-p
   (concat symex--re-left
           symex--re-optional-whitespace
           symex--re-right)))

(defun symex-empty-string-p ()
  "Check if we're looking at an empty list."
  (save-excursion
    (and (symex-lisp-string-p)
         (progn (forward-char 1)
                (symex-lisp-string-p)))))

(defun symex-inside-empty-form-p ()
  "Check if point is inside an empty form."
  (and (looking-back (concat symex--re-left
                             symex--re-optional-whitespace)
                     (line-beginning-position))
       (looking-at-p
        (concat symex--re-optional-whitespace
                symex--re-right))))

(defun symex--racket-syntax-object-p ()
  "Check if the symex is a racket syntax object."
  (looking-at-p symex--re-racket-syntax-object))

(defun symex--splicing-unquote-p ()
  "Check if the symex is a spliced unquoted list."
  (looking-at-p symex--re-splicing-unquote))

(defun symex--racket-unquote-syntax-p ()
  "Check if the symex is a racket unquoted syntax list."
  (looking-at-p symex--re-racket-unquote-syntax))

(defun symex--racket-splicing-unsyntax-p ()
  "Check if the symex is a racket spliced unsyntaxed list."
  (looking-at-p symex--re-racket-splicing-unsyntax))

(defun symex--quoted-list-p ()
  "Check if the symex is a quoted list."
  (looking-at-p symex--re-quoted-list))

(defun symex--unquoted-list-p ()
  "Check if the symex is an unquoted list."
  (looking-at-p symex--re-unquoted-list))

(defun symex--clojure-deref-reader-macro-p ()
  "Check if the symex is a Clojure deref reader macro."
  (looking-at-p symex--re-clojure-deref-reader-macro))

(defun symex--clojure-literal-lambda-p ()
  "Check if the symex is a Clojure anonymous function literal."
  (looking-at-p symex--re-clojure-literal-lambda))

(defun symex--special-left-p ()
  "Check if point is at a \"special\" opening delimiter.

This includes any special cases that should be treated as lists for
the purpose of movement, such as quoted lists and Lisp flavor-specific
special forms.  This may be the sort of information that's best
obtained from an AST-aware primitives layer, rather than parsed
as special cases here."
  (or (symex--quoted-list-p)
      (symex--unquoted-list-p)
      (symex--racket-syntax-object-p)
      (symex--splicing-unquote-p)
      (symex--racket-splicing-unsyntax-p)
      (symex--racket-unquote-syntax-p)
      (symex--clojure-deref-reader-macro-p)
      (symex--clojure-literal-lambda-p)))

(defun symex--re-or (&rest strings)
  "An OR combinator for regular expression STRINGS."
  (concat (string-join strings "\\|") ))

(defun symex--form-offset ()
  "Get the offset for entry into the form."
  (cond ((symex--racket-splicing-unsyntax-p) 4)
        ((or (symex--racket-syntax-object-p)
             (symex--racket-unquote-syntax-p)
             (symex--splicing-unquote-p))
         3)
        ((or (symex--quoted-list-p)
             (symex--unquoted-list-p)
             (symex--clojure-deref-reader-macro-p)
             (symex--clojure-literal-lambda-p))
         2)
        ((or (symex-lisp-form-p)
             (symex-lisp-string-p)) 1)
        (t 0)))

(defun symex--special-empty-list-p ()
  "Check if we're looking at a \"special\" empty list."
  (or (save-excursion
        (and (symex--racket-splicing-unsyntax-p)
             (progn (forward-char 4)
                    (looking-at-p (concat symex--re-whitespace symex--re-right)))))
      (save-excursion
        (and (or (symex--racket-syntax-object-p)
                 (symex--racket-unquote-syntax-p)
                 (symex--splicing-unquote-p))
             (progn (forward-char 3)
                    (looking-at-p (concat symex--re-whitespace symex--re-right)))))
      (save-excursion
        (and (or (symex--quoted-list-p)
                 (symex--unquoted-list-p)
                 (symex--clojure-deref-reader-macro-p)
                 (symex--clojure-literal-lambda-p))
             (progn (forward-char 2)
                    (looking-at-p (concat symex--re-whitespace symex--re-right)))))))

(defun symex-lisp-atom-p ()
  "Check if the symex is an atom."
  (or (not (symex-left-p))
      (symex-empty-list-p)))

(defun symex-lisp-form-p ()
  "Check if the symex is a composite expression, i.e. a nonatom."
  (or (not (symex-lisp-atom-p))
      (symex-empty-list-p)))

(defun symex--intervening-comment-line-p (start end)
  "Check if there is a comment line between the positions START and END."
  (save-excursion
    (goto-char start)
    (catch 'stop
      (when (symex-comment-line-p)
        (throw 'stop (progn (back-to-indentation)
                            (point))))
      (while (not (= (line-number-at-pos)
                     (line-number-at-pos end)))
        (forward-line 1)
        (when (symex-comment-line-p)
          (throw 'stop (progn (back-to-indentation)
                              (point))))))))

(defmacro symex-lisp--if-stuck (do-what operation &rest body)
  "Attempt OPERATION and if it fails, then do DO-WHAT."
  (let ((orig-pt (gensym)))
    `(let ((,orig-pt (point)))
       ,operation
       (if (= ,orig-pt (point))
           ,do-what
         ,@body))))

(defun symex-lisp--point-at-root-symex-p ()
  "Check if point is at a root symex."
  (save-excursion
    (symex-lisp--if-stuck t
                          (symex-lisp--go-down)
                          nil)))

(defun symex-lisp--point-at-first-symex-p ()
  "Check if point is at the first symex at some level."
  (save-excursion
    (symex-lisp--if-stuck t
                          (symex-lisp--backward)
                          nil)))

(defun symex-lisp--point-at-last-symex-p ()
  "Check if point is at the last symex at some level."
  (save-excursion
    (symex-lisp--if-stuck t
                          (symex-lisp--forward)
                          nil)))

(defun symex-lisp--point-at-final-symex-p ()
  "Check if point is at the last symex in the buffer."
  (and (symex-lisp--point-at-last-symex-p)
       (symex-lisp--point-at-root-symex-p)))

(defun symex-lisp--point-at-initial-symex-p ()
  "Check if point is at the first symex in the buffer."
  ;; this is used in the primitive motions, so it cannot
  ;; be defined in terms of them, as the other predicates
  ;; above are
  (save-excursion
    (condition-case nil
        (or (bobp)
            (progn (backward-sexp 1)
                   (not (thing-at-point 'sexp))))
      (error nil))))

(defun symex-lisp--point-at-start-p ()
  "Check if point is at the start of a symex."
  (and (not (eolp))
       (not (looking-at-p symex--re-right))
       (or (symex-left-p)                           ; |(*
           (symex--special-left-p)                  ; |'(*
           ;; looking at the start of any non-whitespace:
           (and (not (looking-at-p "[[:space:]]"))
                (or (bolp)                          ; ^|.
                    (looking-back "[[:space:]]"     ; _|.
                                  (line-beginning-position))
                    (save-excursion (backward-char)
                                    (symex-lisp-string-p))
                    (looking-back symex--re-left    ; (*|.
                                  (line-beginning-position)))))))

(defun symex-lisp--point-at-end-p ()
  "Check if point is at the end of a symex."
  (condition-case nil
      (symex-right-p)
    (error nil)))

(defun symex-lisp--previous-p ()
  "Check if a preceding symex exists at this level."
  (save-excursion (symex-lisp--backward)))

(defun symex-lisp--next-p ()
  "Check if a succeeding symex exists at this level."
  (save-excursion (symex-lisp--forward)))

(defun symex-lisp--selected-p ()
  "Check if a symex is currently selected."
  (and (not (looking-at-p "[[:space:]]"))
       (not (symex-right-p))))

;;; User Interface

(defun symex-lisp--adjust-point ()
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
        (backward-sexp)
      (error nil))))

;;; Navigation

(defun symex-lisp-select-nearest ()
  "Select the appropriate symex nearest to point."
  (cond ((symex-left-p) nil)
        ((and (symex-right-p)
              (looking-back symex--re-left
                            (line-beginning-position))) ; (|)
         nil) ; don't change level in selection
        ((and (not (eobp))
              (symex-right-p)) ; |)
         (backward-sexp)
         (symex-lisp-select-nearest))
        ((condition-case nil  ; (thing-at-point string) raises error at EOB
             (thing-at-point 'string)
           (error nil))       ; "som|e string"
         (beginning-of-thing 'string))
        ((thing-at-point 'sexp) ; som|ething or even (something)|
         (beginning-of-thing 'sexp))
        (t (symex-lisp--if-stuck (symex-lisp--backward)
                                 (symex-lisp--forward)))))

(defun symex-lisp--get-starting-point ()
  "Get the point value at the start of the current symex."
  (save-excursion
    (unless (symex-lisp--point-at-start-p)
      (condition-case nil
          (backward-sexp)
        (error nil)))
    (point)))

(defun symex-lisp--get-end-point-helper (count)
  "Helper to get the point value after COUNT symexes.

If the containing expression terminates earlier than COUNT
symexes, returns the end point of the last one found.

Note that this mutates point - it should not be called directly."
  (if (= count 0)
      (point)
    (let ((at-end (condition-case nil
                      (progn (forward-sexp)
                             nil)
                    (error t))))
      (if at-end
          (error "Out of range!")
        (symex-lisp--get-end-point-helper (1- count))))))

(defun symex-lisp--get-end-point (count &optional include-whitespace)
  "Get the point value after COUNT symexes.

If the containing expression terminates earlier than COUNT
symexes, returns the end point of the last one found.

If INCLUDE-WHITESPACE is non-nil, this returns the end point
including trailing whitespace at the end of the last symex."
  (save-excursion
    (let ((endpoint (symex-lisp--get-end-point-helper count)))
      (if include-whitespace
          (progn (goto-char endpoint)
                 (if (and (not (eobp))
                          (symex-whitespace-p))
                     (1+ endpoint)
                   endpoint))
        endpoint))))

(defun symex-lisp--point-height-offset ()
  "Compute the height offset of the current symex.

This is measured from the lowest symex indicated by point.  For
symex-oriented languages like Lisp, this is always zero."
  0)

(defun symex-lisp--forward-one ()
  "Forward one symex."
  (let ((original-location (point))
        (result 0))
    (if (and (thing-at-point 'sexp)
             (not (or (eolp)
                      (looking-at-p "[[:space:]]"))))
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
        (setq result 0))
      (when (< current-location original-location)
        ;; happens in whitespace at end of buffer
        ;; where forward-sexp goes to eob
        ;; better to revise the above logic so that
        ;; point does not go backwards.
        (goto-char original-location)
        (setq result 0))
      (when (and (< original-location current-location)
                 (= result 0))
        ;; happens when point is in whitespace like
        ;; a |  b
        ;; and the result is
        ;; a   |b
        ;; but the logic above concludes there has been
        ;; no movement.
        ;; probably better to revise the above logic to
        ;; explicitly reason about the source text instead
        ;; of relying on implicit assumptions about the
        ;; behavior of forward-sexp - maybe use thing-at-point
        (setq result 1)))
    result))

(defun symex-lisp--forward (&optional count)
  "Forward symex.

Go forward COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-forward` instead)."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (_ count)
      (let ((res (symex-lisp--forward-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move result 0))))

(defun symex-lisp--backward-one ()
  "Backward one symex."
  (let ((result 0))
    (unless (symex-lisp--point-at-initial-symex-p)
      (condition-case nil
          (progn (backward-sexp 1)
                 (setq result (1+ result)))
        (error nil)))
    result))

(defun symex-lisp--backward (&optional count)
  "Backward symex.

Go backward COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-backward` instead)."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (_ count)
      (let ((res (symex-lisp--backward-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move (- result) 0))))

(defun symex--find-next ()
  "Find the next symex."
  (re-search-forward symex--re-symex-line)
  (back-to-indentation))

(defun symex-lisp--go-up-by-one ()
  "Go up one level."
  (let ((result 1))
    ;; TODO: this should enter at the primitive / command level
    ;; but perhaps not at the user level
    (cond ((or (symex-empty-list-p)
               (symex--special-empty-list-p))
           (forward-char (symex--form-offset)))
          ((symex-left-p) (forward-char))
          ;; note that at least some symex features would benefit by
          ;; treating these special cases as "symex-left-p" but it
          ;; would likely be too much special case handling to be
          ;; worth it to support those cases naively, without an AST
          ((or (symex--racket-syntax-object-p)
               (symex--splicing-unquote-p)
               (symex--racket-unquote-syntax-p))
           (forward-char 3))
          ((symex--racket-splicing-unsyntax-p)
           (forward-char 4))
          ((or (symex--quoted-list-p)
               (symex--unquoted-list-p)
               (symex--clojure-deref-reader-macro-p)
               (symex--clojure-literal-lambda-p))
           (forward-char 2))
          (t (setq result 0)))
    ;; find first non-whitespace character
    (symex--go-to-next-non-whitespace-char)
    result))

(defun symex-lisp--go-up (&optional count)
  "Go up to a higher level.

Go up COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-up` instead)."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (_ count)
      (let ((res (symex-lisp--go-up-by-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move 0 result))))

(defun symex-lisp--go-down-by-one ()
  "Go down one level."
  (condition-case nil
      (progn (paredit-backward-up 1)
             ;; one-off - better to recognize these as delimiters
             ;; at the AST level
             (cond ((looking-back "#['`,]" (line-beginning-position))
                    (backward-char 2))
                   ((looking-back "[#'`,]" (line-beginning-position))
                    (backward-char))
                   ((looking-back "#,@" (line-beginning-position))
                    (backward-char 3))
                   ((looking-back ",@" (line-beginning-position))
                    (backward-char 2))
                   ((looking-back "@" (line-beginning-position))
                    (backward-char)))
             1)
    (error 0)))

(defun symex-lisp--go-down (&optional count)
  "Go down to a lower level.

Go down COUNT times, defaulting to one.

This is a primitive operation that is provided below the public
abstraction level of symex.el.  It currently uses built-in Emacs
commands and third party tools like paredit to perform its function.
This procedure is not to be used except in the low-level internals
of symex mode (use the public `symex-go-down` instead)."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (_ count)
      (let ((res (symex-lisp--go-down-by-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move 0 (- result)))))

;;; Transformations

(defun symex--join-to-next ()
  "Join current position to the next symex, eliminating whitespace.

If there is an intervening comment line, then join only up to that
line."
  (let* ((start (point))
         (end (save-excursion (symex-lisp--forward)
                              (point)))
         (comment-line-position
          (symex--intervening-comment-line-p start end)))
    (if comment-line-position
        (delete-region start comment-line-position)
      (delete-region start end))))

(defun symex-lisp--reset-after-delete ()
  "Tidy after deletion and select the appropriate symex."
  (cond ((symex--current-line-empty-p) ; ^<>$
         ;; only join up to the next symex if the context suggests
         ;; that a line break is not desired
         (if (or (not (symex--next-line-empty-p))
                 (symex--previous-line-empty-p))
             (symex--join-to-next)
           ;; don't leave an empty line where the symex was
           (symex--delete-whole-line)))
        ((or (save-excursion (symex-last-non-blank) ; (<>$
                             (symex-left-p)))
         (symex--join-to-next))
        ((looking-at-p "\n")         ; (abc <>
         (unless (symex--next-line-empty-p)
           ;; only join up to the next symex if the context suggests
           ;; that a line break is not desired
           (symex--join-to-next)))
        ((symex-comment-line-p) ; <> ;
         (indent-according-to-mode))
        ((save-excursion (back-to-indentation) ; ^<>)
                         (symex-right-p))
         ;; Cases 2 and 3 in issue #18
         ;; if the deleted symex is preceded by a comment line
         ;; or if the preceding symex is followed by a comment
         ;; on the same line, then don't attempt to join lines
         (let ((original-position (point)))
           (when (symex-lisp--backward)
             (save-excursion
               (let ((previous-symex-end-pos (symex-lisp--get-end-point 1)))
                 (unless (symex--intervening-comment-line-p previous-symex-end-pos
                                                            original-position)
                   (goto-char previous-symex-end-pos)
                   ;; ensure that there isn't a comment on the
                   ;; preceding line before joining lines
                   (unless (condition-case nil
                               (save-excursion (re-search-forward ";" (line-end-position))
                                               t)
                             (error nil))
                     (symex--join-to-match symex--re-right))))))))
        ((or (symex-right-p)     ; abc <>)
             (looking-at-p " ")) ; (<> )
         (fixup-whitespace))
        (t nil)))

(defun symex-lisp--select-end (count &optional include-whitespace)
  "Select endpoint of COUNT symexes starting nearest to point.

See `symex--get-end-point' for more on INCLUDE-WHITESPACE and
INCLUDE-SEPARATOR."
  (goto-char (symex-lisp--get-end-point count include-whitespace))
  (point))

(defun symex-lisp--following-line-empty-p ()
  "Check if the line following the current expression is empty."
  (save-excursion
    (symex-lisp--select-end 1)
    (forward-line)
    (symex--current-line-empty-p)))

(defun symex-lisp--padding (&optional before)
  "Determine paste padding needed for current point position.

Padding is dependent on whether we are pasting BEFORE the current
symex or after it."
  (if (symex-inside-empty-form-p)
      ""
    (let* ((after (not before))
           (island
            (and (bolp)
                 (condition-case nil
                     (save-excursion (forward-sexp)
                                     (eolp))
                   (error nil))))
           (at-eob
            (condition-case nil
                (save-excursion (forward-sexp)
                                (eobp))
              (error nil)))
           (previous-line-empty
            (symex--previous-line-empty-p))
           (next-line-empty
            (symex-lisp--following-line-empty-p))
           (surrounding-lines-empty (and previous-line-empty
                                         next-line-empty))
           (paste-text-contains-newlines
            (string-match-p "\n" (symex--current-kill)))
           (at-eol (condition-case nil
                       (save-excursion (forward-sexp)
                                       (eolp))
                     (error nil)))
           (multiline (let ((original-line (line-number-at-pos)))
                        (condition-case nil
                            (save-excursion (forward-sexp)
                                            (not (= original-line
                                                    (line-number-at-pos))))
                          (error nil)))))
      (cond ((and island
                  ;; if we're at the toplevel, on an "island" symex
                  ;; (i.e. with no peers occupying the same lines),
                  (or (and after next-line-empty)
                      ;; and if the side we want to paste on already
                      ;; contains an empty line,
                      (and before previous-line-empty)
                      ;; or if we happen to be at the end of the buffer
                      ;; for pasting after, then check the opposite side
                      ;; instead for the clue on what's expected
                      (and at-eob previous-line-empty))
                  ;; and if the text to be pasted contains newlines, or
                  ;; if both surrounding lines are empty _and_ we aren't
                  ;; at the first symex
                  (or paste-text-contains-newlines
                      (and surrounding-lines-empty
                           (not (symex-lisp--point-at-first-symex-p)))))
             ;; then we typically want an extra newline separator
             "\n\n")
            ((or (symex--point-at-indentation-p)
                 at-eol
                 multiline)
             "\n")
            ((and before
                  (string-match-p (concat symex--re-whitespace "$")
                                  (symex--current-kill)))
             ;; if the text to be pasted before includes whitespace already,
             ;; then don't add more
             "")
            (t " ")))))

;;; Utilities

(defun symex-lisp-enter ()
  "Take necessary actions upon Symex mode entry in Lisp modes."
  (when (member major-mode (symex-get-lisp-modes))
    (symex--ensure-minor-mode)))

(defun symex-lisp-exit ()
  "Take necessary actions upon Symex mode exit.

There are no Lisp-specific actions to take, at the present time, so
this function does nothing.  It is only here for completeness and
symmetry with the tree-sitter primitives."
  nil)


(provide 'symex-primitives-lisp)
;;; symex-primitives-lisp.el ends here
