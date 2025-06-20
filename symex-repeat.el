;;; symex-repeat.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Repeat symex commands

;;; Code:

(require 'edmacro)
(require 'pubsub)
(require 'mantra)
(require 'repeat-ring)

(require 'symex-utils)

;; defined in symex-lithium
;; but declaring it as it is used here
(defvar symex-editing-mode)

(defmacro symex--kbd-macro-list (&rest keys)
  "Produce a list of key sequence vectors from KEYS."
  (declare (indent 0))
  (let ((key-vectors (seq-map (lambda (key)
                                `(read-kbd-macro ,key :need-vector))
                              keys)))
    `(list ,@key-vectors)))

(defvar symex-repeatable-keys
  (symex--kbd-macro-list
    "0"
    "1"
    "2"
    "3"
    "4"
    "5"
    "6"
    "7"
    "8"
    "9"
    "("
    "["
    ")"
    "]"
    "C-'"
    "C-,"
    "`"
    "C-`"
    "p"
    "P"
    "x"
    "X"
    "D"
    "C--"
    "S"
    "H"
    "L"
    "M-H"
    "M-L"
    "K"
    "C-S-j"
    "C-("
    "C-S-h"
    "C-{"
    "C-S-l"
    "C-}"
    "C-S-k"
    "C-)"
    "z"
    "Z"
    "|"
    "&"
    "-"
    ">"
    "<"
    "C->"
    "C-<"
    "C-S-o"
    "J"
    "M-J"
    "M-<"
    "M->"
    "C-M-<"
    "C-M->"
    "="
    "<tab>"
    "C-="
    "C-<tab>"
    "M-="
    "M-<tab>"
    ";"
    "M-;"
    "c"
    "C"
    "s"
    "o"
    "O"
    "A"
    "a"
    "i"
    "I"
    "w"
    "W")
  "Key sequences in Symex (Lithium) mode that are repeatable.")

(defconst symex--ascii-numeral-0 48
  "The ASCII code for 0.")

(defconst symex--ascii-numeral-9 57
  "The ASCII code for 9.")

(defun symex--key-number-p (code)
  "Whether CODE encodes a number."
  (and (numberp code)
       (<= symex--ascii-numeral-0
           code
           symex--ascii-numeral-9)))

(defvar symex--change-series
  nil
  "Changes associated with current command.")

(defun symex--push-change (elt)
  "Push ELT into `symex--change-series' at the end, modifying it in place."
  ;; appending rather than consing is inefficient in general, but as
  ;; the change series is cleared after each key sequence is entered,
  ;; and as each may result in either no buffer changes, an insertion,
  ;; a deletion, or both, we expect the length of the change series to
  ;; be small enough (0-2) to be treated as constant.
  (setq symex--change-series
        (append symex--change-series
                (list elt))))

(defun symex--clear-change-series ()
  "Clear the change series buffer."
  (setq symex--change-series nil))

(defvar symex--initial-buffer nil
  "Initial buffer when recording changes.")

(defvar symex--initial-point nil
  "Initial point position when recording changes.")

(defvar symex--current-keys nil
  "The current key sequence.

This is set at the pre-command stage to serve as context throughout
the duration of the command. In particular, it's used to determine
whether repeat parsing should be disabled upon exiting Symex mode, or
not. We want to continue parsing if we happen to exit as part of a
repeatable command.")

(defun symex-changes-listener (start end length)
  "Listen for buffer content changes and store them in the change buffer.

Store the changes in the order they occur, oldest first."
  (when (and (mantra-parsing-in-progress-p symex-repeat-parser)
             (eq symex--initial-buffer
                 (current-buffer)))
    (symex--push-change
     (list start end length))))

(defun symex-insertion-p (change)
  "Is CHANGE an insertion?"
  (let ((start (car change))
        (end (cadr change))
        (len (caddr change)))
    (zerop len)))

(defun symex-deletion-p (change)
  "Is CHANGE a deletion?"
  (let ((start (car change))
        (end (cadr change))
        (len (caddr change)))
    (and (not (zerop len))
         (= start end))))

(defun symex-parse-insertion (change)
  "Parse CHANGE as an insertion."
  (let ((start (car change))
        (end (cadr change))
        (len (caddr change)))
    (list 'insertion (buffer-substring start end))))

(defun symex-parse-deletion (change)
  "Parse CHANGE as a deletion."
  (let* ((original-position symex--initial-point)
         (start (car change))
         (change-start (- start original-position)) ; relative to point
         (len (caddr change)))
    (list 'deletion change-start len)))

(defun symex-parse-change (change)
  "Parse a CHANGE."
  (cond ((symex-insertion-p change)
         (symex-parse-insertion change))
        ((symex-deletion-p change)
         (symex-parse-deletion change))
        (t nil)))

(defun symex-clear-parsing-state ()
  "Clear parsing state."
  (setq symex--initial-buffer nil)
  (setq symex--initial-point nil))

(defun symex-repeat-parser-start (key-seq)
  "Start parsing."
  (and (member key-seq
               symex-repeatable-keys)
       ;; note that this check isn't enough on its own to prevent a
       ;; repetition from being parsed afresh, as repetition could be
       ;; initiated once parsing is _already_ in progress, for
       ;; instance, following the specification of a count argument.
       ;; So it is also necessary, in addition, to abort parsing
       ;; before initiating the repetition (see `symex-repeat' for
       ;; where that's done).
       (not (repeat-ring-repeating symex-repeat-ring))))

(defun symex--seq-number-p (entry)
  "Is ENTRY a number?

It is expected to be a mantra seq."
  (or (not (vectorp entry))
      (not (symex--key-number-p entry))))

(defun symex-repeat-parser-stop (_key-seq state)
  "Stop (accept) parsing."
  (message "STATE IS %s" state)
  ;; (message "STOP called")
  (symex--clear-change-series)
  (let* ((last-entry (car state))) ; note state is in reverse order
    (let ((accept (and symex-editing-mode
                       (symex--seq-number-p last-entry))))
      (when accept
        (symex-clear-parsing-state))
      accept)))

(defun symex-repeat-parser-abort (key-seq _state)
  "Abort parsing."
  (if (or (not (fboundp #'rigpa-current-mode))
          (not (rigpa-current-mode)))
      nil
    (let ((abort (not (member (chimera-mode-name (rigpa-current-mode))
                              '("symex" "insert" "emacs")))))
      (when abort
        (unless symex-editing-mode
          (symex-repeat-disable))
        (symex-clear-parsing-state))
      abort)))

(defun symex-repeat-parser-map (key-seq)
  "Parse each KEY-SEQ for repeat.

If the action did not leave Symex mode, then parse it purely as a key
sequence.

If it left Symex mode and did not result in any insertions, then, too,
parse it as a key sequence. Otherwise, if there are any insertions
associated with the key sequence, then parse it as the series of
buffer changes (which may include both insertions as well as
deletions)."
  ;; assumes
  ;;  change-series :=   (change)
  ;;                   | (deletion insertion)
  ;;  change := deletion | insertion | neither
  (if (and (boundp 'symex-editing-mode) symex-editing-mode)
      key-seq
    (cond ((and (null symex--change-series)
                (boundp 'company-my-keymap) company-my-keymap)
           mantra--null)
          ((null symex--change-series) key-seq)
          ((null (cdr symex--change-series))
           (let ((result (symex-parse-change (car symex--change-series))))
             (if (or (null result)
                     (mantra-deletion-p result))
                 key-seq
               result)))
          (t (list 'seq
                   (seq-map #'symex-parse-change
                            symex--change-series))))))

(defun symex-repeat-parser-compose (state input)
  "Incorporate INPUT into STATE.

We simply `cons' the input onto the `state' here, as that is
efficient. However, it produces the sequence in the reverse order, and
so it must eventually be reversed before being incorporated into a
mantra."
  (message "inp %s state %s" input state)
  (cons input state))

(defun symex-repeat-parser-finish (state)
  "Finish STATE before publishing it as the result of parsing.

Parse the list of mantras as a seq."
  (let ((result (apply #'mantra-make-seq (nreverse state))))
    (message "publishing %s" result)
    result))

(defvar symex-repeat-parser
  (mantra-make-parser
   "symex-repeat-parser"
   #'symex-repeat-parser-start
   #'symex-repeat-parser-stop
   #'symex-repeat-parser-abort
   #'symex-repeat-parser-map
   #'symex-repeat-parser-compose
   (mantra-initial-value nil)
   #'symex-repeat-parser-finish)
  "Parser for symex key sequences.")

(defvar symex-repeat-ring
  ;; use unique names across all clients of the pub/sub
  ;; system since that is a single namespace. In particular,
  ;; we can't call both the repeat ring as well as the repeat
  ;; parser "symex", since one needs to subscribe to the other
  ;; and they should be distinguishable on the pub/sub side
  (repeat-ring-make "symex-repeat-ring" 4) ; TESTING
  "Repeat ring for use in Symex (Lithium) mode.")

(defun symex-repeat (count)
  "Repeat the last key sequence entered while in Symex mode."
  (interactive "p")
  ;; since repeat is "self-referential" in a way,
  ;; we cannot abort it in the usual way for the parser
  ;; when doing something like `2 .`
  ;; so we explicitly abort any in-progress parsing before
  ;; executing the repetition.
  (mantra-parser-clear-state symex-repeat-parser)
  (symex--with-undo-collapse
    (dotimes (_ count)
      (repeat-ring-repeat symex-repeat-ring))))

(defun symex-repeat-pop ()
  "Cycle through previous repetitions."
  (interactive)
  (mantra-parser-clear-state symex-repeat-parser)
  (repeat-ring-repeat-pop symex-repeat-ring))

(defun symex-repeat-recent ()
  "Repeat a recent key sequence entered while in Symex mode."
  (interactive)
  (mantra-parser-clear-state symex-repeat-parser)
  (repeat-ring-repeat-recent symex-repeat-ring))

(defun symex-set-pre-command-state (key-seq)
  "Set the pre-command buffer and point position."
  (setq symex--initial-buffer (current-buffer))
  (setq symex--initial-point (point))
  (setq symex--current-keys key-seq))

(defun symex-repeat-initialize ()
  "Do any necessary setup for repeat functionality.

This simply subscribes to and maintains pre-command key sequences in
order to determine if symex exits need to suspend the repeat parser or
(if we are exiting as part of a repeatable command) keep it going."
  (pubsub-subscribe "mantra-key-sequences-pre-command"
                    "symex-set-pre-command-state"
                    #'symex-set-pre-command-state))

(defun symex-repeat-teardown ()
  "Do any necessary teardown for repeat functionality."
  (pubsub-unsubscribe "mantra-key-sequences-pre-command"
                      "symex-set-pre-command-state"))

(defun symex-repeat-enable ()
  "Enable parsing for repeat."
  ;; Subscribe to symex key sequences entered
  ;; on the main Emacs command loop
  (mantra-subscribe "mantra-key-sequences"
                    symex-repeat-parser)
  ;; Subscribe the symex repeat ring to the parsed sequence of Symex
  ;; activity
  (repeat-ring-subscribe symex-repeat-ring
                         (mantra-parser-name symex-repeat-parser))
  ;; Listen for buffer changes performed as part
  ;; of command execution.
  (add-hook 'after-change-functions
            #'symex-changes-listener))

(defun symex-repeat-disable ()
  "Disable parsing for repeat."
  (mantra-unsubscribe "mantra-key-sequences"
                      symex-repeat-parser)
  (repeat-ring-unsubscribe symex-repeat-ring
                           (mantra-parser-name symex-repeat-parser))
  (remove-hook 'after-change-functions
               #'symex-changes-listener))

(provide 'symex-repeat)
;;; symex-repeat.el ends here
