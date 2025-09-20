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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Parsing context  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar symex-repeat--recorded-length
  0
  "Length of the recorded activity.")

(defconst symex-repeat--max-recording-length
  300
  "Maximum length of changes to track for repetition.

If exceeded, parsing will be aborted.

Tracking changes is efficient, so there's no real harm in tracking
even, perhaps, tens of thousands of changes.  But it doesn't seem
useful to track long changes for repetition.  We could use an explicit
keyboard macro if we really mean to do that, or simply copy and paste
a newly entered (substantial) block of text.  So, just for good
measure, abort parsing if the sequence gets long, as it probably isn't
intended for repetition, anyway, and at the same time, is a failsafe
against any pathological bugs where we end up parsing indefinitely for
some reason.")

(defvar symex--initial-buffer nil
  "Initial buffer when recording changes.")

(defvar symex--initial-point nil
  "Initial point position when recording changes.")

(defvar symex--initial-mode-was-symex nil
  "True if the initial mode was Symex editing mode.")

(defvar symex--replaying-point nil
  "Dynamic point position simulating replay of changes.

This is necessary as the point position sets the context of insertion.
As captured insertions may move point, we need to keep track of the
simulated position of point to properly situate capture of subsequent
insertions.")

(defvar symex--current-keys nil
  "The current key sequence.

This is set at the pre-command stage to serve as context throughout
the duration of the command.  In particular, it's used to determine
whether repeat parsing should be disabled upon exiting Symex mode, or
not.  We want to continue parsing if we happen to exit as part of a
repeatable command.")

(defun symex-clear-parsing-context ()
  "Clear state variables that form the context of parsing."
  (setq symex--initial-buffer nil)
  (setq symex--initial-point nil)
  (setq symex--initial-mode-was-symex nil)
  (setq symex--replaying-point nil)
  (setq symex-repeat--recorded-length 0))

(defun symex-set-parsing-context (key-seq)
  "Set some state variables that form the context of parsing.

This sets the buffer and point position as well as the currently
entered key sequence, KEY-SEQ, that is initiating parsing.

It is expected to be called at the pre-command stage, i.e., prior to
the command taking effect."
  (setq symex--initial-buffer (current-buffer))
  (setq symex--initial-point (point))
  (setq symex--initial-mode-was-symex symex-editing-mode)
  (setq symex--replaying-point (point))
  (setq symex--current-keys key-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Recording and parsing the series of buffer changes  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar symex--change-series
  nil
  "Changes associated with current command.")

(defun symex--clear-change-series ()
  "Clear the change series buffer."
  (setq symex--change-series nil))

(defun symex-insertion-p (change)
  "Is CHANGE an insertion?"
  (pcase-let ((`(,_start ,_end ,len) change))
    (zerop len)))

(defun symex-deletion-p (change)
  "Is CHANGE a deletion?"
  (pcase-let ((`(,start ,end ,len) change))
    (and (not (zerop len))
         (= start end))))

(defun symex-parse-insertion (change)
  "Parse CHANGE as an insertion.

To do this, two pieces of information need to be extracted from the
CHANGE within the context of parsing:

1. The offset of the inserted text from the original point position.
2. The motion of point in relation to the original point position.

In the most common case, point moves precisely to the end of the
inserted text, but sometimes, point may not move at all (e.g., for
Symex's \"I\" which inserts a space but preserves point), or may move
to a different location (e.g., paredit's implicit insertion of a
closing delimiter, while moving point to the end of the opening
delimiter).

In repeating such an insertion, we want to repeat both the text
insertion in relation to point as well as any point motion."
  (pcase-let ((`(,start ,end ,_len) change))
    (let ((offset (- start symex--replaying-point))
          (point-offset (- (point) symex--replaying-point)))
      (let ((insertion  (mantra-make-insertion (buffer-substring start end)
                                               offset
                                               ;; we handle the insertion separately
                                               ;; from the point motion, so we preserve
                                               ;; point as part of insertion
                                               nil))
            (motion (mantra-make-move point-offset)))
        (setq symex--replaying-point (+ point-offset symex--replaying-point))
        (mantra-make-seq insertion motion)))))

(defun symex-parse-deletion (change)
  "Parse CHANGE as a deletion."
  (pcase-let ((`(,start ,_end ,len) change))
    ;; relative to point
    (let* ((original-position symex--initial-point)
           (change-start (- start original-position)))
      (mantra-make-deletion change-start len))))

(defun symex-parse-change (change)
  "Parse a CHANGE."
  (cond ((symex-insertion-p change)
         (symex-parse-insertion change))
        ((symex-deletion-p change)
         (symex-parse-deletion change))
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Specification of the (mantra) repeat parser for Symex  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-repeat-parser-start (key-seq)
  "Whether to start parsing.

KEY-SEQ is the currently-entered key sequence on the Emacs command
loop."
  (and (member key-seq
               symex-repeatable-keys)
       ;; note that this check isn't enough on its own to prevent a
       ;; repetition from being parsed afresh, as repetition could be
       ;; initiated once parsing is _already_ in progress, for
       ;; instance, following the specification of a count argument.
       ;; So it is also necessary, in addition, to abort parsing
       ;; before initiating the repetition (see `symex-repeat' for
       ;; where that's done).
       ;; TODO: reference to `symex-repeat-ring' before definition
       (not (repeat-ring-repeating symex-repeat-ring))))

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

(defun symex--seq-number-p (entry)
  "Is ENTRY a number?

It is expected to be a mantra seq."
  (or (not (vectorp entry))
      (not (symex--key-number-p entry))))

(defun symex-repeat-parser-stop (_key-seq state)
  "Whether to stop (accept) parsing.

STATE is the accumulated parsed state.  Not to be confused with the
\"parsing state\" referred to in the body of the function, which is
metadata used in parsing, but isn't what's actually parsed."
  (symex--clear-change-series)
  (let* ((last-entry (car state))) ; note state is in reverse order
    (let ((accept (and symex-editing-mode
                       (symex--seq-number-p last-entry))))
      (when accept
        (symex-clear-parsing-context))
      accept)))

(defun symex-repeat-parser-abort (_key-seq _state)
  "Abort parsing.

KEY-SEQ is the currently entered key sequence."
  (let ((abort (or (not (eq symex--initial-buffer
                            (current-buffer)))
                   (> symex-repeat--recorded-length
                      symex-repeat--max-recording-length))))
    (when abort
      (unless symex-editing-mode
        (symex-repeat-disable))
      (symex-clear-parsing-context))
    abort))

(defun symex-repeat--noop ()
  "Did the current key sequence have no effect in the source buffer?"
  (and (= (point)
          symex--initial-point)
       (null symex--change-series)))

(defun symex--initiating-key-p (key-seq)
  "Did the current key sequence initiate repeat parsing?"
  (and (member key-seq symex-repeatable-keys)
       symex--initial-mode-was-symex))

(defvar symex-insertion-key-sequences
  (symex--kbd-macro-list
    "("
    "["
    "\"")
  "Key sequences used during insertion that should be parsed as key sequences.

Typically, all changes made when in an insertion state are parsed as
buffer changes, i.e., as insertions and deletions. But in some cases
(such as \"(\" and \"[\"), the keys result in structural changes
(often managed by tools like paredit), whose precise effect is
context-dependent. Capturing the exact insertions and deletions in
such cases would not produce the right effect when the same command is
repeated in a different context, such as in an expression with lower
or higher indentation. We could parse such keys either as the commands
themselves, or as key sequences, rather than as buffer changes. We opt
for the latter for simplicity.")

(defun symex--insertion-key-sequence-p (key-seq)
  "Should the current key sequence be parsed as a key sequence?

This includes keys that introduce structure, such as parentheses,
brackets, etc., which may have context-sensitive side effects such as
indenting affected expressions, and thus should not be parsed as
buffer changes, which are not context-sensitive."
  (member key-seq symex-insertion-key-sequences))

(defun symex-repeat-parser-map (key-seq)
  "Parse each KEY-SEQ for repeat.

If the action did not leave Symex mode, then parse it purely as a key
sequence.

If it left Symex mode and did not result in any insertions, then, too,
parse it as a key sequence.  Otherwise, if there are any insertions
associated with the key sequence, then parse it as the series of
buffer changes (which may include both insertions as well as
deletions).

This function assumes:
    change-series :=   (change)
                     | (deletion insertion)
    change := deletion | insertion | neither"
  ;; first, as `map' is called on every input that is to be
  ;; incorporated into the parsed state, as a side-effect, increment
  ;; the recorded length counter here. This allows us to efficiently
  ;; track the length of the recorded sequence, for the purposes of
  ;; aborting if it gets too long.
  (setq symex-repeat--recorded-length
        (1+ symex-repeat--recorded-length))
  (if symex-editing-mode
      key-seq
    (cond ((symex-repeat--noop) mantra--null)
          ((symex--initiating-key-p key-seq) key-seq)
          ((null symex--change-series) key-seq)
          ((symex--insertion-key-sequence-p key-seq) key-seq)
          ((null (cdr symex--change-series))
           (let ((result (symex-parse-change (car symex--change-series))))
             (if (or (null result)  ; perhaps a motion, e.g., C-f
                     (mantra-deletion-p result))
                 key-seq
               ;; otherwise it's an insertion
               result)))
          (t (apply #'mantra-make-seq
                    (seq-map #'symex-parse-change
                             symex--change-series))))))

(defun symex-repeat-parser-compose (state input)
  "Incorporate INPUT into STATE.

We simply `cons' the input onto the `state' here, as that is
efficient.  However, it produces the sequence in the reverse order,
and so it must eventually be reversed before being incorporated into a
mantra."
  (cons input state))

(defun symex-repeat-parser-finish (state)
  "Finish STATE before publishing it as the result of parsing.

Parse the list of mantras as a seq."
  (let ((result (apply #'mantra-make-seq (nreverse state))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  User-facing repeat features, powered by repeat-ring  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar symex-repeat-ring
  ;; use unique names across all clients of the pub/sub
  ;; system since that is a single namespace. In particular,
  ;; we can't call both the repeat ring as well as the repeat
  ;; parser "symex", since one needs to subscribe to the other
  ;; and they should be distinguishable on the pub/sub side
  (repeat-ring-make "symex-repeat-ring")
  "Repeat ring for use in Symex (Lithium) mode.")

(defun symex-repeat (count)
  "Repeat the last action performed while in Symex mode.

And do it COUNT times."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Enable/disable repeat in connection with the modal UI  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun symex-record-buffer-change (start end length)
  "Listen for buffer content changes and store them in the change buffer.

Store the changes in the order they occur, oldest first.

See `after-change-functions' for more on START, END, and LENGTH."
  (when (and (or (mantra-parsing-in-progress-p symex-repeat-parser)
                 ;; either parsing is already in progress, or the current
                 ;; key sequence is about to start parsing
                 (and symex-editing-mode
                      (member symex--current-keys symex-repeatable-keys)))
             (eq symex--initial-buffer
                 (current-buffer)))
    (symex--push-change
     (list start end length))))

(defun symex-repeat-enable ()
  "Enable parsing for repeat.

This should be called each time the Symex modal UI is entered."
  ;; Subscribe to symex key sequences entered
  ;; on the main Emacs command loop
  (mantra-subscribe mantra-key-sequences-post-command-topic
                    symex-repeat-parser)
  ;; Subscribe the symex repeat ring to the parsed sequence of Symex
  ;; activity
  (repeat-ring-subscribe symex-repeat-ring
                         (mantra-parser-name symex-repeat-parser))
  ;; Listen for buffer changes performed as part
  ;; of command execution.
  (add-hook 'after-change-functions
            #'symex-record-buffer-change))

(defun symex-repeat-disable ()
  "Disable parsing for repeat.

This should be called each time the Symex modal UI is exited, unless
it is exited via an insertion command that needs to continue parsing
while outside Symex mode."
  (mantra-unsubscribe mantra-key-sequences-post-command-topic
                      symex-repeat-parser)
  (repeat-ring-unsubscribe symex-repeat-ring
                           (mantra-parser-name symex-repeat-parser))
  (remove-hook 'after-change-functions
               #'symex-record-buffer-change))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  One-time configuration  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-repeat-initialize ()
  "Do any necessary setup for repeat functionality.

This simply subscribes to and maintains pre-command key sequences in
order to determine if symex exits need to suspend the repeat parser or
\(if we are exiting as part of a repeatable command) keep it going.

This should be called just once, to set up using Symex mode. It isn't
relevant for routine entry and exit from the Symex modal UI."
  (mantra-connect)
  (pubsub-subscribe mantra-key-sequences-pre-command-topic
                    "symex-set-parsing-context"
                    #'symex-set-parsing-context))

(defun symex-repeat-teardown ()
  "Do any necessary teardown for repeat functionality.

This reverts any one-time configuration changes that were made in
setting up Symex mode. It should be called, if at all, at most once,
and isn't part of routine entry into and exit from the modal UI."
  (pubsub-unsubscribe mantra-key-sequences-pre-command-topic
                      "symex-set-parsing-context"))

(provide 'symex-repeat)
;;; symex-repeat.el ends here
