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

(defun symex-repeat-parser-start (key-seq)
  "Start parsing."
  (member key-seq
          symex-repeatable-keys))

(defun symex-repeat-parser-stop (_key-seq state)
  "Stop (accept) parsing."
  (let ((last-entry (aref state (- (length state)
                                   1))))
    (and symex-editing-mode
         (not (symex--key-number-p last-entry)))))

(defun symex-repeat-parser-abort (key-seq _state)
  "Abort parsing."
  (if (or (not (fboundp #'rigpa-current-mode))
          (not (rigpa-current-mode)))
      nil
    (let ((abort (not (member (chimera-mode-name (rigpa-current-mode))
                              '("symex" "insert" "emacs")))))
      (when (and abort (not symex-editing-mode))
        (symex-repeat-disable))
      abort)))

(defvar symex-repeat-parser
  (mantra-make-parser
   "symex-repeat-parser"
   #'symex-repeat-parser-start
   #'symex-repeat-parser-stop
   #'symex-repeat-parser-abort)
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
  ;; TODO: verify infinite loop without this
  ;; TODO: do repeat-pop and repeat-recent need it too?
  (mantra-parser-clear-state symex-repeat-parser)
  (symex--with-undo-collapse
    (dotimes (_ count)
      (repeat-ring-repeat symex-repeat-ring))))

(defun symex-repeat-pop ()
  "Cycle through previous repetitions."
  (interactive)
  (repeat-ring-repeat-pop symex-repeat-ring))

(defun symex-repeat-recent ()
  "Repeat a recent key sequence entered while in Symex mode."
  (interactive)
  (repeat-ring-repeat-recent symex-repeat-ring))

(defvar symex--current-keys nil
  "The current key sequence.")

(defun symex-repeat-initialize ()
  "Do any necessary setup for repeat functionality.

This simply subscribes to and maintains pre-command key sequences in
order to determine if symex exits need to suspend the repeat parser or
(if we are exiting as part of a repeatable command) keep it going."
  (pubsub-subscribe "mantra-key-sequences-pre-command"
                    "symex--current-keys"
                    (lambda (key-seq)
                      (setq symex--current-keys key-seq))))

(defun symex-repeat-teardown ()
  "Do any necessary teardown for repeat functionality."
  (pubsub-unsubscribe "mantra-key-sequences-pre-command"
                      "symex--current-keys"))

(defun symex-repeat-enable ()
  "Enable parsing for repeat."
  ;; Parse keyboard events for symex commands
  (mantra-subscribe "mantra-key-sequences"
                    symex-repeat-parser)
  ;; Subscribe the symex repeat ring to symex key sequences entered
  ;; in the main Emacs command loop
  (repeat-ring-subscribe symex-repeat-ring
                         (mantra-parser-name symex-repeat-parser)))

(defun symex-repeat-disable ()
  "Disable parsing for repeat."
  (mantra-unsubscribe "mantra-key-sequences"
                      symex-repeat-parser)
  (repeat-ring-unsubscribe symex-repeat-ring
                           (mantra-parser-name symex-repeat-parser)))

(provide 'symex-repeat)
;;; symex-repeat.el ends here
