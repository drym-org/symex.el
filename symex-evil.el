;;; symex-evil.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; Evil-related things

;;; Code:

(require 'evil nil :no-error)
(require 'symex-motions)

(defun symex-evil-repeat-start-recording-advice (&rest _)
  "Prepare the current command for recording the repetition.

This function is meant to advise `evil-repeat-pre-hook' which
starts recording a repeation during the `pre-command-hook', but
only records a repition when in normal or visual state. This
calls `evil-repeat-start' if the buffer is currently in symex
state."
  (when evil-local-mode
    (let ((repeat-type (evil-repeat-type this-command nil)))
      ;; `evil-repeat-pre-hook' has several paths that it can take and only one
      ;; of them results in `evil-repeat-start' being called. We only need to
      ;; account for the conditions which would have started recording repeat
      ;; information if the buffer were in normal state. That is to say, we only
      ;; start recording if:
      ;;
      ;; 1. The current command has a `:repeat' property that is non-`nil' and
      ;; not set to force abort a repitition
      ;; 2. The current command is not a mouse event
      ;; 3. The buffer is currently in symex state
      (when (and repeat-type
                 (not (evil-repeat-force-abort-p repeat-type))
                 (not (evil-mouse-events-p (this-command-keys)))
                 symex-editing-mode)
        (evil-repeat-start)))))

(defun symex-evil-repeat-stop-recording-advice (&rest _)
  "Finish recording of repeat information for the current command.

This function is meant to advise `evil-repeat-post-hook' which
cleans up a recording during the `post-command-hook', but assumes
no recording was started unless the buffer is in normal or visual
state. This calls `evil-repeat-stop' if the buffer is currently
in symex state as well."
  (when (and evil-local-mode evil-recording-repeat)
    (let ((repeat-type (evil-repeat-type this-command t)))
      ;; We only need to call `evil-repeat-stop' if recording would have been
      ;; started by `symex-evil-repeat-start-recording-advice'. If recording was
      ;; started for any other reason, then it will already have been turned off
      ;; by `post-command-hook'. That is to say, we only stop recording if:
      ;;
      ;; 1. The current command has a `:repeat' property that is non-`nil' and
      ;; not set to force abort a repitition
      ;; 2. The current command is not a mouse event
      ;; 3. The buffer is currently in symex state
      (when (and repeat-type
                 (not (evil-repeat-force-abort-p repeat-type))
                 (not (evil-mouse-events-p (this-command-keys)))
                 symex-editing-mode)
        (evil-repeat-stop)))))

(defun symex-initialize-evil ()
  "Initialize evil interop.

This includes the repeat command (uses evil-repeat), and preserving
the evil state at symex after running evil commands which may attempt
to set the state to Normal."
  (advice-add 'evil-repeat-pre-hook
              :after #'symex-evil-repeat-start-recording-advice)
  (advice-add 'evil-repeat-post-hook
              :after #'symex-evil-repeat-stop-recording-advice)
  (when (fboundp 'undo-tree-undo)
    (advice-add #'undo-tree-undo :after #'symex-select-nearest-advice))
  (when (fboundp 'undo-tree-redo)
    (advice-add #'undo-tree-redo :after #'symex-select-nearest-advice)))

(defun symex-disable-evil ()
  "Disable evil interop."
  (advice-remove 'evil-repeat-pre-hook
                 #'symex-evil-repeat-start-recording-advice)
  (advice-remove 'evil-repeat-post-hook
                 #'symex-evil-repeat-stop-recording-advice)
  (when (fboundp 'undo-tree-undo)
    (advice-remove #'undo-tree-undo #'symex-select-nearest-advice))
  (when (fboundp 'undo-tree-redo)
    (advice-remove #'undo-tree-redo #'symex-select-nearest-advice)))


(provide 'symex-evil)
;;; symex-evil.el ends here
