;;; symex-evaluator.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6.1") (lispy "0.26.0") (paredit "24") (evil-cleverparens "20170718.413") (dash-functional "2.15.0") (evil "20180914.1216") (smartparens "20181007.1501") (racket-mode "20181030.1345") (geiser "0.10") (evil-surround "20180102.1401"))

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
;; An interpreter to execute symex traversals.
;;

;;; Code:

(require 'symex-data)
(require 'symex-primitives)
(require 'symex-computations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVALUATION AND EXECUTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-tree-move (move &optional computation)
  "Execute the specified MOVE at the current point location in the tree.

Evaluates to the actual move executed or nil if no move was executed.
Optional argument COMPUTATION currently unused."
  (let ((move-x (symex--move-x move))
        (move-y (symex--move-y move)))
    (cond ((> move-x 0)
           (symex-forward move-x))
          ((< move-x 0)
           (symex-backward (abs move-x)))
          ((> move-y 0)
           (symex-enter move-y))
          ((< move-y 0)
           (symex-exit (abs move-y))))))

(defun symex-execute-move (move &optional computation)
  "Execute the MOVE as a traversal.

This returns a list of moves (singleton, in this case) rather than the
executed move itself.  TODO: not sure this is needed anymore.
Optional argument COMPUTATION currently unused."
  (let ((executed-move (execute-tree-move move computation)))
    (when executed-move
      (list executed-move))))

(cl-defun symex-go-forward (&optional (count 1))
  "Move forward COUNT symexes."
  (interactive)
  (execute-tree-move (symex-make-move count 0)))

(cl-defun symex-go-backward (&optional (count 1))
  "Move backward COUNT symexes."
  (interactive)
  (execute-tree-move (symex-make-move (- count) 0)))

(cl-defun symex-go-in (&optional (count 1))
  "Move in COUNT symexes."
  (interactive)
  (execute-tree-move (symex-make-move 0 count)))

(cl-defun symex-go-out (&optional (count 1))
  "Move out COUNT symexes."
  (interactive)
  (execute-tree-move (symex-make-move 0 (- count))))

(defun symex--compute-results (a b computation)
  "Combine component computed results A and B into an aggregate result.

The aggregate result is constructed according to the specified COMPUTATION."
  ;; TODO: ruminate here
  ;; a and b should each have as many elements as the number of components
  ;; in the computation
  ;; later, a and b could be generators instead of lists of results
  (funcall (symex--computation-act computation)
           a
           b))

(defun symex-execute-maneuver (maneuver computation)
  "Attempt to execute a given MANEUVER.

Attempts the maneuver in the order of its phases, accepting partial completion
of phases.  If any phase fails entirely, then the maneuver it is part of is
terminated at that step.

Evaluates to a COMPUTATION on the maneuver actually executed."
  (let ((phases (symex--maneuver-phases maneuver)))
    (when phases
      (let ((current-phase (car phases))
            (remaining-phases (cdr phases)))
        (let ((executed-phase (symex-execute-traversal current-phase)))
          (when executed-phase
            (let ((executed-remaining-phases
                   (symex-execute-traversal (apply #'symex-make-maneuver
                                                   remaining-phases)
                                            computation)))
              (symex--compute-results executed-phase
                                      executed-remaining-phases
                                      computation))))))))

(defun symex-execute-circuit (circuit computation)
  "Execute a CIRCUIT.

This repeats some traversal as specified.

Evaluates to a COMPUTATION on the maneuver actually executed."
  (let ((traversal (symex--circuit-traversal circuit))
        (times (symex--circuit-times circuit)))
    (when (or (not times)  ; loop indefinitely
              (> times 0))
      (let ((result (symex-execute-traversal traversal
                                             computation)))
        (when result
          (let ((times (if times
                           (1- times)
                         times)))
            (let ((remaining-circuit
                   (symex-execute-traversal (symex-make-circuit traversal
                                                                times)
                                            computation)))
              (symex--compute-results result
                                      remaining-circuit
                                      computation))))))))

(defun symex-execute-detour (detour computation)
  "Execute the DETOUR.

Apply a reorientation and then attempt the traversal.

If the traversal fails, then the reorientation is attempted as many times as
necessary until either it succeeds, or the reorientation fails.

Evaluates to a COMPUTATION on the maneuver actually executed."
  (let ((reorientation (symex--detour-reorientation detour))
        (traversal (symex--detour-traversal detour)))
    (let ((executed-reorientation (symex-execute-traversal reorientation)))
      (when executed-reorientation
        (let ((path (symex-make-protocol traversal
                                         detour)))
          (let ((executed-path (symex-execute-traversal path
                                                        computation)))
            (when executed-path
              (symex--compute-results executed-reorientation
                                      executed-path
                                      computation))))))))

(defun symex-execute-precaution (precaution computation)
  "Attempt to execute a given PRECAUTION.

The traversal is only executed if PRE-CONDITION holds, and is reversed if
POST-CONDITION does not hold after the provisional execution of the traversal.

Evaluates to a COMPUTATION on the maneuver actually executed."
  (let ((traversal (symex--precaution-traversal precaution))
        (pre-condition (symex--precaution-pre-condition precaution))
        (post-condition (symex--precaution-post-condition precaution)))
    (when (funcall pre-condition)
      (let ((executed-traversal (symex-execute-traversal traversal
                                                         computation)))
        (when (funcall post-condition)
          executed-traversal)))))

(defun symex-execute-protocol (protocol computation)
  "Attempt to execute a given PROTOCOL.

Given a protocol including a set of options, attempt to execute them
in order until one succeeds.

Evaluates to a COMPUTATION on the maneuver actually executed."
  (let ((options (symex--protocol-options protocol)))
    (when options
      (let ((option (car options))
            (remaining-options (cdr options)))
        (let ((executed-option (symex-execute-traversal option
                                                        computation)))
          (if executed-option
              executed-option
            (symex-execute-traversal (apply #'symex-make-protocol
                                            remaining-options)
                                     computation)))))))

(defun symex-execute-traversal (traversal &optional computation)
  "Execute a tree TRAVERSAL.

Evaluates to a COMPUTATION on the traversal actually executed."
  (let ((computation (if computation
                         computation
                       computation-default)))
    (let ((original-location (point))
          (executed-traversal (cond ((is-maneuver? traversal)
                                     (symex-execute-maneuver traversal
                                                             computation))
                                    ((is-circuit? traversal)
                                     (symex-execute-circuit traversal
                                                            computation))
                                    ((is-protocol? traversal)
                                     (symex-execute-protocol traversal
                                                             computation))
                                    ((is-precaution? traversal)
                                     (symex-execute-precaution traversal
                                                               computation))
                                    ((is-detour? traversal)
                                     (symex-execute-detour traversal
                                                           computation))
                                    ((is-move? traversal)
                                     (symex-execute-move traversal
                                                         computation))
                                    (t (error "Syntax error: unrecognized traversal type!")))))
      (let ((result (funcall (symex--computation-perceive computation)
                             executed-traversal)))
        (unless result
          (goto-char original-location))
        result))))


(provide 'symex-evaluator)
;;; symex-evaluator.el ends here
