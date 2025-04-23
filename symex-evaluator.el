;;; symex-evaluator.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

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

;; An interpreter to execute symex traversals.

;;; Code:

(require 'cl-lib)

(require 'symex-data)
(require 'symex-primitives)
(require 'symex-computations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVALUATION AND EXECUTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symex--execute-tree-move (move)
  "Execute the specified MOVE at the current point location in the tree.

Evaluates to the actual move executed or nil if no move was executed.

This interface is an \"abstraction barrier\" to keep the details of the
elementary navigation of symexes as a black box.  The implementation
of these elementary operations could be changed in the future (e.g. to
incorporate an explicit AST representation for greater precision) without
requiring changes to higher-level code that uses the present interface."
  (let ((move-x (symex--move-x move))
        (move-y (symex--move-y move)))
    (cond ((> move-x 0)
           (symex--go-forward move-x))
          ((< move-x 0)
           (symex--go-backward (abs move-x)))
          ((> move-y 0)
           (symex--go-up move-y))
          ((< move-y 0)
           (symex--go-down (abs move-y)))
          (t symex--move-zero))))

(defun symex-eval-move (move &optional computation result)
  "Execute the MOVE as a traversal.

Executing a move is the main base case of traversal execution, and
this is where the overall computation is stitched together in terms of
the executed move and the accumulated result.

To do this, the executed move is first \"perceived\" (i.e., mapped),
and then \"synthesized\" (i.e., combined) with the RESULT so far, as
specified in the COMPUTATION being performed on the traversal.

When COMPUTATION is left unspecified, `symex--computation-default' is
used, guided by which, this function returns a list of moves made.
Note that, in particular, if RESULT happens to be empty and if a move
is made, then this doesn't return the executed move itself but rather,
a singleton list containing the move."
  (let ((executed-move (symex--execute-tree-move move))
        (result (or result
                    (funcall (symex--computation-perceive computation)
                             symex--move-zero))))
    (when executed-move
      (symex-compose-computation result
                                 (funcall (symex--computation-perceive computation)
                                          executed-move)
                                 computation))))

(defun symex-eval-maneuver (maneuver computation result)
  "Execute a MANEUVER.

Attempts the maneuver in the order of its phases.  The maneuver
succeeds only if all of the phases succeed, and otherwise fails.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (if (symex--maneuver-null-p maneuver)
      result
    (let ((current-phase (symex--maneuver-first maneuver))
          (remaining-maneuver (symex--maneuver-rest maneuver)))
      (let ((executed-phase (symex-eval current-phase
                                        computation
                                        result)))
        (when executed-phase
          (symex-eval-maneuver remaining-maneuver
                               computation
                               executed-phase))))))

(defun symex-eval-venture (venture computation result)
  "Execute a VENTURE.

Similar to maneuver execution, except that it accepts partial
completion.  That is, this attempts the venture in the order of its
phases.  If any phase fails, then the venture is terminated at that
step.  In general, the venture succeeds if at least one phase
succeeds, and otherwise fails.  In the special case where nothing is
ventured, then even though nothing is gained, it is considered to have
succeeded.  In particular, venturing nothing is a base case in venture
evaluation.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (if (symex--venture-null-p venture)
      result
    (let ((current-phase (symex--venture-first venture))
          (remaining-venture (symex--venture-rest venture)))
      (let ((executed-phase (symex-eval current-phase
                                        computation
                                        result)))
        (when executed-phase
          (let ((accumulated-result
                 (symex-eval-venture remaining-venture
                                     computation
                                     executed-phase)))
            (if accumulated-result
                accumulated-result
              executed-phase)))))))

(defun symex-eval-circuit (circuit computation result)
  "Execute a CIRCUIT.

This repeats some traversal as specified.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (if (symex--circuit-null-p circuit)
      result
    (let ((traversal (symex--circuit-traversal circuit))
          (times (symex--circuit-times circuit))
          (remaining-circuit (symex--circuit-rest circuit)))
      (let ((executed-phase (symex-eval traversal
                                        computation
                                        result)))
        (if executed-phase
            (symex-eval-circuit remaining-circuit
                                computation
                                executed-phase)
          (when (not times)
            ;; if looping indefinitely, then count 0
            ;; times executed as success
            result))))))

;; TODO: we now pass the computation result to all predicates used
;; with Symex, i.e. in precautions and decisions. This *almost* allows
;; us to merge `loop` functionality into `circuit`, by implementing it
;; as a circuit repeating a precaution on an underlying traversal. But
;; it's a bit awkward since we can't distinguish the initial movement
;; ("move at least once before checking the condition") in, e.g., leap
;; forward and backward, which requires us to wrap that outer circuit
;; with *another* precaution to check that it indeed reached the
;; desired point. That makes the implementation unnecessarily
;; complicated, so we'll leave `loop` for now.
(defun symex-eval-loop (loop computation result)
  "Execute a LOOP.

This repeats some traversal according to a condition on the computation.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (let ((traversal (symex--loop-traversal loop))
        (condition (symex--loop-condition loop)))
    (let ((accumulated-result (symex-eval traversal
                                          computation
                                          result)))
      (when accumulated-result
        (if (symex-eval condition
                        computation
                        accumulated-result)
            accumulated-result
          (symex-eval-loop loop
                           computation
                           accumulated-result))))))

(defun symex-eval-detour (detour computation result)
  "Execute a DETOUR.

Apply a reorientation and then attempt the traversal.

If the traversal fails, then the reorientation is attempted as many times as
necessary until either it succeeds, or the reorientation fails.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (let ((reorientation (symex--detour-reorientation detour))
        (traversal (symex--detour-traversal detour)))
    (let ((executed-reorientation (symex-eval reorientation
                                              computation
                                              result)))
      (when executed-reorientation
        (let ((path (symex-make-protocol traversal
                                         detour)))
          (symex-eval path
                      computation
                      executed-reorientation))))))

(defun symex-eval-precaution (precaution computation result)
  "Execute a PRECAUTION.

The traversal is only executed if PRE-CONDITION holds, and is reversed if
POST-CONDITION does not hold after the provisional execution of the traversal.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (let ((traversal (symex--precaution-traversal precaution))
        (pre-condition (symex--precaution-pre-condition precaution))
        (post-condition (symex--precaution-post-condition precaution)))
    (when (symex-eval pre-condition
                      computation
                      result)
      (let ((executed-traversal (symex-eval traversal
                                            computation
                                            result)))
        (when (and executed-traversal
                   (symex-eval post-condition
                               computation
                               executed-traversal))
          ;; only check the post-condition if the traversal
          ;; was successful
          executed-traversal)))))

(defun symex-eval-protocol (protocol computation result)
  "Execute a PROTOCOL.

Given a protocol including a set of options, attempt to execute them
in order until one succeeds.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (unless (symex--protocol-null-p protocol)
    (let ((option (symex--protocol-first protocol))
          (remaining-protocol (symex--protocol-rest protocol)))
      (let ((executed-option (symex-eval option
                                         computation
                                         result)))
        (if executed-option
            executed-option
          (symex-eval-protocol remaining-protocol
                               computation
                               result))))))

(defun symex-eval-decision (decision computation result)
  "Execute a DECISION.

The consequent traversal is executed if the condition holds, and the
alternative traversal is executed if the condition does not hold.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (let ((condition (symex--decision-condition decision))
        (consequent (symex--decision-consequent decision))
        (alternative (symex--decision-alternative decision)))
    (if (symex-eval condition
                    computation
                    result)
        (symex-eval consequent
                    computation
                    result)
      (symex-eval alternative
                  computation
                  result))))

(defun symex-eval-deletion (deletion _computation result)
  "Execute a DELETION.

This could delete `this`, `previous` or `next`.  Of these, favor the
latter two as those are explicit that the placement of point is
unaffected by the transformation.  On the other hand, the first is
provided as a convenience for cases where posterior placement of point
is irrelevant for the purposes of the traversal, because, indeed,
there is no fixed rule on where it will be placed.

TODO: either there should be no guarantee and either next or previous
could be selected, or it should leave a void and have point indicate
the whitespace there rather than explicitly select either the next or
previous expression.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (let ((what (symex--deletion-what deletion)))
    (let ((this-result (symex-prim-delete what)))
      ;; TODO: compute based on an appropriate result here
      (when this-result
        result))))

(defun symex-eval-paste (paste _computation result)
  "Execute a PASTE.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (let ((side (symex--paste-side paste)))
    (let ((this-result (symex-prim-paste side)))
      ;; TODO: compute based on an appropriate result here
      (when this-result
        result))))

(defun symex-eval-effect (effect computation result)
  "Execute an EFFECT.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (let ((traversal (symex--effect-traversal effect))
        (effect (symex--effect-effect effect)))
    (let ((executed-traversal (symex-eval traversal
                                          computation
                                          result)))
      (when executed-traversal
        (let ((_executed-effect (funcall effect)))
          ;; ignore the result of the effect
          executed-traversal)))))

(defun symex--eval (traversal computation result)
  "Helper to execute TRAVERSAL.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (cond ((symex-maneuver-p traversal)
         (symex-eval-maneuver traversal
                              computation
                              result))
        ((symex-venture-p traversal)
         (symex-eval-venture traversal
                             computation
                             result))
        ((symex-circuit-p traversal)
         (symex-eval-circuit traversal
                             computation
                             result))
        ((symex-loop-p traversal)
         (symex-eval-loop traversal
                          computation
                          result))
        ((symex-protocol-p traversal)
         (symex-eval-protocol traversal
                              computation
                              result))
        ((symex-precaution-p traversal)
         (symex-eval-precaution traversal
                                computation
                                result))
        ((symex-detour-p traversal)
         (symex-eval-detour traversal
                            computation
                            result))
        ((symex-decision-p traversal)
         (symex-eval-decision traversal
                              computation
                              result))
        ((symex-move-p traversal)
         (symex-eval-move traversal
                          computation
                          result))
        ((symex-delete-p traversal)
         (symex-eval-deletion traversal
                              computation
                              result))
        ((symex-paste-p traversal)
         (symex-eval-paste traversal
                           computation
                           result))
        ((symex-effect-p traversal)
         (symex-eval-effect traversal
                            computation
                            result))
        ;; fall back to a lambda. It must still accept
        ;; the same arguments as any traversal, so that
        ;; it could in principle produce a valid result
        ;; but in practice the fallback lambda may be
        ;; used for predicates where we only care whether
        ;; the output is truthy or not.
        (t (funcall traversal
                    computation
                    result))))

(defun symex-eval (traversal
                   &optional
                   computation
                   result)
  "Execute a tree TRAVERSAL.

TRAVERSAL could be a move, a maneuver, or any other Symex traversal.
If it is not a Symex expression, then it is assumed to be an ELisp
function, and the rule for interpretation is to apply the function.

The evaluation is done in a \"tail-recursive\" way, by passing the
in-progress RESULT forward through subsequent stages of traversal
evaluation.

This function, along with any of the more specific traversal
evaluators such as `symex-eval-maneuver', evaluates to a COMPUTATION
on the traversal actually executed.

See `symex-eval-move' for more on COMPUTATION and RESULT."
  (let* ((computation (if computation
                         computation
                       symex--computation-default))
         (result (or result
                     (symex-eval-move symex--move-zero
                                      computation))))
    ;; TODO: a macro similar to `symex-save-excursion`
    ;; where it conditionally returns to the original
    ;; point / node depending on whether BODY succeeds
    ;; or which tests for success before moving point
    (let ((original-location (point))
          (original-point-height-offset
           (symex--point-height-offset))
          (new-result (symex--eval traversal
                                   computation
                                   result)))
      (if new-result
          new-result
        ;; TODO: simply returning to the original location isn't
        ;; enough when the traversal might include transformations. It
        ;; may be necessary to execute transformations in a temporary
        ;; buffer. That should work for Lisp, but it may not work for
        ;; treesitter as the syntax isn't explicit in the structure
        ;; and also depends on context. Copying "sufficient" context
        ;; (e.g., up to a top level definition or "tree root") might
        ;; work.
        (goto-char original-location)
        (symex-select-nearest)
        (let* ((current-point-height-offset (symex--point-height-offset))
               (height-differential (- original-point-height-offset
                                       current-point-height-offset)))
          ;; necessary because point does not uniquely identify
          ;; a node for non-symex (i.e. tree-sitter) languages
          (symex--go-up height-differential))
        new-result))))


(provide 'symex-evaluator)
;;; symex-evaluator.el ends here
