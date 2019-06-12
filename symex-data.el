;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA ABSTRACTIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-make-move (x y)
  "Construct a tree 'move' representing the number of steps to be taken
along the X or forward-backward axis, and the Y or in-out axis."
  (list 'move x y))

(defun symex--move-x (move)
  "X (horizontal) component of move."
  (nth 1 move))

(defun symex--move-y (move)
  "Y (vertical) component of move."
  (nth 2 move))

(defun is-move? (obj)
  "Checks if the data specifies a move."
  (condition-case nil
      (equal 'move
             (nth 0 obj))
    (error nil)))

(defconst move-zero (symex-make-move 0 0))
(defconst move-go-forward (symex-make-move 1 0))
(defconst move-go-backward (symex-make-move -1 0))
(defconst move-go-in (symex-make-move 0 1))
(defconst move-go-out (symex-make-move 0 -1))

(defun are-moves-equal? (m1 m2)
  "Check if two moves are identical, including any conditions."
  (equal m1 m2))

(defun symex--add-moves (moves)
  "Add moves together as vectors, indicating height and distance
along the branches of the tree."
  (if moves
      (let ((current (car moves))
            (remaining (cdr moves)))
        (let ((result (symex--add-moves remaining)))
          (symex-make-move (+ (symex--move-x current)
                              (symex--move-x result))
                           (+ (symex--move-y current)
                              (symex--move-y result)))))
    move-zero))

(defun symex--move-length (move)
  "Compute the length of the move. This is most naturally meaningful when
the move is entirely along one axis, but a result will be returned even
if the move is across multiple axes, as standard linear vector magnitude
computation is used."
  (let ((x (symex--move-x move))
        (y (symex--move-y move)))
    (if (not (= x 0))
        x
      y)))

(cl-defun symex-make-precaution (traversal &key pre-condition post-condition)
  "A specification to check conditions before and/or after execution
of a traversal."
  (let ((pre-condition (or pre-condition (lambda () t)))
        (post-condition (or post-condition (lambda () t))))
    (list 'precaution
          traversal
          pre-condition
          post-condition)))

(defun symex--precaution-traversal (precaution)
  "The traversal component of the precaution, i.e. the traversal to be
executed with precautions."
  (nth 1 precaution))

(defun symex--precaution-pre-condition (precaution)
  "Pre-condition of precaution"
  (nth 2 precaution))

(defun symex--precaution-post-condition (precaution)
  "Post-condition of precaution"
  (nth 3 precaution))

(defun is-precaution? (obj)
  "Checks if the data specifies a precaution."
  (condition-case nil
      (equal 'precaution
             (nth 0 obj))
    (error nil)))

(defun symex-make-circuit (traversal &optional times)
  "A specification to repeat a TRAVERSAL TIMES times.

If TIMES is nil, repeat indefinitely until the traversal fails."
  (list 'circuit
        traversal
        times))

(defun symex--circuit-traversal (circuit)
  "Get the traversal component of the circuit, i.e. the traversal
to be looped."
  (nth 1 circuit))

(defun symex--circuit-times (circuit)
  "Get the times component of the circuit, i.e. the number of times
the traversal should be repeated."
  (nth 2 circuit))

(defun is-circuit? (obj)
  "Checks if the data specifies a circuit."
  (condition-case nil
      (equal 'circuit
             (nth 0 obj))
    (error nil)))

(defun symex-make-maneuver (&rest phases)
  "Construct a maneuver from the given moves."
  (list 'maneuver
        phases))

(defun symex--maneuver-phases (maneuver)
  "Get the phases of a maneuver (which are themselves maneuvers or moves)."
  (nth 1 maneuver))

(defun is-maneuver? (obj)
  "Checks if the data specifies a maneuver."
  (condition-case nil
      (equal 'maneuver
             (nth 0 obj))
    (error nil)))

(defun symex-make-detour (reorientation traversal)
  "Construct a detour.

A detour consists of two components -- a TRAVERSAL that we wish to execute, and
a REORIENTATION which is a transformation we want to apply prior to attempting
the traversal. Both the reorientation as well as the traversal could be any
type of traversal, for instance a detour or a maneuver.

The reorientation is applied repeatedly and the traversal is re-attempted each
time, until it succeeds. If the reorientation itself fails, then the detour fails
as well."
  (list 'detour
        reorientation
        traversal))

(defun symex--detour-reorientation (detour)
  "Get the reorientation component of the DETOUR."
  (nth 1 detour))

(defun symex--detour-traversal (detour)
  "Get the traversal component of the DETOUR."
  (nth 2 detour))

(defun is-detour? (obj)
  "Checks if the data specifies a detour."
  (condition-case nil
      (equal 'detour
             (nth 0 obj))
    (error nil)))

(defun symex-make-protocol (&rest options)
  "Construct a protocol abstraction for the given OPTIONS.

An option could be either a maneuver, or a protocol itself."
  (list 'protocol
        options))

(defun symex--protocol-options (protocol)
  "Get the set of options that are part of the PROTOCOL."
  (nth 1 protocol))

(defun is-protocol? (obj)
  "Checks if the data specifies a protocol."
  (condition-case nil
      (equal 'protocol
             (nth 0 obj))
    (error nil)))

(defun is-traversal? (obj)
  "Checks if the data specifies a traversal."
  (or (is-move? obj)
      (is-maneuver? obj)
      (is-circuit? obj)
      (is-detour? obj)
      (is-precaution? obj)
      (is-protocol? obj)))

(provide 'symex-data)
