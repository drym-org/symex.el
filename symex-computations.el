;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;
;;; COMPUTATIONS ;;;
;;;;;;;;;;;;;;;;;;;;

(defun symex--type-integer (obj)
  "Convert an object to the integer type."
  (cond ((integerp obj)
         obj)
        ((stringp obj)
         (string-to-number obj))
        ((listp obj)
         (length obj))
        (t (error "Unexpected type %s in integer type conversion!" obj))))

(defun symex--type-list (obj)
  "Convert an object to the list type."
  (cond ((is-traversal? obj)
         (list obj))
        ((listp obj)
         obj)
        (t (list obj))))

(cl-defun symex-make-computation (&key
                                  components
                                  (perceive #'identity)
                                  (select #'identity)
                                  (filter #'identity)
                                  (decide #'identity)
                                  (express #'identity)
                                  (act #'identity))
  "A computation to be performed as part of a traversal.

COMPONENTS - a list of nested computations that will each process the
input independently and produce a result. These results will then be
filtered and finally considered collectively to produce a decision.
PERCEIVE - the function to be applied to the result of each traversal step,
which transforms it to the perceived type.
SELECT - a predicate function that is applied to perceived values in order
to select the subset of perceptions that will be operated on by nested
computations.
FILTER - a predicate function to be applied to results from nested computations
to select those that will factor into the decision.
DECIDE - a binary function to be applied in combining results from nested
computations (each of the 'perceived' type) to yield the provisional result
(also of the perceived type).
EXPRESS - a function to transform data of the perceived type (e.g. the
type produced by the decision) to the application type (the type that can be
used by the application).
ACT - a binary function to be applied in combining results from the overall
computation (each of the 'expressed' type) to yield the final result
(also of the expressed type)."
  (list 'computation
        components
        perceive
        select
        filter
        decide
        express
        act))

(defun symex--computation-components (computation)
  "The components of the computation."
  (nth 1 computation))

(defun symex--computation-perceive (computation)
  "The perception procedure of the computation."
  (nth 2 computation))

(defun symex--computation-select (computation)
  "The selection procedure of the computation."
  (nth 3 computation))

(defun symex--computation-filter (computation)
  "The filtration/redaction procedure of the computation."
  (nth 4 computation))

(defun symex--computation-decide (computation)
  "The decision procedure of the computation."
  (nth 5 computation))

(defun symex--computation-express (computation)
  "The expression procedure of the computation."
  (nth 6 computation))

(defun symex--computation-act (computation)
  "The act procedure of the computation."
  (nth 7 computation))

(defun symex--ruminate (computation components input)
  "Helper to process input in nested computations."
  (let ((current (car components))
        (remaining (cdr components)))
    (if current
        (funcall (symex--computation-decide computation)
                 (symex-ruminate current
                                 input)
                 (symex--ruminate computation
                                  remaining
                                  input))
      input)))

(defun symex-ruminate (computation input)
  "Have the COMPUTATION process the INPUT and produce a result."
  (let ((perceived-input (funcall (symex--computation-perceive computation)
                                  input)))
    (let ((components (symex--computation-components computation)))
      (funcall (symex--computation-express computation)
               (symex--ruminate components perceived-input)))))

(defconst computation-default
  ;; each result is wrapped in a list
  ;; the results are concatenated using list concatenation
  (symex-make-computation :perceive #'symex--type-list
                          :act #'append))

(defun symex--traversal-account (obj)
  "Represents the result of a traversal as a traversal."
  (cond ((is-traversal? obj)
         obj)
        (t (apply #'symex-make-maneuver obj))))

;; (defconst computation-account
;;   ;; each result is cast as a maneuver and wrapped in a list for composition
;;   ;; the results are concatenated using list concatenation
;;   (symex-make-computation :f-to-aggregation #'symex--type-list
;;                           :map #'symex--traversal-account
;;                           :reduce #'append
;;                           :f-from-aggregation #'car))

(defun symex--simplify-maneuver-phases (phases)
  "Helper to flatten maneuver to moves."
  (when phases
    (let ((phase (car phases))
          (remaining-phases (cdr phases)))
      (let ((moves (if (is-move? phase)
                       (list phase)
                     (let ((simplified-phase (symex--simplify-maneuver phase)))
                       (symex--maneuver-phases simplified-phase)))))
        (append moves
                (symex--simplify-maneuver-phases remaining-phases))))))

(defun symex--simplify-maneuver (maneuver)
  "Reduce a complex maneuver to a flat maneuver whose phases are moves."
  (let ((phases (symex--maneuver-phases maneuver)))
    (let* ((simplified-phases (symex--simplify-maneuver-phases phases))
           (maneuver-length (length simplified-phases)))
      (cond ((= maneuver-length 1)
             ;; just return the move
             (nth 0 simplified-phases))
            ((> maneuver-length 1)
             (apply #'symex-make-maneuver simplified-phases))))))

(defun symex--interpret-simple-traversal (traversal)
  "Interpret a traversal as a single, flat maneuver or move."
  (cond ((is-maneuver? traversal)
         (symex--simplify-maneuver traversal))
        ((is-move? traversal)
         traversal)
        (t (error "Syntax error: unrecognized traversal type!"))))

;; (defconst computation-simple-account
;;   ;; each result is cast as a maneuver and wrapped in a list for composition
;;   ;; the results are concatenated using list concatenation
;;   (symex-make-computation :f-to-aggregation #'symex--type-list
;;                           :map (-compose #'symex--interpret-simple-traversal
;;                                          #'symex--traversal-account)
;;                           :reduce #'append
;;                           :f-from-aggregation #'car))

(defun symex--streamline-to-maneuver (maneuver-or-move)
  "Streamline traversal to a representation as a maneuver.

If the argument is a maneuver, leave as is.
If it is a move, convert to the equivalent maneuver (via simple casting)."
  (if (is-move? maneuver-or-move)
      (symex-make-maneuver maneuver-or-move)
    maneuver-or-move))

(defun my-add-numbers (&rest numbers)
  "Sum numbers."
  (apply #'+ numbers))

;; (defconst computation-length
;;   ;; each result is interpreted down to a simple maneuver
;;   ;; the phases of this maneuver are extracted and summed using
;;   ;; move addition.
;;   ;; the results are accumulated using addition of numbers
;;   (symex-make-computation :f-to-aggregation #'symex--type-integer
;;                           :map (-compose #'symex--move-length
;;                                          #'symex--add-moves
;;                                          #'symex--maneuver-phases
;;                                          #'symex--streamline-to-maneuver
;;                                          #'symex--interpret-simple-traversal
;;                                          #'symex--traversal-account)
;;                           :reduce #'my-add-numbers
;;                           :f-from-aggregation #'car))

(provide 'symex-computations)
