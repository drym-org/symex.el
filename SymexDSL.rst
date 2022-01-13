Symex DSL
=========

.. contents:: :depth: 3

Introduction
------------

The Symex DSL allows you to specify arbitrary tree traversals in a cursor-oriented manner. That is, instead of describing traversals from an absolute reference point such as a root node, it allows you to describe traversals from the first-person vantage point of the cursor -- what would you do if you were where this cursor is right now? It allows you to describe what you'd like to do in intuitive terms that make sense for movement in a tree. Symex is the language a squirrel might use to describe a traversal to another squirrel.

Movement
--------

Any action that moves your cursor from one node in the tree to another is called a traversal. Symex gives you several linguistic forms with which to describe traversals. You can describe traversals using this language via the `symex-traversal` macro, which serves as an entry point into the Symex language -- that is, within the interior of this form, the language is Symex instead of ELisp. A traversal described in this way yields a traversal specification which can be executed by using `symex-execute-traversal`.

Each form of the language has its own syntax, and typically the clauses of these forms are expected to be traversals themselves. For instance, the `maneuver` form specifies a traversal as a sequence of other traversals to be executed in order.

In general, if a traversal succeeds, it returns a list of executed moves which if replayed manually from the starting position would have the same effect as running the traversal did. If the traversal fails, it returns `nil`.

move
~~~~

The most basic movement, a move simply takes a single step in a particular direction, to a neighboring node in the tree.

Examples
````````

"Move forward."

::

  (symex-execute-traversal
    (symex-traversal
      (move forward)))

"Move backward."

::

  (symex-execute-traversal
    (symex-traversal
      (move backward)))

"Move up."

::

  (symex-execute-traversal
    (symex-traversal
      (move up)))

"Move down."

::

  (symex-execute-traversal
    (symex-traversal
      (move down)))

Note that in the Symex language, "up" and "down" are defined in relation to the root node being considered the bottom of the tree and increasingly nested expressions as being higher. We "go down towards the root and up towards the nest."

maneuver
~~~~~~~~

Execute a sequence of traversals in order. If the maneuver is partially completed, i.e. if at least one traversal was executed, then the maneuver is treated as successful. Otherwise it is considered to have failed.

Examples
````````

"Go forward, then up, and then forward again."

::

  (symex-execute-traversal
    (symex-traversal
      (maneuver (move forward)
                (move up)
                (move forward))))

"Go up and then keep going forward, and then go up again."

::

  (symex-execute-traversal
    (symex-traversal
      (maneuver (maneuver (move up)
                          (circuit (move forward)))
                (move up))))

protocol
~~~~~~~~

Try executing traversals, in order, until one succeeds (and then stop).

Examples
````````

"Try going forward, if that doesn't work try going backward."

::

  (symex-execute-traversal
    (symex-traversal
      (protocol (move forward)
                (move backward))))

"Try going forward and up, if that doesn't work try going backward and down."

::

  (symex-execute-traversal
    (symex-traversal
      (protocol (maneuver (move forward)
                          (move up))
                (maneuver (move backward)
                          (move down)))))

detour
~~~~~~

Try executing a traversal. If it fails, execute an alternative traversal ("take a detour") and then try the main one again. Keep repeating this until either the main one succeeds, or the alternative fails.

A detour is useful in cases where you are interested in doing traversal A, but if it fails, you have a backup plan in traversal B that will allow you to try A again. You are still mainly interested in doing A, and B is just the way you'd like to reorient yourself in the tree before trying again.

Examples
````````

"Try going forward, and if that fails try again by first going down."

::

  (symex-execute-traversal
    (symex-traversal
      (detour (move down)
              (move forward))))

"Try going forward, and if that fails try again by first going down, but only as long as we don't descend to the root of the tree."

::

  (symex-execute-traversal
    (symex-traversal
      (detour (precaution (move down)
                          (afterwards (not (at root))))
              (move forward))))

decision
~~~~~~~~

Do either traversal A or traversal B, depending on whether a condition holds.

The condition can be any predicate -- either a built-in predicate form, or an arbitrary lambda. See `Predicates`_ for details.

Examples
````````

"If we're at the root of the tree, then go forward, otherwise go down."

::

  (symex-execute-traversal
    (symex-traversal
      (decision (at root)
                (move forward)
                (move down))))

"If we are at a position in the buffer less than previously-stored-position, then go forward, otherwise don't move."

::

  (symex-execute-traversal
    (symex-traversal
      (decision (lambda () (< (point) previously-stored-position))
                (move forward)
                symex--move-zero)))

`symex--move-zero` is just a convenient traversal for cases where you need to indicate a traversal but would like to not move at all. It is defined as `(symex-make-move 0 0)`.

circuit
~~~~~~~

Repeat a traversal a given number of times or as long as it succeeds. When it fails, stop.

Examples
````````

"Move forward three times."

::

  (symex-execute-traversal
    (symex-traversal
      (circuit (move forward) 3)))

"Keep moving forward."

::

  (symex-execute-traversal
    (symex-traversal
      (circuit (move forward))))

"Keep moving down and forward, as long as we don't descend to the root node."

::

  (symex-execute-traversal
    (symex-traversal
      (circuit
        (precaution
          (maneuver (move down)
                    (move forward))
          (afterwards (not (at root)))))))

precaution
~~~~~~~~~~

Execute a traversal, but ensure that certain conditions hold either before or after executing the traversal (or both). If a condition does not hold, then abort the traversal, considering it to have failed.

Each of the conditions can be any predicate -- either a built-in predicate form, or an arbitrary lambda. See `Predicates`_ for details.


Examples
````````

"Go down but don't descend to the root node."

::

  (symex-execute-traversal
    (symex-traversal
      (precaution (move down)
                  (afterwards (not (at root))))))

"Go backward as long as we aren't at the first node at this level."

::

  (symex-execute-traversal
    (symex-traversal
      (precaution (move backward)
                  (beforehand (not (at first))))))

Note that this executes a *single* traversal while taking precautions. It is not repeated unless wrapped in a circuit or employed as a detour.

Predicates
----------

Symex offers a few standard predicates to use as conditions. In addition to these, you may also use any lambda as a predicate, so that you can specify arbitrary conditions to use in e.g. the `decision` and `precaution` forms.

* `(at root)` -- Are we (i.e. is the cursor) at the root node? Any toplevel form in the source file is considered to be a root node.
* `(at first)` -- Are we at the first node at the present level / on the current branch of the tree?
* `(at last)` -- Are we at the last node at the present level / on the current branch of the tree?
* `(at initial)` -- Are we at the first root-level node in the entire file?
* `(at final)` -- Are we at the last root-level node in the entire file?

There is also the modifier `not` which can be used with any of the above predicates. E.g. `(not (at root))` returns true if cursor is not at the root node of the tree.

Side Effects
------------

Traversals may be executed with arbitrary side effects. A side effect is simply a function (e.g. specified via a lambda expression) that is executed *after* the conclusion of a traversal, if that traversal succeeds.

Typically, we are interested in attaching such side effects to a repeated traversal so that the side effect is performed at each step of the traversal as long as it succeeds. For this purpose, you can use the `symex--do-while-traversing` function.

Examples
~~~~~~~~

"Evaluate the remaining expressions at this level in tree." (e.g. if at the root level, this will evaluate the remaining top-level expressions in the file).

::

  (symex--do-while-traversing #'symex--evaluate
                              symex--move-forward)

`symex--move-forward` used here is a traversal provided for convenience that simply moves forward by one step. It is defined as `(symex-make-move 1 0)`.
