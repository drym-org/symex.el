symex-mode
==========
An evil way to edit Lisp symbolic expressions ("symexes") as trees in Emacs

.. raw:: html

  <p align="center">
    <img src="https://thenypost.files.wordpress.com/2017/07/shutterstock_681631765.jpg?quality=90&strip=all&w=618&h=410&crop=1" alt="Symex the Squirrel" title="Symex the Squirrel"/>
  </p>

Symex mode (pronounced sym-ex, as in symbolic expression) is a vim-inspired way of editing Lisp code as trees. Entering symex mode allows you to reason about your code in terms of its structure, similar to other tools like paredit and lispy. But while in those packages the tree representation is implicit, symex mode models the tree structure explicitly so that tree navigations and operations can be described using an expressive DSL, and invoked in a vim-style modal interface implemented with a Hydra.

At the moment, symex mode uses `paredit`, `lispy`, and `evil-cleverparens` to provide much of its low level functionality. In the future, this layer of primitives may be replaced with a layer that explicitly uses the abstract syntax tree, for greater precision.
