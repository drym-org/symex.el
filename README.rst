*Symex (pron. "sym-ex", pl. symexes): A Lisp symbolic expression, which designates a computation to be performed.*

symex-mode
==========
An `evil <https://github.com/emacs-evil/evil>`_ way to edit Lisp symbolic expressions ("symexes") as trees in Emacs

.. raw:: html

  <p align="center">
    <img src="https://thenypost.files.wordpress.com/2017/07/shutterstock_681631765.jpg?quality=90&strip=all&w=618&h=410&crop=1" alt="Symex the Squirrel" title="Symex the Squirrel"/>
  </p>

Symex mode (pronounced sym-ex, as in symbolic expression) is a vim-inspired way of editing Lisp code as trees. Entering symex mode allows you to reason about your code in terms of its structure, similar to other tools like `paredit <https://www.emacswiki.org/emacs/ParEdit>`_ and `lispy <https://github.com/abo-abo/lispy>`_. But while those packages provide a curated number of useful tree operations, symex mode treats the tree structure explicitly so that arbitrary tree navigations and operations can be described using an expressive DSL, and invoked conveniently in a vim-style modal interface implemented with a `Hydra <https://github.com/abo-abo/hydra>`_.

At the moment, symex mode uses ``paredit``, ``lispy``, and `evil-cleverparens <https://github.com/luxbock/evil-cleverparens>`_ to provide much of its low level functionality. In the future, this layer of primitives may be replaced with a layer that explicitly uses the abstract syntax tree, for greater precision.

.. raw:: html

  <p align="center">
    <img src="https://user-images.githubusercontent.com/401668/59328521-6db96280-8ca1-11e9-8b32-24574a0af676.png" alt="Screenshot" title="Screenshot"/>
  </p>

A Note on the Name
==================
A little while ago I was discussing Lisp syntax with `@apromessi <https://github.com/apromessi>`_:

    Me: "...And so we have these sex-puhs..."
    
    A: "Excuse me?"
    
    Me: "Oh, I mean ess expressions! It stands for symbolic expression."
    
    A: "Why not just call it sym-ex?"
    
    Me: [mindblown]
    
    A: "..."

Lisp has inherited a few oddball names from its deep prehistory, including the infamous ``car`` and ``cdr`` for the ``first`` and the ``rest`` of the `elements in a list <http://www.blogbyben.com/2011/04/best-bumper-sticker-ever.html>`_. But S-expression / sex-puh / symbolic expression are all somewhat of a mouthful too. Here are a few reasons why we might want to consider using "sym-ex" instead:

"Symbolic expression": 6 syllables, long in written form too

"S-expression": 4 syllables, I find this name confusing at least partially because it is a single-letter acronym which is unusual. In addition, it is long in written form.

"Sexpuh" / "sex-p" / "sexpr": 2 syllables, short in written form. But I mean, these are terrible.

"s-ex": Speaks for itself.

"Symex": 2 syllables, short in written form, has normal linguistic analogues like "complex/complexes," and it's fun to say! Symex also sounds like `Ibex <https://en.wikipedia.org/wiki/Ibex>`_, and that's obviously a plus.
