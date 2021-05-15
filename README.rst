*Symex (pron. "sym-ex", pl. symexes): A Lisp symbolic expression, which describes a computation to be performed.*

.. image:: https://melpa.org/packages/symex-badge.svg
    :alt: MELPA
    :target: https://melpa.org/#/symex

.. image:: https://stable.melpa.org/packages/symex-badge.svg
    :alt: MELPA Stable
    :target: https://stable.melpa.org/#/symex

symex.el
========
An `evil <https://github.com/emacs-evil/evil>`_ way to edit Lisp symbolic expressions ("symexes") as trees in Emacs.

.. raw:: html

  <p align="center">
    <img src="https://user-images.githubusercontent.com/401668/98453162-e3ca2f00-210a-11eb-8669-c1048ff4547c.jpg" width="618" height="410" alt="Symex the Squirrel" title="Symex the Squirrel" style="cursor:default;"/>
  </p>

.. contents:: :depth: 2

Introduction
============

Symex mode (pronounced sym-ex, as in symbolic expression) is a vim-inspired way of editing Lisp code as trees. Entering symex mode allows you to reason about your code in terms of its structure, similar to other tools like `paredit <https://www.emacswiki.org/emacs/ParEdit>`_ and `lispy <https://github.com/abo-abo/lispy>`_. But while those packages provide a curated number of useful tree operations, symex mode treats the tree structure explicitly so that arbitrary tree navigations and operations can be described using an expressive DSL, and invoked conveniently via a vim-style modal interface. As a consequence of this:

- Symex provides many novel features, such as "leap branch," "climb/descend branch," "goto highest/lowest," "skip forward/backward", recursive indent, recursive evaluate, among many others
- Implementing new structure-related features in general is easy [1]_.
- Keybindings are short and memorable

At the moment, symex mode uses ``paredit``, ``lispy``, and `evil-cleverparens <https://github.com/luxbock/evil-cleverparens>`_ to provide much of its low level functionality. In the future, this layer of primitives may be replaced with a layer that explicitly uses the abstract syntax tree, for still greater precision.

.. raw:: html

  <p align="center">
    <img src="https://user-images.githubusercontent.com/401668/59328521-6db96280-8ca1-11e9-8b32-24574a0af676.png" alt="Screenshot" title="Screenshot" style="cursor:default;"/>
  </p>

.. [1] As long as, from a theoretical perspective, the intended traversal can be accomplished using a `finite automaton <https://en.wikipedia.org/wiki/Deterministic_finite_automaton>`_. More complex traversals can be implemented (such as "leap branch"), but not as easily. Symex may be made Turing-complete at some point in the future, if there is interest in a feature that cannot be implemented in the DSL in its current form.

Installation
============

1. Install the package the usual way via MELPA (e.g. :code:`M-x package-install`).

2. Then, assuming you're using `use-package <https://github.com/jwiegley/use-package>`__ to manage your configuration, add the following config to your ``init.d`` (note these instructions have changed as of `version 0.9 <https://github.com/countvajhula/symex.el/releases/tag/0.9>`__):

::

  (use-package symex
    :config
    (symex-initialize)
    (global-set-key (kbd "s-;") 'symex-mode-interface))  ; or whatever keybinding you like

This provides a keybinding to load the symex editing interface, and also enables the symex minor mode in all recognized lisp modes (the minor mode ensures that manual edits respect the tree structure, e.g. keeps parens balanced like paredit).

Usage and Customization
=======================

Evil or Hydra?
--------------

Symex provides both an evil state as well as a hydra-based modal interface. Which one should you use?

**TL;DR**: in the long term, use the evil option (this is the default, and it is available to both evil and vanilla emacs users). If you're learning and would like some hand-holding as you familiarize yourself with the keybindings, use the hydra option.

The evil option is less obtrusive and allows you to, for instance, execute ``M-x`` commands without leaving symex mode. It should feel very similar to using Normal state, and doesn't interfere with normal Emacs usage including any custom keybindings you may be using.

The hydra operates almost identically to the evil state, but it provides a comprehensive menu that can be toggled on and off, and can therefore help you learn the keybindings as you go along. On the other hand, the drawback is that the hydra will exit if you do something not specifically connected to symex mode -- for instance, if you run an ``M-x`` command, or do a text search, or save the buffer, or run a custom command of some kind. You could customize the hydra so that it is more persistent (e.g. "pink" or "amaranth" hydra) but doing so could cause it to interfere with normal Emacs functions, as hydra keybindings take precedence over everything else.

In short, evil provides a more seamless experience, but hydra may be a good option while you are learning to use symex.

Depending on your choice, put one of these in the ``:custom`` `section <https://github.com/jwiegley/use-package#customizing-variables>`__ (not the ``:config`` section) of your ``use-package`` form:

::

  (symex-modal-backend 'evil)

::

  (symex-modal-backend 'hydra)

Key Bindings
------------

The following table lists the key bindings in symex mode. You would only need this table for the evil frontend, as with the hydra frontend, you can lookup the keybindings at any time by pulling up the hydra menu (default binding: ``H-m``).

Movement
~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Key
     - Action
     - Remarks

   * - ``h``, ``j``, ``k``, ``l``
     - backwards, down, up, forwards
     -

   * - ``f``, ``b``
     - traverse forwards, backwards
     -

   * - ``C-f``, ``C-b``
     - traverse forwards, backwards more
     - quicker ways to get around

   * - ``F``, ``B``
     - skip forwards, backwards
     - quick ways to move forwards and backwards -- traverse without entering nested expressions

   * - ``C-,``, ``C-/``
     - leap backwards, forwards
     - "leap" to adjacent branches in the current tree, preserving position on branch

   * - ``C-M-,``, ``C-M-/``
     - soar backwards, forwards
     - leap, but crossing trees if necessary

   * - ``C-k``, ``C-j``
     - climb, descend
     - a quick way to go up and down a tree

   * - ``0`` / ``M-h``
     - go to first symex at this level
     -

   * - ``$``, ``M-l``
     - go to last symex at this level
     -

   * - ``M-j``, ``M-k``
     - go to lowest, highest symex in the tree
     -

Editing
~~~~~~~

.. list-table::

   * - ``i``, ``a``
     - insert at beginning, append at end
     -

   * - ``I``, ``A``
     - insert before, append after
     -

   * - ``o``, ``O``
     - open line below, above
     -

   * - ``(``, ``[``, ``{``
     - create symex with indicated delimiter
     -

   * - ``)``, ``]``, ``}``
     - wrap symex with indicated delimiter
     -

   * - ``w``
     - wrap with parens and insert
     -

   * - ``x``
     - delete
     -

   * - ``c``
     - change
     -

   * - ``y``, ``p``, ``P``
     - yank (copy), paste after, paste before
     -

   * - ``C``, ``s``
     - clear, replace/substitute
     -

   * - ``S``
     - change "surrounding" delimiter
     -

   * - ``H``, ``L``
     - move/shift symex backwards, forwards
     -

   * - ``K``
     - raise
     -

   * - ``C-S-j`` / ``C-{``, ``C-S-k`` / ``C-}``
     - emit backwards, forwards
     -

   * - ``C-S-h`` / ``C-(``, ``C-S-l`` / ``C-)``
     - capture backwards, forwards
     -

   * - ``z``, ``Z``
     - swallow head, swallow tail
     -

   * - ``|``, ``m``
     - split, join/merge
     -

   * - ``\``
     - splice
     - clip the delimiters, joining the symex to the containing expression

   * - ``>``, ``C->`` / ``C-S-o``
     - insert newline before, append newline after
     -

   * - ``<``,  ``J`` / ``C-<``
     - join with preceding line, join with next line
     -

   * - ``M-J``
     - collapse to a single line
     -

   * - ``=``, ``<tab>``
     - tidy
     - indent and remove extraneous whitespace

   * - ``M-=`` / ``M-<tab>``
     - tidy recursively
     - tidies while traversing the symex from the highest branch to the root, for cases where a simple tidy isn't adequate

   * - ``;``
     - comment out
     -

Control
~~~~~~~

.. list-table::

   * - ``e``, ``E``, ``d``, ``M-e``, ``T``
     - evaluate, pretty evaluate, evaluate definition, evaluate recursively, evaluate as "thunk"
     - ``T`` evaluates the indicated symex as if it were wrapped with parentheses, i.e. invoking it as a function, passing no arguments

   * - ``:``
     - eval-expression
     - evaluate an arbitrary expression in the minibuffer

   * - ``t``
     - switch to a scratch buffer
     -

   * - ``M``
     - display the messages buffer alongside
     -

   * - ``r``
     - go to REPL
     -

   * - ``R`` / ``X``
     - run/eval the buffer
     -

   * - ``C-;``
     - evaluate, and insert result
     -

   * - ``H-h``
     - toggle highlight
     -

   * - ``?``
     - describe / lookup documentation
     -

   * - ``<return>``
     - enter insertion state
     -

   * - ``<escape>``
     - exit
     -

The Menu (Hydra-only)
---------------------

Entering the symex modal interface (via e.g. :code:`s-;`) using the hydra option shows you a comprehensive menu of all possible actions, by default. This is helpful initially, but over time you may prefer to dismiss the menu and bring it up only on demand, in order to conserve screen real estate. To do this, either run ``symex-toggle-menu`` via the menu entry point (``H-m``) while in symex mode, or add this to your ``init.d`` (e.g. in the ``:config`` section of the ``use-package`` form):

::

  (symex-hide-menu)

Up and Down
-----------

The default keybindings in symex mode treat increasingly nested code as being "higher" and elements closer to the root as "lower." Think going "up" to the nest and "down" to the root. But symex allows you to modify these or any other keybindings to whatever you may find most natural.

If you're using evil, put something resembling this in your configuration *before* the call to ``(symex-initialize)``:

::

  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)))

If you're using hydra, put something resembling this in your configuration *after* the call to ``(symex-initialize)``:

::

  (defhydra+ hydra-symex (:columns 4
                          :post (symex-exit-mode)
                          :after-exit (symex--signal-exit))
      "Symex mode"
      ("j" symex-go-up "up")
      ("k" symex-go-down "down")
      ("C-j" symex-climb-branch "climb branch")
      ("C-k" symex-descend-branch "descend branch")
      ("M-j" symex-goto-highest "go to highest")
      ("M-k" symex-goto-lowest "go to lowest"))

Branch Memory
-------------

When going up and down, the choice of initial position on the branch is arbitrary. By default, symex the squirrel remembers where it was on each branch as it goes up and down the tree, so you return to your last position when going up and down. If you'd like to move to the first or last position, you can use (for instance) ``0`` or ``$`` at each level, as usual, or traverse the tree using ``f`` and ``b`` instead. If, on the other hand, you'd like to start always at the first position when going up (as it was in older versions of Symex), disable the branch memory feature by adding this to the ``:custom`` `section <https://github.com/jwiegley/use-package#customizing-variables>`__ (not the ``:config`` section) of your ``use-package`` form:

::

   (symex-remember-branch-position-p nil)

Tips
====

Macros
------

When you define macros in symex mode (e.g. via ``q`` for evil users), make sure that the commands you use are those that have the same effect in every situation. For instance, the "up" and "down" motions (default: ``k`` and ``j``) could vary based on "branch memory" - up may sometimes move you to the first position on the higher level, but at other times it may move you to the third position, if that happens to be your most recent position. Using up and down in your macro would mean that it could have different results in each tree depending on your activities in the tree, unless you remember to reset the frame of reference by using something like ``0`` or ``$``. Instead, it may be more natural to use the "flow" traversal commands (default: ``f`` and ``b``), repeating them or prefixing them with count arguments if necessary, to move around in a fully deterministic way. This will ensure that your macros behave the same way in every case.

Learn More
==========

Learn more about the implementation and see some usage examples in the video overview (given at an `Emacs SF <https://www.meetup.com/Emacs-SF/>`_ meetup in 2019):

.. raw:: html

  <p align="center">
    <a href="https://www.youtube.com/watch?v=a5s1ScTx8Zk">
      <img src="https://i.imgur.com/tk1x1p0.jpg" alt="Watch video" title="Watch video"/>
    </a>
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

"License"
==========
This work is "part of the world." You are free to do whatever you like with it and it isn't owned by anybody, not even the creators. Attribution would be appreciated and would help, but it is not strictly necessary nor required. If you'd like to learn more about this way of doing things and how it could lead to a peaceful, efficient, and creative world, and how you can help, visit `drym.org <https://drym.org>`_.
