.. raw:: html

  <p align="center">
    <img src="https://github.com/user-attachments/assets/5c1896bd-f3e6-49b9-b2fe-539e0b741e1a" alt="Symex logo" title="Symex logo" style="cursor:default;"/>
  </p>

*Symex (pron. "sim-ex", pl. symexes): A Lisp symbolic expression, which describes a computation to be performed.*

symex.el
========

.. image:: https://github.com/drym-org/symex.el/actions/workflows/test.yml/badge.svg
    :target: https://github.com/drym-org/symex.el/actions

An intuitive way to edit Lisp symbolic expressions ("symexes") structurally in Emacs.

.. contents:: :depth: 2

Introduction
============

Symex (pronounced @emph{sim-ex}) is an intuitive modal way to edit code, built on top of an expressive domain-specific language (DSL) for tree-oriented operations.

This design allows it to offer a large array of useful features with short and memorable (and of course, customizable) keybindings, allowing you to edit code fluently. It also gives you the ability to define your own structural operations using the same DSL that Symex implements many of its features in.

Some features of Symex include:

- Expressive ways to get around in your code, including "leap branch," "climb/descend branch," "goto highest/lowest," "skip forward/backward"
- On-demand, structural evaluation of code (e.g., ``e`` to evaluate the selected expression)
- Easily repeat recent commands (like Vim's "dot" operator) and record rich macros
- UI highlighting of selected expressions for intuitive feedback
- Short and memorable keybindings that engender fluency

Under the hood, Symex mode uses ``paredit`` and Tree-Sitter for parsing the code syntax tree.

Installation
============

Symex is distributed as a collection of small, composable packages, directly maintained at this source repository. This allows you maximum flexibility to install just the functionality you actually need, without getting anything extra. But it also means that you must explicitly list the Symex packages you are interested in. Each of the Symex packages is described below, along with sample config using Straight.el.

``symex-core``
--------------

This includes a DSL for structural operations, which is the core functionality of Symex, and is required for all users of Symex.

.. code-block:: elisp

  (use-package symex-core
    :straight
    (symex-core
     :type git
     :host github
     :repo "drym-org/symex.el"
     :files ("symex-core/symex*.el")))

This core package provides the structural editing engine, supporting both Lisp (via `paredit`) and other languages (via Treesitter). This is a lean package with few dependencies, so that, in addition to use by end users, it's also appropriate as a library dependency for third party packages to gain Symex features.

``symex``
---------

The main user-facing package, required for all end users. It provides a modal UI for advanced structural editing, analogous to Vim but simpler and more specialized.

.. code-block:: elisp

  (use-package symex
    :after (symex-core)
    :straight
    (symex
     :type git
     :host github
     :repo "drym-org/symex.el"
     :files ("symex/symex*.el" "symex/doc/*.texi" "symex/doc/figures"))
    :config
    (symex-mode 1)
    (global-set-key (kbd "s-;") #'symex-mode-interface))  ; or whatever keybinding you like

    ;; and any other customizations you like

In addition to making the core structural editing features conveniently available to you, this package also contains many UX features, such as highlighting the selected expression, the ability to easily repeat recent commands, record rich macros, and more.

Note the keybinding ``s-;`` which enters the Symex modal UI. Feel free to set it to whatever you find convenient. There are many ways to enter Symex mode in different contexts, and these are covered in detail in the manual, along with ways to customize Symex.

``symex-ide``
-------------

A recommended, optional, extension integrating Symex with major modes for IDE-like features, such as easy evaluation and documentation lookup, and entering a REPL.

.. code-block:: elisp

  (use-package symex-ide
    :after (symex)
    :straight
    (symex-ide
     :type git
     :host github
     :repo "drym-org/symex.el"
     :files ("symex-ide/symex*.el"))
    :config
    (symex-ide-mode 1))

``symex-evil``
--------------

An extension for seamless integration with Evil mode. This is only required for Evil users.

.. code-block:: elisp

  (use-package symex-evil
    :after (symex evil)
    :straight
    (symex-evil
     :type git
     :host github
     :repo "drym-org/symex.el"
     :files ("symex-evil/symex*.el"))
    :config
    (symex-evil-mode 1))

``symex-rigpa``
---------------

An extension for seamless integration with the `Rigpa <https://github.com/countvajhula/rigpa>`_ modal interface framework. This is only required for Rigpa users.

.. code-block:: elisp

  (use-package symex-rigpa
    :after (symex rigpa symex-evil)
    :straight
    (symex-rigpa
     :type git
     :host github
     :repo "drym-org/symex.el"
     :files ("symex-rigpa/symex*.el"))
    :config
    (symex-rigpa-mode 1))

Documentation
=============

The best way to learn about Symex is to read the included Info manual, which you can view and navigate efficiently within Emacs.

To view the manual within Emacs, ``C-h R`` and select ``symex`` (or ``C-h i`` and navigate to the Symex manual). You'll need to know some basics of how to navigate Info manuals. If you don't already know how, then try ``C-h R`` and select ``info``. This will take you to a helpful and short (~30 minutes) manual that will tell you all you need to know to navigate Info manuals.

For now, some documentation is also included below in this README, but it will eventually be removed in favor of the Info manual (either directly or perhaps as rendered and hosted HTML), to have a single source of truth and avoid errors in documentation.

Usage and Customization
=======================

The Animated Guide
------------------

The `Animated Guide to Symex <https://countvajhula.com/2021/09/25/the-animated-guide-to-symex/>`_ is a great way to learn about what you can do with Symex. Besides animations, it also contains lots of helpful field notes. Go check it out!

Key Bindings
------------

The table below lists the key bindings in Symex mode. You can also always use Emacs's ``C-h k`` to learn what a key does, as another way of learning the bindings.

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

   * - ``gj``, ``gk``
     - linewise down, up
     - don't rely too heavily on these -- e.g. "leap branch" is often better

   * - ``f``, ``b``
     - traverse forwards, backwards
     -

   * - ``C-f``, ``C-b``
     - traverse forwards, backwards more
     - quicker ways to get around

   * - ``F``, ``B``
     - skip forwards, backwards
     - quick ways to move forwards and backwards -- traverse without entering nested expressions

   * - ``{``, ``}``
     - leap backwards, forwards
     - "leap" to adjacent branches in the current tree, preserving position on branch

   * - ``M-{``, ``M-}``
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

   * - ``(``, ``[``
     - create symex with indicated delimiter
     -

   * - ``)``, ``]``
     - wrap symex with indicated delimiter
     -

   * - ``C-'``, ``C-,``, `````, ``C-```
     - cycle quoting, cycle unquoting, add quoting level, remove quoting level
     -

   * - ``w``, ``W``
     - wrap with parens and insert, wrap and append
     -

   * - ``x``, ``X``, ``D``
     - delete, delete backwards, delete remaining
     -

   * - ``c``, ``C``
     - change, change remaining
     -

   * - ``y``, ``Y``, ``p``, ``P``
     - yank (copy), yank remaining, paste after, paste before
     -

   * - ``C--``, ``s``
     - clear, replace/substitute
     -

   * - ``S``
     - change "surrounding" delimiter
     -

   * - ``H``, ``L``
     - move/shift symex backwards, forwards
     -

   * - ``M-H``, ``M-L``
     - move/shift symex backwards, forwards as far as possible on line or column
     - Remember that usually the Meta prefix ``M-`` means "the most" and the Shift prefix ``S-`` means an action or "to shift" in a direction

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

   * - ``|``, ``&``
     - split, join/merge
     -

   * - ``-``
     - splice
     - clip the delimiters, joining the symex to the containing expression

   * - ``>``, ``C->`` / ``C-S-o``
     - insert newline before, append newline after
     -

   * - ``<``,  ``J`` / ``C-<``
     - join with preceding line, join with next line
     -

   * - ``M-J`` / ``M-<``
     - collapse to a single line
     -

   * - ``M->``
     - unfurl across multiple lines
     -

   * - ``C-M-<``, ``C-M->``
     - collapse remaining, unfurl remaining
     -

   * - ``=``, ``<tab>``
     - tidy
     - indent and remove extraneous whitespace

   * - ``C-=``, ``C-<tab>``
     - tidy remaining
     - tidy remaining symexes at this level

   * - ``M-=`` / ``M-<tab>``
     - tidy recursively
     - tidies while traversing the symex from the highest branch to the root, for cases where a simple tidy isn't adequate

   * - ``;``, ``M-;``
     - comment out, comment out remaining
     -

Control
~~~~~~~

.. list-table::

   * - ``e``, ``E``, ``d``, ``M-e``, ``C-M-e``, ``T``
     - evaluate, evaluate remaining, evaluate definition, evaluate recursively, pretty evaluate, evaluate as "thunk"
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

Customization
-------------

Please read the manual to learn how to customize Symex.

Learn More
==========

.. raw:: html

  <p align="center">
    <img src="https://user-images.githubusercontent.com/401668/98453162-e3ca2f00-210a-11eb-8669-c1048ff4547c.jpg" width="618" height="410" alt="Symex the Squirrel" title="Symex the Squirrel" style="cursor:default;"/>
  </p>

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

Supporting this Project
=======================

Please make any financial contributions in one of the following ways:

- by Venmo to ``@Sid-K``
- by Paypal to skasivaj at gmail dot com

Please mention "Symex" in your message.

This project follows Attribution-Based Economics as described in `drym-org/foundation <https://github.com/drym-org/foundation>`_. Any financial contributions will be distributed to contributors and antecedents as agreed-upon in a collective process that anyone may participate in. To see the current distributions, take a look at `abe/attributions.txt <https://github.com/drym-org/symex.el/blob/main/abe/attributions.txt>`_. To see payments made into and out of the project, see the `abe <https://github.com/drym-org/symex.el/blob/main/abe/>`__ folder. If your payment is not reflected there within 3 days, or if you would prefer to, you are welcome to submit an issue or pull request to report the payment yourself -- all payments into and out of the repository are to be publicly reported (but may be anonymized if desired).

Additionally, if your voluntary payments exceed the agreed-upon "market price" of the project (see `price.txt <https://github.com/drym-org/symex.el/blob/main/abe/price.txt>`_), that additional amount will be treated as an investment, entitling you to a share in future revenues, including payments made to the project in the future or attributive revenue from other projects.

This project will distribute payments according to the ABE guidelines specified in the constitution. In particular, it may take up to 90 days to distribute the initial payments if DIA has not already been conducted for this project. After that, payments will be distributed to contributors (including investors) at each meeting of the `DIA congress <https://github.com/drym-org/dia-symex>`__ (e.g. approximately quarterly).

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.
