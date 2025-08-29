.. raw:: html

  <p align="center">
    <img src="https://github.com/user-attachments/assets/5c1896bd-f3e6-49b9-b2fe-539e0b741e1a" alt="Symex logo" title="Symex logo" style="cursor:default;"/>
  </p>

*Symex (pron. "sim-ex", pl. symexes): A Lisp symbolic expression, which describes a computation to be performed.*

symex.el
========

.. image:: https://github.com/drym-org/symex.el/actions/workflows/test.yml/badge.svg
    :target: https://github.com/drym-org/symex.el/actions

.. image:: https://img.shields.io/badge/user%20manual-HTML-blue
    :target: https://drym-org.github.io/symex.el/

An expressive modal way to edit code in Emacs.

.. contents:: :depth: 2

Introduction
============

Symex (pronounced *sim-ex*) is an intuitive modal way to edit code, built on top of an expressive domain-specific language (DSL) for tree-oriented operations.

This unique design allows it to offer a large array of useful features with short and memorable (and of course, customizable) keybindings, allowing you to edit code fluently. It also gives you the ability to define your own structural operations using the same DSL that Symex implements many of its features in.

Symex supports all flavors of Lisp, and most other languages, too (e.g., Python, JavaScript), via Emacs's built-in tree-sitter facilities. Please see the manual for `how best to set up Symex for your favorite language <https://drym-org.github.io/symex.el/Languages-Supported.html>`_.

Some features of Symex include:

- Expressive ways to get around in your code, including "leap branch," "climb/descend branch," "goto highest/lowest," "skip forward/backward"
- On-demand, structural evaluation of code (e.g., ``e`` to evaluate the selected expression)
- Easily repeat recent commands (like Vim's "dot" operator) and record rich macros
- UI highlighting of selected expressions for intuitive feedback
- Short and memorable keybindings that engender fluency

Under the hood, Symex mode uses ``paredit`` and Tree-Sitter for parsing the code syntax tree.

Installation
============

Symex is distributed as a collection of small, composable packages, directly maintained at this source repository. This allows you maximum flexibility to install just the functionality you actually need, without getting anything extra. But it also means that you must explicitly list the Symex packages you are interested in. Each of the Symex packages is described below, along with sample config using Straight.el. For installation instructions using other package managers, please consult the `user manual <https://drym-org.github.io/symex.el/>`_.

``symex-core``
--------------

This includes a DSL for structural operations, which is the core functionality of Symex, and is required for all users of Symex.

.. code-block:: elisp

  (use-package symex-core
    :straight
    (symex-core
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
     :host github
     :repo "drym-org/symex.el"
     :files ("symex-rigpa/symex*.el"))
    :config
    (symex-rigpa-mode 1))

Documentation
=============

User Manual
-----------

The best way to learn about Symex is to read the included Info manual, which you can view and navigate efficiently within Emacs or even `view online <https://drym-org.github.io/symex.el/>`__.

To view the manual within Emacs, ``C-h R`` and select ``symex`` (or ``C-h i`` and navigate to the Symex manual). You'll need to know some basics of how to navigate Info manuals. If you don't already know how, then try ``C-h R`` and select ``info``. This will take you to a helpful and short (~30 minutes) manual that will tell you all you need to know to navigate Info manuals.

The Animated Guide
------------------

The `Animated Guide to Symex <https://countvajhula.com/2021/09/25/the-animated-guide-to-symex/>`_ is a great way to learn about what you can do with Symex. Besides animations, it also contains lots of helpful field notes. Go check it out!

Introductory Video
------------------

Watch the talk introducing Symex at an `Emacs SF <https://www.meetup.com/Emacs-SF/>`_ meetup in 2019 to learn more about the implementation and see some usage examples.

There have been many improvements since then, but the essential ideas and motivation remain the same.

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

.. raw:: html

  <p align="center">
    <img src="https://user-images.githubusercontent.com/401668/98453162-e3ca2f00-210a-11eb-8669-c1048ff4547c.jpg" width="618" height="410" alt="Symex the Squirrel" title="Symex the Squirrel" style="cursor:default;"/>
  </p>
