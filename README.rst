*Symex (pron. "sym-ex", pl. symexes): A Lisp symbolic expression, which designates a computation to be performed.*

.. image:: https://melpa.org/packages/symex-badge.svg
    :alt: MELPA
    :target: https://melpa.org/#/symex

.. image:: https://stable.melpa.org/packages/symex-badge.svg
    :alt: MELPA Stable
    :target: https://stable.melpa.org/#/symex

symex.el
========
An `evil <https://github.com/emacs-evil/evil>`_ way to edit Lisp symbolic expressions ("symexes") as trees in Emacs

.. raw:: html

  <p align="center">
    <img src="https://user-images.githubusercontent.com/401668/98453162-e3ca2f00-210a-11eb-8669-c1048ff4547c.jpg" width="618" height="410" alt="Symex the Squirrel" title="Symex the Squirrel"/>
  </p>

Symex mode (pronounced sym-ex, as in symbolic expression) is a vim-inspired way of editing Lisp code as trees. Entering symex mode allows you to reason about your code in terms of its structure, similar to other tools like `paredit <https://www.emacswiki.org/emacs/ParEdit>`_ and `lispy <https://github.com/abo-abo/lispy>`_. But while those packages provide a curated number of useful tree operations, symex mode treats the tree structure explicitly so that arbitrary tree navigations and operations can be described using an expressive DSL, and invoked conveniently in a vim-style modal interface implemented with a `Hydra <https://github.com/abo-abo/hydra>`_.

At the moment, symex mode uses ``paredit``, ``lispy``, and `evil-cleverparens <https://github.com/luxbock/evil-cleverparens>`_ to provide much of its low level functionality. In the future, this layer of primitives may be replaced with a layer that explicitly uses the abstract syntax tree, for greater precision.

.. raw:: html

  <p align="center">
    <img src="https://user-images.githubusercontent.com/401668/59328521-6db96280-8ca1-11e9-8b32-24574a0af676.png" alt="Screenshot" title="Screenshot"/>
  </p>

Installation and Usage
======================
Install the package the usual way via MELPA. Then add the following config to your ``init.d``:

::

  (global-set-key (kbd "s-;") 'symex-mode-interface)  ; or whatever keybinding you like
  (dolist (mode-name symex-lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (add-hook mode-hook 'symex-mode)))

This provides a keybinding to load the symex editing interface, and also enables the symex minor mode in all recognized lisp modes (the minor mode is simply there to ensure that manual edits respect the tree structure, e.g. keeps parens balanced like paredit).

By default, entering the symex modal interface shows you a comprehensive menu of all possible actions. This is helpful initially, but over time you may prefer to dismiss the menu and bring it up on demand in order to conserve screen real estate. To do this, either run ``symex-toggle-menu`` via the menu entry point (``H-m``) while in symex mode, or add this to your ``init.d`` (as part of the config above):

::

  (symex-hide-menu)

The default keybindings in symex mode treat increasingly nested code as being "higher" and elements closer to the root as "lower." Think going "up" to the nest and "down" to the root. But if you'd prefer to modify these or any other key bindings to whatever you find most natural, you can add the following config to your ``init.d``. If you're using `use-package <https://github.com/jwiegley/use-package>`__ to manage your configuration, put this in the ``:config`` section:

::

  (defhydra+ hydra-symex (:idle 1.0
                          :columns 4
                          :color pink
                          :body-pre (progn (evil-symex-state)
                                           (symex-select-nearest))
                          :post (deactivate-mark))
      "Symex mode"
      ("j" symex-go-up "up")
      ("k" symex-go-down "down")
      ("C-j" symex-climb-branch "climb branch")
      ("C-k" symex-descend-branch "descend branch")
      ("M-j" symex-goto-highest "go to highest")
      ("M-k" symex-goto-lowest "go to lowest")
      ("F" nil nil))

If you want to learn more about the implementation and see some usage examples, watch the video overview:

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
