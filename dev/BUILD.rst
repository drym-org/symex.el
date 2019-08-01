Build steps
===========

1. Run lint checks

.. code-block:: bash

  make lint

  # may want to pipe the output to less to parse it since there are a lot
  # of errors that are not really errors
  make lint 2>&1 | less

2. Run checkdoc (docstring checker)

.. code-block:: bash

  make checkdoc

3. Install any new dependencies

.. code-block:: bash

  make install

4. Byte compile

.. code-block:: bash

  make build

5. Test in MELPA sandbox

.. code-block:: bash

  # In melpa git repo
  make clean
  make sandbox INSTALL=symex

6. For a new release, you can check that MELPA stable finds it

.. code-block:: bash

  # In melpa git repo
  STABLE=t make recipes/symex

Drafting a New Release
======================

1. Tag the release commit

.. code-block:: bash

  git tag -n  # list existing tags and annotations
  git tag -a <new version number> -m "<release message>"

2. Push the new tag to origin:

.. code-block:: bash

  git push --follow-tags  # push new tag to remote
