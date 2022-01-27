Development Workflows
=====================

.. contents:: :depth: 1

Build steps
-----------

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

Drafting a New Release
----------------------

1. Bump the version in the :code:`symex.el` header and commit the changes

.. code-block:: elisp

  ;; Version: i.j.k [use MAJOR.MINOR.PATCH]

2. Tag the release commit

.. code-block:: bash

  git tag -n  # list existing tags and annotations
  git tag -a <new version number>  # add the release notes when prompted

3. Push the new tag to origin:

.. code-block:: bash

  git push --follow-tags  # push new tag to remote

4. Test in MELPA sandbox

.. code-block:: bash

  # In melpa git repo
  make clean
  make recipes/symex
  make sandbox INSTALL=symex

5. For a new release, you can check that MELPA stable finds it

.. code-block:: bash

  # In melpa git repo
  STABLE=t make recipes/symex

Collaborating on a PR
---------------------

Using an Integration Branch
```````````````````````````

When collaborating on project, it is recommended that each component of the project be done as a pull request against a shared "integration" branch, which itself should have an open PR against the master branch. When all of the components are in, and the PR for the integration branch has been reviewed and is considered complete, the integration branch can be merged into the master branch.

Rebasing the Integration Branch
```````````````````````````````

For projects using integration branches, the integration branch should periodically be rebased onto the master branch to ensure that it doesn't diverge significantly and complicate the eventual merge. **This rebase should only be done if there are no open pull requests against the integration branch**. Even if there are no open pull requests, as a courtesy, participants in the integration branch should ideally be given advance warning of an impending rebase in case they happen to have active branches that don't have associated pull requests yet, to give everyone an opportunity to merge in such work before the rebase.

The integration branch should be rebased one final time prior to merging it into master.
