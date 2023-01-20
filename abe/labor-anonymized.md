* The package was named.
* The initial prototype was written using Hydra, Lispy, Paredit.
* The DSL was designed.
* The traversal-based features unique to symex were designed and implemented.
* There was a suggestion to call the package Symex instead of Symex mode.
* A code review was done and some implementation improvements were suggested.
* An in-person meetup was hosted in SF.
* The video from the meetup was edited and posted on youtube
* Basic docs were written.
* The modal interface was implemented in a more robust way using evil.
* CI workflows including melpazoid were added.
* There was a suggestion to generalize Symex to work with non-lisp languages by using tree-sitter
* An early proof-of-concept of using tree-sitter was written.
* A more complete proof-of-concept using tree-sitter was written.
* The proof of concept was turned into a full implementation.
* The tree-sitter functionality was tested.
* A lightweight UI feedback overlay was added for tree-sitter
* The overlay was generalized for use in lisp
* Many early issues were reported.
* There was a suggestion to add a table to the docs documenting keybindings.
* A keybinding table was added to the docs.
* There was a suggestion to add "demo gifs" / an animated guide
* The animated guide was created.
* A bug was reported in yank/paste with quoted symbols
* There was a suggestion for a useful keybinding recipe for interoperating with Evil
* A generic ecosystem/config-related issue with test modules providing features that conflict in the global namespace was reported
* There was a bug reported in join-lines-backwards
* A deprecated bash dependency was updated
* A typo was fixed in the readme
* Parser support for clojure(script) deref reader macro was added
* A failing melpazoid config was fixed
* UX was unified across clojure (i.e. clojure, clojurescript, clojurec) modes
* Symex's override of evil's : keybinding was removed
* Support was added for evil's . (dot/repeat) operator in symex state
* Support was added for pretty printing in clojure
* SLY support was added for common lisp
* Dependency on undo-tree was removed
