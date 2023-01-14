* Ariana named the package
* Sid wrote the initial prototype using Hydra, Lispy, Paredit
* Sid designed the DSL
* Sid designed and implemented traversal-based features unique to symex
* Riscy suggested calling the package Symex instead of Symex mode
* Riscy did a code review and suggested some implementation improvements
* Jeff hosted an in-person meetup in SF
* Jeff edited and posted the Symex video on youtube
* Sid wrote basic docs
* Sid added evil modal interface support
* Sid added CI workflows including melpazoid
* Anonymous suggested generalizing Symex to work with non-lisp languages by using tree-sitter
* Tommy wrote an early proof-of-concept of using tree-sitter
* Simon wrote a more complete proof-of-concept using tree-sitter
* Simon and Sid turned the proof of concept into a full implementation
* Simon added a lightweight UI feedback overlay for tree-sitter
* Sid generalized the overlay for use in lisp
* Pepperblue reported many early issues
* Tommy suggested "demo gifs" / animated guide
* Sid created the animated guide
* MarkgDawson reported a bug in yank/paste with quoted symbols
* Tommy suggested a keybinding recipe for Insert/Normal/Symex
* Tarsius reported a generic ecosystem/config-related issue with generic test modules providing features that conflict in the global namespace
* MarkgDawson reported a bug in join-lines-backwards
* BasilConto updated deprecated bash dependency
* Markgdawson fixed a typo in the readme
* dcostaras added parser support for clojure(script) deref reader macro
* dcostaras fixed failing melpazoid config
* anonimitoraf unified UX across clojure (i.e. clojure, clojurescript, clojurec) modes
* tommy removed override of evil's : keybinding
* j-shilling added support for . (dot) operator in symex state
* markgdawson added support for pretty printing in clojure
* Doyougnu added SLY support for common lisp
* devcarbon removed dependency on undo-tree
