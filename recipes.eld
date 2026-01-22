(
 ;; primary recipes
 (symex-core
  :host github
  :repo "drym-org/symex.el"
  :files ("symex-core/*.el"))

 (symex
  :host github
  :repo "drym-org/symex.el"
  :files ("symex/*.el" "symex/doc/*.texi" "symex/doc/figures"))

 (symex-ide
  :host github
  :repo "drym-org/symex.el"
  :files ("symex-ide/*.el"))

 (symex-evil
  :host github
  :repo "drym-org/symex.el"
  :files ("symex-evil/*.el"))

 (symex-rigpa
  :host github
  :repo "drym-org/symex.el"
  :files ("symex-rigpa/*.el"))

 ;; pointers (to dependencies)
 (rigpa
  :host github
  :repo "countvajhula/rigpa"))
