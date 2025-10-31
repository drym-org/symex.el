((symex-core
  :repo nil
  :host nil
  :local-repo "."
  :type git
  :files ("symex-core/*.el"))

 (symex
  :repo nil
  :host nil
  :local-repo "."
  :type git
  :files ("symex/*.el" "symex/doc/*.texi" "symex/doc/figures"))

 (symex-ide
  :repo nil
  :host nil
  :local-repo "."
  :type git
  :files ("symex-ide/*.el"))

 (symex-evil
  :repo nil
  :host nil
  :local-repo "."
  :type git
  :files ("symex-evil/*.el"))

 (symex-rigpa
  :repo nil
  :host nil
  :local-repo "."
  :type git
  :files ("symex-rigpa/*.el"))

 (rigpa
  :host github
  :repo "countvajhula/rigpa"
  :type git)

 (package-lint
  :host github
  :repo "countvajhula/package-lint"
  ;; Ensure the necessary data files are included in the build.
  :files ("*.el" "data")))
