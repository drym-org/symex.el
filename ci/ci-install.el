;; ci-install.el
;; This script bootstraps straight.el and installs all package dependencies.

(defvar straight-base-dir (expand-file-name "ci-init"))

;; --- Bootstrap straight.el ---
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        straight-base-dir))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-allow-recipe-inheritance nil)

;; --- Install all packages ---
(message "--- Installing packages ---")

(straight-use-package
 `(symex-core :local-repo ,(expand-file-name "symex" "../../") :files ("symex-core/*.el")))

(straight-use-package
 `(symex :local-repo ,(expand-file-name "symex" "../../") :files ("symex/*.el")))

(straight-use-package
 `(symex-ide :local-repo ,(expand-file-name "symex" "../../") :files ("symex-ide/*.el")))

(straight-use-package
 `(symex-evil :local-repo ,(expand-file-name "symex" "../../") :files ("symex-evil/*.el")))

;; (straight-use-package
;;  `(rigpa :repo "countvajhula/rigpa" :host github :type git))

;; (straight-use-package
;;  `(symex-rigpa :repo nil :host nil :local-repo ,(expand-file-name "symex.el" "..") :type git :files ("symex-rigpa/*.el")))

(message "--- Package installation complete ---")
