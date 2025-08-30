;; ci-install.el
;; This script bootstraps straight.el and installs all package dependencies.
;; -*- lexical-binding: t -*-
;;
;; Note: some flags passed to checkdoc and lint from a calling script
;; (e.g., this one) typically presuppose dynamic binding, but in the
;; present case we're running those tools as subprocesses, so they
;; should use the default dynamic binding, even though this script
;; uses lexical binding.

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

;; The repository root is the parent directory of the `ci/` directory
;; where this script is located.
(let ((repo-root (expand-file-name "..")))
  (straight-use-package
   `(symex-core :local-repo ,repo-root :files ("symex-core/*.el")))

  (straight-use-package
   `(symex :local-repo ,repo-root :files ("symex/*.el")))

  (straight-use-package
   `(symex-ide :local-repo ,repo-root :files ("symex-ide/*.el")))

  (straight-use-package
   `(symex-evil :local-repo ,repo-root :files ("symex-evil/*.el")))

  (straight-use-package
   `(rigpa :repo "countvajhula/rigpa" :host github :type git))

  (straight-use-package
   `(symex-rigpa :local-repo ,repo-root :files ("symex-rigpa/*.el")))

  (message "--- Package installation complete ---"))
