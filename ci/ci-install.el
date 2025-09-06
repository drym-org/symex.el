;; ci-install.el
;; This script bootstraps straight.el and installs all package dependencies.
;; -*- lexical-binding: t -*-

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

;; Load the shared CI helper functions and constants.
(require 'ci-helpers (expand-file-name "ci-helpers.el"))

;; --- Install external dependencies ---
(message "--- Installing external dependencies ---")

;; First, install any external dependencies that are not part of the present repo.
;; This section can be customized for each project.

;; This is only needed for building/testing symex-rigpa
(straight-use-package
 '(rigpa :host github :repo "countvajhula/rigpa"))

;; --- Install all packages ---
(message "--- Installing packages ---")

;; Next, install all packages from the local repo by looping
;; over the `ci-packages` variable.
(let ((repo-root (expand-file-name "..")))
  (dolist (pkg ci-packages)
    ;; Construct the recipe dynamically for each package in the repo.
    (straight-use-package
     `(,(intern pkg) :local-repo ,repo-root :files (,(format "%s/*.el" pkg))))))

(message "--- Package installation complete ---")
