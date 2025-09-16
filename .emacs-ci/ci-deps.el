;; ci-deps.el
;; This script defines external, third-party dependencies for the project.
;; It is intended to be `require`'d by the main `ci-install.el` script.
;; -*- lexical-binding: t -*-

(message "--- Defining external dependencies ---")

;; Install any third-party packages required by the project.
;; This is the place to add packages from MELPA, GitHub, etc.
(straight-use-package
 '(rigpa :host github :repo "countvajhula/rigpa" :type git))

;; This makes the file a loadable library.
(provide 'ci-deps)
