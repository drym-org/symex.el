;;; symex-rigpa.el --- Use Symex with Rigpa -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/drym-org/symex.el
;; Version: 2.0
;; Package-Requires: ((emacs "25.1") (symex "2.0") (rigpa "0.0"))
;; Keywords: lisp, convenience, languages

;; This program is "part of the world," in the sense described at
;; https://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.
;;
;; This work transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:

;; Use Symex with Rigpa.

;;; Code:

(require 'rigpa)
(require 'symex)
(require 'chimera)

;; These override the definitions in the symex package, so this
;; package (symex-evil) should be loaded after symex
(defun symex-escape-higher ()
  "Exit symex mode via an \"escape\"."
  (interactive)
  (rigpa-enter-higher-level))

(defun symex-enter-lower ()
  "Exit symex mode via an \"enter\"."
  (interactive)
  (rigpa-enter-lower-level))

(defun symex-enter-lowest ()
  "Enter the lowest (manual) editing level."
  (interactive)
  (rigpa-enter-lowest-level))

(defvar symex-chimera-mode
  (make-chimera-mode :name "symex"
                     :enter #'symex-mode-interface
                     :exit #'symex-editing-mode-exit
                     :pre-entry-hook 'symex-editing-mode-pre-entry-hook
                     :post-exit-hook 'symex-editing-mode-post-exit-hook
                     :entry-hook 'symex-editing-mode-post-entry-hook
                     :exit-hook 'symex-editing-mode-pre-exit-hook
                     :manage-hooks nil))

(defvar symex-rigpa--lisp-modes
  (append symex-lisp-modes
          ;; and some treesitter modes too
          '(clojure-ts-mode))
  "Modes where it should use the Lisp tower.")

;;;###autoload
(defun symex-rigpa-initialize ()
  "Rigpa interconnects for Symex."
  ;; TODO: return to tower instead. See
  ;; comment on line mode post exit
  (rigpa-register-mode symex-chimera-mode
                       :post-exit #'rigpa--enter-local-evil-state)
  (ht-set rigpa-lithium-modes
          'symex-editing-mode "symex")
  (defvar symex-rigpa-lisp-tower
    (make-editing-ensemble :name "lisp"
                           :default "symex"
                           :members (list chimera-insert-mode
                                          symex-chimera-mode
                                          chimera-normal-mode)))
  (setf (editing-ensemble-members rigpa-general-complex)
        (append (editing-ensemble-members rigpa-general-complex)
                (list symex-rigpa-lisp-tower)))
  (dolist (mode-name symex-rigpa--lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (add-hook mode-hook (lambda ()
                            (setq rigpa--current-tower-index 3)
                            (setq rigpa--current-level 2))))))


(defun symex-rigpa-disable ()
  "Disable Symex and Rigpa integration."
  nil)

;;;###autoload
(define-minor-mode symex-rigpa-mode
  "A mode seamlessly integrating Symex with Rigpa."
  :lighter " symex-rigpa"
  :global t
  :group 'symex
  (if symex-rigpa-mode
      (symex-rigpa-initialize)
    (symex-rigpa-disable)))

(provide 'symex-rigpa)
;;; symex-rigpa.el ends here
