;;; symex-interface.el --- An evil way to edit Lisp symbolic expressions as trees  -*- lexical-binding: t; -*-

;; URL: https://github.com/drym-org/symex.el


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

;; Miscellaneous Lisp editing-related features

;;; Code:

(defvar symex-interfaces '())

(defvar symex-methods
  (list :eval
        :eval-definition
        :eval-pretty
        :eval-thunk
        :eval-print
        :describe-symbol
        :repl
        :run
        :switch-to-scratch-buffer))

(defun symex-interface--plist-keys (plist)
  "Return a list of keys in the property list PLIST."
  (let (keys)
    (while plist
      (setq keys (cons (car plist) keys))
      (setq plist (cddr plist)))
    (reverse keys)))

(defun symex-interface-check (interface)
  "Assert that INTERFACE is a valid symex interface."
  (mapc (lambda (k)
          (cl-assert (member k symex-methods)
                     (concat "symex interface: unknown method: " (symbol-name k)) ))
        (symex-interface--plist-keys interface)))

(defun symex-interface-extend (modes interface)
  "Extend symex MODES with given INTERFACE."
  (symex-interface-check interface)
  (setq symex-interfaces
       (append (mapcar (lambda (m) (cons m interface)) modes)
                symex-interfaces)))

(defun symex-interface-get-method (method)
  "Find a symex interface METHOD for the current major-mode."
  (let ((interface (alist-get major-mode symex-interfaces)))
    (or (plist-get interface method)
        (lambda () (error (concat "Symex mode: no method " (symbol-name method) " for major-mode " (symbol-name major-mode) ))))))

(provide 'symex-interface)
;;; symex-interface.el ends here
