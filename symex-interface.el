;;; symex-interface.el --- An evil way to edit Lisp symbolic expressions as trees  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1"))

;;; Code:

(defvar symex-interfaces '())

(defun symex-interface-add (major-mode methods-plist)
  (mapcar (lambda (k)
            (cl-assert (plist-get methods-plist k)
                       (concat "symex interface: missing method: " k) ))
          (list :eval
                :eval-definition
                :eval-pretty
                :eval-thunk
                :eval-print
                :describe-symbol
                :repl
                :run))
  (setq symex-interfaces
        (cons (cons major-mode methods-plist)
              symex-interfaces)))

(defun symex-interface-get-method (method)
  (let ((interface (alist-get major-mode symex-interfaces)))
    (or (plist-get interface method)
        (lambda () (error (concat "Symex mode: no method " method " for major-mode " major-mode ))))))

(provide 'symex-interface)
;;; symex-interface.el ends here
