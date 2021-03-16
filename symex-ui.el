
(defun symex--toggle-highlight ()
  "Toggle highlighting of selected symex."
  (interactive)
  (if mark-active
      (deactivate-mark)
    (mark-sexp))
  (setq symex-highlight-p
        (not symex-highlight-p)))

(provide 'symex-ui)
;;; symex-ui.el ends here
