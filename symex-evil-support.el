(require 'evil)


(defun symex--define-evil-key (key fn map)
  "Define a keybinding in the symex evil state."
  (evil-define-key (list 'symex 'visual 'operator)
                   map
                   (kbd key)
                   fn))

(defun symex--define-evil-keys-from-spec (keyspec keymap)
  "Define evil keys from a specification."
  (dolist (keybinding keyspec)
    (symex--define-evil-key (car keybinding)
                            (cdr keybinding)
                            keymap)))

(provide 'symex-evil-support)
;;; symex-evil-support.el ends here
