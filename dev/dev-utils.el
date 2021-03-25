
;;; Utilities for development and testing

;; test different evil keybindings - nil to pass through
(setq symex--user-evil-keyspec
      '(("C-," . symex-leap-backward)
        ("C-/" . symex-leap-forward)
        ("C-M-," . symex-soar-backward)
        ("C-M-/" . symex-soar-forward)))

(let ((keyspec (symex--combine-alists symex--user-evil-keyspec
                                      symex--evil-keyspec)))
  (symex--define-evil-keys-from-spec keyspec
                                     symex-editing-mode-map))

