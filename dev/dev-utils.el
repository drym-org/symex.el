
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


;; Some edge-casey test data

(list 1 2 '(1 2 3) 4)
(list 1 2 #(1 2 3) 4)
(list 1 2 #'(1 2 3) 4)
(list 1 2 '(3) 4)
(list 1 2 #(3) 4)
(list 1 2 #'(3) 4)
(list 1 2 '() 4)
(list 1 2 #() 4)
(list 1 2 #'() 4)

#'() 4

((a 'a)
 (b 'b))
