
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

((save-excursion (evil-last-non-blank)  ; (<>$
                 (lispy-left-p))
 (symex--go-forward-to-start)
 (save-excursion
   (symex--join-lines t)))

((save-excursion (evil-last-non-blank)
                 (lispy-left-p))  ; (<1>$
 (symex--go-forward-to-start)  ; (<2>$
 (save-excursion  ; (<3>$
   (symex--join-lines t)))
