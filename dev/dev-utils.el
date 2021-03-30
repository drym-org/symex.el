
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

(list 1 2 (3 4 5) (6 7) (8) 9)

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

((save-excursion (blah-blah)  ; (<>$
                 (etc))
 (blah-blah)
 (save-excursion
   (blah-blah t)))

((save-excursion (blah-blah)
                 (etc))  ; (<1>$
 (blah-blah)  ; (<2>$
 (save-excursion  ; (<3>$
   (blah-blah t)))

((save-excursion (blah-blah)
                 (etc)))
((save-excursion (blah-blah)
                 
                 
                 (etc))

(blah
  ;; hi
  asdf
  )


(blah
 sdf
 ;; hello
 )

(blah ([abc (blahblah #'etc 'hello)]
       [abc (blahblah #'etc 'hello)]
       [abc (blahblah #'etc 'hello)]))

(blah
 "../blah.etc"
 abc/pqr)
