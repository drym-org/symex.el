
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


;; Symex-TS hydra: useful for debugging Tree Sitter movements outside
;; of the rest of Symex.

(defun symex-ts--hydra-exit ()
  "Handle Hydra exit."
  (symex-ts--delete-overlay))

(defhydra hydra-symex-ts (:post (symex-ts--hydra-exit))
  "Symex-TS."
  ("d" symex-ts-current-node-sexp "DEBUG NODE")

  ("h" symex-ts-move-prev-sibling "prev")
  ("l" symex-ts-move-next-sibling "next")
  ("j" symex-ts-move-parent "parent")
  ("k" symex-ts-move-child "child"))

(defun symex-ts-launch ()
  "Start the Symex-TS hydra."
  (interactive)

  ;; Set the current node to the top-most node at point
  (symex-ts-set-current-node-from-point)

  ;; Launch hydra
  (hydra-symex-ts/body))


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
