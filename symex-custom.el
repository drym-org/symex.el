;;; symex-custom.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex.el

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
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
;;
;; User Customization configuration
;;

;;; Code:


(defgroup symex nil
  "A language for editing symbolic expressions."
  :group 'lisp)

(defcustom symex-highlight-p nil
  "Whether selected symexes should be highlighted."
  :type 'boolean
  :group 'symex)

(defcustom symex-refocus-p t
  "Whether to refocus on the selected symex when it's close to the edge of the screen."
  :type 'boolean
  :group 'symex)

(defcustom symex-remember-branch-positions-p t
  "Whether movement in the vertical direction should remember branch positions."
  :type 'boolean
  :group 'symex)

(defcustom symex-modal-backend 'evil
  "Whether to use hydra or evil as the backend for the modal interface."
  :type 'symbol
  :group 'symex)


(provide 'symex-custom)
;;; symex-custom.el ends here
