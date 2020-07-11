;;;; ***********************************************************************
;;;;
;;;; Name:          character-tries.lisp
;;;; Project:       nym
;;;; Purpose:       representing generator options as tries of alternative sequences
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

;;; ---------------------------------------------------------------------
;;; NOTES
;;; ---------------------------------------------------------------------

(defmethod character-trie ((text null))
  (fset:map))

(defmethod character-trie ((text list))
  (fset:with (character-trie nil)
             (first text)
             (character-trie (rest text))))

(defmethod character-trie ((text string))
  (character-trie (coerce text 'list)))

(defmethod subtrie-at ((trie null) text)
  trie)

(defmethod subtrie-at ((trie fset:wb-map)(text null))
  trie)

(defmethod subtrie-at ((trie fset:wb-map)(text list))
  (subtrie-at (fset:lookup trie (first text))
              (rest text)))

(defmethod subtrie-at ((trie fset:wb-map)(text string))
  (subtrie-at trie (coerce text 'list)))

;;; (setf $trie0 (character-trie nil))
;;; (setf $trie1 (character-trie "A"))
;;; (setf $trie2 (character-trie "AB"))
;;; (setf $trie3 (character-trie "ABC"))
;;; (subtrie-at $trie3 "AB")
