;;;; ***********************************************************************
;;;;
;;;; Name:          tries-travesty.lisp
;;;; Project:       nym
;;;; Purpose:       generating travesties from character-tries
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

(defmethod any-trie-name-start ((trie character-trie))
  (any (prefixes trie)))

;;; (setf $trie (character-trie))
;;; (setf $samples (read-names "/home/mikel/Workshop/src/nym/data/gnome.names"))
;;; (loop for nm in $samples do (character-trie-insert $trie nm))
;;; (time (fset:size (prefixes $trie)))
;;; (time (fset:size (nuclei $trie)))
;;; (time (fset:size (suffixes $trie)))
;;; (time (any (prefixes $trie)))

(defmethod find-trie-name-extensions ((trie character-trie)(prefix string))
  )

;;; (setf $trie (character-trie))
;;; (setf $samples (read-names "/home/mikel/Workshop/src/nym/data/gnome.names"))
;;; (loop for nm in $samples do (character-trie-insert $trie nm))
;;; (setf $start (any-trie-name-start $trie))
;;; (find-trie-name-extensions $trie $start)
