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
;;; ^ denotes the beginning of a character sequence.
;;; $ denotes the end of a character sequence.
;;;
;;; When generating a new string, candidate sequences must start with
;;; some character that appears directly after a '^' in the tree. They
;;; must end with some character that is directly followed by a '$'.
;;; Characters between '^' and '$' must be placed in sequences that
;;; are represented in the trie.
;;;
;;; For example, consider the following trie:
;;;
;;; (^ (C (a (t ($)))))
;;;
;;; This tree can generate "Cat" and only "Cat".  All strings that it
;;; generates must begin with the letter "C". No other letter
;;; (including "c") is allowed at the start, because no other letter
;;; follows "^" in the trie.
;;;
;;; The only letter that can follow "C" is "a". The only letter that
;;; can follow "a" is "t", and no letter may follow "t" because the
;;; only datum that follows "t" in the trie is the end marker, "$".
;;;
;;; Here's a trie that can generate "Cat" or "Bat":
;;;
;;; (^ (B (a (t ($))))
;;;    (C (a (t ($)))))
;;;
;;; ...and here is a variation that can generate "Box", too:
;;;
;;; (^ (B (a (t ($)))
;;;       (o (x ($))))
;;;    (C (a (t ($)))))

(defclass character-trie ()
  ((prefixes :reader %prefixes :initform (make-hash-table))
   (nuclei :reader %nuclei :initform (make-hash-table))
   (suffixes :reader %suffixes :initform (make-hash-table))))

(defmethod print-object ((trie character-trie)(out stream))
  (print-unreadable-object (trie out :type t)
    (format out "prefixes[~A], nuclei[~A], suffixes[~A]"
            (hash-table-count (%prefixes trie))
            (hash-table-count (%nuclei trie))
            (hash-table-count (%suffixes trie)))))

(defun character-trie ()(make-instance 'character-trie))

;;; (character-trie)

(defmethod prefixes ((trie character-trie))
  (let ((result nil))
    (maphash (lambda (key val) (push key result))
             (%prefixes trie))
    (reverse result)))

(defmethod nuclei ((trie character-trie))
  (let ((result nil))
    (maphash (lambda (key val) (push key result))
             (%nuclei trie))
    (reverse result)))

(defmethod suffixes ((trie character-trie))
  (let ((result nil))
    (maphash (lambda (key val) (push key result))
             (%suffixes trie))
    (reverse result)))

(defmethod initials ((text string))
  (let* ((characters (coerce text 'list))
         (charlists (loop for i from 1 below (length characters)
                       collect (subseq characters 0 i))))
    (mapcar (lambda (cl)(coerce cl 'string))
            charlists)))

;;; (initials "Fred")

(defmethod finals ((text string))
  (let* ((characters (coerce text 'list))
         (len (length characters))
         (charlists (loop for i from 1 below (length characters)
                       collect (subseq characters i len))))
    (mapcar (lambda (cl)(coerce cl 'string))
            charlists)))

;;; (finals "Fred")

(defmethod middles ((text string))
  (let* ((characters (drop-first (drop-last (coerce text 'list))))
         (subseqs (reduce #'append
                          (loop for count from 2 below (1+ (length characters))
                             collect (take-by count 1 characters)))))
    (mapcar (lambda (s)(coerce s 'string))
            subseqs)))

;;; (middles "Fred")
;;; (middles "Davey")
;;; (middles "Jonathan")

(defmethod add-prefix ((trie character-trie) (pref string))
  (setf (gethash pref (%prefixes trie))
        (1+ (gethash pref (%prefixes trie) 0))))

(defmethod add-nucleus ((trie character-trie) (nuc string))
  (setf (gethash nuc (%nuclei trie))
        (1+ (gethash nuc (%nuclei trie) 0))))

(defmethod add-suffix ((trie character-trie) (suf string))
  (setf (gethash suf (%suffixes trie))
        (1+ (gethash suf (%suffixes trie) 0))))

(defmethod character-trie-insert ((trie character-trie)(text string))
  (let ((prefixes (initials text))
        (nuclei (middles text))
        (suffixes (finals text)))
    (loop for pref in prefixes do (add-prefix trie pref))
    (loop for nuc in nuclei do (add-nucleus trie nuc))
    (loop for suf in suffixes do (add-suffix trie suf))
    trie))

;;; (setf $trie (character-trie))
;;; (character-trie-insert $trie "Fred")
;;; (prefixes $trie)
;;; (nuclei $trie)
;;; (suffixes $trie)
