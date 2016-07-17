;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       nym
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defmethod choose-any ((seq sequence))
  (elt seq (random (length seq))))

(defmethod sum-lengths ((seqs null))
  0)

(defmethod sum-lengths ((seqs cons))
  (loop for seq in seqs sum (length seq)))

(defun join (strings)
  (reduce (lambda (s1 s2)(concatenate 'string s1 s2))
          strings :initial-value ""))

(defmethod take ((n integer)(s string))
  (if (< (length s) n)
      s
      (subseq s 0 n)))

;;; (take 3 "food")
;;; (take 3 "foo")
;;; (take 3 "fo")
;;; (take 3 "")
