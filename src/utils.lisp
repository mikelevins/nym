;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       nym
;;;; Purpose:       general helpers
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defun choose-any (sequence)
  (let ((len (length sequence)))
    (cond
      ((< len 1) nil)
      ((= len 1) (first sequence))
      (t (elt sequence
              (random (length sequence)))))))

(defmethod triples ((str string))
  (loop for i from 0 below (- (length str) 2)
     collect (subseq str i (+ i 3))))

(defmethod last-element ((thing list))
  (first (last thing)))

(defmethod drop-first ((thing null)) nil)
(defmethod drop-first ((thing list))
  (cdr thing))

;;; (triples "")
;;; (triples "A")
;;; (triples "AB")
;;; (triples "ABC")
;;; (triples "ABCD")

;;; (time (defparameter $names (read-names "/Users/mikel/Workshop/src/clnamer/us.names")))
;;; (time (defparameter $samples (parse-names $names)))
