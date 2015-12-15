;;;; ***********************************************************************
;;;;
;;;; Name:          names.lisp
;;;; Project:       nym
;;;; Purpose:       name-data operations
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defmethod part-long-enough? ((s string))
  (> (length s) 2))

(defmethod triples ((str string))
  (loop for i from 0 below (- (length str) 2)
     collect (subseq str i (+ i 3))))

(defmethod last-element ((thing list))
  (first (last thing)))

(defmethod drop-first ((thing null)) nil)
(defmethod drop-first ((thing list))
  (cdr thing))

(defmethod parts ((thing list))
  (drop-first thing))

;;; (triples "")
;;; (triples "A")
;;; (triples "AB")
;;; (triples "ABC")
;;; (triples "ABCD")

(defun parse-names (names)
  (let* ((triples (loop for name in names
                     collect (triples name)))
         (starts (sort (remove-duplicates (mapcar #'first triples) :test #'equalp)
                       #'string<))
         (parts (sort (remove-duplicates (apply #'append (mapcar #'drop-first triples)) :test #'equalp)
                      #'string<))
         (ends (sort (remove-duplicates (mapcar #'last-element triples) :test #'equalp)
                     #'string<)))
    (list starts parts ends)))

;;; (time (defparameter $names (read-names "/Users/mikel/Workshop/src/clnamer/us.names")))
;;; (time (defparameter $samples (parse-names $samples)))
