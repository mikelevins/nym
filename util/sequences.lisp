;;;; ***********************************************************************
;;;;
;;;; Name:          sequences.lisp
;;;; Project:       nym
;;;; Purpose:       sequence helpers
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defmethod any ((seq sequence))
  (let ((len (length seq)))
    (cond
      ((< len 1) nil)
      ((= len 1) (first seq))
      (t (elt seq (random len))))))

(defmethod any ((seq fset:wb-seq))
  (let ((len (fset:size seq)))
    (cond
      ((< len 1) nil)
      ((= len 1) (fset:@ seq 0))
      (t (fset:@ seq (random len))))))

(defmethod chunk-sequence ((seq sequence) (chunk-size integer))
  (take-by chunk-size 1 seq))

(defmethod drop-first ((thing null)) nil)
(defmethod drop-first ((thing list))
  (cdr thing))

(defmethod drop-last ((thing null)) nil)
(defmethod drop-last ((thing list))
  (subseq thing
          0 (1- (length thing))))

(defmethod empty? ((thing null)) t)
(defmethod empty? ((thing sequence))
  (zerop (length thing)))

(defun filter (fn sequence)
  (remove-if-not fn sequence))

(defmethod first-element ((thing sequence))
  (elt thing 0))

(defmethod second-element ((thing sequence))
  (elt thing 1))

(defmethod last-element ((thing list))
  (first (last thing)))

(defmethod last-element ((thing sequence))
  (elt thing (1- (length thing))))

(defmethod next-last-element ((thing sequence))
  (elt thing (- (length thing) 2)))

(defmethod last-n-elements ((thing sequence)(n integer))
  (subseq thing (- (length thing) n) (length thing)))

(defmethod leave ((n integer)(s sequence))
  (if (< (length s) n)
      s
      (subseq s (- (length s) n))))

(defun range (start end &optional (by 1))
  (loop for i from start below end by by collect i))

(defmethod take-by ((count integer)(step integer)(seq sequence))
  (loop for i from 0 below (- (length seq) (1- count)) by step
     collect (subseq seq i (+ i count))))

