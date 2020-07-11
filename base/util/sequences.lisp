;;;; ***********************************************************************
;;;;
;;;; Name:          sequences.lisp
;;;; Project:       nym
;;;; Purpose:       sequence helpers
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

(defmethod any ((seq sequence))
  (let ((len (length seq)))
    (cond
      ((< len 1) nil)
      ((= len 1) (first seq))
      (t (elt seq
              (random (length seq)))))))

(defmethod any ((set fset:wb-set))
  (let ((len (fset:size set)))
    (cond
      ((< len 1) nil)
      ((= len 1) (fset:at-rank set 0))
      (t (fset:at-rank set
                 (random len))))))

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

(defmethod last-element ((thing list))
  (first (last thing)))

(defun range (start end &optional (by 1))
  (loop for i from start below end by by collect i))

(defmethod take-by ((count integer)(step integer)(seq sequence))
  (loop for i from 0 below (- (length seq) (1- count)) by step
     collect (subseq seq i (+ i count))))

