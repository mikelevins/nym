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

(defun choose-any (sequence)
  (let ((len (length sequence)))
    (cond
      ((< len 1) nil)
      ((= len 1) (first sequence))
      (t (elt sequence
              (random (length sequence)))))))

(defmethod drop-first ((thing null)) nil)
(defmethod drop-first ((thing list))
  (cdr thing))

(defmethod empty? ((thing null)) t)
(defmethod empty? ((thing sequence))
  (zerop (length thing)))

(defmethod last-element ((thing list))
  (first (last thing)))
