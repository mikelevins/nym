;;;; ***********************************************************************
;;;;
;;;; Name:          strings.lisp
;;;; Project:       nym
;;;; Purpose:       string helpers
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

(defmethod empty-line? ((s string))
  (or (zerop (length s))
      (every #'whitespace? s)))

(defmethod triples ((str string))
  (loop for i from 0 below (- (length str) 2)
     collect (subseq str i (+ i 3))))

(defmethod whitespace? ((ch character))
  (member ch '(#\space #\tab #\return #\linefeed)
          :test #'char=))

