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

(defmethod prefix-match? ((left string)(right string))
  (cond ((zerop (length left)) t)
        ((< (length right)(length left)) nil)
        (t (and (block searching
                  (loop for i from 0 below (length left)
                     do (unless (char= (elt left i)
                                       (elt right i))
                          (return-from searching nil)))
                  t)
                t))))

(defmethod suffix-match? ((left string)(right string))
  (cond ((zerop (length left)) t)
        ((< (length right)(length left)) nil)
        (t (and (block searching
                  (let ((leftlen (length left))
                        (rightlen (length right)))
                    (loop for i from 0 below leftlen
                       do (let ((left-index (- leftlen i 1))
                                (right-index (- rightlen i 1)))
                            (unless (char= (elt left left-index)
                                           (elt right right-index))
                              (return-from searching nil)))))
                  t)
                t))))

(defmethod triples ((str string))
  (loop for i from 0 below (- (length str) 2)
     collect (subseq str i (+ i 3))))

(defmethod whitespace? ((ch character))
  (member ch '(#\space #\tab #\return #\linefeed)
          :test #'char=))

