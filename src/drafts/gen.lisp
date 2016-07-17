;;;; ***********************************************************************
;;;;
;;;; Name:          gen.lisp
;;;; Project:       nym
;;;; Purpose:       generating names
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defmethod mergeable? ((left string)(right string))
  (let ((left-len (length left)))
    (string= left right
             :start1 (- left-len 2) :end1 left-len
             :start2 0 :end2 2)))

(defmethod merge-parts ((left string)(right string))
  (concatenate 'string left (subseq right 2)))

(defun any (sequence)
  (let ((len (length sequence)))
    (cond
      ((< len 1) nil)
      ((= len 1) (first sequence))
      (t (elt sequence
              (random (length sequence)))))))

(defmethod find-extension ((start string)(parts list))
  (any (remove-if-not (lambda (part)(mergeable? start part))
                  parts)))

(defmethod generate-name ((start string)(parts list)(ends list))
  (let ((next (find-extension start parts)))
    (if next
        (if (find next ends :test #'equalp)
            (merge-parts start next)
            (generate-name (merge-parts start next)
                           parts ends))
        start)))

(defmethod generate-names ((count integer)(starts list)(parts list)(ends list))
  (loop for i from 0 below count
     collect (generate-name (any starts) parts ends)))

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (defparameter $names (read-names "/Users/mikel/Workshop/src/nym/data/us.names"))
;;; (defparameter $names (read-names "/Users/mikel/Workshop/src/nym/data/dickens.names"))
;;; (defparameter $names (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names"))
;;; (defparameter $parse (parse-names $names))
;;; (defparameter $starts (first $parse))
;;; (defparameter $parts (second $parse))
;;; (defparameter $ends (third $parse))
;;; (generate-name (any $starts) $parts $ends)
;;; (generate-names 100 $starts $parts $ends)
