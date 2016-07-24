;;;; ***********************************************************************
;;;;
;;;; Name:          import.lisp
;;;; Project:       nym
;;;; Purpose:       reading names from a text file
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defparameter *samples* nil)
(defparameter *sample-triples* nil)
(defparameter *sample-starts* nil)
(defparameter *sample-parts* nil)
(defparameter *sample-ends* nil)

(defun empty-name? (str)
  (let ((trimmed (string-trim '(#\space) str)))
    (or (string= trimmed "")
        (char= #\# (elt trimmed 0)))))

(defmethod read-names ((filename pathname))
  (let* ((lines (with-open-file (in filename)
                  (loop for line = (read-line in nil nil nil) then (read-line in nil nil nil)
                     while line
                     collect line)))
         ;; filter out empty and comment lines
         (filtered (remove-if #'empty-name?
                              lines)))
    ;; sort the output
    (setf *samples* (sort filtered #'string<))
    ;; parse the names
    (setf *sample-triples* (loop for sample in *samples* collect (triples sample)))
    (setf *sample-starts* (sort (remove-duplicates (remove nil (mapcar #'first *sample-triples*)) :test #'equalp)
                                #'string<))
    (setf *sample-parts* (sort (remove-duplicates (remove nil (apply #'append (mapcar #'drop-first *sample-triples*))) :test #'equalp)
                               #'string<))
    (setf *sample-ends* (sort (remove-duplicates (remove nil (mapcar #'last-element *sample-triples*)) :test #'equalp)
                              #'string<))
    *samples*))

(defmethod read-names ((filename string))
  (read-names (pathname filename)))

;;; (time (read-names "/Users/mikel/Workshop/src/clnamer/us.names"))
;;; *samples*
;;; *sample-triples*
;;; *sample-starts*
;;; *sample-parts*
;;; *sample-ends*


