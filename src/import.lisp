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
    (sort filtered #'string<)))

(defmethod read-names ((filename string))
  (read-names (pathname filename)))

;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/clnamer/us.names")))
