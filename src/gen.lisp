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

(defmethod find-extension ((start string))
  (choose-any (remove-if-not (lambda (part)(mergeable? start part))
                             *sample-parts*)))

(defun choose-start ()
  (choose-any *sample-starts*))

(defun generate-name (&optional start)
  (let* ((start (or start (choose-start)))
         (next (find-extension start)))
    (if next
        (if (find next *sample-ends* :test #'equal)
            (merge-parts start next)
            (generate-name (merge-parts start next)))
        start)))

(defmethod generate-names ((count integer))
  (let ((gencount 0)
        (names nil))
    (block generating
      (loop
         (when (>= gencount count)
           (return-from generating
             (sort names #'string<)))
         (let ((next (generate-name)))
           (unless (find next names :test #'equal)
             (push next names)
             (incf gencount)))))))

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (time (read-names "/Users/mikel/Workshop/src/nym/data/us.names"))
;;; (time (read-names "/Users/mikel/Workshop/src/nym/data/dickens.names"))
;;; (time (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names"))
;;; (time (generate-name))
;;; (time (generate-names 100))
