;;;; ***********************************************************************
;;;;
;;;; Name:          triples-travesty.lisp
;;;; Project:       nym
;;;; Purpose:       generating names using travesties
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

;;; ---------------------------------------------------------------------
;;; CLASS travesty-map
;;; ---------------------------------------------------------------------

(defclass travesty-map ()
  ((samples :accessor samples :initform nil :initarg :samples)
   (triples :accessor triples :initform nil :initarg :triples)
   (starts :accessor starts :initform nil :initarg :starts)
   (parts :accessor parts :initform nil :initarg :parts)
   (ends :accessor ends :initform nil :initarg :ends)))

(defun tidy (namelist)
  (sort (remove-duplicates
         (remove nil namelist)
         :test #'equalp)
        #'string<))

(defmethod initialize-instance :after ((map travesty-map)
                                       &rest initargs
                                       &key &allow-other-keys)
  (with-slots (samples triples starts parts ends) map
    (when samples
      (setf triples (loop for sample in samples collect (triples sample)))
      (setf starts (tidy (mapcar #'first triples)))
      (setf parts (tidy (apply #'append (mapcar #'drop-first triples))))
      (setf ends (tidy (mapcar #'last-element triples))))))

(defun make-travesty-map (name-list)
  (make-instance 'travesty-map :samples name-list))

;;; ---------------------------------------------------------------------
;;; generating names
;;; ---------------------------------------------------------------------

(defmethod mergeable? ((left string)(right string))
  (let ((left-len (length left)))
    (string= left right
             :start1 (- left-len 2) :end1 left-len
             :start2 0 :end2 2)))

(defmethod merge-parts ((left string)(right string))
  (concatenate 'string left (subseq right 2)))

(defmethod find-extension ((map travesty-map)(start string))
  (any (remove-if-not (lambda (part)(mergeable? start part))
                             (parts map))))

(defmethod choose-start ((map travesty-map))
  (any (starts map)))

(defmethod generate-name ((map travesty-map) &optional start)
  (let* ((start (or start (choose-start map)))
         (next (find-extension map start)))
    (if next
        (if (find next (ends map) :test #'equal)
            (merge-parts start next)
            (generate-name map (merge-parts start next)))
        start)))

(defmethod generate-names ((map travesty-map)(count integer))
  (loop for i from 0 below count collect (generate-name map)))

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (time (defparameter $samples (read-names "~/Workshop/src/nym/data/gnome.names")))

;;; (time (defparameter $tmap (make-instance 'travesty-map :samples $samples)))
;;; (time (generate-names $tmap 50))
