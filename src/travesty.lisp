;;;; ***********************************************************************
;;;;
;;;; Name:          travesty.lisp
;;;; Project:       nym
;;;; Purpose:       generating names using travesties
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

;;; ---------------------------------------------------------------------
;;; CLASS travesty-map
;;; ---------------------------------------------------------------------

(defclass travesty-map ()
  ((samples :accessor samples :initform nil :initarg :samples)
   (triples :accessor triples :initform nil :initarg :triples)
   (starts :accessor starts :initform nil :initarg :starts)
   (parts :accessor parts :initform nil :initarg :parts)
   (ends :accessor ends :initform nil :initarg :ends)))

(defmethod initialize-instance :after ((map travesty-map) &rest initargs &key &allow-other-keys)
  (with-slots (samples triples starts parts ends) map
    (when samples
      (setf triples (loop for sample in samples collect (triples sample)))
      (setf starts (sort (remove-duplicates (remove nil (mapcar #'first triples)) :test #'equalp)
                         #'string<))
      (setf parts (sort (remove-duplicates (remove nil (apply #'append (mapcar #'drop-first triples))) :test #'equalp)
                        #'string<))
      (setf ends (sort (remove-duplicates (remove nil (mapcar #'last-element triples)) :test #'equalp)
                       #'string<)))))

;;; ---------------------------------------------------------------------
;;; reading sample names
;;; ---------------------------------------------------------------------

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
;;; (time (defparameter $tmap (make-instance 'travesty-map :samples $samples)))

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
  (choose-any (remove-if-not (lambda (part)(mergeable? start part))
                             (parts map))))

(defmethod choose-start ((map travesty-map))
  (choose-any (starts map)))

(defmethod generate-name ((map travesty-map) &optional start)
  (let* ((start (or start (choose-start map)))
         (next (find-extension map start)))
    (if next
        (if (find next (ends map) :test #'equal)
            (merge-parts start next)
            (generate-name map (merge-parts start next)))
        start)))

(defmethod generate-names ((map travesty-map)(count integer))
  (let ((gencount 0)
        (names nil))
    (block generating
      (loop
         (when (>= gencount count)
           (return-from generating
             (sort names #'string<)))
         (let ((next (generate-name map)))
           (unless (find next names :test #'equal)
             (push next names)
             (incf gencount)))))))

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/us.names")))
;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/dickens.names")))
;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names")))
;;; (time (defparameter $tmap (make-instance 'travesty-map :samples $samples)))
;;; (time (generate-name $tmap))
;;; (time (generate-names $tmap 100))
;;; (remove-if-not (lambda (nm)(char= #\W (elt nm 0))) (generate-names $tmap 100))
