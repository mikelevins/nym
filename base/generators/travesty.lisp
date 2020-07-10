;;;; ***********************************************************************
;;;;
;;;; Name:          travesty.lisp
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
  (loop for i from 0 below count collect (generate-name map)))


;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/us.names")))
;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/dickens.names")))
;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names")))
;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/goblin.names")))
;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/troll.names")))

;;; (time (defparameter $tmap (make-instance 'travesty-map :samples $samples)))
;;; (time (generate-names $tmap 100))

(defmethod generate-names-ending-in-us ((map travesty-map)(count integer))
  (let ((result nil))
    (loop until (>= (length result) count)
          do (let ((nm (generate-name map)))
               (if (and (char= #\u (elt nm (- (length nm) 2)))
                        (char= #\s (elt nm (- (length nm) 1))))
                   (pushnew nm result :test #'string=))))
    result))

;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names")))
;;; (time (defparameter $tmap (make-instance 'travesty-map :samples $samples)))
;;; (time (generate-names-ending-in-us $tmap 100) )

(defmethod generate-names-ending-in-y ((map travesty-map)(count integer))
  (let ((result nil))
    (loop until (>= (length result) count)
          do (let ((nm (generate-name map)))
               (if (char= #\y (elt nm (- (length nm) 1)))
                   (pushnew nm result :test #'string=))))
    result))

;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names")))
;;; (time (defparameter $tmap (make-instance 'travesty-map :samples $samples)))
;;; (setf $nms (generate-names-ending-in-y $tmap 10))

(defmethod generate-names-ending-in-ina ((map travesty-map)(count integer))
  (let ((result nil))
    (loop until (>= (length result) count)
          do (let ((nm (generate-name map)))
               (when (>= (length nm) 3)
                   (if (and (char= #\i (elt nm (- (length nm) 3)))
                            (char= #\n (elt nm (- (length nm) 2)))
                            (char= #\a (elt nm (- (length nm) 1))))
                       (pushnew nm result :test #'string=)))))
    result))

;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names")))
;;; (time (defparameter $tmap (make-instance 'travesty-map :samples $samples)))
;;; (setf $nms (nconc $nms (generate-names-ending-in-y $tmap 10)))
;;; (setf $nms (nconc $nms (generate-names-ending-in-ina $tmap 10)))
;;; (length $nms)
;;; (delete-duplicates $nms :test #'string=)
;;; (sort $nms #'string<)
;;; (loop for nm in $nms do (format t " ~A" nm))

(defmethod generate-names-ending-in-dget ((map travesty-map)(count integer))
  (let ((result nil))
    (loop until (>= (length result) count)
          do (let ((nm (generate-name map)))
               (when (>= (length nm) 4)
                   (if (and (char= #\d (elt nm (- (length nm) 4)))
                            (char= #\g (elt nm (- (length nm) 3)))
                            (char= #\e (elt nm (- (length nm) 2)))
                            (char= #\t (elt nm (- (length nm) 1))))
                       (pushnew nm result :test #'string=)))))
    result))

;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names")))
;;; (time (defparameter $tmap (make-instance 'travesty-map :samples $samples)))
;;; (time (generate-names-ending-in-dget $tmap 10))
;;; (setf $nms (nconc $nms (generate-names-ending-in-dget $tmap 10)))
