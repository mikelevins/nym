;;;; ***********************************************************************
;;;;
;;;; Name:          modeling.lisp
;;;; Project:       nym
;;;; Purpose:       modeling names for generation
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defparameter *maximum-name-length* 16)

(defmethod as-chunks ((str string))
  (let ((limit (length str)))
    (loop for i from 0 below limit by 3
       collect (let ((end (+ i 3)))
                 (subseq str i (if (< end limit)
                                   end nil))))))

;;; (as-chunks "Aardvark")

(defmethod as-chunk-pairs ((str string))
  (let ((chunks (as-chunks str)))
    (loop for tail on chunks by #'cdr
       collect (list (car tail)(cadr tail)))))

;;; (as-chunk-pairs "Aardvark")

(defclass chunk-map ()
  ((samples :accessor samples :initform nil :initarg :samples)
   (starts :accessor starts :initform nil :initarg :starts)
   (parts :accessor parts :initform nil :initarg :parts)
   (ends :accessor ends :initform nil :initarg :ends)))

(defmethod initialize-instance :after ((chunk-map chunk-map) &rest initargs &key &allow-other-keys)
  (when (samples chunk-map)
    (map-samples chunk-map (samples chunk-map))))

(defmethod add-start ((chunk-map chunk-map)(key string) val)
  (let* ((old-starts (starts chunk-map))
         (old-entry (assoc key old-starts :test #'equalp)))
    (if old-entry
        (if (member val (cdr old-entry) :test #'string=)
            chunk-map
            (progn (setf (cdr old-entry)
                         (cons val (cdr old-entry)))
                   chunk-map))
        (setf (starts chunk-map)
              (cons (cons key (list val))
                    (starts chunk-map))))))

(defmethod add-part ((chunk-map chunk-map)(key string) val)
  (let* ((old-parts (parts chunk-map))
         (old-entry (assoc key old-parts :test #'equalp)))
    (if old-entry
        (if (member val (cdr old-entry) :test #'string=)
            chunk-map
            (progn (setf (cdr old-entry)
                         (cons val (cdr old-entry)))
                   chunk-map))
        (setf (parts chunk-map)
              (cons (cons key (list val))
                    (parts chunk-map))))))

(defmethod add-end ((chunk-map chunk-map)(key string))
  (let* ((old-ends (ends chunk-map))
         (new-ends (if (member key old-ends :test #'equalp)
                       old-ends
                       (cons key old-ends))))
    (setf (ends chunk-map)
          new-ends)
    chunk-map))

(defmethod map-sample ((chunk-map chunk-map)(sample string))
  (let* ((chunk-pairs (as-chunk-pairs sample))
         (start (first chunk-pairs))
         (parts (rest chunk-pairs))
         (end (first (last chunk-pairs))))
    (add-start chunk-map (first start) (second start))
    (add-end chunk-map (first end))
    (dolist (part parts)
      (add-part chunk-map (first part)(second part)))
    chunk-map))

(defmethod map-samples ((chunk-map chunk-map) samples)
  (dolist (sample samples)
    (map-sample chunk-map sample))
  chunk-map)

;;; (defparameter $map (make-instance 'chunk-map))
;;; (defparameter $map (make-instance 'chunk-map :samples '("Boffo")))
;;; (defparameter $map (make-instance 'chunk-map :samples '("Boffo" "Farvo" "Gyrus" "Sprightly" "Womby" "Wombelius" "Wombely")))

(defmethod choose-start ((chunk-map chunk-map))
  (let* ((chunk (choose-any (starts chunk-map)))
         (first-chunk (first chunk))
         (next-chunk (choose-any (cdr chunk))))
    (list first-chunk next-chunk)))

;;; (choose-start $map)

(defmethod choose-next-chunk ((chunk-map chunk-map)(chunks null))
  nil)

(defmethod choose-next-chunk ((chunk-map chunk-map)(chunks cons))
  (let ((old-end (first (last chunks))))
    (if (null old-end)
        nil
        (let ((entry (assoc old-end (parts chunk-map) :test #'equalp)))
          (if entry
              (choose-any (cdr entry))
              nil)))))

;;; (choose-next-chunk $map '("Wom" "bel"))

(defmethod build-name ((chunk-map chunk-map))
  (let* ((chunks (choose-start chunk-map)))
    (block building
      (loop (if (>= (sum-lengths chunks)
                    *maximum-name-length*)
                (return-from building (join (remove nil chunks)))
                (let ((next-chunk (choose-next-chunk chunk-map chunks)))
                  (if next-chunk
                      (setf chunks
                            (nconc chunks (list next-chunk)))
                      (return-from building (join (remove nil chunks))))))))))

;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/clnamer/us.names")))
;;; (time (defparameter $map (make-instance 'chunk-map :samples $samples)))
;;; (build-name $map)

