;;;; ***********************************************************************
;;;;
;;;; Name:          travesty.lisp
;;;; Project:       nym
;;;; Purpose:       modeling name generation as a travesty
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this travesty generator uses the following rules:
;;;
;;; 1. we can start a name with any chunk we've seen before at the
;;;    start of a name
;;;
;;; 2. the next chunk we add must be a character that has been seen
;;;    before following the previous two characters
;;;
;;; 3. if there is no character matching the requirements of (2),
;;;    we end the name
;;;
;;; 4. if the end of the name is not an end we have seen before,
;;;    we discard the name and try again

(defmethod triples ((str string))
  (loop for i from 0 below (1- (length str))
     collect (take 3 (subseq str i))))

;;; (triples "Abraham")
;;; (triples "John")
;;; (triples "Joh")
;;; (triples "Jo")

(defmethod pair-single-map ((str string))
  (let ((len (length str)))
    (assert (<= len 3)(str) "Too many characters: ~S" str)
    (if (< len 3)
        (list str nil)
        (list (subseq str 0 2)
              (subseq str 2)))))

;;; (mapcar #' pair-single-map (triples "Abraham"))
;;; (mapcar #' pair-single-map (triples "John"))


;;; ---------------------------------------------------------------------
;;; class travesty-map
;;; ---------------------------------------------------------------------
;;; models the data needed to generate travesties of input samples
;;; the state space of name chunks is represented as lists of pairs
;;; (HEAD TAIL1 TAIL2 TAIL3 ...) where any combination of HEAD+TAILN
;;; is a valid chunk for a new name.
;;; Heads are 1- or 2-character strings. Tails are 1-character strings
;;; or nil. For a 1-character head, the only valid tail is nil.
;;; nil marks the end of a name.
;;;
;;; the travesty map stores:
;;;  samples: the list of sample names used
;;;  starts: the chunk-pairs that can start a name
;;;  parts: chunk-pairs that can appear in a name after the start.
;;;  ends: a chunk that can appear at the end.

(defclass travesty-map ()
  ((samples :accessor samples :initform nil :initarg :samples)
   (starts :accessor starts :initform nil :initarg :starts)
   (parts :accessor parts :initform nil :initarg :parts)
   (ends :accessor ends :initform nil :initarg :ends)))

;;; (defparameter $map (make-instance 'travesty-map))
;;; (defparameter $map (make-instance 'travesty-map :samples '("Abraham" "John")))
;;; (describe $map)

(defmethod initialize-instance :after ((tmap travesty-map) &rest initargs &key &allow-other-keys)
  (when (samples tmap)
    (let ((triples-list (mapcar #'triples (samples tmap))))
      (loop for triples in triples-list
         do (let* ((chunk-pairs (mapcar #'pair-single-map triples))
                   (new-start (first chunk-pairs))
                   (new-parts (rest chunk-pairs))
                   (new-end (first (last chunk-pairs))))
              (add-start tmap new-start)
              (add-end tmap new-end)
              (dolist (new-part new-parts)
                (add-part tmap new-part)))))))

(defmethod add-start ((tmap travesty-map)(start-pair null))
  tmap)

(defmethod add-start ((tmap travesty-map)(start-pair cons))
  (let ((start-key (first start-pair))
        (start-tail (second start-pair)))
    (assert (stringp start-key)(start-pair) "Invalid key in start-pair: ~S" start-key)
    (assert (or (stringp start-tail)(null start-tail))(start-pair)
            "Invalid tail in start-pair: ~S" start-tail)
    (let* ((old-starts (starts tmap))
           (found-entry (assoc start-key old-starts :test #'equalp)))
      (if found-entry
          (let ((old-tails (cdr found-entry)))
            (unless (member start-tail old-tails :test #'equalp)
              (setf (cdr found-entry)
                    (cons start-tail old-tails))))
          (setf (starts tmap)
                (cons (list start-key start-tail)
                      (starts tmap)))))
    tmap))

(defmethod add-part ((tmap travesty-map)(part-pair cons))
  (let ((part-key (first part-pair))
        (part-tail (second part-pair)))
    (assert (stringp part-key)(part-pair) "Invalid key in part-pair: ~S" part-key)
    (assert (or (stringp part-tail)(null part-tail))(part-pair)
            "Invalid tail in part-pair: ~S" part-tail)
    (let* ((old-parts (parts tmap))
           (found-entry (assoc part-key old-parts :test #'equalp)))
      (if found-entry
          (let ((old-tails (cdr found-entry)))
            (unless (member part-tail old-tails :test #'equalp)
              (setf (cdr found-entry)
                    (cons part-tail old-tails))))
          (setf (parts tmap)
                (cons (list part-key part-tail)
                      (parts tmap)))))
    tmap))

(defmethod add-end ((tmap travesty-map)(end-pair cons))
  (let ((end-key (first end-pair))
        (end-tail (second end-pair)))
    (assert (stringp end-key)(end-pair) "Invalid key in end-pair: ~S" end-key)
    (assert (null end-tail)(end-pair) "Invalid tail in end-pair: ~S" end-tail)
    (let* ((old-ends (ends tmap))
           (found-entry (member end-key old-ends :test #'equalp)))
      (unless found-entry
        (setf (ends tmap)
              (cons end-key (ends tmap)))))
    tmap))

