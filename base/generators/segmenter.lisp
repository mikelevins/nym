;;;; ***********************************************************************
;;;;
;;;; Name:          segmenter.lisp
;;;; Project:       nym
;;;; Purpose:       experiment: dynaically disocver common sgements
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; (time (setf $samples (read-samples "~/Workshop/src/nym/data/gnome.names")))

(defmethod sample-onsets ((sample string))
  (remove-if (lambda (x)(< (length x) 3))
             (loop for count from 2 below (length sample)
                collect (subseq sample 0 count))))

;;; (sample-onsets "Gyrus")

(defmethod sample-codas ((sample string))
  (remove-if (lambda (x)(< (length x) 3))
             (loop for count from 1 below (length sample)
                collect (subseq sample count))))

;;; (sample-codas "Gyrus")

(defun make-segmenter (path)
  (let* ((samples (read-samples path))
         (onsets (reduce #'append (mapcar #'sample-onsets samples)))
         (codas (reduce #'append (mapcar #'sample-codas samples)))
         (permitted-segments (reduce #'append (mapcar #'(lambda (s)(chunk-sequence s 3)) samples))))
    {:samples samples
              :onsets onsets
              :codas codas
              :permitted-segments permitted-segments}))

(defun segmenter-gen-name (segmenter)
  (let* ((permitted-segments (folio2:get-key segmenter :permitted-segments))
         (onset (any (folio2:get-key segmenter :onsets)))
         (codas (folio2:get-key segmenter :codas)))
    (block searching
      (loop
         (unless codas (return-from searching nil))
         (setf codas (alexandria:shuffle codas))
         (let* ((coda (first codas))
                (candidate (concatenate 'string onset coda))
                (segments (chunk-sequence candidate 3)))
           (setf codas (rest codas))
           (when (every (lambda (s)(member s permitted-segments :test #'equal))
                        segments)
             (return-from searching candidate)))))))


(defun segmenter-gen-names (segmenter count)
  (loop for i from 0 below count collect (segmenter-gen-name segmenter)))

;;; (setf $g (make-segmenter "~/Workshop/src/nym/data/gnome.names"))
;;; (segmenter-gen-names $g 100)

;;; (setf $v (make-segmenter "~/Workshop/src/nym/data/vulpera.names"))
;;; (segmenter-gen-names $v 100)

;;; (setf $us (make-segmenter "~/Workshop/src/nym/data/us.names"))
;;; (segmenter-gen-names $us 100)
