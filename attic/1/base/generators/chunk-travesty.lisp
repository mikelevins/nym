;;;; ***********************************************************************
;;;;
;;;; Name:          chunk-travesty.lisp
;;;; Project:       nym
;;;; Purpose:       generating names using travesties built from chunked samples
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defmethod extend-chunk-travesty ((start string) (parts list) (ends list)
                                  &key
                                    (chunk-size 3)
                                    (end-preference 0.5))
  (let* ((pref (last-n-elements start (1- chunk-size)))
         (matching-parts (remove-if-not (lambda (part)(prefix-match? pref part))
                                        parts))
         (matching-ends (remove-if-not (lambda (end)(prefix-match? pref end))
                                       ends)))
    (if (and matching-ends
             (< end-preference (random 1.0)))
        (concatenate 'string start (subseq (any matching-ends) (1- chunk-size)))
        (if matching-parts
            (extend-chunk-travesty (concatenate 'string start (subseq (any matching-parts) (1- chunk-size)))
                                   parts ends)
            start))))

(defmethod gen-chunk-travesty ((starts list)(parts list)(ends list) &key (chunk-size 3))
  (extend-chunk-travesty (any starts) parts ends :chunk-size chunk-size))

(defmethod gen-chunk-travesties ((samples list)(count integer) &key (chunk-size 3))
  (let* ((chunked-samples (remove-duplicates
                           (loop for sample in samples collect (remove nil (chunk-sequence sample chunk-size)))
                           :test #'equal))
         (starts (remove nil (mapcar #'first chunked-samples)))
         (parts (reduce #'append
                        (remove nil (mapcar #'(lambda (sample)(drop-first (drop-last sample))) chunked-samples))))
         (ends (remove nil (mapcar #'last-element chunked-samples))))
    (loop for i from 0 below count collect (gen-chunk-travesty starts parts ends :chunk-size chunk-size))))

;;; (time (gen-chunk-travesties (read-samples "~/Workshop/src/nym/data/dickens.names") 50))
;;; (time (gen-chunk-travesties (read-samples "~/Workshop/src/nym/data/gnome.names") 50))
