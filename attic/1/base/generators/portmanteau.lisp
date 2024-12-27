;;;; ***********************************************************************
;;;;
;;;; Name:          portmanteau.lisp
;;;; Project:       nym
;;;; Purpose:       simple portmanteau generator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defun gen-portmanteau (samples &key (max-tries 10))
  (block generating
    (loop for i from 0 below max-tries
       do (let* ((start-word (any samples))
                 (seed (any (chunk-sequence start-word 3)))
                 (end-word (find-sample-containing seed samples))
                 (result (multiple-value-bind (left seed _)(split-sample start-word seed)
                           (declare (ignore _))
                           (multiple-value-bind (_ seed right)(split-sample end-word seed)
                             (declare (ignore _))
                             (concatenate 'string left seed right)))))
            (unless (member result samples :test #'equalp)
              (return-from generating result))))
    ;; if we get here then we failed to find a novel portmanteau
    nil))

(defun gen-portmanteaus (samples count)
  (loop for i from 0 below count collect (gen-portmanteau samples)))

;;; (progn (setf $samples (read-samples "/usr/share/dict/words")) t)
;;; (length $samples)
;;; (gen-portmanteau $samples)
;;; (gen-portmanteaus $samples 200)
