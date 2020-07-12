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
;;; reading sample names
;;; ---------------------------------------------------------------------

(defun empty-sample? (str)
  (or (empty? str)
      (every #'whitespace? str)))

(defmethod sample-contains? ((sample string) (chunk string))
  (and (search sample chunk)
       t))

(defmethod read-samples ((filename pathname))
  (sort (remove-if #'empty-sample?
                   (read-lines filename))
        #'string<))

(defmethod read-samples ((filename string))
  (read-samples (pathname filename)))

;;; (time (defparameter $samples (read-samples "~/Workshop/src/nym/data/us.names")))

(defun split-sample (word chunk)
  (let ((pos (search chunk word)))
    (if pos
        (values (subseq word 0 pos)
                chunk
                (subseq word (+ pos (length chunk))))
        (error "Subsequence ~S not found in word ~S" chunk word))))

(defun find-sample-containing (chunk samples)
  (any (remove-if-not (lambda (s)(search chunk s))
                      samples)))
