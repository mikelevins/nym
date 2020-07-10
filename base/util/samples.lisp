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

(defun empty-name? (str)
  (or (empty? str)
      (every #'whitespace? str)))

(defmethod read-names ((filename pathname))
  (sort (remove-if #'empty-name?
                   (read-lines filename))
        #'string<))

(defmethod read-names ((filename string))
  (read-names (pathname filename)))

;;; (time (defparameter $samples (read-names "/Users/mikel/Workshop/src/clnamer/us.names")))
