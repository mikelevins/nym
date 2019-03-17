;;;; ***********************************************************************
;;;;
;;;; Name:          files.lisp
;;;; Project:       nym
;;;; Purpose:       file helpers
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

(defun nym-data-directory ()
  (asdf/system:system-relative-pathname :nym-gui "data/"))

;;; (nym-data-directory)