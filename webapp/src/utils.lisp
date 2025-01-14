;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       useful utilities
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :nym)

(defun system-version ()
  (slot-value (asdf:find-system :nymweb) 'asdf:version))

#+test (system-version :nym)
