;;;; ***********************************************************************
;;;;
;;;; Name:          samples-list.lisp
;;;; Project:       nym
;;;; Purpose:       a name generator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

;;; ---------------------------------------------------------------------
;;; <samples-list-pane>
;;; ---------------------------------------------------------------------

(define-interface <samples-list-pane> ()
  ;; -- slots ---------------------------------------------
  ((samples :accessor pane-samples :initform nil :initarg :samples))

  ;; -- panes ---------------------------------------------
  (:panes
   (samples-pane list-panel
                 :items samples
                 :reader samples-pane))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(samples-pane)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout))


;;; ---------------------------------------------------------------------
;;; UI functions
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (defparameter $names (read-names "/Users/mikel/Workshop/src/clnamer/us.names"))
;;; (contain (make-instance '<samples-list-pane> :samples $names))
