;;;; ***********************************************************************
;;;;
;;;; Name:          nym-window.lisp
;;;; Project:       nym
;;;; Purpose:       the main GUI window
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-gui)

;;; ---------------------------------------------------------------------
;;; nym-window
;;; ---------------------------------------------------------------------

(define-interface nym-window ()
  ;; -- slots ---------------------------------------------
  ((data-directory :accessor data-directory :initform (nym-base::nym-data-directory)))

  ;; -- panes ---------------------------------------------
  (:panes
   )

  ;; -- layouts ---------------------------------------------
  (:layouts
   
   (main-layout column-layout '()
                :reader main-layout))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :title "Nym"
   :width 800 :height 600
   :create-callback (lambda (intf) 
                      )))


;;; (setf $win (contain (make-instance 'nym-window)))
