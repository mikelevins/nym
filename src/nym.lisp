;;;; ***********************************************************************
;;;;
;;;; Name:          nym.lisp
;;;; Project:       nym
;;;; Purpose:       the main UI
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

;;; ---------------------------------------------------------------------
;;; <nym-ui>
;;; ---------------------------------------------------------------------

(define-interface <nym-ui> ()
  ;; -- slots ---------------------------------------------
  ((languages-folder :accessor languages-folder :initform nil :initarg :languages-folder)
   (samples :accessor pane-samples :initform nil :initarg :samples)
   (triples :accessor pane-triples :initform nil)
   (starts :accessor pane-starts :initform nil)
   (parts :accessor pane-parts :initform nil)
   (ends :accessor pane-ends :initform nil))

  ;; -- panes ---------------------------------------------
  (:panes
   (languages-folder-label title-pane :title "Languages folder:")
   (languages-pathname-label title-pane :title "")
   (languages-pathname-button push-button :text "Find Languages...")
   (languages-label title-pane :title "Languages")
   (languages-pane list-panel)
   (count-control text-input-range :start 1 :end 25)
   (generate-button push-button :text "Generate")
   (samples-label title-pane :title "Examples")
   (samples-pane list-panel)
   (names-label title-pane :title "Names")
   (names-pane list-panel))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (languages-label-layout row-layout '(languages-label) :external-max-height 32 :adjust :center)
   (languages-layout column-layout '(languages-label-layout languages-pane))
   (samples-label-layout row-layout '(samples-label) :external-max-height 32 :adjust :center)
   (samples-layout column-layout '(samples-label-layout samples-pane))
   (generate-layout row-layout '(names-label nil generate-button count-control)
                    :external-max-height 32 :adjust :center)
   (names-layout column-layout '(generate-layout names-pane))
   (contents-layout row-layout '(languages-layout samples-layout names-layout))
   (find-languages-layout row-layout '(languages-folder-label
                                       languages-pathname-label
                                       nil languages-pathname-button))
   (main-layout column-layout '(contents-layout
                                find-languages-layout)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout))

;;; ---------------------------------------------------------------------
;;; UI functions
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (defparameter $names (read-names "/Users/mikel/Workshop/src/nym/data/gnome.names"))
;;; (defparameter $win (contain (make-instance '<nym-ui> :samples $names :width 800 :height 400)))
