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
  ((samples :accessor pane-samples :initform nil :initarg :samples)
   (triples :accessor pane-triples :initform nil)
   (starts :accessor pane-starts :initform nil)
   (parts :accessor pane-parts :initform nil)
   (ends :accessor pane-ends :initform nil))

  ;; -- panes ---------------------------------------------
  (:panes
   (languages-label title-pane :title "Languages")
   (languages-pane list-panel)
   (find-languages-button push-button :text "Find languages...")
   (generate-label title-pane :title "Generate")
   (count-control slider :start 1 :end 25)
   (count-label title-pane :title "1")
   (generate-button push-button :text "Names")
   (samples-label title-pane :title "Example names")
   (samples-pane list-panel)
   (names-label title-pane :title "Generated names")
   (names-pane list-panel))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (languages-layout column-layout '(languages-label languages-pane))
   (samples-layout column-layout '(samples-label samples-pane))
   (names-layout column-layout '(names-label names-pane))
   (contents-layout row-layout '(languages-layout samples-layout names-layout))
   (footer-layout row-layout '(find-languages-button nil
                               generate-label count-control count-label generate-button))
   (main-layout column-layout '(contents-layout
                                footer-layout)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout))

;;; ---------------------------------------------------------------------
;;; UI functions
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (defparameter $names (read-names "/Users/mikel/Workshop/src/nym/data/dwarf.names"))
;;; (defparameter $win (contain (make-instance '<nym-ui> :samples $names :width 800 :height 400)))
