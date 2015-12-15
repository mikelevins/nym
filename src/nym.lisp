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
   (samples-pane <samples-list-pane> :samples samples :reader samples-pane)
   (starts-pane <samples-list-pane> :samples starts :reader starts-pane)
   (parts-pane <samples-list-pane> :samples parts :reader parts-pane)
   (ends-pane <samples-list-pane> :samples ends :reader ends-pane)
   (names-pane <samples-list-pane> :samples nil :reader names-pane)
   (import-button push-button :text "Import..." :reader import-button
                  :callback-type :data-interface
                  :selection-callback 'import-selection-callback)
   (generate-button push-button :text "Generate" :reader generate-button
                    :callback-type :data-interface
                    :selection-callback 'generate-selection-callback))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (controls-layout row-layout '(import-button nil generate-button))
   (samples-layout column-layout '(samples-pane))
   (parts-layout row-layout '(starts-pane parts-pane ends-pane))
   (names-layout column-layout '(names-pane))
   (data-layout row-layout '(samples-layout parts-layout names-layout))
   (main-layout column-layout '(controls-layout data-layout)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout))

(defmethod update-instance ((ui <nym-ui>)(samples list))
  (setf (pane-samples ui) samples)
  (let* ((parsed (parse-names (pane-samples ui)))
         (starts (first parsed))
         (parts (second parsed))
         (ends (third parsed)))
    ;; update the slots
    (setf (pane-starts ui) starts)
    (setf (pane-parts ui) parts)
    (setf (pane-ends ui) ends)
    ;; update the panes
    (setf (collection-items (samples-pane (starts-pane ui)))
          (pane-starts ui))
    (setf (collection-items (samples-pane (parts-pane ui)))
          (pane-parts ui))
    (setf (collection-items (samples-pane (ends-pane ui)))
          (pane-ends ui))))

(defmethod initialize-instance :after ((ui <nym-ui>) &rest initargs &key &allow-other-keys)
  (update-instance ui (pane-samples ui)))

;;; ---------------------------------------------------------------------
;;; UI functions
;;; ---------------------------------------------------------------------

(defun import-selection-callback (data interface)
  (let* ((path (prompt-for-file "Choose a name file" :filter "*.names")))
    (when path
      (update-instance interface (read-names path)))))

(defun generate-selection-callback (data interface)
  (let ((names (sort (remove-duplicates
                      (generate-names 10
                                      (pane-starts interface)
                                      (pane-parts interface)
                                      (pane-ends interface))
                      :test #'equalp)
                     #'string<)))
    (setf (collection-items (samples-pane (names-pane interface)))
          names)))

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (defparameter $names (read-names "/Users/mikel/Workshop/src/nym/data/dwarf.names"))
;;; (defparameter $win (contain (make-instance '<nym-ui> :samples $names :width 800 :height 600)))
