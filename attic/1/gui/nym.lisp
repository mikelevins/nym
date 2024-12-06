;;;; ***********************************************************************
;;;;
;;;; Name:          nym.lisp
;;;; Project:       nym
;;;; Purpose:       the main UI
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-gui)

;;; ---------------------------------------------------------------------
;;; <nym-ui>
;;; ---------------------------------------------------------------------

(define-interface <nym-ui> ()
  ;; -- slots ---------------------------------------------
  ((travesty-map :accessor travesty-map :initform nil))

  ;; -- panes ---------------------------------------------
  (:panes
   (count-control text-input-range :start 1 :end 100 :value 10 :reader count-control)
   (generate-button push-button :text "Generate" :callback #'make-names :callback-type :interface-data)
   (samples-label title-pane :title "Sample names")
   (samples-pane list-panel :reader samples-pane :font (gp:make-font-description :size 14))
   (names-label title-pane :title "Generated names")
   (names-pane editor-pane :enabled :read-only :buffer-name :temp :reader names-pane
               :font (gp:make-font-description :size 14)
               :background :white))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (samples-label-layout row-layout '(samples-label) :external-max-height 32 :adjust :center)
   (samples-layout column-layout '(samples-label-layout samples-pane))
   (generate-layout row-layout '(names-label nil generate-button count-control)
                    :external-max-height 32 :adjust :center)
   (names-layout column-layout '(generate-layout names-pane))
   (contents-layout row-layout '(samples-layout names-layout))
   (main-layout column-layout '(contents-layout)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :title "Nym"
   :width 828 :height 500
   :create-callback (lambda (intf) 
                      (when (travesty-map intf)
                        (setf (collection-items (samples-pane intf))
                              (samples (travesty-map intf))))
                      (setf (simple-pane-background (names-pane intf)) :white))))

(defmethod initialize-instance :after ((ui <nym-ui>)
                                       &rest initargs
                                       &key &allow-other-keys)
  (let ((samples (getf initargs :samples nil)))
    (when samples
      (setf (travesty-map ui)
            (make-instance 'travesty-map :samples samples)))))


;;; ---------------------------------------------------------------------
;;; UI functions
;;; ---------------------------------------------------------------------

(defmethod make-names ((intf <nym-ui>) data)
  (let* ((count (text-input-range-value (count-control intf)))
         (names (sort (generate-names (travesty-map intf)
                                      count)
                      #'string<))
         (names-text (with-output-to-string (out)
                       (loop for name in names 
                             do (format out "~A~%" name)))))
    (setf (editor-pane-text (names-pane intf))
          names-text)))

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;; (defparameter $names (read-samples "~/Workshop/src/nym/data/dickens.names"))
;;; (defparameter $names (read-samples "~/Workshop/src/nym/data/dwarf.names"))
;;; (defparameter $names (read-samples "~/Workshop/src/nym/data/forsaken.names"))
;;; (defparameter $names (read-samples "~/Workshop/src/nym/data/gnome.names"))
;;; (defparameter $names (read-samples "~/Workshop/src/nym/data/goblin.names"))
;;; (defparameter $names (read-samples "~/Workshop/src/nym/data/troll.names"))
;;; (defparameter $names (read-samples "~/Workshop/src/nym/data/us.names"))
;;; (defparameter $win (contain (make-instance '<nym-ui> :samples $names :width 800 :height 400)))
