;;;; ***********************************************************************
;;;;
;;;; Name:          nym-window.lisp
;;;; Project:       nym
;;;; Purpose:       the main GUI window
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :nym-gui)

;;; ---------------------------------------------------------------------
;;; nym-window
;;; ---------------------------------------------------------------------

(define-interface nym-window ()
  ;; -- slots ---------------------------------------------
  ((data-directory :accessor data-directory :initform (nym-base::nym-data-directory))
   (samples :accessor samples :initform nil))

  ;; -- panes ---------------------------------------------
  (:panes
   (languages-pane list-panel :reader languages-pane  :interaction :single-selection
                   :font (gp:make-font-description :size 16)
                   :title "Languages" :title-position :top :title-adjust :left
                   :selection-callback 'did-select-language :callback-type :data-interface
                   :items (nym-base::list-languages (data-directory interface)))
   (count-control text-input-range :start 1 :end 100 :value 10 :reader count-control
                  :title "Generate how many?" :title-position :left
                  :visible-max-height 32)
   (generate-button push-button :text "Generate" :callback #'make-names
                    :callback-type :interface-data
                    :visible-min-height 32
                    :visible-max-height 32)
   (names-pane editor-pane :enabled :read-only :buffer-name :temp :reader names-pane
               :font (gp:make-font-description :size 16)
               :background :white))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (browser-layout column-layout '(languages-pane))
   (generator-controls-layout row-layout '(count-control nil generate-button))
   (generator-layout column-layout '(generator-controls-layout names-pane))
   (main-layout row-layout '(browser-layout :divider generator-layout)
                :reader main-layout))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :title "Nym"
   :width 800 :height 600
   :create-callback (lambda (intf) 
                      (setf (simple-pane-background (names-pane intf)) :white)
                      (let ((selected-language (choice-selected-item (languages-pane intf))))
                        (when selected-language
                          (update-language-selection intf selected-language))))))


(defmethod update-language-selection ((intf nym-window)(language-name string))
  (let ((language-path (merge-pathnames (concatenate 'string language-name ".names")
                                        (nym-base::nym-data-directory))))
    (when language-path
      (let* ((sample-names (nym-base::read-samples language-path))
             (tidied-names (nym-base::tidy-lines sample-names)))
        (setf (samples intf) tidied-names)
        (setf (editor-pane-text (names-pane intf)) "")))))

(defun did-select-language (language-name intf)
  (when language-name
    (update-language-selection intf language-name)))

(defmethod make-names ((intf nym-window) data)
  (let* ((count (text-input-range-value (count-control intf)))
         (names (sort (nym-base::gen-chunk-travesties (samples intf) count)
                      #'string<))
         (names-text (with-output-to-string (out)
                       (loop for name in names 
                             do (format out "~A~%" name)))))
    (setf (editor-pane-text (names-pane intf))
          names-text)))

;;; (setf $win (capi:contain (make-instance 'nym-window)))
