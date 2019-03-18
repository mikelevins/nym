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
  ((data-directory :accessor data-directory :initform (nym-base::nym-data-directory))
   (travesty-map :accessor travesty-map :initform nil))

  ;; -- panes ---------------------------------------------
  (:panes
   (languages-pane list-panel :reader languages-pane  :interaction :single-selection
                   :title "Languages" :title-position :top :title-adjust :left
                   :selection-callback 'did-select-language :callback-type :data-interface
                   :items (list-languages (data-directory interface)))
   (samples-pane list-panel :reader samples-pane :interaction :no-selection
                 :title "Samples" :title-position :top :title-adjust :left)
   (count-control text-input-range :start 1 :end 100 :value 10 :reader count-control
                  :title "Generate how many?" :title-position :left
                  :visible-max-height 32)
   (generate-button push-button :text "Generate" :callback #'make-names
                    :callback-type :interface-data
                    :visible-min-height 32
                    :visible-max-height 32)
   (names-pane editor-pane :enabled :read-only :buffer-name :temp :reader names-pane
               :font (gp:make-font-description :size 14)
               :background :white))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (browser-layout column-layout '(languages-pane :divider samples-pane))
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
      (let* ((sample-names (read-lines language-path))
             (tidied-names (tidy-lines sample-names)))
        (setf (collection-items (samples-pane intf))
              tidied-names)
        (setf (titled-object-title (samples-pane intf))
              language-name)
        (setf (editor-pane-text (names-pane intf)) "")
        (setf (travesty-map intf)
              (make-travesty-map tidied-names))))))

(defmethod list-languages ((dir pathname))
  (let* ((language-files (directory (merge-pathnames "*.names" dir)))
         (languages (loop for file in language-files
                          collect (pathname-name file))))
    (sort languages #'string<)))

(defun did-select-language (language-name intf)
  (when language-name
    (update-language-selection intf language-name)))

(defmethod make-names ((intf nym-window) data)
  (let* ((count (text-input-range-value (count-control intf)))
         (names (sort (generate-names (travesty-map intf)
                                      count)
                      #'string<))
         (names-text (with-output-to-string (out)
                       (loop for name in names 
                             do (format out "~A~%" name)))))
    (setf (editor-pane-text (names-pane intf))
          names-text)))

;;; (setf $win (capi:contain (make-instance 'nym-window)))
