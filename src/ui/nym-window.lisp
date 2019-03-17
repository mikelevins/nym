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
   (languages-pane list-panel :reader languages-pane  :interaction :single-selection
                   :title "Languages" :title-position :top :title-adjust :left
                   :selection-callback 'did-select-language :callback-type :data-interface
                   :items (list-languages (data-directory interface)))
   (samples-pane list-panel :reader samples-pane :interaction :no-selection
                 :title "Samples" :title-position :top :title-adjust :left))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (browser-layout column-layout '(languages-pane :divider samples-pane))
   (main-layout row-layout '(browser-layout)
                :reader main-layout))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :title "Nym"
   :width 800 :height 600
   :create-callback (lambda (intf) 
                      (let ((selected-language (choice-selected-item (languages-pane intf))))
                        (when selected-language
                          (let ((language-path (merge-pathnames (concatenate 'string selected-language ".names")
                                                                (nym-base::nym-data-directory))))
                            (when language-path
                              (let* ((sample-names (read-lines language-path))
                                     (tidied-names (tidy-lines sample-names)))
                                (setf (collection-items (samples-pane intf))
                                      tidied-names)))))))))

(defmethod list-languages ((dir pathname))
  (let* ((language-files (directory (merge-pathnames "*.names" dir)))
         (languages (loop for file in language-files
                          collect (pathname-name file))))
    (sort languages #'string<)))

(defun did-select-language (language-name intf)
  (when language-name
    (let ((language-path (merge-pathnames (concatenate 'string language-name ".names")
                                          (nym-base::nym-data-directory))))
      (when language-path
        (let* ((sample-names (read-lines language-path))
               (tidied-names (tidy-lines sample-names)))
          (setf (collection-items (samples-pane intf))
                tidied-names))))))

;;; (setf $win (contain (make-instance 'nym-window)))
