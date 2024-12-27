;;;; ***********************************************************************
;;;;
;;;; Name:          ui.lisp
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       main program and UI
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :nym)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :capi))

;;; ---------------------------------------------------------------------
;;; UI
;;; ---------------------------------------------------------------------

#+test (setf $nym (contain (make-instance 'nym)))

(defparameter *default-name-count* 12)

(define-interface nym ()
  ;; -- slots ---------------------------------------------
  ((namefile :accessor namefile :initform nil)
   (name-count :accessor name-count :initform *default-name-count*))
  ;; -- menus ---------------------------------------------
  (:menus)
  ;; -- menubar ---------------------------------------------
  (:menu-bar)
  ;; -- panes ---------------------------------------------
  (:panes
   (language-label title-pane :text "Language:")
   (language-menu-button
    capi:popup-menu-button
    :reader language-menu-button
    :text "Choose naming language"
    :menu-function 'make-language-menu)
   (clear-button push-button :text "Clear"
                 :callback (lambda (data interface)(setf (editor-pane-text (name-collector interface)) "")))
   (generate-button push-button :text "Generate"
                    :callback (lambda (data interface)
                                (let ((namefile (namefile interface)))
                                  (if namefile
                                      (let* ((nametable (make-nametable namefile))
                                             (names (sort (generate-names nametable (name-count interface))
                                                          (lambda (p1 p2)(string< (pathname-name p1)(pathname-name p1))))))
                                        (loop for nm in names 
                                              do (format (collector-pane-stream (name-collector interface)) "~%~A" nm)))
                                    (capi:display-message "First you must choose a naming language!")))))
   (count-label title-pane :text "Count:")
   (count-input text-input-range :accessor count-input
                 :start 1 :end 100
                 :callback-type :data-interface
                 :callback (lambda (data interface)(setf (name-count interface) data)))
   (name-collector collector-pane :accessor name-collector 
                   :font (gp:make-font-description :family "helvetica" :weight :medium :slant :roman :size 16)))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (language-layout row-layout '(language-label language-menu-button nil count-label count-input clear-button generate-button))
   (main-layout column-layout '(language-layout name-collector)))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :initial-focus 'generate-button
   :title "nym"
   :width 800 :height 600
   :create-callback (lambda (intf) 
                      (setf (text-input-range-value (count-input intf))
                            (name-count intf)))))

(defun make-language-menu (button)
  (make-instance 'capi:menu
                 :items-function (lambda (interface)
                                   (sort (series:collect (folio3::all-keys +nametables+))
                                         (lambda (p1 p2)(string< (pathname-name p1)(pathname-name p2)))))
                 :print-function (lambda (item)(string-capitalize (pathname-name item)))
                 :callback 'language-menu-callback
                 :callback-type :data-interface
                 :selection-callback (lambda (data interface)
                                       (setf (namefile interface) data)
                                       (setf (item-text (language-menu-button interface)) 
                                             (string-capitalize (pathname-name data))))))

(defun language-menu-callback (data interface)
  (with-slots (language-menu-button) interface
    (let ((next-title data))
      (setf (capi:item-text language-menu-button)
            next-title))))
