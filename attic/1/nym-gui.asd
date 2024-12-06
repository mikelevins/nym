;;;; ***********************************************************************
;;;;
;;;; Name:          nym-gui.asd
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       system definitions
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(asdf:defsystem #:nym-gui
    :description "The nym desktop GUI"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache-2.0"
    :depends-on (:nym-base)
    :serial t
    :components ((:module "gui"
                          :serial t
                          :components ((:file "nym-gui-package")
                                       (:file "nym-window")))))

;;; (asdf:load-system :nym-gui)
