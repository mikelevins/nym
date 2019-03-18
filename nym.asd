;;;; ***********************************************************************
;;;;
;;;; Name:          nym.asd
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       system definitions
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(asdf:defsystem #:nym-base
    :description "nym: a customizable name generator"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache-2.0"
    :serial t
    :components ((:module "base"
                          :serial t
                          :components ((:file "nym-base-package")
                                       (:module "util"
                                                :serial t
                                                :components ((:file "sequences")
                                                             (:file "strings")
                                                             (:file "files")))
                                       (:module "generator"
                                                :serial t
                                                :components ((:file "travesty")
                                                             (:file "portmanteau")))))))


;;; (asdf:load-system :nym-base)

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



