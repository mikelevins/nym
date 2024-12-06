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


(asdf:defsystem #:nym-webserver
    :description "The nym desktop GUI"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache-2.0"
    :depends-on (:nym-base :hunchentoot :st-json)
    :serial t
    :components ((:module "webserver"
                          :serial t
                          :components ((:file "nym-server-package")
                                       ))))

;;; (asdf:load-system :nym-webserver)
