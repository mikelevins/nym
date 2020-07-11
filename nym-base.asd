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
                                                             (:file "files")
                                                             (:file "samples")))
                                       (:module "generators"
                                                :serial t
                                                :components ((:file "triples-travesty")
                                                             (:file "portmanteau")))))))


;;; (asdf:load-system :nym-base)
