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
    :depends-on (:alexandria :folio2)
    :components ((:module "base"
                          :serial t
                          :components ((:file "nym-base-package")
                                       (:module "util"
                                                :serial t
                                                :components ((:file "bind")
                                                             (:file "sequences")
                                                             (:file "strings")
                                                             (:file "files")
                                                             (:file "samples")))
                                       (:module "generators"
                                                :serial t
                                                :components ((:file "chunk-travesty")
                                                             (:file "portmanteau")
                                                             (:file "phonotactic")
                                                             (:file "segmenter")))))))


;;; (asdf:load-system :nym-base)
