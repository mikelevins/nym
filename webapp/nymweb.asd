;;;; ***********************************************************************
;;;;
;;;; Name:          nymweb.asd
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       system definitions: web version of nym
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(require :asdf)

(asdf:defsystem #:nymweb
    :description "nym: a customizable name generator"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache-2.0"
    :serial t
    :version (:read-file-form "../version.lisp")
    :depends-on (:alexandria :folio3+ :command-line-arguments)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "utils"))))
  :build-operation asdf/bundle:program-op
  :build-pathname "nym"
  :entry-point "nym::main")


;;; (asdf:load-system :nymweb)
